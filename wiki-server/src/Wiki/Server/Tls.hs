-- | The public listener's TLS: the certificate and private key given by
-- @--tls-cert@ and @--tls-key@, read once as the server starts, and the
-- settings Warp serves HTTPS with ("Network.Wai.Handler.WarpTLS").
--
-- The files are PEM. The certificate file holds the server's certificate
-- first and the chain after it, if any, as a certificate authority hands
-- them out; the key file holds the private key. One @.pem@ with both the
-- certificate and the key may be given as both arguments: the certificates
-- are read from its CERTIFICATE blocks, the key from its PRIVATE KEY block,
-- and every other block is skipped.
--
-- The files are read here, before any socket is bound, so that a
-- configuration the server cannot serve with stops it at once, saying
-- which file is wrong: one that cannot be read, one with no certificate or
-- no key in it, or a key that is not the certificate's. TLS itself would
-- only fail at the first handshake, after the deploy has reported the
-- server running. A renewed certificate is read by the next start.
--
-- The listener also takes plain HTTP on the same port ('allowingPlain'),
-- so that a reader who types the address without @https://@ is sent to
-- it ('httpsOnly') rather than shown warp-tls's refusal. With @--hsts@,
-- every answer over TLS carries @Strict-Transport-Security@.
module Wiki.Server.Tls
  ( loadTls
  , keyMatches

    -- * Plain HTTP on the TLS port
  , allowingPlain
  , HttpsOnly (..)
  , httpsOnly
  , redirectHost
  )
where

import Crypto.PubKey.DSA qualified as DSA
import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.PubKey.Ed448 qualified as Ed448
import Crypto.PubKey.RSA qualified as RSA
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.X509 (Certificate (..), CertificateChain (..), PrivKey (..), PrivKeyEC (..), PubKey (..), PubKeyEC (..), getCertificate)
import Data.X509.EC (ecPrivKeyCurve, ecPubKeyCurve, unserializePoint)
import Network.HTTP.Types (hContentType, hLocation, status308, status400)
import Network.TLS (Credentials (..), credentialLoadX509ChainFromMemory)
import Network.Wai (Middleware, Request (..), mapResponseHeaders, responseLBS)
import Network.Wai.Handler.WarpTLS (OnInsecure (..), TLSSettings (..), defaultTlsSettings)
import Protolude
import Wiki.Core.Url (Url (..))
import Wiki.Server.Address (Origin (..), addressOrigin, parseOrigin, showOrigin)

-- | The settings the public listener serves HTTPS with, from a certificate
-- file and a key file (possibly the same file); or why the server cannot
-- serve with them, naming the file. TLS 1.2 and 1.3 are offered, with
-- warp-tls's ciphers.
loadTls :: FilePath -> FilePath -> IO (Either Text TLSSettings)
loadTls certFile keyFile = runExceptT $ do
  certBytes <- readPem "certificate" certFile
  keyBytes <- readPem "private key" keyFile
  (chain, key) <- case credentialLoadX509ChainFromMemory certBytes [] keyBytes of
    Left _ -> throwError ("The TLS private key " <> toS keyFile <> " holds no private key in PEM (a -----BEGIN PRIVATE KEY----- block)")
    Right credential -> pure credential
  leaf <- case chain of
    CertificateChain (signed : _) -> pure (getCertificate signed)
    CertificateChain [] -> throwError ("The TLS certificate " <> toS certFile <> " holds no certificate in PEM (a -----BEGIN CERTIFICATE----- block)")
  unless (keyMatches leaf.certPubKey key) $
    throwError ("The TLS private key " <> toS keyFile <> " is not the key of the certificate " <> toS certFile <> ": give the key the certificate was issued for")
  pure defaultTlsSettings {tlsCredentials = Just (Credentials [(chain, key)])}
  where
    readPem what file =
      liftIO (try @IOException (BS.readFile file))
        >>= either (\e -> throwError ("The TLS " <> what <> " " <> toS file <> " cannot be read: " <> show e)) pure

-- | Whether a private key is the one of this public key: the public key
-- worked out from the private one is the certificate's. RSA, DSA, elliptic
-- curves, Ed25519 and Ed448 are checked so. For the kinds left (X25519 and
-- X448, which cannot sign, and ML-DSA) only the kind is compared, and a key
-- of the right kind that does not match fails at the first handshake.
keyMatches :: PubKey -> PrivKey -> Bool
keyMatches public private = case (public, private) of
  (PubKeyRSA pub, PrivKeyRSA priv) -> RSA.private_pub priv == pub
  (PubKeyDSA pub, PrivKeyDSA priv) ->
    DSA.public_params pub == DSA.private_params priv
      && DSA.public_y pub == DSA.calculatePublic (DSA.private_params priv) (DSA.private_x priv)
  (PubKeyEC pub, PrivKeyEC priv) -> fromMaybe False $ do
    curve <- ecPrivKeyCurve priv
    pubCurve <- ecPubKeyCurve pub
    point <- unserializePoint curve (pubkeyEC_pub pub)
    pure (curve == pubCurve && generateQ curve (privkeyEC_priv priv) == point)
  (PubKeyEd25519 pub, PrivKeyEd25519 priv) -> Ed25519.toPublic priv == pub
  (PubKeyEd448 pub, PrivKeyEd448 priv) -> Ed448.toPublic priv == pub
  (PubKeyX25519 _, PrivKeyX25519 _) -> True
  (PubKeyX448 _, PrivKeyX448 _) -> True
  (PubKeyMLDSA44 _, PrivKeyMLDSA44 _) -> True
  (PubKeyMLDSA65 _, PrivKeyMLDSA65 _) -> True
  (PubKeyMLDSA87 _, PrivKeyMLDSA87 _) -> True
  _ -> False

--------------------------------------------------------------------------------
-- Plain HTTP on the TLS port

-- | TLS settings that also take plain HTTP on the same port, which
-- warp-tls refuses by default. A plain request then reaches the
-- application with 'isSecure' false, and 'httpsOnly' must stand in front
-- of the application to answer it: the two go together.
allowingPlain :: TLSSettings -> TLSSettings
allowingPlain settings = settings {onInsecure = AllowInsecure}

-- | 'HttpsOnly' is what 'httpsOnly' needs to know about the server.
data HttpsOnly = HttpsOnly
  { hsts :: Bool
  -- ^ Whether answers over TLS carry @Strict-Transport-Security@: 'True'
  -- with @--hsts@.
  , publicUrl :: IO (Maybe Url)
  -- ^ The public address set in the administration's Server section, read
  -- when a plain request's @Host@ cannot be used.
  }

-- | 'httpsOnly' is the public listener's guard when the listener speaks
-- TLS and also takes plain HTTP ('allowingPlain'). A request that came
-- over TLS goes to the application, and with 'hsts' its answer carries
-- @Strict-Transport-Security: max-age=31536000@ (one year; not for
-- subdomains, since the wiki may share its domain with hosts it does not
-- own). A request that did not come over TLS never reaches the
-- application: it is answered @308 Permanent Redirect@ to the same
-- address over @https://@, or @400@ when there is no host to send it to.
-- So no cookie is read or set, and no page, API or WebSocket is served,
-- over plain HTTP. 308 keeps the method and the body, so a form sent over
-- plain HTTP is sent again to the HTTPS address.
--
-- Only the connection decides: a header such as @X-Forwarded-Proto@ is
-- not believed here, because no proxy stands in front of a server that
-- terminates TLS itself, and a client could send it to skip the redirect.
httpsOnly :: HttpsOnly -> Middleware
httpsOnly settings app request respond
  | isSecure request = app request (respond . withHsts)
  | otherwise = do
      host <- case redirectHost request of
        Just o -> pure (Just o)
        Nothing -> (>>= publicHost) <$> settings.publicUrl
      respond $ case host of
        Just o ->
          let location = encodeUtf8 (showOrigin o) <> target request
           in responseLBS status308 [(hLocation, location), plainText] ("This wiki is served over HTTPS only: " <> toS location <> "\n")
        Nothing -> responseLBS status400 [plainText] "This wiki is served over HTTPS only, and the request names no host to send it to: open the wiki's address with https:// in front.\n"
  where
    withHsts
      | settings.hsts = mapResponseHeaders (("Strict-Transport-Security", "max-age=31536000") :)
      | otherwise = identity
    plainText = (hContentType, "text/plain; charset=utf-8")

-- | The HTTPS origin a plain request is sent to, from its @Host@: the host
-- and port the reader typed, with @https@ in front. A wiki is often reached
-- by more than one name (its public one, and the machine's own on the
-- local network or a tailnet; 'Wiki.Server.Address.ownOrigins'), and the
-- name in @Host@ is one this listener was reached by, so the reader stays
-- on it. The public address might not be reachable from where the reader
-- is, and a session cookie is kept per host. A false @Host@ sends only its
-- own sender away.
--
-- The header is checked strictly: a host name (letters, digits, @-@, @.@,
-- @_@), an IPv4 address or an IPv6 address in brackets, then an optional
-- port from 1 to 65535, and nothing else. Anything else is 'Nothing'.
-- The port stays as it was written: none stays none, which is 443 over
-- HTTPS.
redirectHost :: Request -> Maybe Origin
redirectHost request = do
  raw <- requestHeaderHost request
  host <- rightToMaybe (decodeUtf8' raw)
  guard (T.length host <= 255)
  let name = if "[" `T.isPrefixOf` host then T.takeWhile (/= ']') host <> "]" else T.takeWhile (/= ':') host
      allowed
        | "[" `T.isPrefixOf` name = T.all (\c -> isHexDigit c || c `elem` (":.[]" :: [Char])) name
        | otherwise = T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("-._" :: [Char]))) name
  guard (not (T.null name) && allowed)
  parseOrigin ("https://" <> host)

-- | The HTTPS origin of the public address: its host, and its port, but
-- for an address over plain HTTP on port 80, whose HTTPS port is 443. Its
-- path is not taken: the request's path is kept.
publicHost :: Url -> Maybe Origin
publicHost u = do
  o <- addressOrigin u.text
  pure o {scheme = "https", port = if o.scheme == "http" && o.port == 80 then 443 else o.port}

-- | The path and query a plain request asked for, to ask again over
-- HTTPS. A target that is not a path (the @*@ of @OPTIONS *@), or has
-- bytes an address does not carry as they are (spaces, control
-- characters, bytes beyond ASCII), is sent to @/@ instead.
target :: Request -> ByteString
target request
  | "/" `BS.isPrefixOf` asked && BC.all (\c -> c > ' ' && c < '\DEL') asked = asked
  | otherwise = "/"
  where
    asked = rawPathInfo request <> rawQueryString request

-- | The public listener over TLS ("Wiki.Server.Tls"): a server started with
-- a certificate and key the test makes with @openssl@, reached over HTTPS.
-- Its API, its session cookie (@Secure@), its feed's addresses (@https@
-- without a proxy's header) and its live channel, over TLS; its admin
-- listener, still plain HTTP. A key that is not the certificate's, a file
-- that cannot be read and a file with no certificate stop the server
-- before it listens, naming the file; one @.pem@ holding both is taken as
-- both. Without @openssl@ on the PATH these tests are pending.
--
-- Plain HTTP on the TLS port is redirected to HTTPS, and answers over TLS
-- carry @Strict-Transport-Security@: the guard in front of the
-- application ('httpsOnly') called in process ('httpsSpec'), and once
-- over a real socket. A server without a certificate, behind a proxy or
-- not, does neither.
module Test.Server.Tls (spec, keySpec, httpsSpec) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (lookup)
import Data.Text qualified as T
import Network.Connection (ConnectionParams (..), connectTo, connectionClose, connectionGetChunk, connectionPut, initConnectionContext)
import Network.HTTP.Client (Request (redirectCount, requestHeaders), defaultManagerSettings, httpLbs, newManager, parseRequest, responseHeaders, responseStatus)
import Network.HTTP.Types (HeaderName, ResponseHeaders, hHost, hLocation, hSetCookie, methodGet, methodOptions, methodPost, status200, statusCode)
import Network.Wai (Application, defaultRequest, responseLBS, responseToStream)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as Stream
import Options.Applicative (defaultPrefs, execParserPure, getParseResult, info)
import Protolude
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Database (Backing)
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Server.Feed (children, parsed, textOf)
import Test.Server.Fixture
import Wiki.Api.Events (ClientMessage (..), ServerMessage (..), eventsPath)
import Wiki.Core.Id (PageId (..))
import Wiki.Core.Url (Url (..))
import Wiki.Server (ServerError (..))
import Wiki.Server.Options (ServeOptions (..), serveOptions)
import Wiki.Server.Options qualified as Options
import Wiki.Server.Tls (HttpsOnly (..), httpsOnly, loadTls)

spec :: Backing -> Spec
spec backing = describe "the public listener over TLS" $ do
  overHttps backing
  plainOnTheTlsPort backing
  refused backing

-- | A certificate and its key, as files the test made.
data Pair = Pair
  { cert :: FilePath
  , key :: FilePath
  }

-- | A self-signed certificate and its key for @localhost@, made by
-- @openssl@ in a directory of the test's, of the kind given (@rsa:2048@,
-- @ec@, @ed25519@), under a name: what an operator would make to try the
-- server with.
makePair :: FilePath -> Text -> Text -> IO Pair
makePair dir kind name = do
  let pair = Pair {cert = dir </> toS name <> ".crt", key = dir </> toS name <> ".key"}
      curve = if kind == "ec" then ["-pkeyopt", "ec_paramgen_curve:P-256"] else []
  (code, _, err) <-
    readProcessWithExitCode
      "openssl"
      (["req", "-x509", "-newkey", toS kind] <> curve <> ["-nodes", "-subj", "/CN=localhost", "-days", "2", "-keyout", pair.key, "-out", pair.cert])
      ""
  when (code /= ExitSuccess) $ assertFailure ("openssl could not make a certificate: " <> err)
  pure pair

-- | Run a test with a directory to make certificates in, or mark it
-- pending where there is no @openssl@ to make them with.
withOpenssl :: (FilePath -> IO ()) -> IO ()
withOpenssl test =
  findExecutable "openssl" >>= \case
    Nothing -> pendingWith "openssl is not on the PATH, so no certificate can be made for the test"
    Just _ -> withSystemTempDirectory "wiki-server-tls" test

-- | The options of a server whose public listener speaks TLS with a pair.
serving :: FilePath -> FilePath -> ServeOptions -> ServeOptions
serving cert key o = o {tlsCert = Just cert, tlsKey = Just key}

overHttps :: Backing -> Spec
overHttps backing = describe "with a certificate of its own" $ do
  it "serves the API over HTTPS with a Secure cookie, and the admin listener over plain HTTP" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    withDemoRunning backing (serving pair.cert pair.key) $ \r -> do
      r.public `shouldSatisfy` T.isPrefixOf "https://127.0.0.1:"
      r.admin `shouldSatisfy` T.isPrefixOf "http://127.0.0.1:"
      infoReply <- getUrl r (r.public <> "/api/v1/info")
      infoReply.status `shouldBe` 200
      -- No Strict-Transport-Security without --hsts.
      lookup hstsHeader infoReply.headers `shouldBe` Nothing
      ((.status) <$> getUrl r (r.admin <> "/admin/api/v1/admin/status")) `shouldReturn` 200
      -- The cookie is Secure without --secure-cookies: a server that
      -- speaks TLS is reached over HTTPS only.
      givePassword r "user-admin" "admin's password"
      reply <- passwordSignIn r (\l -> r.public <> "/" <> l) [] "admin" "admin's password"
      reply.status `shouldBe` 200
      (snd <$> setCookie reply) `shouldSatisfy` maybe False (elem "Secure")

  it "gives the feed's addresses under https, with no proxy to say so" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    withDemoRunning backing (serving pair.cert pair.key) $ \r -> do
      atom <- getUrl r (r.public <> "/s/blog/feed.xml")
      atom.status `shouldBe` 200
      root <- parsed (decodeUtf8 (BSL.toStrict atom.body))
      map textOf (children "id" root) `shouldBe` [r.public <> "/s/blog/"]

  it "answers on the live channel over TLS" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    withDemoRunning backing (serving pair.cert pair.key) $ \r -> secureChannel r $ \c -> do
      WS.sendTextData c (encode (Subscribe (PageId "no-such-page")))
      timeout 5_000_000 (WS.receiveData c) >>= \case
        Nothing -> expectationFailure "no answer on the live channel"
        Just bytes -> eitherDecode bytes `shouldBe` Right (Refused (PageId "no-such-page"))

  it "takes one .pem that holds the certificate and the key as both" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    let both = dir </> "server.pem"
    BS.writeFile both =<< ((<>) <$> BS.readFile pair.cert <*> BS.readFile pair.key)
    withRunningAs backing (serving both both) $ \r ->
      ((.status) <$> getUrl r (r.public <> "/api/v1/info")) `shouldReturn` 200

-- | The live channel of a server whose public listener speaks TLS: a
-- WebSocket over a TLS connection, accepting the test's certificate.
secureChannel :: Running -> (WS.Connection -> IO a) -> IO a
secureChannel r app = do
  tlsContext <- initConnectionContext
  let port = fromMaybe (panic ("no port in " <> r.public)) (readMaybe (T.takeWhileEnd (/= ':') r.public))
      params = ConnectionParams {connectionHostname = "127.0.0.1", connectionPort = port, connectionUseSecure = Just trustingTls, connectionUseSocks = Nothing}
  bracket (connectTo tlsContext params) connectionClose $ \connection -> do
    let receive = (\chunk -> if BS.null chunk then Nothing else Just chunk) <$> connectionGetChunk connection
    stream <- Stream.makeStream receive (traverse_ (connectionPut connection . BSL.toStrict))
    WS.runClientWithStream stream "127.0.0.1" ("/" <> toS eventsPath) WS.defaultConnectionOptions [] app

-- | Plain HTTP on the port of a server that speaks TLS, over real
-- sockets: redirected to the same address over HTTPS, while with
-- @--hsts@ its answers over HTTPS carry @Strict-Transport-Security@. A
-- server without a certificate, here one behind a proxy (Secure cookies,
-- a proxy's @X-Forwarded-Proto@), answers plain HTTP itself, with neither.
plainOnTheTlsPort :: Backing -> Spec
plainOnTheTlsPort backing = describe "plain HTTP on its port" $ do
  it "is redirected to HTTPS, and with --hsts the HTTPS answers carry Strict-Transport-Security" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    withDemoRunning backing (\o -> (serving pair.cert pair.key o) {Options.hsts = True}) $ \r -> do
      let address = fromMaybe (panic ("not https: " <> r.public)) (T.stripPrefix "https://" r.public)
      plainManager <- newManager defaultManagerSettings
      asked <- parseRequest (toS ("http://" <> address <> "/api/v1/info?x=1"))
      -- A session cookie the server does not know: over HTTPS the wiki
      -- answers it with a cookie that clears it, over plain HTTP nothing
      -- of the wiki reads it.
      let stale = ("Cookie", "wiki_session=stale")
      response <- httpLbs asked {redirectCount = 0, requestHeaders = [stale]} plainManager
      statusCode (responseStatus response) `shouldBe` 308
      lookup hLocation (responseHeaders response) `shouldBe` Just (encodeUtf8 (r.public <> "/api/v1/info?x=1"))
      lookup hSetCookie (responseHeaders response) `shouldBe` Nothing
      overHttpsReply <- getUrlWith r (r.public <> "/api/v1/info") [stale]
      overHttpsReply.status `shouldBe` 200
      lookup hSetCookie overHttpsReply.headers `shouldSatisfy` isJust
      lookup hstsHeader overHttpsReply.headers `shouldBe` Just "max-age=31536000"

  it "is answered as it is by a server without a certificate, behind a proxy too, with no Strict-Transport-Security" $
    withDemoRunning backing (\o -> o {secureCookies = True}) $ \r -> do
      proxied <- getUrlWith r (r.public <> "/api/v1/info") [("X-Forwarded-Proto", "https")]
      proxied.status `shouldBe` 200
      lookup hstsHeader proxied.headers `shouldBe` Nothing
      direct <- getUrl r (r.public <> "/api/v1/info")
      direct.status `shouldBe` 200
      lookup hLocation direct.headers `shouldBe` Nothing
      lookup hstsHeader direct.headers `shouldBe` Nothing

-- | The header that tells browsers to reach this host over HTTPS only.
hstsHeader :: HeaderName
hstsHeader = "Strict-Transport-Security"

-- | The guard of a listener that speaks TLS ('httpsOnly'), in front of an
-- application that counts its calls and sets a cookie, called in process:
-- no server, no database.
httpsSpec :: Spec
httpsSpec = describe "the guard of a listener that speaks TLS" $ do
  it "sends a plain request to the same host, port, path and query over HTTPS, without calling the wiki" $ do
    (calls, app) <- guardedApp True Nothing
    for_ [methodGet, methodPost] $ \method -> do
      (status, headers, body) <- answer app (plain method (Just "wiki.example.org:8443") "/s/blog/p/hello" "?q=a%20b&x=1")
      status `shouldBe` 308
      lookup hLocation headers `shouldBe` Just "https://wiki.example.org:8443/s/blog/p/hello?q=a%20b&x=1"
      lookup hSetCookie headers `shouldBe` Nothing
      lookup hstsHeader headers `shouldBe` Nothing
      body `shouldBe` "This wiki is served over HTTPS only: https://wiki.example.org:8443/s/blog/p/hello?q=a%20b&x=1\n"
    -- A proxy's header does not make a plain request secure: no proxy
    -- stands in front of a server that speaks TLS itself.
    (forwarded, _, _) <- answer app (withHeader ("X-Forwarded-Proto", "https") (plain methodGet (Just "wiki.example.org") "/" ""))
    forwarded `shouldBe` 308
    readIORef calls `shouldReturn` 0
    -- The same application is called, and sets its cookie, for a request
    -- over TLS: the checks above can fail.
    (secureStatus, secureHeaders, _) <- answer app (overTls (plain methodGet (Just "wiki.example.org:8443") "/" ""))
    secureStatus `shouldBe` 200
    lookup hSetCookie secureHeaders `shouldBe` Just "wiki-session=token; Secure"
    readIORef calls `shouldReturn` 1

  it "keeps the port of the Host as it was written, none included, and takes IPv6 addresses" $ do
    (_, app) <- guardedApp True Nothing
    let locationFor host = (\(_, hs, _) -> lookup hLocation hs) <$> answer app (plain methodGet (Just host) "/x" "")
    locationFor "wiki.example.org" `shouldReturn` Just "https://wiki.example.org/x"
    locationFor "Wiki.Example.org:8443" `shouldReturn` Just "https://wiki.example.org:8443/x"
    locationFor "box.tail1234.ts.net:8443" `shouldReturn` Just "https://box.tail1234.ts.net:8443/x"
    locationFor "192.168.1.20:8443" `shouldReturn` Just "https://192.168.1.20:8443/x"
    locationFor "[::1]:8443" `shouldReturn` Just "https://[::1]:8443/x"

  it "refuses a Host that is not a host and a port, with 400 when no public address is set" $ do
    (calls, app) <- guardedApp True Nothing
    for_ badHosts $ \host -> do
      (status, headers, body) <- answer app (plain methodGet (Just host) "/x" "")
      (host, status) `shouldBe` (host, 400)
      lookup hLocation headers `shouldBe` Nothing
      body `shouldBe` "This wiki is served over HTTPS only, and the request names no host to send it to: open the wiki's address with https:// in front.\n"
    (status, _, _) <- answer app (plain methodGet Nothing "/x" "")
    status `shouldBe` 400
    readIORef calls `shouldReturn` 0

  it "takes the public address's host and port, not its path, when the Host cannot be used" $ do
    let locationWith public host = do
          (_, app) <- guardedApp True (Just public)
          (\(_, hs, _) -> lookup hLocation hs) <$> answer app (plain methodGet host "/s/blog/" "?a=1")
    locationWith "https://wiki.example.org/team/" Nothing `shouldReturn` Just "https://wiki.example.org/s/blog/?a=1"
    locationWith "https://wiki.example.org:8443/" (Just "a@evil.example") `shouldReturn` Just "https://wiki.example.org:8443/s/blog/?a=1"
    -- An address over plain HTTP on its own port is the HTTPS one on 443.
    locationWith "http://wiki.example.org/" Nothing `shouldReturn` Just "https://wiki.example.org/s/blog/?a=1"
    locationWith "http://wiki.example.org:8443/" Nothing `shouldReturn` Just "https://wiki.example.org:8443/s/blog/?a=1"
    -- A usable Host comes first: the reader stays on the name typed.
    locationWith "https://wiki.example.org/" (Just "box.tail1234.ts.net:8443") `shouldReturn` Just "https://box.tail1234.ts.net:8443/s/blog/?a=1"

  it "sends a target that is not a path, or has bytes an address does not carry, to the root" $ do
    (_, app) <- guardedApp True Nothing
    let locationOf method path = (\(_, hs, _) -> lookup hLocation hs) <$> answer app (plain method (Just "wiki.example.org") path "")
    locationOf methodOptions "*" `shouldReturn` Just "https://wiki.example.org/"
    locationOf methodGet "/a\tb" `shouldReturn` Just "https://wiki.example.org/"
    locationOf methodGet "/caf\195\169" `shouldReturn` Just "https://wiki.example.org/"

  it "gives every answer over TLS Strict-Transport-Security with --hsts, and none without" $ do
    (_, withHsts) <- guardedApp True Nothing
    (_, onHeaders, _) <- answer withHsts (overTls (plain methodGet (Just "wiki.example.org") "/" ""))
    lookup hstsHeader onHeaders `shouldBe` Just "max-age=31536000"
    (_, withoutHsts) <- guardedApp False Nothing
    (_, offHeaders, _) <- answer withoutHsts (overTls (plain methodGet (Just "wiki.example.org") "/" ""))
    lookup hstsHeader offHeaders `shouldBe` Nothing

  it "is turned on by --hsts, and off by default" $ do
    let parsedHsts args = (.hsts) <$> getParseResult (execParserPure defaultPrefs (info serveOptions mempty) args)
    parsedHsts ["--data", "d"] `shouldBe` Just False
    parsedHsts ["--data", "d", "--hsts"] `shouldBe` Just True
    parsedHsts ["--data", "d", "--no-hsts"] `shouldBe` Nothing
  where
    badHosts =
      [ ""
      , "a@b"
      , "a/b"
      , "a b"
      , "a\r\nb"
      , "h:"
      , "h:0"
      , "h:99999"
      , "h:8a"
      , ":8443"
      , "wiki.example.org:8443/evil"
      , "wiki.example.org?x"
      , "a!b"
      , "[::1"
      , "[::1]x"
      , "x:80@y"
      , "caf\195\169.example"
      , "\255"
      ]

-- | The guard with HSTS on or off and a public address or none, in front
-- of an application that counts its calls and answers 200 with a cookie.
guardedApp :: Bool -> Maybe Text -> IO (IORef Int, Application)
guardedApp hstsOn public = do
  calls <- newIORef 0
  let inner _ respond = do
        modifyIORef' calls (+ 1)
        respond (responseLBS status200 [(hSetCookie, "wiki-session=token; Secure")] "the page")
  pure (calls, httpsOnly HttpsOnly {hsts = hstsOn, publicUrl = pure (Url <$> public)} inner)

-- | A request as it arrives over plain HTTP: its method, its @Host@ if
-- any, its path and its query, as raw bytes.
plain :: ByteString -> Maybe ByteString -> ByteString -> ByteString -> Wai.Request
plain method host path query =
  defaultRequest
    { Wai.requestMethod = method
    , Wai.requestHeaderHost = host
    , Wai.requestHeaders = [(hHost, h) | Just h <- [host]]
    , Wai.rawPathInfo = path
    , Wai.rawQueryString = query
    , Wai.isSecure = False
    }

-- | The request as it arrives over TLS.
overTls :: Wai.Request -> Wai.Request
overTls request = request {Wai.isSecure = True}

-- | The request with one more header.
withHeader :: (HeaderName, ByteString) -> Wai.Request -> Wai.Request
withHeader h request = request {Wai.requestHeaders = h : Wai.requestHeaders request}

-- | An application's answer to a request: its status, headers and body.
answer :: Application -> Wai.Request -> IO (Int, ResponseHeaders, LByteString)
answer app request = do
  got <- newIORef Nothing
  _ <- app request $ \response -> do
    let (status, headers, withBody) = responseToStream response
    body <- newIORef mempty
    withBody $ \stream -> stream (\chunk -> modifyIORef' body (<> chunk)) pass
    bytes <- toLazyByteString <$> readIORef body
    writeIORef got (Just (statusCode status, headers, bytes))
    pure ResponseReceived
  readIORef got >>= maybe (panic "the application did not answer") pure

refused :: Backing -> Spec
refused backing = describe "does not start" $ do
  let starts :: (ServeOptions -> ServeOptions) -> IO (Maybe [Text])
      starts change = (withRunningAs backing change (const pass) $> Nothing) `catch` \(ServerError reasons) -> pure (Just reasons)
      saying text = maybe False (any (text `T.isInfixOf`))

  it "with a key that is not the certificate's, and says which files" $ withOpenssl $ \dir -> do
    mine <- makePair dir "rsa:2048" "one"
    other <- makePair dir "rsa:2048" "other"
    starts (serving mine.cert other.key)
      >>= (`shouldSatisfy` saying ("The TLS private key " <> toS other.key <> " is not the key of the certificate " <> toS mine.cert))

  it "with a file it cannot read, or one with no certificate in it, and says which" $ withOpenssl $ \dir -> do
    pair <- makePair dir "rsa:2048" "server"
    starts (serving (dir </> "missing.crt") pair.key) >>= (`shouldSatisfy` saying ("The TLS certificate " <> toS (dir </> "missing.crt") <> " cannot be read"))
    starts (serving pair.cert (dir </> "missing.key")) >>= (`shouldSatisfy` saying ("The TLS private key " <> toS (dir </> "missing.key") <> " cannot be read"))
    starts (serving pair.key pair.key) >>= (`shouldSatisfy` saying ("The TLS certificate " <> toS pair.key <> " holds no certificate"))
    starts (serving pair.cert pair.cert) >>= (`shouldSatisfy` saying ("The TLS private key " <> toS pair.cert <> " holds no private key"))

  it "with --tls-cert and no --tls-key, or the other way round" $ do
    starts (\o -> o {tlsCert = Just "server.crt"}) >>= (`shouldSatisfy` saying "--tls-cert needs --tls-key")
    starts (\o -> o {tlsKey = Just "server.key"}) >>= (`shouldSatisfy` saying "--tls-key needs --tls-cert")

-- | Keys of the kinds certificates are issued for: each matches its own
-- certificate and not another's of its kind, and the key is the first
-- certificate's when a chain follows it. No server, no database.
keySpec :: Spec
keySpec = describe "a TLS certificate's key" $ do
  it "is told apart from another of its kind, for RSA, elliptic curves and Ed25519" $ withOpenssl $ \dir ->
    for_ ["rsa:2048", "ec", "ed25519"] $ \kind -> do
      mine <- makePair dir kind (T.filter isAlphaNum kind <> "-one")
      other <- makePair dir kind (T.filter isAlphaNum kind <> "-other")
      matched <- loadTls mine.cert mine.key
      for_ (leftToMaybe matched) $ \why -> expectationFailure (toS kind <> ": its own key is refused: " <> toS why)
      mismatched <- loadTls mine.cert other.key
      when (isRight mismatched) $ expectationFailure (toS kind <> ": another certificate's key is taken")

  it "is the first certificate's, when the chain follows it in the file" $ withOpenssl $ \dir -> do
    authority <- makePair dir "rsa:2048" "authority"
    let run args = readProcessWithExitCode "openssl" args "" >>= \(code, _, err) -> when (code /= ExitSuccess) (assertFailure ("openssl: " <> err))
        leaf = Pair {cert = dir </> "leaf.crt", key = dir </> "leaf.key"}
        chained = dir </> "chained.crt"
    run ["req", "-newkey", "rsa:2048", "-nodes", "-subj", "/CN=localhost", "-keyout", leaf.key, "-out", dir </> "leaf.csr"]
    run ["x509", "-req", "-in", dir </> "leaf.csr", "-CA", authority.cert, "-CAkey", authority.key, "-CAcreateserial", "-days", "2", "-out", leaf.cert]
    BS.writeFile chained =<< ((<>) <$> BS.readFile leaf.cert <*> BS.readFile authority.cert)
    matched <- loadTls chained leaf.key
    for_ (leftToMaybe matched) $ \why -> expectationFailure ("the leaf's key is refused: " <> toS why)
    authorityKey <- loadTls chained authority.key
    when (isRight authorityKey) $ expectationFailure "the key of the chain's authority is taken for the leaf's"

-- | The wiki's absolute addresses, as a reader outside the server follows
-- them: the addresses a feed, a sitemap and a link preview give, which
-- must be absolute (WP4.14 of docs/PLAN.md).
--
-- The wiki's public address is the one set in the panel's Server section
-- (WP4.4), which is the address the wiki is known by behind a proxy or a
-- domain name. Without one, the request's own is used: its @Host@ (or the
-- @X-Forwarded-Host@ a proxy in front sets), the scheme a proxy says it was
-- reached by (@X-Forwarded-Proto@), else the listener's own (@https@ when
-- it speaks TLS, @http@ when not), and the path where the listener serves
-- the wiki.
--
-- The wiki's own origins are decided here too, once, for the checks that
-- need them: the origin a passkey must be made on ('ownOrigin',
-- "Wiki.Server.Vault") and the origins the live channel's WebSocket may
-- come from ('ownOrigins', "Wiki.Server.Events"). Two origins are compared
-- as a browser compares them ('Origin'), so that a port written out on one
-- side only, or a host in capitals, does not make them differ.
module Wiki.Server.Address
  ( publicAddress
  , requestOrigin
  , addressOf
  , hrefAddress

    -- * The wiki's origin
  , Origin (..)
  , parseOrigin
  , showOrigin
  , ownOrigin
  , ownOrigins
  , fromOwnOrigin
  )
where

import Data.CaseInsensitive qualified as CI
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types (hOrigin)
import Network.URI (URI (..), URIAuth (..), escapeURIString, isAllowedInURI, parseAbsoluteURI)
import Network.Wai (Request (..))
import Protolude
import Wiki.Api.ServerSettings (ServerSettingsOf (..))
import Wiki.Core.Url (Url (..))
import Wiki.Server.Storage (Storage, readServerSettings)

-- | The wiki's public address for a request to a listener that serves the
-- wiki at this path, always ending in a slash.
publicAddress :: Storage -> Text -> Request -> IO Text
publicAddress storage mount request = do
  settings <- readServerSettings storage
  pure (maybe (requestAddress mount request) (\u -> u.text) settings.publicUrl)

-- | The address a request names the wiki by: its host, as the browser or
-- a proxy in front of the server requested it, under the listener's path.
requestAddress :: Text -> Request -> Text
requestAddress mount request = requestOrigin request <> mount

-- | The origin a request names the wiki by: the scheme and the host, as
-- the browser or a proxy in front of the server requested them. Without a
-- proxy's @X-Forwarded-Proto@, the scheme is the connection's: @https@ on
-- a listener that speaks TLS ('isSecure').
requestOrigin :: Request -> Text
requestOrigin request = fromMaybe (requestScheme request <> "://localhost") (requestOriginIfNamed request)

-- | 'requestOrigin' when the request names a host (in @X-Forwarded-Host@,
-- else @Host@), or 'Nothing' when it names none. A browser always sends
-- @Host@; a check of the wiki's origin refuses a request without one
-- rather than guess.
requestOriginIfNamed :: Request -> Maybe Text
requestOriginIfNamed request = (\h -> requestScheme request <> "://" <> firstOf h) <$> (header request "X-Forwarded-Host" <|> (decodeUtf8With lenientDecode <$> requestHeaderHost request))

-- | The scheme a request names the wiki by. A proxy says @https@ for a
-- request that came over TLS, and some say @wss@ for a WebSocket that
-- did.
requestScheme :: Request -> Text
requestScheme request = case T.toLower . firstOf <$> header request "X-Forwarded-Proto" of
  Just proto | proto `elem` ["https", "wss"] -> "https"
  Just _ -> "http"
  Nothing -> if isSecure request then "https" else "http"

-- | A request's header, as text. A proxy may list several values, one
-- per proxy, of which the first is the browser's ('firstOf').
header :: Request -> ByteString -> Maybe Text
header request name = decodeUtf8With lenientDecode <$> lookup (CI.mk name) (requestHeaders request)

-- | The first of a header's values, separated by commas.
firstOf :: Text -> Text
firstOf = T.strip . T.takeWhile (/= ',')

-- | An address in the wiki (a path relative to the wiki's path, as the
-- API's links are, @api\/v1\/…@), under its public address.
addressOf :: Text -> Text -> Text
addressOf base path = base <> T.dropWhile (== '/') path

-- | An address the router wrote, under the listener's path, moved under
-- the public address instead.
hrefAddress :: Text -> Text -> Text -> Text
hrefAddress base mount href = addressOf base (fromMaybe href (T.stripPrefix mount href))

--------------------------------------------------------------------------------
-- The wiki's origin

-- | 'Origin': where a page comes from, as a browser compares two pages:
-- the scheme (@http@ or @https@), the host in lower case, and the port.
-- The port is always written here: when an address leaves it out, it is
-- the scheme's own (443 for https, 80 for http). So
-- @https:\/\/Wiki.Example.org@ and @https:\/\/wiki.example.org:443@ are
-- the same origin, and @https:\/\/wiki.example.org:8443@ is another.
data Origin = Origin
  { scheme :: Text
  , host :: Text
  , port :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | The origin an @Origin@ header names, as a browser writes it:
-- @scheme:\/\/host@ with an optional port, and nothing else (no user, no
-- path, no query). Anything else is 'Nothing', which a check refuses: the
-- @null@ a sandboxed frame sends, too.
parseOrigin :: Text -> Maybe Origin
parseOrigin text = do
  uri <- parseAbsoluteURI (toS text)
  guard (null uri.uriPath && null uri.uriQuery && null uri.uriFragment)
  originOfUri uri

-- | The origin of an address that may go on with a path, such as the
-- public address (@https:\/\/wiki.example.org\/team\/@). Characters
-- that an address may not hold unescaped, which the settings let a path
-- have, are escaped first, so that they do not make the address
-- unreadable.
addressOrigin :: Text -> Maybe Origin
addressOrigin text = parseAbsoluteURI (escapeURIString isAllowedInURI (toS text)) >>= originOfUri

-- | The origin of an absolute address: its scheme, which must be @http@ or
-- @https@, and its host and port. An address with a user (@user\@host@),
-- an empty host, a host with a percent-escape, or a port that is not a
-- number from 1 to 65535 has none. A browser writes a host without
-- escapes (a name outside ASCII in its @xn--@ form), so a host with one
-- never matches a browser's origin.
originOfUri :: URI -> Maybe Origin
originOfUri uri = do
  scheme <- case T.toLower (toS uri.uriScheme) of
    "https:" -> Just "https"
    "http:" -> Just "http"
    _ -> Nothing
  authority <- uri.uriAuthority
  guard (null authority.uriUserInfo)
  let host = T.toLower (toS authority.uriRegName)
  guard (not (T.null host) && not ("%" `T.isInfixOf` host))
  port <- case authority.uriPort of
    "" -> Just (if scheme == "https" then 443 else 80)
    ':' : digits | not (null digits), all isDigit digits, length digits <= 5 -> mfilter (\p -> p >= 1 && p <= 65535) (readMaybe digits)
    _ -> Nothing
  pure Origin {scheme, host, port}

-- | An origin as a browser writes it: without the port when it is the
-- scheme's own, with it otherwise (@https:\/\/wiki.example.org:8443@).
showOrigin :: Origin -> Text
showOrigin o = o.scheme <> "://" <> o.host <> (if o.port == defaultPort then "" else ":" <> show o.port)
  where
    defaultPort = if o.scheme == "https" then 443 else 80

-- | The origin the request names ('requestOrigin': the host a proxy
-- passes on, else the @Host@, over the scheme a proxy reports, else the
-- listener's). 'Nothing' when the request names no host, or one that
-- cannot be read.
requestOwnOrigin :: Request -> Maybe Origin
requestOwnOrigin request = requestOriginIfNamed request >>= parseOrigin

-- | The wiki's one origin, the one a passkey is made on: the public
-- address's when one is given and can be read, else the one the request
-- names. The public listener gives the public address set in the
-- administration's Server section.
--
-- The settings store a public address that has no origin here: one with
-- a user (@https:\/\/user\@wiki.example.org\/@) or a port that is not a
-- number ('Wiki.Api.ServerSettings.parsePublicUrl' checks neither). Such
-- an address falls back to the request's origin, as before this module
-- read origins, so that passkeys registered there keep working.
ownOrigin :: Maybe Url -> Request -> Maybe Origin
ownOrigin publicUrl request = (publicUrl >>= \u -> addressOrigin u.text) <|> requestOwnOrigin request

-- | Every origin that is the wiki's own for a listener with this public
-- address: the one the request names, and the public address's when one
-- is given. A wiki is often reached by more than one address (its public
-- one, and the machine's own on the local network or a tailnet), and a
-- page served from either is the wiki's. The admin listener gives no
-- public address, because its pages are served from its own address.
-- Empty when neither can be read: then no origin is the wiki's.
ownOrigins :: Maybe Url -> Request -> [Origin]
ownOrigins publicUrl request = ordNub (catMaybes [requestOwnOrigin request, publicUrl >>= \u -> addressOrigin u.text])

-- | Whether a request comes from a page of one of the wiki's own origins
-- ('ownOrigins'), as far as its @Origin@ header tells: the header names
-- one of them, or the request has none. A browser sends @Origin@ with
-- every WebSocket upgrade and every request that changes something, so a
-- request without it does not come from a page; a program that sends none
-- (curl, the tests' clients) is taken. An @Origin@ that cannot be read,
-- such as the @null@ of a sandboxed frame, is refused.
fromOwnOrigin :: [Origin] -> Request -> Bool
fromOwnOrigin own request = case lookup hOrigin (requestHeaders request) of
  Nothing -> True
  Just sent -> case parseOrigin <$> decodeUtf8' sent of
    Right (Just o) -> o `elem` own
    _ -> False

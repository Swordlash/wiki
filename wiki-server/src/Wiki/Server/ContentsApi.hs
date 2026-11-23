-- | A page's table of contents and one section of a page, in the API for
-- programs (WP5.138 of docs/PLAN.md, the owner's issue #338): what a
-- program on a user's own machine (a local assistant, later an MCP server)
-- reads to take a part of a long page rather than all of it. Both sign in
-- as the API for a page's Markdown does ("Wiki.Server.MarkdownApi"), with
-- HTTP Basic alone ('signedInBasic'), and both only read.
--
-- = Addresses
--
-- * @GET api\/contents\/s\/\<space path>\/p\/\<page path>\/@ (a space's
--   home page at @api\/contents\/s\/\<space path>\/@): the page's title,
--   its version (the value of the Markdown's @ETag@), the address of its
--   Markdown, the address of its top when it has text before its first
--   heading, and its headings as a tree, each with its level, its text,
--   its anchor and the address of its section. As JSON.
-- * @GET api\/markdown\/…\/?section=\<anchor>@, at the page's Markdown
--   address: the source text of one section, a slice of the page's
--   Markdown character for character, as @text\/markdown@. Without more,
--   the section runs from its heading's line to the next heading of the
--   same or a higher level, its subsections included; with @&own=true@ it
--   stops at the next heading of any level. The anchor @~top@ is the text
--   before the first heading ('topAnchor'). The @ETag@ is the page's: a
--   section has no version of its own.
--
-- The sections and their anchors are "Wiki.Core.Markdown.Sections"', from
-- the same table of contents the page shows, so an anchor here is the
-- fragment of a link to the heading in the page.
--
-- A section is read at the page's own Markdown address, as a part of that
-- resource, rather than at an address of its own: the application's
-- dispatch sends a request there with @section@ in its query here
-- ('asksForSection'). Writing a section is not taken yet: a @PUT@ with
-- @section@ is 405, so that it never writes the section's text over the
-- whole page.
--
-- = Refusals
--
-- As the Markdown's: a page the user may not read is 404, as one that is
-- not there, and a page of an encrypted space is 409 @Stale@. An anchor
-- that names no section is 404, with the page's anchors in the message.
module Wiki.Server.ContentsApi
  ( contentsApplication
  , sectionApplication
  , asksForSection
  )
where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode, genericToEncoding, genericToJSON)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types (Header, hAllow, hCacheControl, hContentType, methodGet, methodHead, status200, status400, status404, status405, status500)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Application, Request (..), Response, responseLBS)
import Protolude
import Web.HttpApiData (toHeader)
import Wiki.Api.Version (pageVersion)
import Wiki.App.Backend (MonadWiki (getPage, getSpace), WikiError)
import Wiki.App.Route qualified as Route
import Wiki.Core.Id (UserId)
import Wiki.Core.Markdown.Sections (Outline (..), Section (..), outline, outlineAnchors, sectionRange, sectionText, topAnchor)
import Wiki.Core.Page (Page (..))
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (id))
import Wiki.Server.Api (Env)
import Wiki.Server.MarkdownApi (addressUnder, markdownMediaType, notAllowed, parameter, refusal, refusalWith, runAs, signedInBasic, wikiRefusal)
import Wiki.Server.Render (routeOf)

-- | A page's table of contents, as the API gives it. @top@ is left out of
-- the JSON for a page whose text before its first heading is blank.
data Contents = Contents
  { title :: Text
  , version :: Text
  -- ^ The page's version: the value of its Markdown's @ETag@, quotes
  -- included, to send back as @If-Match@.
  , markdown :: Text
  -- ^ The address of the page's Markdown.
  , top :: Maybe Text
  -- ^ The address of the text before the first heading.
  , headings :: [Heading]
  }
  deriving stock (Generic)

-- | A heading of the table of contents, with the headings of its section.
data Heading = Heading
  { level :: Int
  , text :: Text
  -- ^ The heading's text, without its markup.
  , anchor :: Text
  -- ^ The heading's id in the rendered page, and the name of its section.
  , section :: Text
  -- ^ The address of the heading's section.
  , children :: [Heading]
  }
  deriving stock (Generic)

-- | The JSON of the answers: fields in the order of the records, and a
-- field that is 'Nothing' left out.
contentsOptions :: Options
contentsOptions = defaultOptions {omitNothingFields = True}

instance ToJSON Contents where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

instance ToJSON Heading where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

-- | Whether a request to the API for a page's Markdown asks for one
-- section of the page: its query has @section@.
asksForSection :: Request -> Bool
asksForSection request = any ((== "section") . fst) (queryString request)

-- | The table of contents, on a listener that serves the wiki under this
-- path (with a slash at each end), for a request from this address.
contentsApplication :: Env -> Text -> Text -> Application
contentsApplication env mount address request respond =
  signedInBasic env address request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead]
      then respond (notAllowed [methodGet, methodHead])
      else
        pageAt env address u "api/contents" mount request >>= \case
          Left refused -> respond refused
          Right (r, p) -> withOutline respond p $ \o ->
            let markdown = addressUnder mount "api/markdown" r
                sectionAt anchor = markdown <> "?section=" <> decodeUtf8 (urlEncode True (encodeUtf8 anchor))
                heading s = Heading {level = s.level, text = s.text, anchor = s.id, section = sectionAt s.id, children = map heading s.below}
                contents =
                  Contents
                    { title = p.title
                    , version = decodeUtf8 (toHeader (pageVersion p))
                    , markdown
                    , top = guard (T.any (not . isSpace) (sectionText p.body (0, o.top))) $> sectionAt topAnchor
                    , headings = map heading o.sections
                    }
             in respond (responseLBS status200 [(hContentType, "application/json"), etag p, private] (encode contents))

-- | One section of a page, for a request to the API for a page's Markdown
-- that asks for one ('asksForSection'), on a listener that serves the wiki
-- under this path, from this address.
sectionApplication :: Env -> Text -> Text -> Application
sectionApplication env mount address request respond =
  signedInBasic env address request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead]
      then respond writeRefused
      else case (anchorOf, own) of
        (Left why, _) -> respond (refusal status400 "Invalid" (Just why))
        (_, Left why) -> respond (refusal status400 "Invalid" (Just why))
        (Right anchor, Right ownOnly) ->
          pageAt env address u "api/markdown" mount request >>= \case
            Left refused -> respond refused
            Right (_, p) -> withOutline respond p $ \o -> case sectionRange o anchor ownOnly of
              Nothing ->
                respond . refusal status404 "NotFound" . Just $
                  "This page has no section with the anchor " <> show anchor <> ". Its anchors: " <> T.intercalate ", " (outlineAnchors o) <> "."
              Just bounds -> respond (responseLBS status200 [(hContentType, markdownMediaType), etag p, private] (toS (encodeUtf8 (sectionText p.body bounds))))
  where
    -- The anchor of the section asked for, from @section@.
    anchorOf :: Either Text Text
    anchorOf =
      parameter request "section"
        >>= maybe (Left ("Give the section's anchor: ?section=anchor, one of the anchors of the page's contents, or " <> topAnchor <> ".")) Right
    -- Whether only the section's own part is asked for: @own@ alone, or
    -- @own=true@; @own=false@ and no @own@ ask for the section whole. Read
    -- here rather than by 'parameter', which takes @own@ alone for no
    -- @own@.
    own :: Either Text Bool
    own = case lookup "own" (queryString request) of
      Nothing -> Right False
      Just Nothing -> Right True
      Just (Just "true") -> Right True
      Just (Just "false") -> Right False
      Just (Just _) -> Left "The parameter own is true or false."

-- | The page a request's address names after the API's prefix (@api\/contents@,
-- @api\/markdown@), with its route, as the user may see it; or the refusal
-- to answer with. The address is read by the application's own router, as
-- the Markdown's is, without its query.
pageAt :: Env -> Text -> UserId -> Text -> Text -> Request -> IO (Either Response (Route.Route, Page))
pageAt env address u prefix mount request = do
  r <- routeOf "/" (fromMaybe "/" (BS.stripPrefix (encodeUtf8 (mount <> prefix)) (rawPathInfo request)))
  case r of
    Route.Page sp pp -> found r <$> pageOf sp pp
    Route.Space sp -> found r <$> pageOf sp []
    _ -> pure (Left (refusal status404 "NotFound" Nothing))
  where
    found r = bimap wikiRefusal (r,)
    pageOf :: [Slug] -> [Slug] -> IO (Either WikiError Page)
    pageOf sp pp = runAs env address u (getSpace sp) >>= either (pure . Left) (\s -> runAs env address u (getPage s.id pp))

-- | Go on with the outline of a page's Markdown, or answer 500 for a
-- page that has none: 'outline' finds none only for a text that does not
-- parse, or whose headings it cannot place in the text, which its tests
-- say does not happen.
withOutline :: (Response -> IO a) -> Page -> (Outline -> IO a) -> IO a
withOutline respond p continue = case outline p.body of
  Just o -> continue o
  Nothing -> respond (refusal status500 "Internal" (Just "The page's headings could not be found in its text."))

-- | The answer to a method other than a read at a section's address: 405,
-- since a section cannot be written yet. The request is not passed on to
-- the page's Markdown, whose PUT would write the body over the whole page.
writeRefused :: Response
writeRefused = refusalWith status405 [(hAllow, "GET, HEAD")] "Invalid" (Just "A section cannot be written yet: write the page whole, at its address without ?section=.")

-- | The answer's header with the page's version.
etag :: Page -> Header
etag p = ("ETag", toHeader (pageVersion p))

-- | The header that keeps an answer out of shared caches: it is what one
-- user may read.
private :: Header
private = (hCacheControl, "private, no-store")

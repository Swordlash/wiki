-- | A page's table of contents and one section of a page, in the API for
-- programs (WP5.138 of docs/PLAN.md, the owner's issue #338): what a
-- program on a user's own machine (a local assistant, later an MCP server)
-- reads to take a part of a long page rather than all of it, and writes to
-- change that part alone. Both sign in as the API for a page's Markdown
-- does ("Wiki.Server.MarkdownApi"), with HTTP Basic or an API key
-- ('signedInProgram'): a key with the right @read@ reads, and a write
-- needs one with the right @write@.
--
-- = Addresses
--
-- * @GET api\/contents\/s\/\<space path>\/p\/\<page path>\/@ (a space's
--   home page at @api\/contents\/s\/\<space path>\/@): the page's title,
--   its version (the value of the Markdown's @ETag@), the address of its
--   Markdown, the address of its top when it has text before its first
--   heading, and its headings as a tree, each with its level, its text,
--   its anchor, whether it is inside a block spoiler, and the address of
--   its section. As JSON.
-- * @GET api\/markdown\/…\/?section=\<anchor>@, at the page's Markdown
--   address: the source text of one section, a slice of the page's
--   Markdown character for character, as @text\/markdown@. Without more,
--   the section runs from its heading's line to the next heading of the
--   same or a higher level, its subsections included; with @&own=true@ it
--   stops at the next heading of any level. The @ETag@ is the page's: a
--   section has no version of its own.
-- * @GET api\/markdown\/…\/?top=true@, at the same address: the text
--   before the first section, the same way. It is a parameter of its own,
--   not a value of @section@, since a heading's id written out
--   (@{id="~top"}@) can be any text: every value of @section@ is an
--   anchor. Both together are 400.
-- * @PUT api\/markdown\/…\/?section=\<anchor>@ (and @&own=true@, and
--   @?top=true@): the body, UTF-8 Markdown, takes the place of that part
--   of the page ('replaceSection'), and the page is saved through the same
--   'savePage' as a whole page's @PUT@: a version in the history, the push,
--   the notifications, the same refusals. An @If-Match@ with the page's
--   version makes the write conditional (412 when the page has moved on);
--   without it the body is written in the page as it is when the request
--   comes. Either way the save is made against the version the body was
--   written in, so that a save in between is never written over: without
--   @If-Match@ the page is read again and the body written again, a few
--   times. @If-None-Match@ is 400: a section is never created. A body that
--   would change any section outside the part, or that is not that one
--   section, is 422 with a message that says what would change, and
--   nothing is saved. The answer is 200 with JSON: the section's anchor
--   (the body may change its heading) and address, or the top's address,
--   and the page's new version, which is also the @ETag@.
--
-- The sections and their anchors are "Wiki.Core.Markdown.Sections"', from
-- the same table of contents the page shows, so an anchor here is the
-- fragment of a link to the heading in the page. A section never crosses a
-- spoiler's edge: a block spoiler is one block of the section it is in,
-- and the headings inside it have sections of their own within the
-- spoiler's body. In the tree, such a heading is a child of the heading
-- whose section holds the spoiler (or at the top level, for a spoiler
-- before the first heading), marked @inSpoiler@, with the headings of its
-- own section inside the spoiler as its children.
--
-- A section is read and written at the page's own Markdown address, as a
-- part of that resource, rather than at an address of its own: the
-- application's dispatch sends a request there with @section@ or @top@ in
-- its query here ('asksForSection'), so that a @PUT@ with either never
-- writes its body over the whole page, nor makes a page where there is
-- none. Other methods are 405.
--
-- = Refusals
--
-- As the Markdown's: a page the user may not read is 404, as one that is
-- not there, a write to a page the user may not edit is 403 whatever its
-- body (the right is asked before the body is judged), and a page of
-- an encrypted space is 409 @Stale@. An anchor that names no section is
-- 404, with the page's anchors in the message.
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
import Network.HTTP.Types (Header, hCacheControl, hContentType, hIfMatch, hIfNoneMatch, methodGet, methodHead, methodPut, status200, status400, status404, status412, status422)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Application, Request (..), Response, responseLBS, strictRequestBody)
import Protolude
import Web.HttpApiData (parseHeader, toHeader)
import Wiki.Api.Version (PageVersion, pageVersion)
import Wiki.App.Backend (MonadWiki (getPage, getSpace, savePage), WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Id (UserId)
import Wiki.Core.Markdown.Sections (Outline (..), Refusal (..), Replaced (..), Section (..), SpoilerBody (..), Target (..), outline, outlineAnchors, replaceSection, sectionRange, sectionText, topRange)
import Wiki.Core.Page (Page (..))
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (id))
import Wiki.Server.Api (Env (..))
import Wiki.Server.MarkdownApi (Asking (..), addressUnder, markdownMediaType, notAllowed, parameter, refusal, runAs, signedInProgram, wikiRefusal)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (Who (..), editingAs)

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
  , inSpoiler :: Bool
  -- ^ Whether the heading is inside a block spoiler, its section within
  -- the spoiler's body.
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
-- section of the page, or its top: its query has @section@ or @top@.
asksForSection :: Request -> Bool
asksForSection request = any ((`elem` ["section", "top"]) . fst) (queryString request)

-- | The table of contents, on a listener that serves the wiki under this
-- path (with a slash at each end), for a request the listener knows this
-- of ('Asking').
contentsApplication :: Env -> Text -> Asking -> Application
contentsApplication env mount asking@Asking {address} request respond =
  signedInProgram env asking request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead]
      then respond (notAllowed [methodGet, methodHead])
      else
        pageAt env address u "api/contents" mount request >>= \case
          Left refused -> respond refused
          Right (r, p) ->
            let o = outline p.body
                markdown = addressUnder mount "api/markdown" r
                -- A section's heading, with the headings of the spoilers
                -- in its own part, then those of its subsections.
                heading hidden s = Heading {level = s.level, text = s.text, anchor = s.id, inSpoiler = hidden, section = sectionAt markdown s.id, children = concatMap spoilerHeadings s.spoilers <> map (heading hidden) s.below}
                -- The headings of a spoiler's body: those of the spoilers
                -- in its top, then its sections'.
                spoilerHeadings sb = concatMap spoilerHeadings sb.outline.spoilers <> map (heading True) sb.outline.sections
                contents =
                  Contents
                    { title = p.title
                    , version = versionOf p
                    , markdown
                    , top = guard (T.any (not . isSpace) (sectionText p.body (topRange o))) $> topAt markdown
                    , headings = concatMap spoilerHeadings o.spoilers <> map (heading False) o.sections
                    }
             in respond (responseLBS status200 [(hContentType, "application/json"), etag p, private] (encode contents))

-- | One section of a page, for a request to the API for a page's Markdown
-- that asks for one ('asksForSection'), on a listener that serves the wiki
-- under this path, for a request the listener knows this of ('Asking').
sectionApplication :: Env -> Text -> Asking -> Application
sectionApplication env mount asking@Asking {address} request respond =
  signedInProgram env asking request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead, methodPut]
      then respond (notAllowed [methodGet, methodHead, methodPut])
      else case (partOf, own) of
        (Left why, _) -> respond (refusal status400 "Invalid" (Just why))
        (_, Left why) -> respond (refusal status400 "Invalid" (Just why))
        (Right asked, Right ownOnly)
          | requestMethod request == methodPut -> write u (maybe Top (if ownOnly then OwnPart else WholeSection) asked)
          | otherwise ->
              pageAt env address u "api/markdown" mount request >>= \case
                Left refused -> respond refused
                Right (_, p) ->
                  let o = outline p.body
                   in case maybe (Just (topRange o)) (\anchor -> sectionRange o anchor ownOnly) asked of
                        Nothing -> respond (refusal status404 "NotFound" (Just (noSection (fromMaybe "" asked) o)))
                        Just bounds -> respond (responseLBS status200 [(hContentType, markdownMediaType), etag p, private] (toS (encodeUtf8 (sectionText p.body bounds))))
  where
    -- A PUT: the request's headers and body read, then the part written.
    write u target
      | isJust (lookup hIfNoneMatch (requestHeaders request)) =
          respond (refusal status400 "Invalid" (Just "If-None-Match does not go with a section: a section is written in a page that is there. Send If-Match with the page's version, or neither."))
      | otherwise = case traverse (parseHeader @PageVersion) (lookup hIfMatch (requestHeaders request)) of
          Left why -> respond (refusal status400 "Invalid" (Just ("The If-Match header is not a page's version from an ETag: " <> why)))
          Right expected ->
            decodeUtf8' . toS <$> strictRequestBody request >>= \case
              Left _ -> respond (refusal status400 "Invalid" (Just "The body is not UTF-8 text."))
              Right body -> save u target expected body (3 :: Int)
    -- Read the page, write the body in place of the part
    -- ('replaceSection'), and save the page against the version read, as a
    -- whole page's PUT does. A save between that read and this one is a
    -- conflict: with If-Match the answer is 412; without it the page is
    -- read again and the body written in the page as it is now, a few
    -- times, so that the other save is never written over.
    save u target expected body tries =
      pageAt env address u "api/markdown" mount request >>= \case
        Left refused -> respond refused
        Right (r, p)
          | Just v <- expected, v /= pageVersion p -> respond moved
          | otherwise ->
              -- The right to edit the page is asked before the body is
              -- judged, by the rule the save asks first ('editingAs'), so
              -- that a write the save would refuse (no right to edit, a
              -- conversion of the space running) is refused as a whole
              -- page's PUT refuses it, whatever the body. The save asks
              -- it again.
              editingAs env.storage (AsUser u) p.id >>= \case
                Left e -> respond (wikiRefusal e)
                Right _ -> case replaceSection target p.body body of
                  Left NoSuchAnchor -> respond (refusal status404 "NotFound" (Just (noSection (anchorOf target) (outline p.body))))
                  Left why -> respond (refusal status422 "Invalid" (Just (refusalText why)))
                  Right done ->
                    runAs env address u (savePage p.id p.updatedAt p.title done.text) >>= \case
                      Right saved ->
                        let markdown = addressUnder mount "api/markdown" r
                            answer =
                              Written
                                { anchor = done.anchor
                                , section = sectionAt markdown <$> done.anchor
                                , top = guard (target == Top) $> topAt markdown
                                , version = versionOf saved
                                }
                         in respond (responseLBS status200 [(hContentType, "application/json"), etag saved, (hCacheControl, "no-store")] (encode answer))
                      Left (Conflict _)
                        | isJust expected -> respond moved
                        | tries > 1 -> save u target expected body (tries - 1)
                      Left e -> respond (wikiRefusal e)
    moved = refusal status412 "Conflict" (Just "The page has been saved since the version in If-Match: read it again, and write over that version.")
    anchorOf = \case
      WholeSection a -> a
      OwnPart a -> a
      Top -> ""
    -- The part asked for: a section by its anchor, from @section@, or the
    -- top ('Nothing'), from @top@.
    partOf :: Either Text (Maybe Text)
    partOf = do
      anchor <- parameter request "section"
      top <- flag "top"
      case (anchor, top) of
        (Just _, True) -> Left "Ask for a section (?section=anchor) or for the text before the first heading (?top=true), not both."
        (Just a, False) -> Right (Just a)
        (Nothing, True) -> Right Nothing
        (Nothing, False) -> Left "Give the section's anchor, ?section=anchor, one of the anchors of the page's contents, or ask for the text before the first heading with ?top=true."
    noSection anchor o =
      "This page has no section with the anchor "
        <> show anchor
        <> ". Its anchors: "
        <> (if null (outlineAnchors o) then "none" else T.intercalate ", " (outlineAnchors o))
        <> ". The text before the first heading is at ?top=true."
    -- Whether only the section's own part is asked for, @own@: alone or
    -- @own=true@; @own=false@ and no @own@ ask for the section whole.
    own :: Either Text Bool
    own = flag "own"
    -- A parameter that is true alone, or @=true@, and false as @=false@ or
    -- left out. Read here rather than by 'parameter', which takes a
    -- parameter alone for one left out.
    flag :: ByteString -> Either Text Bool
    flag name = case lookup name (queryString request) of
      Nothing -> Right False
      Just Nothing -> Right True
      Just (Just "true") -> Right True
      Just (Just "false") -> Right False
      Just (Just _) -> Left ("The parameter " <> decodeUtf8 name <> " is true or false.")

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

-- | The answer to a section written: the section's anchor and address, or
-- the address of the top, and the page's new version. Fields that are
-- 'Nothing' are left out of the JSON.
data Written = Written
  { anchor :: Maybe Text
  -- ^ The section's anchor, which the body's heading may have changed.
  , section :: Maybe Text
  -- ^ The address of the section, under its anchor now.
  , top :: Maybe Text
  -- ^ The address of the top, for a write of the top.
  , version :: Text
  -- ^ The page's new version, the value of the @ETag@.
  }
  deriving stock (Generic)

instance ToJSON Written where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

-- | The address of a section, by its anchor, at the address of the page's
-- Markdown.
sectionAt :: Text -> Text -> Text
sectionAt markdown anchor = markdown <> "?section=" <> decodeUtf8 (urlEncode True (encodeUtf8 anchor))

-- | The address of the top, at the address of the page's Markdown.
topAt :: Text -> Text
topAt markdown = markdown <> "?top=true"

-- | A page's version as the answers' JSON gives it: the value of its
-- @ETag@, quotes included, to send back as @If-Match@.
versionOf :: Page -> Text
versionOf p = decodeUtf8 (toHeader (pageVersion p))

-- | Why a body was refused, in plain words, for the 422's message.
refusalText :: Refusal -> Text
refusalText = \case
  NoSuchAnchor -> "This page has no section with that anchor."
  EmptyBody -> "The body is empty: a section cannot be deleted through this address. Send the section's heading and its text, or write the page whole."
  NotAHeading level -> "The body must start with a level " <> show level <> " heading (" <> T.replicate level "#" <> " …), as the section does, on its first line."
  AnotherSection text level -> "The body has a second heading of level " <> show level <> ", " <> quoted text <> ", after its own: write one section at a time, with its subsections under it."
  Subsection text -> "With own=true the body is the section's heading and its own text, without subsections, and it has the heading " <> quoted text <> ". Leave own=true out to write the section with its subsections."
  SectionInTop text -> "The text before the first heading has no heading, and the body has " <> quoted text <> "."
  AnchorTaken anchor -> "The anchor " <> quoted anchor <> " is already an earlier heading's or element's, and a link to it would lead there: give the heading another id."
  AnchorChanges anchor new -> "The id " <> quoted anchor <> " of a heading or another element outside the section would change to " <> quoted new <> ", since a heading of the body has the same text as that one: an id is what links lead to. Give the body's heading another text, or an id of its own ({#id})."
  AnchorMoves anchor -> "The id " <> quoted anchor <> " is a heading's or another element's outside the section, and an element of the body has it too: links to it would lead into the body instead. Give the body's element another id."
  SectionChanges anchor after -> "The body would change the section " <> quoted anchor <> place after <> ": check for a code fence or a spoiler left open, or a spoiler's end marker."
  SectionAppears text after -> "The body would make a section of the heading " <> quoted text <> place after <> ": check for a spoiler's end marker."
  SpoilerChanges after -> "The body would change the spoiler " <> (if after then "that follows" else "around the section") <> ": check for a spoiler's end marker, or a spoiler or a code fence left open."
  OutsideChanges -> "The body would change the page outside the section: check for a code fence or a spoiler left open, or a spoiler's end marker."
  where
    quoted t = "'" <> t <> "'"
    place after = if after then " that follows" else " before it"

-- | The answer's header with the page's version.
etag :: Page -> Header
etag p = ("ETag", toHeader (pageVersion p))

-- | The header that keeps an answer out of shared caches: it is what one
-- user may read.
private :: Header
private = (hCacheControl, "private, no-store")

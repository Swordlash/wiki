{-# LANGUAGE DeriveAnyClass #-}

-- | A page's table of contents and one section of a page, in the API for
-- programs (WP5.138 of docs/PLAN.md, the owner's issue #338): what a
-- program on a user's own machine (a local assistant, later an MCP server)
-- reads to take a part of a long page rather than all of it, and writes to
-- change that part alone. Both sign in as the API for a page's Markdown
-- does ("Wiki.Server.MarkdownApi"), with HTTP Basic or an API key
-- ('signedInProgram'): a key with the right @read@ reads, and a write
-- needs one with the right @write@. Their addresses are endpoints of the
-- servant type of the API for programs ("Wiki.Server.ProgramApi"), and
-- this module has their handlers.
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
--   and the page's new version, which is also the @ETag@. The MCP
--   server's @wiki_write_section@ writes through the same function
--   ('putSection').
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
-- part of that resource, rather than at an address of its own: a request
-- there with @section@ or @top@ in its query is answered here
-- ('readMarkdown', 'writeMarkdown'), so that a @PUT@ with either never
-- writes its body over the whole page, nor makes a page where there is
-- none.
--
-- = Refusals
--
-- As the Markdown's: a page the user may not read is 404, as one that is
-- not there, a write to a page the user may not edit is 403 whatever its
-- body (the right is asked before the body is judged), and a page of
-- an encrypted space is 409 @Stale@. An anchor that names no section is
-- 404, with the page's anchors in the message.
module Wiki.Server.ContentsApi
  ( -- * The handlers
    ContentsAnswer (..)
  , readContents
  , readMarkdown
  , MarkdownWrite (..)
  , WriteAnswers
  , SectionWrite (..)
  , writeMarkdown
  , refuseOtherMethods

    -- * A page's contents and sections
  , Contents (..)
  , Heading (..)
  , Written (..)
  , contentsOf
  , sectionOf

    -- * Writing a part of a page
  , SectionPut (..)
  , putSection
  , refusalText
  )
where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, genericToEncoding, genericToJSON)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.SOP (I (..), NP (..))
import Data.Text qualified as T
import Generics.SOP qualified as SOP
import Network.HTTP.Types (status400, status404, status412, status422)
import Network.HTTP.Types.URI (parseQuery, urlEncode)
import Protolude hiding (Handler)
import Servant (Handler, Header)
import Servant.API.MultiVerb (AsHeaders (..), AsUnion, GenericAsUnion (..), RespondAs, RespondEmpty, WithHeaders)
import Web.HttpApiData (toHeader)
import Wiki.Api.Version (PageVersion, pageVersion)
import Wiki.App.Backend (MonadWiki (savePage), WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Id (UserId)
import Wiki.Core.Markdown.Sections (Outline (..), Refusal (..), Replaced (..), Section (..), SpoilerBody (..), Target (..), outline, outlineAnchors, replaceSection, sectionRange, sectionText, topRange)
import Wiki.Core.Page (Page (..))
import Wiki.Core.Slug (Slug)
import Wiki.Server.Api (Env (..))
import Wiki.Server.MarkdownApi (NewPageAt, PageText (..), addressUnder, findPage, pageAt, parameter, pathAfter, privateNoStore, readPage, runAs, writePage)
import Wiki.Server.ProgramApi.Combinators (CacheControl (..), Given, Json, Sent (..), Version (..), refusal, refuse, wikiRefusal)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (Who (..), editingAs)

-- | A page's table of contents, as the API gives it. @top@ is left out of
-- the JSON for a page whose text before its first heading is blank. The
-- addresses (@markdown@, @top@ and each heading's @section@) are the HTTP
-- API's, and are left out of the MCP server's ("Wiki.Server.McpApi"),
-- whose tools name a page by its path and a section by its anchor.
data Contents = Contents
  { title :: Text
  , version :: Text
  -- ^ The page's version: the value of its Markdown's @ETag@, quotes
  -- included, to send back as @If-Match@.
  , markdown :: Maybe Text
  -- ^ The address of the page's Markdown.
  , top :: Maybe Text
  -- ^ The address of the text before the first heading.
  , headings :: [Heading]
  }
  deriving stock (Eq, Show, Generic)

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
  , section :: Maybe Text
  -- ^ The address of the heading's section.
  , children :: [Heading]
  }
  deriving stock (Eq, Show, Generic)

-- | The JSON of the answers: fields in the order of the records, and a
-- field that is 'Nothing' left out. The document's schemas follow it.
contentsOptions :: Options
contentsOptions = defaultOptions {omitNothingFields = True}

instance ToJSON Contents where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

instance ToSchema Contents where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions contentsOptions)

instance ToJSON Heading where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

instance ToSchema Heading where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions contentsOptions)

-- | 'ContentsAnswer' is the answer of the table of contents: the contents,
-- and the page's version, which the answer gives as its @ETag@.
data ContentsAnswer = ContentsAnswer
  { contents :: Contents
  , version :: PageVersion
  }

instance AsHeaders '[Version, CacheControl] Contents ContentsAnswer where
  toHeaders a = (I (Version a.version) :* I privateNoStore :* Nil, a.contents)
  fromHeaders (I (Version v) :* _ :* Nil, c) = ContentsAnswer {contents = c, version = v}

-- | @GET@ of the table of contents, on a listener that serves the wiki
-- under this path, for a request from this address, sent as this.
readContents :: Env -> Text -> Text -> UserId -> Sent -> Handler ContentsAnswer
readContents env mount address u sent =
  pageAt env address u (pathAfter mount "api/contents" sent.path) <&> \(r, p) ->
    ContentsAnswer {contents = contentsOf (Just (addressUnder mount "api/markdown" r)) p, version = pageVersion p}

-- | 'contentsOf' is a page's table of contents: its title, its version,
-- and its headings as a tree. The HTTP API ('readContents') and the MCP
-- server ("Wiki.Server.McpApi") both give it, so that a heading has the
-- same anchor in both. With the address of the page's Markdown, each part
-- has the address it is read at; without it, none has.
contentsOf :: Maybe Text -> Page -> Contents
contentsOf markdown p =
  Contents
    { title = p.title
    , version = versionOf p
    , markdown
    , top = guard (T.any (not . isSpace) (sectionText p.body (topRange o))) *> (topAt <$> markdown)
    , headings = concatMap spoilerHeadings o.spoilers <> map (heading False) o.sections
    }
  where
    o = outline p.body
    -- A section's heading, with the headings of the spoilers in its own
    -- part, then those of its subsections.
    heading hidden s = Heading {level = s.level, text = s.text, anchor = s.id, inSpoiler = hidden, section = (`sectionAt` s.id) <$> markdown, children = concatMap spoilerHeadings s.spoilers <> map (heading hidden) s.below}
    -- The headings of a spoiler's body: those of the spoilers in its top,
    -- then its sections'.
    spoilerHeadings sb = concatMap spoilerHeadings sb.outline.spoilers <> map (heading True) sb.outline.sections

-- | 'sectionOf' is the source text of one part of a page's Markdown, a
-- slice of it character for character: the section whose heading has
-- this anchor, or the text before the first heading ('Nothing'). A section
-- runs to the next heading of the same or a higher level, its subsections
-- included, or with only its own part ('True') to the next heading of any
-- level. 'Nothing' when the page has no section with the anchor. The HTTP
-- API ('readMarkdown') and the MCP server ("Wiki.Server.McpApi") both read
-- a section through it.
sectionOf :: Text -> Maybe Text -> Bool -> Maybe Text
sectionOf body asked ownOnly = sectionText body <$> maybe (Just (topRange o)) (\anchor -> sectionRange o anchor ownOnly) asked
  where
    o = outline body

-- | Whether a request to the address of a page's Markdown asks for one
-- section of the page, or its top: its query has @section@ or @top@, with
-- a value or without one.
asksForSection :: Given -> Given -> Bool
asksForSection section top = isJust section || isJust top

-- | @GET@ at the address of a page's Markdown, on a listener that serves
-- the wiki under this path, for a request from this address, sent as
-- this, with these parameters: one part of the page when the request asks
-- for one ('asksForSection'), or else the whole page's Markdown
-- ("Wiki.Server.MarkdownApi"' 'readPage').
readMarkdown :: Env -> Text -> Text -> UserId -> Sent -> Given -> Given -> Given -> Handler PageText
readMarkdown env mount address u sent section own top
  | not (asksForSection section top) = readPage env address u (pathAfter mount "api/markdown" sent.path <> sent.query)
  | otherwise = case (partOf section top, flag "own" own) of
      (Left why, _) -> refuse (refusal status400 "Invalid" (Just why))
      (_, Left why) -> refuse (refusal status400 "Invalid" (Just why))
      (Right asked, Right ownOnly) -> do
        (_, p) <- pageAt env address u (pathAfter mount "api/markdown" sent.path)
        case sectionOf p.body asked ownOnly of
          Nothing -> refuse (refusal status404 "NotFound" (Just (noSection (fromMaybe "" asked) (outline p.body))))
          Just part -> pure PageText {text = part, version = pageVersion p}

-- | Before a method the address of a page's Markdown does not take is
-- refused (405, "Wiki.Server.ProgramApi.Combinators"' 'OtherMethods'), on
-- a listener that serves the wiki under this path, for a request sent as
-- this: an address that names no page, read by the application's router
-- with its query, is 404 for every method; one part of a page, asked by
-- @section@ or @top@, is refused for its method before its address is
-- read.
refuseOtherMethods :: Text -> Sent -> Handler ()
refuseOtherMethods mount sent
  | any ((`elem` ["section", "top"]) . fst) (parseQuery sent.query) = pass
  | otherwise =
      liftIO (routeOf "/" (pathAfter mount "api/markdown" sent.path <> sent.query)) >>= \case
        Route.Page _ _ -> pass
        Route.Space _ -> pass
        _ -> refuse (refusal status404 "NotFound" Nothing)

-- | 'MarkdownWrite' is what a @PUT@ at the address of a page's Markdown
-- answers: one part of the page written, a new page, or the whole page
-- saved, with its new version.
data MarkdownWrite
  = SectionWritten SectionWrite
  | PageCreated NewPageAt
  | PageSaved Version
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (AsUnion WriteAnswers) via GenericAsUnion WriteAnswers MarkdownWrite

-- | The answers of a @PUT@ at the address of a page's Markdown, in the
-- order of 'MarkdownWrite''s constructors.
type WriteAnswers =
  '[ WithHeaders '[Header "ETag" Version, Header "Cache-Control" CacheControl] SectionWrite (RespondAs Json 200 "A section written, or the text before the first heading: its anchor, which the body's heading may have changed, its address, and the page's new version, which is also the ETag." Written)
   , WithHeaders '[Header "Location" Text, Header "ETag" Version, Header "Cache-Control" CacheControl] NewPageAt (RespondEmpty 201 "A page created where there was none: its address is the Location, and its version the ETag.")
   , WithHeaders '[Header "ETag" Version] Version (RespondEmpty 204 "The page saved: its new version is the ETag.")
   ]

-- | 'SectionWrite' is the answer to a part of a page written: where it is
-- now, and the page's new version, which the answer gives as its @ETag@.
data SectionWrite = SectionWrite
  { written :: Written
  , version :: PageVersion
  }

instance AsHeaders '[Version, CacheControl] Written SectionWrite where
  toHeaders w = (I (Version w.version) :* I (CacheControl "no-store") :* Nil, w.written)
  fromHeaders (I (Version v) :* _ :* Nil, w) = SectionWrite {written = w, version = v}

-- | @PUT@ at the address of a page's Markdown, on a listener that serves
-- the wiki under this path, for a request from this address, sent as
-- this, whose segments after @api\/markdown@ are these, with these
-- parameters, headers and body: one part of the page when the request asks
-- for one ('asksForSection'), or else the whole page
-- ("Wiki.Server.MarkdownApi"' 'writePage').
writeMarkdown :: Env -> Text -> Text -> UserId -> Sent -> [Text] -> Given -> Given -> Given -> Given -> Given -> Maybe (Either Text Version) -> Maybe (Either Text Text) -> LByteString -> Handler MarkdownWrite
writeMarkdown env mount address u sent segments section own top title kind ifMatch ifNoneMatch given
  | not (asksForSection section top) =
      either PageCreated (PageSaved . Version) <$> writePage env mount address u (pathAfter mount "api/markdown" sent.path <> sent.query) segments title kind ifMatch ifNoneMatch given
  | otherwise = case (partOf section top, flag "own" own) of
      (Left why, _) -> refuse (refusal status400 "Invalid" (Just why))
      (_, Left why) -> refuse (refusal status400 "Invalid" (Just why))
      (Right asked, Right ownOnly)
        | isJust ifNoneMatch ->
            refuse (refusal status400 "Invalid" (Just "If-None-Match does not go with a section: a section is written in a page that is there. Send If-Match with the page's version, or neither."))
        | otherwise -> case traverse (fmap (\(Version v) -> v)) ifMatch of
            Left why -> refuse (refusal status400 "Invalid" (Just ("The If-Match header is not a page's version from an ETag: " <> why)))
            Right expected -> case decodeUtf8' (toS given) of
              Left _ -> refuse (refusal status400 "Invalid" (Just "The body is not UTF-8 text."))
              Right body -> SectionWritten <$> write (maybe Top (if ownOnly then OwnPart else WholeSection) asked) expected body
  where
    at = pathAfter mount "api/markdown" sent.path
    -- The route is read here, as 'pageAt' reads it, so that the answer's
    -- addresses are printed from the route the request named.
    write target expected body =
      liftIO (routeOf "/" at) >>= \case
        r@(Route.Page sp pp) -> answer r target =<< liftIO (putSection env address u sp pp target expected body)
        r@(Route.Space sp) -> answer r target =<< liftIO (putSection env address u sp [] target expected body)
        _ -> refuse (refusal status404 "NotFound" Nothing)
    answer r target = \case
      SectionSaved anchor saved ->
        let markdown = addressUnder mount "api/markdown" r
         in pure
              SectionWrite
                { written =
                    Written
                      { anchor
                      , section = sectionAt markdown <$> anchor
                      , top = guard (target == Top) $> topAt markdown
                      , version = versionOf saved
                      }
                , version = pageVersion saved
                }
      SectionMoved -> refuse (refusal status412 "Conflict" (Just "The page has been saved since the version in If-Match: read it again, and write over that version."))
      SectionNotThere anchor o -> refuse (refusal status404 "NotFound" (Just (noSection anchor o)))
      SectionRefused why -> refuse (refusal status422 "Invalid" (Just (refusalText why)))
      SectionPutRefused e -> refuse (wikiRefusal e)

-- | 'SectionPut' is what a write of one part of a page came to
-- ('putSection'). Only 'SectionSaved' wrote anything.
data SectionPut
  = -- | Saved: the anchor of the section written ('Nothing' for the top),
    -- which the body's heading may have changed, and the page after the
    -- save.
    SectionSaved (Maybe Text) Page
  | -- | The page has been saved since the version the write named.
    SectionMoved
  | -- | No section of the page has this anchor; the page's outline, for
    -- the anchors it has.
    SectionNotThere Text Outline
  | -- | 'replaceSection' refused the body: why.
    SectionRefused Refusal
  | -- | The wiki refused: not found, forbidden, an encrypted space, a
    -- conversion of the space running.
    SectionPutRefused WikiError

-- | 'putSection' writes one part of a page at a space's path and a page's
-- path, as a user, for a request from this address. The HTTP API's
-- section @PUT@ ('writeMarkdown') and the MCP server's @wiki_write_section@
-- ("Wiki.Server.McpApi") both write through it, so that the rule, the
-- checks, the save and the refusals are the same whoever asks.
--
-- The page is read, the body takes the place of the part
-- ('replaceSection'), and the page is saved against the version read,
-- through the same 'savePage' as a whole page's write. A version ('Just')
-- makes the write conditional: a page saved since is 'SectionMoved', and
-- nothing is saved. Without a version, a save between the read and this
-- save makes the page be read again and the body written in the page as
-- it is now, a few times, so that the other save is never written over.
putSection :: Env -> Text -> UserId -> [Slug] -> [Slug] -> Target -> Maybe PageVersion -> Text -> IO SectionPut
putSection env address u sp pp target expected body = attempt (3 :: Int)
  where
    attempt tries =
      findPage env address u sp pp >>= \case
        Left e -> pure (SectionPutRefused e)
        Right p
          | maybe False (/= pageVersion p) expected -> pure SectionMoved
          | otherwise ->
              -- The right to edit the page is asked before the body is
              -- judged, by the rule the save asks first ('editingAs'), so
              -- that a write the save would refuse (no right to edit, a
              -- conversion of the space running) is refused as a whole
              -- page's write is, whatever the body. The save asks it again.
              editingAs env.storage (AsUser u) p.id >>= \case
                Left e -> pure (SectionPutRefused e)
                Right _ -> case replaceSection target p.body body of
                  Left NoSuchAnchor -> pure (SectionNotThere (anchorOf target) (outline p.body))
                  Left why -> pure (SectionRefused why)
                  Right done ->
                    runAs env address u (savePage p.id p.updatedAt p.title done.text) >>= \case
                      Right saved -> pure (SectionSaved done.anchor saved)
                      Left (Conflict _)
                        | isJust expected -> pure SectionMoved
                        | tries > 1 -> attempt (tries - 1)
                      Left e -> pure (SectionPutRefused e)
    anchorOf = \case
      WholeSection a -> a
      OwnPart a -> a
      Top -> ""

-- | The part of a page a request asks for: a section by its anchor, from
-- @section@, or the top ('Nothing'), from @top@.
partOf :: Given -> Given -> Either Text (Maybe Text)
partOf section top = do
  anchor <- parameter "section" section
  above <- flag "top" top
  case (anchor, above) of
    (Just _, True) -> Left "Ask for a section (?section=anchor) or for the text before the first heading (?top=true), not both."
    (Just a, False) -> Right (Just a)
    (Nothing, True) -> Right Nothing
    (Nothing, False) -> Left "Give the section's anchor, ?section=anchor, one of the anchors of the page's contents, or ask for the text before the first heading with ?top=true."

-- | A parameter that is true alone, or @=true@, and false as @=false@ or
-- left out. Read here rather than by 'parameter', which takes a parameter
-- alone for one left out.
flag :: Text -> Given -> Either Text Bool
flag name = \case
  Nothing -> Right False
  Just Nothing -> Right True
  Just (Just "true") -> Right True
  Just (Just "false") -> Right False
  Just (Just _) -> Left ("The parameter " <> name <> " is true or false.")

-- | Why an anchor names no section of a page with this outline: the page's
-- anchors.
noSection :: Text -> Outline -> Text
noSection anchor o =
  "This page has no section with the anchor "
    <> show anchor
    <> ". Its anchors: "
    <> (if null (outlineAnchors o) then "none" else T.intercalate ", " (outlineAnchors o))
    <> ". The text before the first heading is at ?top=true."

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
  deriving stock (Eq, Show, Generic)

instance ToJSON Written where
  toJSON = genericToJSON contentsOptions
  toEncoding = genericToEncoding contentsOptions

instance ToSchema Written where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions contentsOptions)

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

-- | Why a body was refused, in plain words, for the 422's message and for
-- the MCP server's @wiki_write_section@.
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
  SectionChanges anchor after' -> "The body would change the section " <> quoted anchor <> place after' <> ": check for a code fence or a spoiler left open, or a spoiler's end marker."
  SectionAppears text after' -> "The body would make a section of the heading " <> quoted text <> place after' <> ": check for a spoiler's end marker."
  SpoilerChanges after' -> "The body would change the spoiler " <> (if after' then "that follows" else "around the section") <> ": check for a spoiler's end marker, or a spoiler or a code fence left open."
  BlockChanges anchor after' ->
    "The body would change how the page reads "
      <> maybe "in the text before the first heading" (\a -> "in the section " <> quoted a) anchor
      <> (if after' then ", after it" else ", before it")
      <> ": check for a link's definition ([name]: address) or a footnote's ([^name]: text) that the page uses there, or a code fence or a spoiler left open."
  HeadingSpoilerChanges -> "With own=true the heading cannot become or stop being a spoiler, or change its spoiler's name, while the section has subsections, which a heading spoiler hides with it: write the section whole, without own=true."
  OutsideChanges -> "The body would change the page outside the section: check for a code fence or a spoiler left open, or a spoiler's end marker."
  where
    quoted t = "'" <> t <> "'"
    place a = if a then " that follows" else " before it"

-- | The listing and the search of the API for programs (WP5.138 of
-- docs/PLAN.md): what a program on a user's own machine (a local
-- assistant, later an MCP server) reads to find the pages it then reads
-- and writes through the API for a page's Markdown
-- ("Wiki.Server.MarkdownApi"). Both sign in as that API does, with HTTP
-- Basic or an API key ('signedInProgram'), and answer JSON. They live at
-- @api\/list@ and @api\/search@, beside the application's own API under
-- @api\/v1\/@, whose later versions are @api\/v2\/@ and so on: neither
-- name can be one.
--
-- = Addresses
--
-- * @GET api\/list\/@: every space the user may read, from the top level
--   down, with their pages. @GET api\/list\/s\/\<space path>\/@: one space,
--   with its subspaces and its pages, as trees. The space is found as the
--   application finds it ('getSpace'): one the user may not read is 404,
--   as one that is not there. @?depth=N@ cuts the tree N levels below the
--   space listed (or the top level): 1 is the space's own subspaces and the
--   pages under its home page, without what is under them.
-- * @PUT api\/list\/s\/\<space path>\/@ creates the space at that path:
--   the path's last slug is its slug, and the slugs before it name its
--   parent, which has to exist. The body is JSON, @{"title": "…"}@, and may
--   be empty: the title is then made from the slug ('titleOfSlug'). It goes
--   through 'createSpaceAt', the application's "New space" with the slug
--   given: the same checks, the same private space with its home page, the
--   same grant to its creator. A space already at the path is 409, and the
--   answer to a new one is 201, with its listing's address as @Location@
--   and its listing as the body. A @PUT@ here only creates: it never
--   changes a space. An encrypted space is refused: its keys are made in
--   the browser.
-- * @GET api\/search?q=\<words>@: the application's own search
--   ('searchPages', as the search box asks it, 'searchFor'), with
--   @&space=\<space path>@ to keep to a space and its subspaces and
--   @&limit=N@ (50 by default, at most 200).
--
-- = What is listed
--
-- The spaces are the ones 'listSpaces' gives the application's sidebar,
-- and each space's pages the ones 'listPages' gives its tree: the same
-- permissions, checked by the same code. 'listSpaces' also gives a space the
-- user may not read when a space under it is one they may read, so that
-- the tree has a top. Such a space is listed with its title, its path and
-- its subspaces only, without the address of its listing or its pages
-- ('listPages' answers 'NotFound' for it). An encrypted space is listed,
-- marked so, with only the pages whose titles the server can read: none,
-- unless the space is being converted.
module Wiki.Server.ListApi
  ( listApplication
  , searchApplication
  )
where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, eitherDecode, encode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Network.HTTP.Types (hCacheControl, hContentType, hLocation, methodGet, methodHead, methodPut, status200, status201, status400, status404)
import Network.Wai (Application, Request (..), Response, responseLBS, strictRequestBody)
import Protolude
import Wiki.App.Backend (MonadWiki (getSpace, listPages, listSpaces, searchPages), WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Page (PageSummary (..), childPages)
import Wiki.Core.PageKind (PageKind)
import Wiki.Core.Path (findChild, parsePath, renderPath, resolvePath)
import Wiki.Core.Permission (spaceChain)
import Wiki.Core.Search (SearchHit (..), Snippet, queryWords, snippetText)
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (..), pagePath, rootPage, subspacesOf)
import Wiki.Server.Api (Env, searchFor)
import Wiki.Server.MarkdownApi (Asking (..), addressUnder, notASlug, notAllowed, parameter, refusal, runAs, signedInProgram, titleOfSlug, wikiRefusal)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (createSpaceAt)

-- | A space as the listing gives it. A field that is 'Nothing' is left out
-- of the JSON: @list@, @home@ and @pages@ for a space the user may not
-- read, @home@ where the server cannot read the home page, and @subspaces@
-- and @pages@ where @depth@ cut the tree. An empty list is a space with
-- nothing there.
data SpaceEntry = SpaceEntry
  { title :: Text
  , path :: Text
  -- ^ The space's path from the top level, slugs separated by slashes.
  , list :: Maybe Text
  -- ^ The address of the space's own listing.
  , home :: Maybe Text
  -- ^ The address of its home page's Markdown.
  , encrypted :: Bool
  , subspaces :: Maybe [SpaceEntry]
  , pages :: Maybe [PageEntry]
  -- ^ The pages under its home page, each with the pages under it.
  }
  deriving stock (Generic)

-- | A page as the listing gives it; @children@ is left out where @depth@
-- cut the tree.
data PageEntry = PageEntry
  { title :: Text
  , kind :: PageKind
  , markdown :: Text
  -- ^ The address of its Markdown, for @GET@ and @PUT@.
  , updated :: UTCTime
  , template :: Bool
  , children :: Maybe [PageEntry]
  }
  deriving stock (Generic)

-- | A hit of the search. The title and the excerpt are given twice: as
-- plain text, and as the search's pieces, each @{"text", "match"}@, the
-- pieces with @match@ true being the query's words where they were found.
data HitEntry = HitEntry
  { title :: Text
  , titleMarked :: Snippet
  , space :: HitSpace
  , markdown :: Text
  , excerpt :: Text
  , excerptMarked :: Snippet
  }
  deriving stock (Generic)

-- | The space of a hit: its path and its title.
data HitSpace = HitSpace
  { path :: Text
  , title :: Text
  }
  deriving stock (Generic)

-- | The listing of the top level: @{"spaces": [...]}@.
newtype TopLevel = TopLevel {spaces :: [SpaceEntry]}
  deriving stock (Generic)

-- | The search's answer: @{"hits": [...]}@.
newtype Hits = Hits {hits :: [HitEntry]}
  deriving stock (Generic)

-- | The JSON of the answers: fields in the order of the records, and a
-- field that is 'Nothing' left out.
entryOptions :: Options
entryOptions = defaultOptions {omitNothingFields = True}

instance ToJSON SpaceEntry where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToJSON PageEntry where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToJSON HitEntry where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToJSON HitSpace where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToJSON TopLevel where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToJSON Hits where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

-- | The listing, on a listener that serves the wiki under this path (with a
-- slash at each end), for a request the listener knows this of
-- ('Asking'), whose path after @api\/list@ is these segments.
listApplication :: Env -> Text -> Asking -> [Text] -> Application
listApplication env mount asking@Asking {address} segments request respond =
  signedInProgram env asking request respond $ \u -> case requestMethod request of
    method
      | method `elem` [methodGet, methodHead] -> case depthOf request of
          Left why -> respond (refusal status400 "Invalid" (Just why))
          Right depth -> do
            target <-
              if top
                then pure (Just Nothing)
                else
                  routeOf "/" spacePath <&> \case
                    Route.Space sp -> Just (Just sp)
                    _ -> Nothing
            case target of
              Nothing -> respond (refusal status404 "NotFound" Nothing)
              Just at -> runAs env address u (runExceptT (listing mount depth at)) >>= either (respond . wikiRefusal) (respond . either (answer . TopLevel) answer)
      | method == methodPut
      , not top ->
          routeOf "/" spacePath >>= \case
            Route.Space sp | Just slug <- lastMay sp -> do
              given <- strictRequestBody request
              case newSpaceOf given of
                Left why -> respond (refusal status400 "Invalid" (Just why))
                Right new
                  | new.encrypted == Just True -> respond (refusal status400 "Invalid" (Just "An encrypted space is made in the browser, where its keys are made: the server has none, and this API cannot make one."))
                  | otherwise -> create u sp slug (fromMaybe (titleOfSlug slug) new.title)
            _ -> respond (maybe (refusal status404 "NotFound" Nothing) (refusal status400 "Invalid" . Just) (notASlug segments))
      | top -> respond (notAllowed [methodGet, methodHead])
      | otherwise -> respond (notAllowed [methodGet, methodHead, methodPut])
  where
    top = segments `elem` [[], [""]]
    -- The path after the prefix, as sent (percent-encoded), without the
    -- query: what the application's router reads as a space's address.
    spacePath = fromMaybe "/" (BS.stripPrefix (encodeUtf8 mount <> "api/list") (rawPathInfo request))
    -- Create the space at its path, under the space that the slugs before
    -- its own slug name, and answer with its listing.
    create u sp slug title =
      parentOf u sp >>= \case
        Left refused -> respond refused
        Right parent ->
          runAs env address u (createSpaceAt parent slug title) >>= \case
            Left e -> respond (wikiRefusal e)
            Right _ ->
              runAs env address u (runExceptT (spaceListing mount Nothing sp)) >>= \case
                Left e -> respond (wikiRefusal e)
                Right entry -> respond (responseLBS status201 [(hContentType, "application/json"), (hCacheControl, "private, no-store"), (hLocation, encodeUtf8 (addressUnder mount "api/list" (Route.Space sp)))] (encode entry))
    -- The space above a new space's path: 'Nothing' for a space at the top
    -- level.
    parentOf u sp = case initMay sp of
      Just above@(_ : _) ->
        runAs env address u (getSpace above) <&> \case
          Right p -> Right (Just p.id)
          Left NotFound -> Left (refusal status404 "NotFound" (Just ("The space above it, " <> renderPath above <> ", is not there: create that space first.")))
          Left e -> Left (wikiRefusal e)
      _ -> pure (Right Nothing)

-- | What a @PUT@ that creates a space may say of it: its title, and whether
-- it is encrypted, which this API refuses. Each is optional; a field of
-- another name is refused, so that a field misspelled is not ignored.
data NewSpace = NewSpace
  { title :: Maybe Text
  , encrypted :: Maybe Bool
  }
  deriving stock (Generic)

instance FromJSON NewSpace where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | The 'NewSpace' of a request's body: an empty body asks for nothing.
newSpaceOf :: LByteString -> Either Text NewSpace
newSpaceOf body
  | BSL.all (`elem` [9, 10, 13, 32]) body = Right NewSpace {title = Nothing, encrypted = Nothing}
  | otherwise = first (\why -> "The body is not a space's JSON, {\"title\": \"…\"}: " <> toS why) (eitherDecode body)

-- | The search, on a listener that serves the wiki under this path, for a
-- request the listener knows this of ('Asking').
searchApplication :: Env -> Text -> Asking -> Application
searchApplication env mount asking@Asking {address} request respond =
  signedInProgram env asking request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead]
      then respond (notAllowed [methodGet, methodHead])
      else case (words', within, limitOf) of
        (Left why, _, _) -> respond (refusal status400 "Invalid" (Just why))
        (_, Left why, _) -> respond (refusal status400 "Invalid" (Just why))
        (_, _, Left why) -> respond (refusal status400 "Invalid" (Just why))
        (Right q, Right sp, Right limit) ->
          runAs env address u (Right <$> searching mount q sp limit) >>= either (respond . wikiRefusal) (respond . answer . Hits)
  where
    words' = case parameter request "q" of
      Right (Just q) | not (null (queryWords (searchFor (Just q) Nothing False Nothing False))) -> Right q
      Right _ -> Left "Give the words to search for: ?q=words."
      Left why -> Left why
    within =
      parameter request "space" >>= \case
        Nothing -> Right Nothing
        Just t
          | T.null (T.strip t) || t == "/" -> Right Nothing
          | otherwise -> bimap (const "The space is not a space's path, such as team/projects.") Just (parsePath t)
    limitOf =
      parameter request "limit" >>= \case
        Nothing -> Right Nothing
        Just t -> maybe (Left "The limit is not a number.") (Right . Just) (readMaybe t)

-- | The answer of a listing or a search: JSON, never kept by a cache, since
-- it is what one user may read.
answer :: (ToJSON a) => a -> Response
answer = responseLBS status200 [(hContentType, "application/json"), (hCacheControl, "private, no-store")] . encode

-- | The listing's @depth@, if the request gives one: a number from 1 up.
depthOf :: Request -> Either Text (Maybe Int)
depthOf request =
  parameter request "depth" >>= \case
    Nothing -> Right Nothing
    Just t -> case readMaybe t of
      Just n | n >= 1 -> Right (Just n)
      _ -> Left "The depth is not a number from 1 up."

-- | The listing: of the top level ('Nothing'), its spaces ('Left'), or of
-- the space at a path ('Right'), @depth@ levels down (all of them for
-- 'Nothing').
listing :: (MonadWiki m) => Text -> Maybe Int -> Maybe [Slug] -> ExceptT WikiError m (Either [SpaceEntry] SpaceEntry)
listing mount depth = \case
  Nothing -> do
    spaces <- lift listSpaces
    listed <- traverse (spaceEntry mount spaces (below depth)) (subspacesOf spaces Nothing)
    pure (Left listed)
  Just sp -> Right <$> spaceListing mount depth sp

-- | The listing of the space at a path, @depth@ levels down.
spaceListing :: (MonadWiki m) => Text -> Maybe Int -> [Slug] -> ExceptT WikiError m SpaceEntry
spaceListing mount depth sp = do
  s <- ExceptT (getSpace sp)
  spaces <- lift listSpaces
  spaceEntry mount spaces depth s

-- | How many levels a tree is still to go down, one level below; 'Nothing'
-- for all of them.
below :: Maybe Int -> Maybe Int
below = fmap (subtract 1)

-- | Whether a tree with this many levels to go shows the level below.
opens :: Maybe Int -> Bool
opens = maybe True (> 0)

-- | A space of the listing, among the spaces 'listSpaces' gave, with this
-- many levels below it.
spaceEntry :: (MonadWiki m) => Text -> [Space] -> Maybe Int -> Space -> ExceptT WikiError m SpaceEntry
spaceEntry mount spaces depth s = do
  -- The pages of the space, if the user may read it: 'NotFound' for a space
  -- listed only for a space under it, and 'Stale' for an encrypted space,
  -- whose pages' titles the server cannot read.
  readable <-
    lift (listPages s.id) >>= \case
      Right ps -> pure (Just ps)
      Left (Stale _ _) -> pure (Just [])
      Left NotFound -> pure Nothing
      Left e -> ExceptT (pure (Left e))
  subspaces <-
    if opens depth
      then Just <$> traverse (spaceEntry mount spaces (below depth)) (subspacesOf spaces (Just s.id))
      else pure Nothing
  let chain = spaceChain spaces s.id
      sp = map (\x -> x.slug) chain
      ps = fromMaybe [] readable
      byId = Map.fromList [(p.id, p) | p <- ps]
      pageEntry level p = do
        at <- pagePath s (`Map.lookup` byId) p.id
        pure
          PageEntry
            { title = p.title
            , kind = p.kind
            , markdown = addressUnder mount "api/markdown" (Route.pageRoute sp at)
            , updated = p.updatedAt
            , template = p.template
            , children = guard (opens level) $> pagesUnder (below level) (Just p.id)
            }
      pagesUnder level parent = mapMaybe (pageEntry level) (childPages s.pageOrder ps parent)
  pure
    SpaceEntry
      { title = s.name
      , path = renderPath sp
      , list = readable $> addressUnder mount "api/list" (Route.Space sp)
      , home = (rootPage s >>= (`Map.lookup` byId)) $> addressUnder mount "api/markdown" (Route.Space sp)
      , -- Encrypted here or above: the server reads the pages of neither.
        encrypted = any (\x -> isJust x.encryption) (s : chain)
      , subspaces
      , pages = guard (isJust readable && opens depth) $> pagesUnder (below depth) (rootPage s)
      }

-- | The search's hits for these words, in this space and its subspaces or
-- everywhere, in the search's order. A space path that names no space the
-- user may see finds nothing, as the search box finds nothing there.
searching :: (MonadWiki m) => Text -> Text -> Maybe [Slug] -> Maybe Int -> m [HitEntry]
searching mount q within limit = do
  spaces <- listSpaces
  let search sid = map (hitEntry spaces) <$> searchPages (searchFor (Just q) sid False limit False)
  case within of
    Nothing -> search Nothing
    Just sp -> case rightToMaybe (resolvePath (findChild spaces) sp) >>= lastMay of
      Nothing -> pure []
      Just s -> search (Just s.id)
  where
    hitEntry spaces h =
      HitEntry
        { title = snippetText h.title
        , titleMarked = h.title
        , space = HitSpace {path = renderPath h.spacePath, title = maybe "" (\s -> s.name) (find (\s -> s.id == h.space) spaces)}
        , markdown = addressUnder mount "api/markdown" (Route.pageRoute h.spacePath h.pagePath)
        , excerpt = snippetText h.snippet
        , excerptMarked = h.snippet
        }

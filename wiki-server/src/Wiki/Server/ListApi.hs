-- | The listing and the search of the API for programs (WP5.138 of
-- docs/PLAN.md): what a program on a user's own machine (a local
-- assistant, later an MCP server) reads to find the pages it then reads
-- and writes through the API for a page's Markdown
-- ("Wiki.Server.MarkdownApi"). Both sign in as that API does, with HTTP
-- Basic alone ('signedInBasic'), and answer JSON. They live at @api\/list@
-- and @api\/search@, beside the application's own API under @api\/v1\/@,
-- whose later versions are @api\/v2\/@ and so on: neither name can be one.
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

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode, genericToEncoding, genericToJSON)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Halogen.Router.Duplex qualified as Duplex
import Network.HTTP.Types (hCacheControl, hContentType, methodGet, methodHead, status200, status400, status404)
import Network.Wai (Application, Request (..), Response, responseLBS)
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
import Wiki.Server.MarkdownApi (notAllowed, refusal, runAs, signedInBasic, wikiRefusal)
import Wiki.Server.Render (routeOf)

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
-- slash at each end), for a request from this address, whose path after
-- @api\/list@ is these segments.
listApplication :: Env -> Text -> Text -> [Text] -> Application
listApplication env mount address segments request respond =
  signedInBasic env address request respond $ \u ->
    if requestMethod request `notElem` [methodGet, methodHead]
      then respond (notAllowed [methodGet, methodHead])
      else case depthOf request of
        Left why -> respond (refusal status400 "Invalid" (Just why))
        Right depth -> do
          target <-
            if segments `elem` [[], [""]]
              then pure (Just Nothing)
              else
                routeOf "/" spacePath <&> \case
                  Route.Space sp -> Just (Just sp)
                  _ -> Nothing
          case target of
            Nothing -> respond (refusal status404 "NotFound" Nothing)
            Just at -> runAs env address u (runExceptT (listing mount depth at)) >>= either (respond . wikiRefusal) (respond . either (answer . TopLevel) answer)
  where
    -- The path after the prefix, as sent (percent-encoded), without the
    -- query: what the application's router reads as a space's address.
    spacePath = fromMaybe "/" (BS.stripPrefix (encodeUtf8 mount <> "api/list") (rawPathInfo request))

-- | The search, on a listener that serves the wiki under this path, for a
-- request from this address.
searchApplication :: Env -> Text -> Text -> Application
searchApplication env mount address request respond =
  signedInBasic env address request respond $ \u ->
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

-- | The value of a parameter of the request's query, as UTF-8 text.
parameter :: Request -> ByteString -> Either Text (Maybe Text)
parameter request name = case join (lookup name (queryString request)) of
  Nothing -> Right Nothing
  Just bytes -> bimap (const ("The parameter " <> decodeUtf8 name <> " is not UTF-8 text.")) Just (decodeUtf8' bytes)

-- | The listing's @depth@, if the request gives one: a number from 1 up.
depthOf :: Request -> Either Text (Maybe Int)
depthOf request =
  parameter request "depth" >>= \case
    Nothing -> Right Nothing
    Just t -> case readMaybe t of
      Just n | n >= 1 -> Right (Just n)
      _ -> Left "The depth is not a number from 1 up."

-- | The address of a route of the application under the API's prefix
-- (@api\/list@, @api\/markdown@), on a listener that serves the wiki under
-- this path: the route's address as the application writes it, with the
-- prefix in front.
addressUnder :: Text -> Text -> Route.Route -> Text
addressUnder mount prefix r = mount <> prefix <> Duplex.print Route.route r

-- | The listing: of the top level ('Nothing'), its spaces ('Left'), or of
-- the space at a path ('Right'), @depth@ levels down (all of them for
-- 'Nothing').
listing :: (MonadWiki m) => Text -> Maybe Int -> Maybe [Slug] -> ExceptT WikiError m (Either [SpaceEntry] SpaceEntry)
listing mount depth = \case
  Nothing -> do
    spaces <- lift listSpaces
    listed <- traverse (spaceEntry mount spaces (below depth)) (subspacesOf spaces Nothing)
    pure (Left listed)
  Just sp -> do
    s <- ExceptT (getSpace sp)
    spaces <- lift listSpaces
    Right <$> spaceEntry mount spaces depth s

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
  let sp = map (\x -> x.slug) (spaceChain spaces s.id)
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
      , encrypted = isJust s.encryption
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

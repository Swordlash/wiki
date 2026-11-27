-- | The server's storage timed at the size of a big wiki
-- ("docs/scalability-audit.md", WP5.16a of docs/PLAN.md): what a page view
-- reads, a save of a watched page, a backup, and a comment on a page many
-- tabs show. Run it by hand, natively, to compare two versions of the
-- storage:
--
-- > cabal run wiki-server-bench --builddir=dist-newstyle/native
-- > cabal run wiki-server-bench --builddir=dist-newstyle/native -- postgres URL
--
-- The first runs on SQLite, in a temporary directory; the second on the
-- PostgreSQL database the URL names, which must be empty (its backups run
-- the pg_dump on the PATH, or the one WIKI_PG_DUMP names, and check the
-- copy with the pg_restore on the PATH). The wiki is made
-- with rows of its own rather than through the storage's operations, which
-- would take far longer: 1,000 users, one
-- public space of 2,000 pages (bodies of about 1 KB, since nothing timed
-- here reads every body) under the space's home page, 10,000 watches, and
-- 200,000 notifications, 500 of them the viewer's. The page timed is
-- watched by 50 users. The bench uses only what both the storage before
-- WP5.16a and after it have.
--
-- With @smallest@ first, the bench runs each step once, on the smallest
-- wiki on which every step has something to do ('smallest'). CI runs it on
-- SQLite, so that a change of the storage that breaks a step fails there:
--
-- > cabal run wiki-server-bench --builddir=dist-newstyle/native -- smallest
--
-- Every step checks what it is answered, and stops the bench on an error,
-- on an empty list where a read should find something, and on an answer
-- of the sitemap or the feed other than 200: a step that timed a
-- 'NotFound' would time nothing.
--
-- Then the reads of WP5.16b, after more is added: 18 private spaces of
-- 1,000 pages each, so that the wiki has 20,000 pages, each with a word
-- of its own (@token<n>@); 19 more versions of each page of the big
-- space, which becomes a blog; and 100,000 entries in the audit log. The
-- sitemap and the feed are timed through their applications, as a
-- request reaches them, written afresh and kept.
--
-- For WP5.16c, two arguments seed a data directory instead, for timing a
-- running server from outside (its resident memory is the process's):
--
-- > wiki-server-bench attachments DIR PAGES MB
--
-- writes a wiki of PAGES pages in one space with MB attachments of 1 MiB
-- each, of bytes that do not compress, one on each of the first pages;
--
-- > wiki-server-bench blobs DIR N
--
-- writes a wiki of one page with N attachments of 1 KiB, each with a
-- blob of its own.
module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (lookup, (!!))
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Beam (all_, default_, desc_, insert, insertExpressions, insertValues, limit_, offset_, orderBy_, runInsert, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import GHC.Clock (getMonotonicTime)
import Network.Wai (Application, defaultRequest, requestHeaders, responseToStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets qualified as WS
import Protolude
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)
import Wiki.Api.Activity (ActivityQuery (..), activityPageSize)
import Wiki.Api.Events (ClientMessage (..), ServerMessage (..), eventsPath)
import Wiki.App.Backend (MonadWiki (..))
import Wiki.App.Backend.Admin (MonadAdmin (..))
import Wiki.App.Backend.Comments (MonadComments (..))
import Wiki.App.Backend.Notifications (MonadNotifications (..))
import Wiki.Core.Id
import Wiki.Core.Notification (WatchTarget (..))
import Wiki.Core.Page (Page (..))
import Wiki.Core.PageKind qualified as Kind
import Wiki.Core.PageMatch (titleQuery)
import Wiki.Core.Permission (SpacePermission (..), Subject (..))
import Wiki.Core.Search (SearchQuery (..), searchQuery)
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (..))
import Wiki.Server.Events qualified as Events
import Wiki.Server.Feed (FeedKind (..), feedApplication, newFeedCache, sitemapApplication)
import Wiki.Server.Storage (Config (..), Place (..), Storage, Who (..), backUp, defaultConfig, inDataDir, postgresAt, runStoredWiki, withStorage, writing)
import Wiki.Server.Storage qualified as Storage
import Wiki.Server.Storage.Layout (PgPrograms (..), defaultPgPrograms)
import Wiki.Server.Storage.Rows (chunksOf, insertPage, insertVersion, micro)
import Wiki.Server.Storage.Schema (AuditLog (..), Notifications (..), PageVersions (..), Spaces (..), Users (..), Watches (..), WikiDb (..), wikiDb)

-- | 'Size' is how big the wiki of the timings is. 'full' is a big wiki, the
-- size the timings are for. 'smallest' is a wiki just big enough for every
-- step to have something to do, for a run that checks that the bench still
-- works.
data Size = Size
  { users :: Int
  -- ^ The users of the wiki.
  , pages :: Int
  -- ^ The pages of the public space, which becomes a blog.
  , watches :: Int
  -- ^ The watches of pages, 'watchers' of them of the page saved.
  , watchers :: Int
  -- ^ The users who watch the page saved.
  , kept :: Int
  -- ^ The notifications.
  , mine :: Int
  -- ^ The notifications of the viewer.
  , tabs :: Int
  -- ^ The tabs subscribed to the page commented on.
  , tabUsers :: Int
  -- ^ The users those tabs belong to.
  , others :: Int
  -- ^ The private spaces that 'grow' adds.
  , otherPages :: Int
  -- ^ The pages of each private space.
  , versions :: Int
  -- ^ The versions that 'grow' adds to each page of the public space.
  , entries :: Int
  -- ^ The entries that 'grow' adds to the audit log: at least 99 pages of
  -- them, since the read of the activity is of page 100.
  , lonePage :: Int
  -- ^ The page of a private space whose word is searched among all pages.
  , spacePage :: Int
  -- ^ The page of the public space whose word is searched in that space.
  , viewedPage :: Int
  -- ^ The page of the public space viewed with its mentions.
  , once :: Bool
  -- ^ Whether each step runs once, rather than as many times as 'report'
  -- is given.
  }

full, smallest :: Size
full = Size {users = 1_000, pages = 2_000, watches = 10_000, watchers = 50, kept = 200_000, mine = 500, tabs = 100, tabUsers = 10, others = 18, otherPages = 1_000, versions = 19, entries = 100_000, lonePage = 12_345, spacePage = 1_234, viewedPage = 1_500, once = False}
smallest = Size {users = 60, pages = 20, watches = 100, watchers = 50, kept = 100, mine = 50, tabs = 4, tabUsers = 2, others = 2, otherPages = 10, versions = 2, entries = 100 * activityPageSize, lonePage = 25, spacePage = 12, viewedPage = 15, once = True}

-- | All the pages of the wiki once 'grow' has added the private spaces.
allPages :: Size -> Int
allPages size = size.pages + size.others * size.otherPages

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    ["attachments", dir, n, mb] -> seedAttachments dir (readNumber n) (readNumber mb)
    ["blobs", dir, n] -> seedBlobs dir (readNumber n)
    "smallest" : rest -> atSize smallest rest
    rest -> atSize full rest
  where
    readNumber t = fromMaybe (panic ("not a number: " <> toS t)) (readMaybe t)
    atSize size = \case
      [] -> timings size Nothing
      ["postgres", url] -> timings size (Just (encodeUtf8 (T.pack url)))
      _ -> die "usage: wiki-server-bench [smallest] [postgres URL] | attachments DIR PAGES MB | blobs DIR N"

-- | Seed a data directory with a wiki of this many pages in one space and
-- this many attachments of 1 MiB, of random bytes, one on each of the
-- first pages.
seedAttachments :: FilePath -> Int -> Int -> IO ()
seedAttachments dir n mb =
  withStorage defaultConfig (inDataDir dir) $ \st -> do
    space <- runStoredWiki st Administrator (createSpace Nothing "Big") >>= must
    now <- micro <$> getCurrentTime
    writing st $
      for_ [0 .. n - 1] $ \i -> do
        let p = pageOf space now i
        insertPage p
        insertVersion p Nothing
    withFile "/dev/urandom" ReadMode $ \random ->
      for_ [0 .. mb - 1] $ \i -> do
        bytes <- BS.hGet random (1024 * 1024)
        runStoredWiki st Administrator (uploadAttachment (pageId (i `mod` n)) ("file-" <> show i <> ".bin") "application/octet-stream" bytes) >>= void . must
    printf "seeded %s: %d pages, %d MiB of attachments\n" dir n mb

-- | Seed a data directory with a wiki of one page and this many
-- attachments of 1 KiB, each with a blob of its own.
seedBlobs :: FilePath -> Int -> IO ()
seedBlobs dir n =
  withStorage defaultConfig (inDataDir dir) $ \st -> do
    space <- runStoredWiki st Administrator (createSpace Nothing "Blobs") >>= must
    p <- runStoredWiki st Administrator (createPage space.id Nothing "Attachments" "Many.") >>= must
    for_ [0 .. n - 1] $ \i ->
      runStoredWiki st Administrator (uploadAttachment p.id ("file-" <> show i <> ".txt") "text/plain" (encodeUtf8 (T.justifyLeft 1024 '.' (show i)))) >>= void . must
    printf "seeded %s: %d attachments\n" dir n

-- | The timings of WP5.16a and WP5.16b, on a wiki of this size, on SQLite
-- or on the PostgreSQL database given.
timings :: Size -> Maybe ByteString -> IO ()
timings size postgres = do
  hub <- Events.newHub
  dump <- fromMaybe defaultPgPrograms.dump <$> lookupEnv "WIKI_PG_DUMP"
  withSystemTempDirectory "wiki-bench" $ \dir -> do
    let at = case postgres of
          Nothing -> inDataDir (dir </> "data")
          Just url -> Place {dataDir = dir </> "data", database = postgresAt url, programs = defaultPgPrograms {dump}}
    printf "on %s%s\n" (maybe "SQLite" (const "PostgreSQL") postgres :: Text) (if size.once then ", the smallest wiki, each step once" else "" :: Text)
    withStorage defaultConfig {publish = Events.publish hub, notify = Events.notify hub} at $ \st -> do
      seeded <- timed (seed size st)
      printf "seeded in %.1f s\n" (snd seeded)
      let (space, target) = fst seeded
          viewer = AsUser (UserId (userId 0))
          saver = AsUser (UserId (userId 1))
          report what n = timeRuns what (if size.once then 1 else n)
          every = commas (allPages size)
          inSpace = commas size.pages
      let pageView = runStoredWiki st viewer $ do
            _ <- session
            _ <- getPage space [slugOf target] >>= must
            void (watching (WatchPage (pageId target)) >>= must)
      report "page view: session, getPage, watching" 20 pageView
      counted st pageView
      report ("page view: the bell (" <> commas size.mine <> " notifications)") 5 (void (runStoredWiki st viewer notifications >>= must))
      report ("save of a page " <> commas size.watchers <> " users watch") 10 $ do
        p <- runStoredWiki st saver (getPage space [slugOf target]) >>= must
        runStoredWiki st saver (savePage p.id p.updatedAt p.title (p.body <> "\n\nMore.")) >>= void . must
      report "backup" 3 (backedUp st)
      readsDuringBackup st viewer space target
      commentWithTabs size st hub viewer target
      grown <- timed (grow size st space)
      printf "grown in %.1f s\n" (snd grown)
      let search q = runStoredWiki st Administrator (searchPages q) >>= found q.text
          lone = "token" <> show size.lonePage
      report ("search: a word on 1 page of " <> every) 10 (search (searchQuery lone))
      report ("search: a word on every page of " <> every) 5 (search (searchQuery "fifty"))
      report ("search: a word on 1 page, in a " <> inSpace <> "-page space") 10 (search SearchQuery {text = "token" <> show size.spacePage, space = Just space, includeSubspaces = True, limit = 50, templates = False})
      -- The lookup of pages by title reads the nodes of every page of the
      -- spaces the asker may view: the administrator's are all the pages,
      -- the viewer's those of the public space.
      let titles who q = runStoredWiki st who (findPagesByTitle (titleQuery q)) >>= must >>= found q
      report ("pages by title: two letters on every page of " <> every) 10 (titles Administrator "pa")
      report ("pages by title: letters on 1 page of " <> every) 10 (titles Administrator ("page " <> show size.lonePage))
      report ("pages by title: two letters, for a viewer of " <> inSpace <> " pages") 10 (titles viewer "pa")
      report ("sitemap of a " <> inSpace <> "-page public space, written") 5 (newFeedCache >>= \cache -> asked (sitemapApplication st cache "/"))
      report ("feed of a " <> inSpace <> "-page blog, written") 5 (newFeedCache >>= \cache -> asked (feedApplication st cache "/" Atom ["big"]))
      cache <- newFeedCache
      report "sitemap, asked again within a minute" 20 (asked (sitemapApplication st cache "/"))
      report "page view: getPage, watching, mentionable (3)" 20 $ runStoredWiki st viewer $ do
        _ <- getPage space [slugOf size.viewedPage] >>= must
        _ <- watching (WatchPage (pageId size.viewedPage)) >>= must
        mentionable [loginOf 1, loginOf 2, "nobody"] >>= found "the mentioned"
      report ("blog listing: " <> inSpace <> " posts of " <> commas (size.versions + 1) <> " versions each") 5 (runStoredWiki st viewer (postDetails space) >>= must >>= found "the posts")
      cursor <- activityCursor st
      report ("activity: page 100 of " <> commas size.entries <> " entries") 10 (void (runStoredWiki st Administrator (activity (pageHundred cursor)) >>= must))

-- | The value of an answer of the storage, or a stop of the bench with the
-- error.
must :: (Applicative f, Show e) => Either e a -> f a
must = either (panic . show) pure

-- | A stop of the bench when a read found nothing: the step would time a
-- read of nothing.
found :: (Applicative f, Foldable t) => Text -> t a -> f ()
found what xs = when (null xs) (panic ("nothing found for " <> what))

-- | A number with commas between its thousands, as the labels write it.
commas :: Int -> Text
commas = T.reverse . T.intercalate "," . T.chunksOf 3 . T.reverse . show

-- | The query for page 100 of the audit log, given the oldest entry of
-- page 99.
pageHundred :: Text -> ActivityQuery
pageHundred cursor = ActivityQuery {actor = Nothing, offset = 0, before = Just cursor}

-- | The oldest entry of page 99 of the audit log.
activityCursor :: Storage -> IO Text
activityCursor st =
  Storage.reading st (runSelectReturningOne (select (limit_ 1 (offset_ (fromIntegral (99 * activityPageSize - 1)) (orderBy_ (\e -> (desc_ e.at, desc_ e.id)) (all_ wikiDb.audit_log))))))
    >>= maybe (panic "no entry ends page 99") (\e -> pure e.id)

-- | Ask an application for its root as a reader would, and read the
-- whole answer. An answer other than 200 stops the bench.
asked :: Application -> IO ()
asked app = void . app defaultRequest $ \response -> do
  let (status, _, withBody) = responseToStream response
  when (fromEnum status /= 200) (panic ("the application answered " <> show status))
  withBody $ \body -> body (\b -> void (evaluate (BSL.length (toLazyByteString b)))) pass
  pure ResponseReceived

-- | Grow the wiki for the reads of WP5.16b: private spaces of pages of
-- their own (18 of 1,000 at the full size), more versions of each page of
-- the big space, which becomes a blog (19), and entries in the audit log
-- (100,000).
grow :: Size -> Storage -> SpaceId -> IO ()
grow size st space = do
  others <- for [1 .. size.others] $ \i -> runStoredWiki st Administrator (createSpace Nothing ("Other " <> show i)) >>= must
  now <- micro <$> getCurrentTime
  writing st $ do
    runUpdate (update wikiDb.spaces (\x -> x.blog <-. val_ True) (\x -> x.id ==. val_ space.text))
    for_ (zip [0 :: Int ..] others) $ \(k, s) ->
      for_ [0 .. size.otherPages - 1] $ \j -> insertPage (pageOf s now (size.pages + k * size.otherPages + j))
    for_ (chunksOf 500 [(i, v) | i <- [0 .. size.pages - 1], v <- [1 .. size.versions]]) $ \chunk ->
      runInsert . insert wikiDb.page_versions $
        insertValues
          [ VersionRow {page_id = pageText i, updated_at = addUTCTime (fromIntegral i + fromIntegral v / 100) now, title = "Page " <> show i, body = "", author = Just (userId (v `mod` size.users))}
          | (i, v) <- chunk
          ]
    for_ (chunksOf 500 [1 .. size.entries]) $ \chunk ->
      runInsert . insert wikiDb.audit_log $
        insertValues
          [ AuditRow {id = "bench-entry-" <> show i, at = addUTCTime (fromIntegral i) now, actor_id = userId (i `mod` 20), actor_login = loginOf (i `mod` 20), action = "group.create", subject_kind = "group", subject_id = Nothing, subject_name = "A group", detail = "{}", address = ""}
          | i <- chunk
          ]

-- | Seed the wiki: the space and its pages through the storage's own rows,
-- and the rest as rows of SQL. The space's id, and the index of the page
-- timed.
seed :: Size -> Storage -> IO (SpaceId, Int)
seed size st = do
  space <- runStoredWiki st Administrator (createSpace Nothing "Big") >>= must
  _ <- runStoredWiki st Administrator (setSpacePublic space.id True) >>= must
  now <- micro <$> getCurrentTime
  writing st $ do
    for_ (chunksOf 500 [0 .. size.users - 1]) $ \chunk ->
      runInsert . insert wikiDb.users $
        insertValues [UserRow {id = userId i, login = loginOf i, display_name = "User " <> show i, password_hash = Nothing, disabled = False, email = Nothing, email_verified_at = Nothing, notification_mail = Nothing, watch_own = True, feed_token = Nothing, avatar_blob = Nothing, avatar_media_type = Nothing, opaque_record = Nothing, opaque_stretch = Nothing} | i <- chunk]
    for_ [0 .. size.pages - 1] $ \i -> do
      let p = pageOf space now i
      insertPage p
      insertVersion p (Just (UserId (userId (i `mod` size.users))))
    -- The page timed is page 0: watched by users 1 to 50 (user 1 saves it,
    -- and is not told of their own save). The other watches are spread
    -- over the other pages.
    let watchRow u p = WatchRow {user_id = userId u, page_id = Just (pageText p), space_id = Nothing, watching = True}
    runInsert (insert wikiDb.watches (insertValues [watchRow u 0 | u <- [1 .. size.watchers]]))
    for_ (chunksOf 500 [0 .. size.watches - size.watchers - 1]) $ \chunk ->
      runInsert (insert wikiDb.watches (insertValues [watchRow (i `mod` size.users) (1 + i `div` size.users) | i <- chunk]))
    -- 500 notifications for the viewer (user 0), the rest for the others,
    -- each about one of the space's pages, numbered by the database in
    -- this order.
    for_ (chunksOf 500 [0 .. size.kept - 1]) $ \chunk ->
      runInsert . insert wikiDb.notifications $
        insertExpressions
          [ NotificationRow
              { seq = default_
              , id = val_ ("n" <> show i)
              , user_id = val_ (userId (if i < size.mine then 0 else 1 + i `mod` (size.users - 1)))
              , kind = val_ "changed"
              , page_id = val_ (pageText (i `mod` size.pages))
              , thread_id = val_ Nothing
              , actor = val_ (userId 2)
              , actor_name = val_ "Someone"
              , title = val_ "A page"
              , excerpt = val_ ""
              , created_at = val_ now
              , read_at = val_ Nothing
              , mail = val_ False
              }
          | i <- chunk
          ]
  -- The user who saves may edit the space; everyone may read it.
  _ <- runStoredWiki st Administrator (setSpaceGrant space.id (SubjectUser (UserId (userId 1))) Edit True) >>= must
  pure (space.id, 0)

userId :: Int -> Text
userId i = "bench-user-" <> show i

loginOf :: Int -> Text
loginOf i = "user" <> show i

pageText :: Int -> Text
pageText i = "bench-page-" <> show i

pageId :: Int -> PageId
pageId = PageId . pageText

slugOf :: Int -> Slug
slugOf i = either (panic . show) identity (mkSlug ("page-" <> show i))

-- | Page i of a space, under the space's home page. A page with no parent
-- is on no path of a space that has a home page, so a read of it by its
-- path finds nothing.
pageOf :: Space -> UTCTime -> Int -> Page
pageOf space now i =
  Page
    { id = pageId i
    , space = space.id
    , parent = space.homePage
    , title = "Page " <> show i
    , slug = slugOf i
    , body = T.replicate 20 ("A line of the page's text, " <> show i <> ", about fifty bytes.\n") <> "Its own word: token" <> show i <> ".\n"
    , attachments = []
    , createdAt = addUTCTime (fromIntegral i) now
    , updatedAt = addUTCTime (fromIntegral i) now
    , position = i
    , template = False
    , kind = Kind.Document
    , createdBy = Nothing
    , updatedBy = Nothing
    }

-- | How long an action takes, in seconds, with what it gave.
timed :: IO a -> IO (a, Double)
timed act = do
  start <- getMonotonicTime
  a <- act
  end <- getMonotonicTime
  pure (a, end - start)

-- | Run an action some times, one after another, and print the median and
-- the slowest time it took.
timeRuns :: Text -> Int -> IO () -> IO ()
timeRuns what n act = do
  ts <- sort <$> replicateM n (snd <$> timed act)
  printf "%-45s median %8.2f ms, slowest %8.2f ms (%d runs)\n" what (1000 * (ts !! (n `div` 2))) (1000 * foldr max 0 ts) n

-- | Run an action once and print how many statements the storage sent the
-- database meanwhile: the statements 'Storage.traceStatements' gives, and
-- one @COMMIT@ for each @BEGIN@ among them, which the trace leaves out.
counted :: Storage -> IO () -> IO ()
counted st act = do
  seen <- newIORef []
  Storage.traceStatements st (\s -> modifyIORef' seen (s :)) act
  statements <- readIORef seen
  let transactions = length (filter ("BEGIN" `T.isPrefixOf`) statements)
  printf "%-45s %d statements in %d transactions\n" ("  the same, statements" :: Text) (length statements + transactions) transactions

-- | Make a backup into the data directory's backups, and delete it (and
-- what is kept beside it) again.
backedUp :: Storage -> IO ()
backedUp st =
  getCurrentTime >>= backUp st >>= \case
    Left why -> panic why
    Right (file, _) -> for_ [file, file <> ".json"] $ \f -> doesFileExist f >>= \there -> when there (removeFile f)

-- | How long reads of a page take while a backup runs: the slowest of the
-- reads made from the start of the backup to its end.
readsDuringBackup :: Storage -> Who -> SpaceId -> Int -> IO ()
readsDuringBackup st who space target = do
  backup <- async (timed (backedUp st))
  let loop acc = do
        (_, t) <- timed (runStoredWiki st who (getPage space [slugOf target]) >>= must)
        poll backup >>= \case
          Nothing -> loop (t : acc)
          Just (Left e) -> throwIO e
          Just (Right (_, took)) -> pure (took, t : acc)
  (took, ts) <- loop []
  printf "%-45s backup %8.2f ms, %d reads, slowest %8.2f ms\n" ("reads during a backup" :: Text) (1000 * took) (length ts) (1000 * foldr max 0 ts)

-- | How long a comment on a page takes to reach every tab subscribed to
-- it: the size's tabs, of its tab users, over real WebSockets.
commentWithTabs :: Size -> Storage -> Events.Hub -> Who -> Int -> IO ()
commentWithTabs size st hub commenter target = do
  -- The clients send no Origin, so the wiki's own origin does not matter.
  let app request = Events.eventsApplication st hub (whoOf request) [] request
      whoOf request = maybe Anonymous (AsUser . UserId . decodeUtf8) (lookup "X-Bench-User" (requestHeaders request))
  testWithApplication (pure app) $ \port -> do
    ready <- newEmptyMVar
    heard <- newEmptyMVar
    clients <- for [0 .. size.tabs - 1] $ \i -> async $
      WS.runClientWith "127.0.0.1" port ("/" <> toS eventsPath) WS.defaultConnectionOptions [("X-Bench-User", encodeUtf8 (userId (10 + i `mod` size.tabUsers)))] $ \c -> do
        WS.sendTextData c (encode (Subscribe (pageId target)))
        let untilSubscribed =
              WS.receiveData c >>= \m -> case decode m of
                Just (Subscribed {}) -> pass
                _ -> untilSubscribed
            untilEvent =
              WS.receiveData c >>= \m -> case decode m of
                Just (Event _ _) -> getMonotonicTime >>= putMVar heard
                _ -> untilEvent
        untilSubscribed
        putMVar ready ()
        untilEvent
    replicateM_ size.tabs (takeMVar ready)
    start <- getMonotonicTime
    _ <- runStoredWiki st commenter (openThread (pageId target) Nothing "A comment.") >>= must
    committed <- getMonotonicTime
    ends <- replicateM size.tabs (takeMVar heard)
    traverse_ wait clients
    printf "%-45s committed %8.2f ms, all %d tabs told %8.2f ms\n" ("comment on a page with " <> commas size.tabs <> " tabs (" <> commas size.tabUsers <> " users)") (1000 * (committed - start)) size.tabs (1000 * (foldr max 0 ends - start))

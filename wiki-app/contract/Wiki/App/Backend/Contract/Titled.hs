-- | The contract tests of the lookup of pages by title
-- ('findPagesByTitle', "Wiki.Core.PageMatch"): the pages someone may
-- view whose title or path match the letters typed, in the spaces the
-- search reads, ranked and cut the same way by every backend.
--
-- Like the backend's contract ("Wiki.App.Backend.Contract", whose
-- 'Fixture' it takes), it runs against the in-memory backend, the server's
-- storage on each database, and the HTTP backend against the server's API.
-- The demo's @admin@ may do anything; its @editor@ may edit the public
-- blog; its @reader@ may read the blog and the docs, and of the team's
-- spaces only its projects.
--
-- Besides the examples, a property builds small wikis through the
-- backend (spaces nested, public, private and sealed, granted to the
-- reader or not, pages with equal titles in different spaces and titles
-- that differ only by case or accents, templates, a page moved, a page
-- deleted) and compares each answer with what the same person's listings
-- give ('listSpaces', 'listPages') ranked by 'rankTitles': so a page is
-- found exactly when its space is one the person may list the pages of,
-- and every backend orders the same pages the same way.
module Wiki.App.Backend.Contract.Titled (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude hiding (reader)
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Gen, Property, choose, chooseInt, conjoin, counterexample, elements, forAll, ioProperty, listOf, oneof, sublistOf, (===))
import Wiki.App.Backend
import Wiki.App.Backend.Contract (As (..), Fixture (..), Run)
import Wiki.Core.HomePage (isHomeSpace)
import Wiki.Core.Id
import Wiki.Core.Page (Page (..), PageSummary (..))
import Wiki.Core.PageMatch (TitleHit (..), TitleQuery (..), maxLetters, maxTitleHits, rankTitles, titleQuery)
import Wiki.Core.Path (parsePath)
import Wiki.Core.Permission (SpacePermission (..), Subject (..))
import Wiki.Core.Slug (Slug, slugText)
import Wiki.Core.Space (Space (..))
import Wiki.Core.Space qualified as Space
import Wiki.Core.User (User (..))

admin, editor, reader :: As
admin = As "admin"
editor = As "editor"
reader = As "reader"

-- | The contract, for a backend made afresh for each example, as in
-- "Wiki.App.Backend.Contract".
spec :: forall m -> (MonadWiki m) => (ActionWith (Fixture m) -> IO ()) -> Spec
spec _ withFixture = around withFixture $ describe "findPagesByTitle contract" $ do
  finding
  leavingOut
  bounds
  changes
  generated

--------------------------------------------------------------------------------
-- Helpers

ok :: (HasCallStack, Show e) => Either e a -> IO a
ok = either (\e -> assertFailure ("expected success, got " <> show e)) pure

path :: Text -> [Slug]
path t = either (\e -> panic ("bad path in a test: " <> t <> ", " <> show e)) identity (parsePath t)

-- | The space at a path, as the administrator finds it.
spaceAt :: (HasCallStack, MonadWiki m) => Run m -> Text -> IO Space
spaceAt run s = run admin (getSpace (path s)) >>= ok

-- | The pages found, each as its space's path, its own path and its title.
found :: (HasCallStack, MonadWiki m) => Run m -> As -> TitleQuery -> IO [(Text, Text, Text)]
found run who q = map shown <$> (run who (findPagesByTitle q) >>= ok)
  where
    shown h = (joined h.spacePath, joined h.pagePath, h.title)
    joined = T.intercalate "/" . map slugText

-- | A query for these letters in every space.
for' :: Text -> TitleQuery
for' = titleQuery

--------------------------------------------------------------------------------
-- What is found

finding :: (MonadWiki m) => SpecWith (Fixture m)
finding = describe "what is found" $ do
  it "finds only what someone may view" $ \(Fixture run) -> do
    found run admin (for' "handbook") `shouldReturn` [("team", "", "Team handbook")]
    found run reader (for' "handbook") `shouldReturn` []
    found run Anonymous (for' "handbook") `shouldReturn` []
    found run editor (for' "handbook") `shouldReturn` []

  it "finds in a subspace someone may view, under a space they may not" $ \(Fixture run) -> do
    found run reader (for' "the wiki") `shouldReturn` [("team/projects", "", "The wiki")]
    found run Anonymous (for' "the wiki") `shouldReturn` []

  it "matches a title's start, a later word, a path's segment, and anything inside, in that order" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    for_ ["Rotation", "Duty rotation", "Carousel"] $ \title -> run admin (createPage docs.id Nothing title "") >>= ok
    carousel <- run admin (getPage docs.id (path "carousel")) >>= ok
    _ <- run admin (createPage docs.id (Just carousel.id) "Spinning" "") >>= ok
    found run admin (for' "rot") `shouldReturn` [("docs", "rotation", "Rotation"), ("docs", "duty-rotation", "Duty rotation")]
    found run admin (for' "ousel") `shouldReturn` [("docs", "carousel", "Carousel"), ("docs", "carousel/spinning", "Spinning")]
    found run admin (for' "ROTATION") `shouldReturn` [("docs", "rotation", "Rotation"), ("docs", "duty-rotation", "Duty rotation")]

  it "orders equal titles by their spaces' paths" $ \(Fixture run) -> do
    blog <- spaceAt run "blog"
    docs <- spaceAt run "docs"
    for_ [docs, blog] $ \s -> run admin (createPage s.id Nothing "Glossary" "") >>= ok
    found run Anonymous (for' "glossary") `shouldReturn` [("blog", "glossary", "Glossary"), ("docs", "glossary", "Glossary")]

  it "leaves out the spaces' templates and the front page" $ \(Fixture run) -> do
    found run admin (for' "weekly sync") `shouldReturn` []
    (_, home) <- run admin getHomeDocument >>= ok
    hits <- run admin (findPagesByTitle (for' (T.take 20 home.title))) >>= ok
    map (\h -> h.page) hits `shouldSatisfy` notElem home.id

--------------------------------------------------------------------------------
-- The space left out

leavingOut :: (MonadWiki m) => SpecWith (Fixture m)
leavingOut = describe "the space left out" $ do
  it "leaves out the space asked, but not its subspaces" $ \(Fixture run) -> do
    team <- spaceAt run "team"
    found run admin (for' "the wiki") {except = Just team.id} `shouldReturn` [("team/projects", "", "The wiki")]
    found run admin (for' "handbook") {except = Just team.id} `shouldReturn` []

  it "leaves out nothing for a space that is not there" $ \(Fixture run) ->
    found run admin (for' "handbook") {except = Just (SpaceId "no-such-space")} `shouldReturn` [("team", "", "Team handbook")]

--------------------------------------------------------------------------------
-- Bounds

bounds :: (MonadWiki m) => SpecWith (Fixture m)
bounds = describe "bounds" $ do
  it "finds nothing for fewer than two letters, and refuses more than the most" $ \(Fixture run) -> do
    found run admin (for' "t") `shouldReturn` []
    found run admin (for' "  T  ") `shouldReturn` []
    found run admin (for' "") `shouldReturn` []
    long <- run admin (findPagesByTitle (for' (T.replicate (maxLetters + 1) "a")))
    case long of
      Left (Invalid _) -> pass
      other -> expectationFailure ("expected Invalid, got " <> show other)
    run admin (findPagesByTitle (for' (T.replicate maxLetters "a"))) `shouldReturn` Right []

  it "gives at most the limit asked, the best first, and never more than the most" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    for_ [1 .. maxTitleHits + 5] $ \n -> run admin (createPage docs.id Nothing ("Quokka " <> show n) "") >>= ok
    everything <- found run admin (for' "quokka") {limit = 100}
    length everything `shouldBe` maxTitleHits
    found run admin (for' "quokka") {limit = 3} `shouldReturn` take 3 everything
    found run admin (for' "quokka") {limit = 0} `shouldReturn` []
    found run admin (for' "quokka") {limit = -1} `shouldReturn` []
    map (\(_, _, t) -> t) (take 3 everything) `shouldBe` ["Quokka 1", "Quokka 10", "Quokka 11"]

--------------------------------------------------------------------------------
-- Changes

changes :: (MonadWiki m) => SpecWith (Fixture m)
changes = describe "after changes" $ do
  it "finds a page by its title now, at its path now, and not once deleted" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    blog <- spaceAt run "blog"
    p <- run admin (createPage docs.id Nothing "Wombat" "") >>= ok
    found run admin (for' "wombat") `shouldReturn` [("docs", "wombat", "Wombat")]
    renamed <- run admin (savePage p.id p.updatedAt "Burrow" "") >>= ok
    found run admin (for' "wombat") `shouldReturn` [("docs", "wombat", "Burrow")]
    found run admin (for' "burrow") `shouldReturn` [("docs", "wombat", "Burrow")]
    _ <- run admin (movePage renamed.id blog.id Nothing) >>= ok
    found run admin (for' "burrow") `shouldReturn` [("blog", "wombat", "Burrow")]
    run admin (deletePage renamed.id) >>= ok
    found run admin (for' "burrow") `shouldReturn` []

  it "finds the pages of a space made private for those still granted only" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    _ <- run admin (createPage docs.id Nothing "Platypus" "") >>= ok
    found run Anonymous (for' "platypus") `shouldReturn` [("docs", "platypus", "Platypus")]
    _ <- run admin (setSpacePublic docs.id False) >>= ok
    found run Anonymous (for' "platypus") `shouldReturn` []
    found run reader (for' "platypus") `shouldReturn` []
    found run admin (for' "platypus") `shouldReturn` [("docs", "platypus", "Platypus")]

--------------------------------------------------------------------------------
-- Generated wikis

-- | A wiki to build: its spaces, each under an earlier one or at the top
-- level, then a page to move and one to delete, by number among the pages
-- made.
data Wiki = Wiki
  { spaces :: [NewSpace]
  , moved :: Maybe (Int, Int)
  -- ^ The page moved, and the space it goes to, at its top level.
  , deleted :: Maybe Int
  }
  deriving stock (Show)

data NewSpace = NewSpace
  { parent :: Maybe Int
  , public :: Bool
  , sealed :: Bool
  , granted :: Bool
  -- ^ Whether the reader is granted to view it.
  , pages :: [NewPage]
  }
  deriving stock (Show)

data NewPage = NewPage
  { title :: Text
  , under :: Maybe Int
  -- ^ An earlier page of the same space it is made under.
  , template :: Bool
  }
  deriving stock (Show)

-- | Titles that share their starts, words and letters, some equal but for
-- case or accents.
titles :: [Text]
titles = ["Onboarding", "onboarding", "ONBOARDING", "Önboarding", "On-call", "On call rota", "Deploys", "Déploiement", "Release notes", "Notes", "Zebra crossing", "A note on zebras"]

-- | Letters typed: starts, words, a path's segment, a piece inside, a
-- letter with an accent, one letter, letters that match nothing.
typings :: [Text]
typings = ["on", "ON", "onb", "ön", "on c", "call", "de", "dé", "not", "es", "ze", "z", "qx", " no "]

genWiki :: Gen Wiki
genWiki = do
  n <- chooseInt (1, 4)
  spaces <- for [0 .. n - 1] $ \i -> do
    parent <- if i == 0 then pure Nothing else oneof [pure Nothing, Just <$> chooseInt (0, i - 1)]
    public <- elements [False, False, True]
    sealed <- elements [False, False, True]
    granted <- elements [False, True]
    k <- chooseInt (0, 5)
    pages <- for [0 .. k - 1] $ \j -> do
      title <- elements titles
      under <- if j == 0 then pure Nothing else oneof [pure Nothing, Just <$> chooseInt (0, j - 1)]
      template <- elements [False, False, False, True]
      pure NewPage {title, under, template}
    pure NewSpace {parent, public, sealed, granted, pages}
  let total = sum (map (\s -> length s.pages) spaces)
  moved <- if total == 0 then pure Nothing else oneof [pure Nothing, (\a b -> Just (a, b)) <$> chooseInt (0, total - 1) <*> chooseInt (0, n - 1)]
  deleted <- if total == 0 then pure Nothing else oneof [pure Nothing, Just <$> chooseInt (0, total - 1)]
  pure Wiki {spaces, moved, deleted}

-- | Queries to ask of a wiki: letters, perhaps one of its spaces left out
-- (by number), and a limit.
genQueries :: Gen [(Text, Maybe Int, Int)]
genQueries = do
  picked <- sublistOf typings
  extra <- listOf (elements typings)
  for (take 6 (picked <> extra)) $ \t -> (t,,) <$> oneof [pure Nothing, Just <$> chooseInt (0, 3)] <*> choose (1, 25)

-- | Build the wiki through the backend as the administrator: the spaces
-- made. A name another case gave a space already gets its slug with a
-- number, as any space does, so the cases do not meet.
build :: (HasCallStack, MonadWiki m) => Run m -> UserId -> Wiki -> IO [Space]
build run readerId w = do
  made <- foldM makeSpace [] (zip [0 :: Int ..] w.spaces)
  let spaces = reverse made
  pages <- fmap concat . for (zip spaces w.spaces) $ \(s, ns) -> do
    ps <- foldM (makePage s) [] ns.pages
    pure (reverse ps)
  for_ w.moved $ \(i, j) -> for_ ((,) <$> (pages `atMay` i) <*> (spaces `atMay` j)) $ \(p, s) ->
    void (run admin (movePage p.id s.id Nothing))
  for_ w.deleted $ \i -> for_ (pages `atMay` i) $ \p -> void (run admin (deletePage p.id))
  pure spaces
  where
    makeSpace done (i, ns) = do
      let above = ns.parent >>= \k -> reverse done `atMay` k
      s <- run admin (createSpace ((\x -> x.id) <$> above) ("Generated " <> show i)) >>= ok
      when ns.public $ void (run admin (setSpacePublic s.id True) >>= ok)
      when ns.sealed $ void (run admin (setSpaceSealed s.id True) >>= ok)
      when ns.granted $ run admin (setSpaceGrant s.id (SubjectUser readerId) View True) >>= ok
      pure (s : done)
    makePage s done np = do
      let above = np.under >>= \k -> reverse done `atMay` k
      p <- run admin (createPage s.id ((\x -> x.id) <$> above) np.title "") >>= ok
      when np.template $ void (run admin (setPageTemplate p.id True) >>= ok)
      pure (p : done)

-- | What a lookup must give someone: the pages of the spaces whose pages
-- they may list, but the front page's space and the space left out,
-- templates left out, ranked by 'rankTitles'.
expected :: (MonadWiki m) => Run m -> As -> TitleQuery -> IO [TitleHit]
expected run who q = do
  s <- run who session
  listed <- run who listSpaces
  let byId = Map.fromList [(x.id, x) | x <- listed]
      pathOf x = reverse (go x)
      go x = x.slug : maybe [] go (x.parent >>= (`Map.lookup` byId))
  candidates <- fmap concat . for listed $ \x ->
    if Just x.id == q.except || isHomeSpace s.homePage x.id || isJust x.encryption
      then pure []
      else
        run who (listPages x.id) <&> \case
          Left _ -> []
          Right ps ->
            let pagesById = Map.fromList [(p.id, p) | p <- ps]
             in [ TitleHit {page = p.id, space = x.id, spacePath = pathOf x, pagePath, title = p.title}
                | p <- ps
                , not p.template
                , Just pagePath <- [Space.pagePath x (`Map.lookup` pagesById) p.id]
                ]
  pure (rankTitles q candidates)

generated :: (MonadWiki m) => SpecWith (Fixture m)
generated = describe "generated wikis" $
  modifyMaxSuccess (const 8) $
    it "finds for each person what their listings hold, ranked the same way" $ \(Fixture run) ->
      forAll genWiki $ \w -> forAll genQueries $ \qs -> ioProperty $ do
        readerId <- run reader currentUser >>= maybe (assertFailure "nobody is signed in as the reader") (pure . (\u -> u.id))
        spaces <- build run readerId w
        let queries = [TitleQuery {letters, except = except >>= \k -> (\x -> x.id) <$> spaces `atMay` k, limit} | (letters, except, limit) <- qs]
        checks <- for [(who, query) | who <- [admin, editor, reader, Anonymous], query <- queries] $ \(who, query) -> do
          got <- run who (findPagesByTitle query) >>= ok
          want <- expected run who query
          pure (counterexample (show (who, query)) (got === want))
        pure (conjoin checks :: Property)

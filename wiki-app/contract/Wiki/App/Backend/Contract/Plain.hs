-- | The contract tests of the question whether a page's text is taken as
-- plain text now ('plainNow'), which the browser asks before it sends any
-- of it: a save, a draft, an attachment, a comment, the letters typed for
-- a mention.
--
-- Like the backend's contract ("Wiki.App.Backend.Contract", whose
-- 'Fixture' it takes), it runs against the in-memory backend, the server's
-- storage on each database, and the HTTP backend against the server's API.
-- The demo's @admin@ may do anything; its @editor@ may edit the public
-- blog; its @reader@ may read the blog and the docs, and of the team's
-- spaces only its projects. The demo has no encrypted space: the answers
-- for one, and for one being converted, are the storage's tests
-- ("Test.Storage.Encrypted" in wiki-server), which make them.
module Wiki.App.Backend.Contract.Plain (spec) where

import Protolude hiding (reader)
import Test.HUnit (assertFailure)
import Test.Hspec
import Wiki.App.Backend
import Wiki.App.Backend.Contract (As (..), Fixture (..), Run)
import Wiki.Core.Id
import Wiki.Core.Page (Page (..))
import Wiki.Core.Path (parsePath)
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (..))

admin, editor, reader :: As
admin = As "admin"
editor = As "editor"
reader = As "reader"

-- | The contract, for a backend made afresh for each example, as in
-- "Wiki.App.Backend.Contract".
spec :: forall m -> (MonadWiki m) => (ActionWith (Fixture m) -> IO ()) -> Spec
spec _ withFixture = around withFixture $ describe "plainNow contract" $ do
  it "says yes for a page of a plain space someone may view, editor or not" $ \(Fixture run) -> do
    docs <- homeOf run "docs"
    blog <- homeOf run "blog"
    run admin (plainNow docs) `shouldReturn` Right ()
    -- The reader may view the docs, not edit them: a comment is a plain
    -- write too.
    run reader (plainNow docs) `shouldReturn` Right ()
    run editor (plainNow blog) `shouldReturn` Right ()
    run Anonymous (plainNow blog) `shouldReturn` Right ()

  it "says yes for the front page, whose space is never encrypted" $ \(Fixture run) -> do
    (_, home) <- run admin getHomeDocument >>= ok
    run admin (plainNow home.id) `shouldReturn` Right ()

  it "says yes for a page made just now, never read" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    made <- run admin (createPage docs.id Nothing "Made now" "Text.") >>= ok
    run admin (plainNow made.id) `shouldReturn` Right ()

  it "says not found for a page someone may not view, as for an id no page has" $ \(Fixture run) -> do
    team <- homeOf run "team"
    run reader (plainNow team) `shouldReturn` Left NotFound
    run Anonymous (plainNow team) `shouldReturn` Left NotFound
    run admin (plainNow (PageId "no-such-page")) `shouldReturn` Left NotFound

  it "answers for the page where it is now, not where it was read" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    team <- spaceAt run "team"
    page <- run admin (createPage docs.id Nothing "Moving" "Text.") >>= ok
    run reader (plainNow page.id) `shouldReturn` Right ()
    void (run admin (movePage page.id team.id Nothing) >>= ok)
    run reader (plainNow page.id) `shouldReturn` Left NotFound
    void (run admin (movePage page.id docs.id Nothing) >>= ok)
    run reader (plainNow page.id) `shouldReturn` Right ()

  it "says not found for a page deleted since" $ \(Fixture run) -> do
    docs <- spaceAt run "docs"
    page <- run admin (createPage docs.id Nothing "Gone soon" "Text.") >>= ok
    void (run admin (deletePage page.id) >>= ok)
    run admin (plainNow page.id) `shouldReturn` Left NotFound

ok :: (HasCallStack, Show e) => Either e a -> IO a
ok = either (\e -> assertFailure ("expected success, got " <> show e)) pure

path :: Text -> [Slug]
path t = either (\e -> panic ("bad path in a test: " <> t <> ", " <> show e)) identity (parsePath t)

-- | The space at a path, as the administrator finds it.
spaceAt :: (HasCallStack, MonadWiki m) => Run m -> Text -> IO Space
spaceAt run s = run admin (getSpace (path s)) >>= ok

-- | The home page of the space at a path.
homeOf :: (HasCallStack, MonadWiki m) => Run m -> Text -> IO PageId
homeOf run s = spaceAt run s >>= \x -> maybe (assertFailure ("no home page in " <> toS s)) pure x.homePage

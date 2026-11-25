-- | External plugins' frames in a real browser
-- ("Wiki.App.Plugin.External.Frames", "Wiki.App.Plugin.External.Frame"):
-- the component the editor mounts, over the demo's wiki with plugins
-- installed for everyone, runs each in a sandboxed frame with the wiki's
-- runtime and its code in a worker, and speaks the protocol with it over
-- the frame's port: the example plugin (examples/plugins/doubled-words)
-- marks a word written twice, and a plugin whose code loops is stopped,
-- its frame removed, while the page goes on.
--
-- The frames' documents are the server's, written by
-- test/web/plugin-frames.mjs: the runner serves a file by its extension,
-- and a frame's address on the server has none, so the test's monad opens
-- the frame at the written file's address in the place of the plugin's
-- ('openPluginFrame'). The rest is the program's, and the browser's. The
-- policy the server puts on the frame, and the editor around the
-- component, are the smoke test's (wiki-smoke's Server.Plugin).
module Test.Browser.PluginFrames (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.IORef (IORef)
import Data.Map.Strict qualified as Map
import Data.NT (type (~>) (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen
import Wiki.Api.Plugins (PluginId (..), noPlugins)
import Wiki.Api.Plugins.External (Contribution (..), ExternalManifest (..), ExternalPlugin (..), ExternalPolicy (..), Permission (..))
import Wiki.App.Backend (MonadWiki)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands (..))
import Wiki.App.Plugin.External (Heard (..))
import Wiki.App.Plugin.External.Frames qualified as Frames
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Markdown.Problems (Problem (..), ProblemKind (..), Severity (..))

-- | The demo's backend, opening each plugin's frame at the document the
-- test wrote for the plugin.
newtype Written m a = Written (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadTrans Written where
  lift = Written

runWritten :: Written m a -> m a
runWritten (Written m) = m

instance (MonadWiki m) => MonadWiki (Written m)

instance (MonadUUID m) => MonadUUID (Written m) where
  generateV4 = lift generateV4

instance (MonadIslands m) => MonadIslands (Written m) where
  openPluginFrame container address = lift . openPluginFrame container written
    where
      written = case filter (\p -> ("/" <> p <> "/") `T.isInfixOf` address) ["doubled-words", "looping"] of
        p : _ -> "plugin-frames/" <> p <> ".html"
        [] -> address

-- | A plugin installed for everyone, by its id and name.
installed :: Text -> Text -> ExternalPlugin
installed pid name =
  ExternalPlugin
    { manifest =
        ExternalManifest
          { id = PluginId pid
          , name
          , version = "1.0.0"
          , description = ""
          , api = 1
          , contributes = Set.singleton Diagnostics
          , permissions = Set.singleton DocumentRead
          , hosts = []
          }
    , bundle = "0123abcd"
    , size = 100
    , mine = False
    , installedAt = UTCTime (fromGregorian 2026 11 25) 0
    }

-- | The demo's wiki, the editor signed in, with these plugins installed
-- for everyone.
wikiWith :: [ExternalPlugin] -> IO (IORef MemState)
wikiWith ps = newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor"), externalPolicy = ExternalWiki, externalWiki = Map.fromList [(p.manifest.id, p) | p <- ps]})

spec :: forall m -> (MonadBrowserTest m, MonadIslands m, MonadUUID m) => Spec
spec m = describe "external plugins' frames" $ do
  let input text = Frames.Input {session = 0, known = noPlugins, text = Just text, encrypted = False}
      frames wiki = H.hoist (NT (runMemWiki wiki . runWritten)) (Frames.component :: H.Component (Const Void) Frames.Input Frames.Output (Written (MemWiki m)))
      lastHeard ui = (\os -> case lastMay os of Just (Frames.Heard h) -> h; Nothing -> mempty) <$> outputs ui

  it "runs the example plugin in its sandboxed frame, and hears it mark a word written twice, on its line" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "doubled-words" "Doubled words"])
    let text = "Fine.\n😀 This is the the end."
    ui <- mount m (frames wiki) (input text)
    -- The frame loads, the runtime starts the code, the code says it is
    -- ready, the text goes out and the problems come back: the program
    -- waits on the plugin's answer, a pause the answer ends.
    waitQuiet
    lastHeard ui
      `shouldReturn` Map.fromList [(PluginId "doubled-words", Heard {text, problems = [Problem {from = 2, to = 2, kind = FromPlugin, severity = Warning, message = "Doubled words: Doubled word: the"}]})]
    length <$> findAll ui "iframe[sandbox='allow-scripts']" `shouldReturn` 1
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0

  it "stops a plugin whose code loops, removes its frame, and says why, while the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "looping" "Looping"])
    ui <- mount m (frames wiki) (input "Any text.")
    length <$> findAll ui "iframe" `shouldReturn` 1
    -- The program waits on the plugin's answer, which never comes, until
    -- the pause after which the plugin is stopped.
    waitQuietWithin 30000
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    stopped `shouldSatisfy` T.isInfixOf "The plugin Looping was stopped: it did not answer within 10 seconds"
    length <$> findAll ui "iframe" `shouldReturn` 0
    lastHeard ui `shouldReturn` mempty

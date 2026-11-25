{-# LANGUAGE TemplateHaskell #-}

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
import Halogen.JSBits (Safety (..), browserJS)
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

$( browserJS
     ["jsbits/test-plugin-frames.js"]
     [("js_frames_open", "wiki_test_plugin_frames_open", Safe, [t|IO Int|])]
 )

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
      written = case filter (\p -> ("/" <> p <> "/") `T.isInfixOf` address) ["doubled-words", "looping", "strict", "sloppy", "many", "tangled-raw", "deep-raw", "flooding-raw"] of
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
  let input text = Frames.Input {session = 0, known = noPlugins, choicesRead = True, text = Just text, encrypted = False}
      frames wiki = H.hoist (NT (runMemWiki wiki . runWritten)) (Frames.component :: H.Component (Const Void) Frames.Input Frames.Output (Written (MemWiki m)))
      lastHeard ui = (\os -> case lastMay os of Just (Frames.Heard h) -> h; Nothing -> mempty) <$> outputs ui

  it "runs the example plugin in its sandboxed frame, and hears it mark a word written twice, on its line" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "doubled-words" "Doubled words"])
    let text = "Fine.\n😀 This is the the end."
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- mount m (frames wiki) (input text)
    -- The frame loads, the runtime starts the code, the code says it is
    -- ready, the text goes out and the problems come back: the program
    -- waits on the plugin's answer, a pause the answer ends.
    waitQuiet
    lastHeard ui
      `shouldReturn` Map.fromList [(PluginId "doubled-words", Heard {text, problems = [Problem {from = 2, to = 2, kind = FromPlugin, severity = Warning, message = "Doubled words: Doubled word: the"}]})]
    length <$> findAll ui "iframe[sandbox='allow-scripts']" `shouldReturn` 1
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0
    -- The editor closes: the program closes the frame it opened, its port
    -- and its callback with it.
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore + 1
    unmount ui
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore

  it "stops a plugin whose code loops, removes its frame, and says why, while the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "looping" "Looping"])
    ui <- mount m (frames wiki) (input "Any text.")
    -- The program waits on the plugin's answer, which never comes, until
    -- the pause after which the plugin is stopped.
    waitQuietWithin 30000
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    stopped `shouldSatisfy` T.isInfixOf "The plugin Looping was stopped: it did not answer within 10 seconds"
    length <$> findAll ui "iframe" `shouldReturn` 0
    lastHeard ui `shouldReturn` mempty

  -- Frames without the wiki's runtime, whose page takes the port itself:
  -- the host keeps its rules whatever the runtime would have passed on.
  it "stops a frame that sends a small graph of shared parts, before writing it out, and the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "tangled-raw" "Tangled"])
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- mount m (frames wiki) (input "Any text.")
    waitQuiet
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    stopped `shouldSatisfy` T.isInfixOf "The plugin Tangled was stopped: it sent a message that is not text of at most 1 MiB, or not JSON"
    length <$> findAll ui "iframe" `shouldReturn` 0
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore

  it "stops a frame that floods its port at its first message beyond those asked for, and the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "flooding-raw" "Flooding"])
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- mount m (frames wiki) (input "Any text.")
    waitQuiet
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    -- Its second ready comes either before the program has asked it for
    -- anything, beyond what it may send, or after, a second ready: the
    -- browser's order of the two, and either stops it there.
    stopped `shouldSatisfy` \s ->
      any (`T.isInfixOf` s) ["The plugin Flooding was stopped: it sent a message it was not asked for", "The plugin Flooding was stopped: it said it was ready a second time"]
    length <$> findAll ui "iframe" `shouldReturn` 0
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore

  it "parses a message as long as one may be and nested as deep as that lets it, then stops the plugin for what it is, and the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "deep-raw" "Deep"])
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- mount m (frames wiki) (input "Any text.")
    waitQuiet
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    stopped `shouldSatisfy` T.isInfixOf "The plugin Deep was stopped: it sent a message the wiki cannot read"
    length <$> findAll ui "iframe" `shouldReturn` 0
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore

  -- The runtime runs the plugin's code after its own line, in the mode the
  -- code asks for, as the frame says it does.
  it "runs a bundle that begins with \"use strict\" in strict mode, and one without in sloppy mode" $ runPage $ do
    for_ [("strict", "Strict"), ("sloppy", "Sloppy")] $ \(pid, name) -> do
      wiki <- unsafeIOToPageM (wikiWith [installed pid name])
      ui <- mount m (frames wiki) (input "Any text.")
      waitQuiet
      (map (\h -> map (\p -> p.message) h.problems) . Map.elems <$> lastHeard ui) `shouldReturn` [[name <> ": " <> pid]]
      unmount ui

  -- What a plugin returns is kept within the protocol's limits by the
  -- wiki's own code in the worker: an author is not stopped for them.
  it "keeps the first 500 problems of an answer and cuts each message to 300 characters, and does not stop the plugin" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "many" "Many"])
    ui <- mount m (frames wiki) (input "Any text.")
    waitQuiet
    heard <- Map.elems <$> lastHeard ui
    map (\h -> length h.problems) heard `shouldBe` [500]
    map (\h -> map (\p -> p.message) (take 1 h.problems)) heard `shouldBe` [["Many: " <> T.replicate 300 "m"]]
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0

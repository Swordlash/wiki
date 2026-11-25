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
-- Then the settings, the whole application over the demo's backend outside
-- the demo: an administrator lets the wiki's plugins run, picks the
-- example plugin's two files, confirms the warning, sees it listed, and
-- finds its problem in an editor.
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
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.ByteString.Base64 qualified as Base64
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map.Strict qualified as Map
import Data.NT (type (~>) (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen qualified as H
import Halogen.Activity.Control (withTestClock)
import Halogen.JSBits (JSText, Safety (..), browserJS, inBrowser, inert, toJSText)
import Halogen.JSBits.Promise (Promise, wait)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad.Class
  ( MonadAttributes
  , MonadBrowserDOM
  , MonadDOM
  )
import Protolude hiding (find, wait)
import Test.Browser.Mount (clickButton, forgetStorage, slugs, startChangedWith, testRouter)
import Test.Hspec (around)
import Test.Hspec.Halogen
import Wiki.Api.Plugins (PluginId (..), noPlugins)
import Wiki.Api.Plugins.External (Contribution (..), ExternalManifest (..), ExternalPlugin (..), ExternalPolicy (..), Permission (..), noExternalPlugins)
import Wiki.App (component, input)
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..))
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Comments (MonadComments)
import Wiki.App.Backend.Encrypted (MonadEncryption)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..), externalPluginsIn)
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Backend.Plugins (MonadPlugins)
import Wiki.App.Backend.Stars (MonadStars)
import Wiki.App.Backend.WhatsNew (MonadWhatsNew)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.DemoUsers (MonadDemoUsers)
import Wiki.App.Guide (exampleCode, exampleManifest)
import Wiki.App.Islands (MonadIslands (..))
import Wiki.App.Plugin.External (Heard (..))
import Wiki.App.Plugin.External.Frames qualified as Frames
import Wiki.App.Route (Route (..))
import Wiki.App.Route qualified as R
import Wiki.App.Security (MonadSecurity)
import Wiki.App.Shell qualified as Shell
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Markdown.Problems (Problem (..), ProblemKind (..), Severity (..))
import Wiki.Core.Page qualified as Core (Page (..))

$( browserJS
     ["jsbits/test-plugin-frames.js", "jsbits/test-moves.js", "jsbits/test-files.js"]
     [ ("js_frames_open", "wiki_test_plugin_frames_open", Safe, [t|IO Int|])
     , ("js_choose_two_filesPromise", "wiki_test_choose_two_files", Safe, [t|JSText -> JSText -> JSText -> JSText -> JSText -> JSText -> JSText -> IO (Promise ())|])
     , ("js_watch_frame_moves", "wiki_test_watch_frame_moves", Safe, [t|IO ()|])
     , ("js_frame_moved", "wiki_test_frame_moved", Safe, [t|IO Bool|])
     ]
 )

-- | The demo's backend, opening each plugin's frame at the document the
-- test wrote for the plugin.
newtype Written m a = Written (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

deriving newtype instance (MonadDOM m) => MonadDOM (Written m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Written m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Written m)

instance (MonadAdmin m) => MonadAdmin (Written m)

instance (MonadComments m) => MonadComments (Written m)

instance (MonadNotifications m) => MonadNotifications (Written m)

instance (MonadStars m) => MonadStars (Written m)

instance (MonadWhatsNew m) => MonadWhatsNew (Written m)

instance (MonadSecurity m) => MonadSecurity (Written m)

instance (MonadPlugins m) => MonadPlugins (Written m)

instance (MonadAssistant m) => MonadAssistant (Written m)

instance (MonadVault m) => MonadVault (Written m)

instance (MonadEncryption m) => MonadEncryption (Written m)

instance (MonadDemoUsers m) => MonadDemoUsers (Written m)

js_choose_two_files :: JSText -> JSText -> JSText -> JSText -> JSText -> JSText -> JSText -> IO ()
js_choose_two_files a1 a2 a3 a4 a5 a6 a7 = if inBrowser then wait =<< js_choose_two_filesPromise a1 a2 a3 a4 a5 a6 a7 else pure inert

instance MonadTrans Written where
  lift = Written

runWritten :: Written m a -> m a
runWritten (Written m) = m

instance (MonadWiki m) => MonadWiki (Written m)

-- The in-memory backend reads everything at once.
instance (MonadWiki m) => MonadLoad (Written m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadUUID m) => MonadUUID (Written m) where
  generateV4 = lift generateV4

instance (MonadIslands m) => MonadIslands (Written m) where
  openPluginFrame container address = lift . openPluginFrame container written
    where
      written = case filter (\p -> ("/" <> p <> "/") `T.isInfixOf` address) ["doubled-words", "looping", "strict", "sloppy", "many", "shadow-self", "shadow-names", "shadow-array", "own-name", "tangled-raw", "deep-raw", "flooding-raw"] of
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

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
spec m = describe "external plugins' frames" $ do
  -- The plugins installed are the editor's input, as the shell's
  -- plugins' state read them from the wiki.
  let inputOf wiki text = unsafeIOToPageM (readIORef wiki) <&> \st -> Frames.Input {session = 0, known = noPlugins, installed = either (const noExternalPlugins) identity (externalPluginsIn st), choicesRead = True, text = Just text, encrypted = False}
      frames wiki = H.hoist (NT (runMemWiki wiki . runWritten)) (Frames.component :: H.Component (Const Void) Frames.Input Frames.Output (Written (MemWiki m)))
      lastHeard ui = (\os -> case lastMay os of Just (Frames.Heard h) -> h; Nothing -> mempty) <$> outputs ui

  it "runs the example plugin in its sandboxed frame, and hears it mark a word written twice, on its line" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "doubled-words" "Doubled words"])
    let text = "Fine.\n😀 This is the the end."
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- inputOf wiki text >>= mount m (frames wiki)
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
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
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
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
    waitQuiet
    stopped <- find ui ".editor-plugin-stopped" >>= textContent
    stopped `shouldSatisfy` T.isInfixOf "The plugin Tangled was stopped: it sent a message that is not text of at most 1 MiB, or not JSON"
    length <$> findAll ui "iframe" `shouldReturn` 0
    unsafeIOToPageM js_frames_open `shouldReturn` openBefore

  it "stops a frame that floods its port at its first message beyond those asked for, and the page goes on" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "flooding-raw" "Flooding"])
    openBefore <- unsafeIOToPageM js_frames_open
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
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
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
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
      ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
      waitQuiet
      (map (\h -> map (\p -> p.message) h.problems) . Map.elems <$> lastHeard ui) `shouldReturn` [[name <> ": " <> pid]]
      unmount ui

  -- What a plugin returns is kept within the protocol's limits by the
  -- wiki's own code in the worker: an author is not stopped for them.
  it "keeps the first 500 problems of an answer and cuts each message to 300 characters, and does not stop the plugin" $ runPage $ do
    wiki <- unsafeIOToPageM (wikiWith [installed "many" "Many"])
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
    waitQuiet
    heard <- Map.elems <$> lastHeard ui
    map (\h -> length h.problems) heard `shouldBe` [500]
    map (\h -> map (\p -> p.message) (take 1 h.problems)) heard `shouldBe` [["Many: " <> T.replicate 300 "m"]]
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0

  -- The wiki's line runs in the same script as the bundle, before it,
  -- while the bundle's top-level bindings exist uninitialized: it names
  -- none of them.
  it "registers and answers a bundle whose top-level bindings name what the wiki's line uses, and stops one that declares wikiPlugin, saying so" $ runPage $ do
    for_ ["shadow-self", "shadow-names", "shadow-array"] $ \pid -> do
      wiki <- unsafeIOToPageM (wikiWith [installed pid "Shadow"])
      ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
      waitQuiet
      (pid, map (\h -> map (\p -> p.message) h.problems) . Map.elems <$> lastHeard ui) `shouldReturn'` (pid, [["Shadow: answered"]])
      unmount ui
    wiki <- unsafeIOToPageM (wikiWith [installed "own-name" "Own name"])
    ui <- inputOf wiki "Any text." >>= mount m (frames wiki)
    waitQuiet
    find ui ".editor-plugin-stopped" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "The plugin Own name was stopped: it failed: its code declares the name wikiPlugin itself, which hides the wiki's")

  -- Codex's fourth round on #381: the frames were among the editor's
  -- notices, and two notices before them going at once ("Take theirs",
  -- with a draft restored and a save's conflict shown) made Halogen move
  -- them, and a frame whose ancestor moves loads again, which stopped its
  -- plugin. The whole application, on the test's clock, under which the
  -- plugin's frame (the server's address, which the runner does not
  -- serve) never says it is ready, and is not stopped for it.
  around withTestClock . it "keeps a plugin's frame where it is when notices before it go together" $ \_ -> runPage $ do
    (ui, wiki) <- startChangedWith m withExample (Just "user-editor") (Page (slugs ["blog"]) (slugs ["hello-world"]))
    page <- unsafeIOToPageM (helloWorld wiki)
    -- A draft of the author's, which the editor restores.
    _ <- unsafeIOToPageM (runMemWiki wiki (saveDraft page.id page.updatedAt page.title "# Hello, world\n\nMy words."))
    find ui ".page-view .actions a[href$='/edit/']" >>= click
    _ <- find ui ".editor-draft-found"
    frame <- find ui "iframe.editor-plugin-frame"
    unsafeIOToPageM js_watch_frame_moves
    -- Someone else saves; the author's save meets theirs.
    unsafeIOToPageM $ do
      modifyIORef' wiki (\st -> st {current = Just (UserId "user-admin")})
      runMemWiki wiki (savePage page.id page.updatedAt page.title "# Hello, world\n\nSomeone else's words.") >>= either (panic . show) (const pass)
      modifyIORef' wiki (\st -> st {current = Just (UserId "user-editor")})
    find ui ".editor-save" >>= click
    _ <- find ui ".editor-conflict"
    clickButton ui ".editor-conflict button" "Take theirs"
    length <$> findAll ui ".editor-draft-found" `shouldReturn` 0
    length <$> findAll ui ".editor-conflict" `shouldReturn` 0
    -- The frame never left its place: nothing holding it was taken out or
    -- put back, so it did not load again.
    unsafeIOToPageM js_frame_moved `shouldReturn` False
    find ui "iframe.editor-plugin-frame" >>= (`shouldBeSameElement` frame)
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0
  -- The settings: the files are the example's, as the guide shows them
  -- ("Wiki.App.Guide"), picked together.
  it "installs the example plugin from the administration after the warning, lists it, and runs it in an editor" $ runPage $ do
    wiki <- unsafeIOToPageM (newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")}))
    page <- unsafeIOToPageM (helloWorld wiki)
    -- The page's text has a word written twice.
    _ <- unsafeIOToPageM (runMemWiki wiki (savePage page.id page.updatedAt page.title "# Hello, world\n\nSaid the the cat.\n")) >>= either (panic . show) pure
    router <- unsafeIOToPageM testRouter
    unsafeIOToPageM forgetStorage
    unsafeIOToPageM (Router.redirect router (Admin R.Permissions))
    ui <- mount m (H.hoist (NT (runMemWiki wiki . runWritten)) component) ((input router) {Shell.demo = Nothing})
    waitQuiet
    find ui ".admin-external-policy-wiki" >>= click
    waitQuiet
    unsafeIOToPageM ((\st -> st.externalPolicy) <$> readIORef wiki) `shouldReturn` ExternalWiki
    let encoded = toJSText . decodeUtf8 . Base64.encode . encodeUtf8
        js t = toJSText (t :: Text)
    unsafeIOToPageM (js_choose_two_files (js ".admin-external .plugin-add input[type=file]") (js "manifest.json") (js "application/json") (encoded exampleManifest) (js "doubled-words.js") (js "text/javascript") (encoded exampleCode))
    waitQuiet
    find ui ".plugin-warning .plugin-hosts" >>= textContent >>= (`shouldSatisfy` T.isPrefixOf "It may connect to no other site")
    unsafeIOToPageM (Map.keys . (\st -> st.externalWiki) <$> readIORef wiki) `shouldReturn` []
    find ui ".plugin-warning .plugin-confirm" >>= click
    waitQuiet
    find ui ".admin-external-plugin[data-plugin=doubled-words] .admin-plugin-name" >>= textContent >>= (`shouldBe` "Doubled words")
    -- In the editor of the page, the plugin runs and marks the word.
    unsafeIOToPageM (Router.navigate router (EditPage (slugs ["blog"]) (slugs ["hello-world"])))
    waitQuiet
    find ui ".editor-problems" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "problem")
    length <$> findAll ui ".editor-plugin-stopped" `shouldReturn` 0
  where
    withExample st = st {externalPolicy = ExternalWiki, externalWiki = Map.fromList [(PluginId "doubled-words", installed "doubled-words" "Doubled words")]}
    helloWorld wiki = maybe (panic "no page Hello, world") identity . find' . Map.elems . (\st -> st.pages) <$> readIORef wiki
    find' = headMay . filter (\p -> p.title == "Hello, world")
    shouldReturn' (label, reading) expected = reading >>= \got -> (label, got) `shouldBe` expected

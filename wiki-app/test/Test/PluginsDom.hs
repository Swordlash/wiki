-- | Plugins turned on and off, the real components on the in-memory DOM
-- ("Test.Harness.MemDom") over the demo's wiki: the settings' section
-- "Plugins" ("Wiki.App.View.Plugins") in each of its states and one walk
-- through it, and the editor ("Wiki.App.View.Editor") with each built-in
-- plugin off and on again. The section's rules over any order of events
-- are its machine's ("Test.PluginsSettings").
--
-- The editor runs over 'Here', the demo's backend with two things a
-- browser has: the writing assistant may run ('assistantHere'), so that
-- its button is drawn, and KaTeX fails a formula that holds @\\broken@, so
-- that an island fails.
module Test.PluginsDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), noPlugins)
import Wiki.App.Assistant (MonadAssistant (..))
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..))
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Backend.Plugins (MonadPlugins (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands (..))
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Common (Context (..), ViewOutput (..))
import Wiki.App.View.Editor qualified as Editor
import Wiki.App.View.Plugins qualified as Plugins
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Permission (Grants (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (id))

spec :: Spec
spec = do
  section
  editor

assistant, errors :: PluginId
assistant = PluginId "assistant"
errors = PluginId "errors"

choice :: PluginState -> PluginChoice
choice s = PluginChoice {state = s, options = Nothing}

-- | The demo's wiki, the editor signed in.
asEditor :: MemState
asEditor = (demoState demoNow) {current = Just (UserId "user-editor")}

--------------------------------------------------------------------------------
-- The section

-- | The section mounted over a wiki.
withSection :: MemState -> (View (Const Void) Plugins.Input ViewOutput -> IO a) -> IO a
withSection st body = do
  wiki <- newMemWiki st
  let run :: MemWiki MemDOM a -> MemDOM a
      run = runMemWiki wiki
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  result <- withView run Plugins.component Plugins.Input {session = 0, router} $ \view -> settle view >> body view
  Router.stop router
  pure result

-- | Each plugin's row: its id, whether it shows on, and its switch's
-- state.
rows :: View q i o -> IO [(Text, Bool, Bool, Bool)]
rows view =
  findAll view ".plugin"
    >>= traverse
      ( \r -> do
          i <- fromMaybe "" <$> N.attribute "data-plugin" r
          on' <- T.isInfixOf "plugin-on" . fromMaybe "" <$> N.attribute "class" r
          toggle <- element view (".plugin[data-plugin=" <> i <> "] .plugin-toggle")
          ticked <- property "checked" toggle <&> \case Just (ScalarBool b) -> b; _ -> False
          off <- N.disabledControl toggle
          pure (i, on', ticked, off)
      )

section :: Spec
section = describe "Wiki.App.View.Plugins, mounted on the in-memory DOM" $ do
  it "lists the registry's plugins, both on by default" $
    withSection asEditor $ \view -> do
      rows view `shouldReturn` [("assistant", True, True, False), ("errors", True, True, False)]
      (findAll view ".plugin-default" >>= traverse N.textContent) `shouldReturn` ["Default: on.", "Default: on."]
      (findAll view ".plugin-name" >>= traverse N.textContent) `shouldReturn` ["Writing assistant", "Errors of formulas and diagrams"]
      -- The assistant's options are its own section; errors has none.
      (findAll view ".plugin-options" >>= traverse (N.attribute "href")) `shouldReturn` [Just "/settings/assistant/"]

  it "shows a plugin the user turned off, with the way back to its default" $
    withSection asEditor {pluginChoices = Map.fromList [(UserId "user-editor", Map.fromList [(assistant, choice PluginOff)])]} $ \view -> do
      rows view `shouldReturn` [("assistant", False, False, False), ("errors", True, True, False)]
      textOf view ".plugin[data-plugin=assistant] .plugin-to-default" `shouldReturn` "Back to the default (on)"

  it "shows a plugin off for the wiki as off, its switch disabled, with the reason" $
    withSection asEditor {pluginsOff = Set.fromList [errors], pluginChoices = Map.fromList [(UserId "user-editor", Map.fromList [(errors, choice PluginOn)])]} $ \view -> do
      rows view `shouldReturn` [("assistant", True, True, False), ("errors", False, False, True)]
      textOf view ".plugin[data-plugin=errors] .plugin-off-for-wiki" >>= (`shouldSatisfy` T.isPrefixOf "An administrator has turned this plugin off for everyone")

  it "says the plugins could not be read when the read fails" $
    withSection (demoState demoNow) {current = Nothing, grants = (demoState demoNow).grants {access = Closed}} $ \view -> do
      rows view `shouldReturn` []
      textOf view ".plugins-error" >>= (`shouldSatisfy` T.isPrefixOf "Your plugins could not be read.")

  it "turns the assistant off, tells the shell, and gives it back to its default" $ do
    wiki <- newMemWiki asEditor
    let run :: MemWiki MemDOM a -> MemDOM a
        run = runMemWiki wiki
        mine = runMemDOM (run (fmap (\p -> p.mine) . rightToMaybe <$> pluginsNow))
    router <- runMemDOM (run (Router.newRouter "/" Unknown route))
    withView run Plugins.component Plugins.Input {session = 0, router} $ \view -> do
      settle view
      -- What the section read is told to the shell once.
      told view `shouldReturn` 1
      click view ".plugin[data-plugin=assistant] .plugin-toggle"
      settle view
      rows view `shouldReturn` [("assistant", False, False, False), ("errors", True, True, False)]
      mine `shouldReturn` Just (Map.fromList [(assistant, choice PluginOff)])
      told view `shouldReturn` 2
      click view ".plugin[data-plugin=assistant] .plugin-to-default"
      settle view
      rows view `shouldReturn` [("assistant", True, True, False), ("errors", True, True, False)]
      textOf view ".plugin[data-plugin=assistant] .plugin-default" `shouldReturn` "Default: on."
      mine `shouldReturn` Just mempty
      told view `shouldReturn` 3
    Router.stop router
  where
    told view = length . filter (\case PluginsChanged -> True; _ -> False) <$> outputs view

--------------------------------------------------------------------------------
-- The editor

-- | The demo's backend with what a browser has that the editor's plugins
-- need: the assistant may run, and a formula that holds @\\broken@ fails.
newtype Here m a = Here (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Here where
  lift = Here

runHere :: Here m a -> m a
runHere (Here m) = m

deriving newtype instance (MonadDOM m) => MonadDOM (Here m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Here m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Here m)

instance (MonadUUID m) => MonadUUID (Here m) where
  generateV4 = lift generateV4

instance (MonadWiki m) => MonadWiki (Here m)

instance (MonadWiki m) => MonadLoad (Here m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Here m)

instance (MonadNotifications m) => MonadNotifications (Here m)

instance (MonadVault m) => MonadVault (Here m)

instance (MonadAssistant m) => MonadAssistant (Here m) where
  assistantHere = pure True

instance (MonadIslands m) => MonadIslands (Here m) where
  renderMath mode macros tex el
    | "\\broken" `T.isInfixOf` tex = pure (Just "KaTeX parse error: Undefined control sequence: \\broken")
    | otherwise = lift (renderMath mode macros tex el)

editor :: Spec
editor = describe "Wiki.App.View.Editor, with the plugins turned off and on, on the in-memory DOM" $
  it "draws no assistant's button with the assistant off, counts no island's failure with errors off, and brings both back" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki asEditor
      let run :: Here (MemWiki MemDOM) a -> MemDOM a
          run = runMemWiki wiki . runHere
          orPanic = either (panic . show) identity
      router <- runMemDOM (run (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps) <- runMemDOM . run $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        pure (me, spaces, blog, ps)
      let ctx = viewContextAs router me spaces 0
          inputWith known = Editor.Input {context = ctx {plugins = known}, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
          offForMe p = noPlugins {mine = Map.fromList [(p, choice PluginOff)]}
          offForAll p = noPlugins {offForWiki = Set.fromList [p]}
          buttons view = length <$> findAll view "nav.editor-providers .editor-provider"
          problems view = find view ".editor-problems" >>= maybe (pure Nothing) (fmap Just . N.textContent)
      withView run Editor.component (inputWith noPlugins) $ \view -> do
        settle view
        -- The preview beside the text, which draws the formula typed.
        click view ".editor-side-by-side"
        settle view
        field <- element view ".editor-text"
        original <- N.textContent field
        typeIntoNode field (original <> "\n\nA formula: $\\broken$.\n")
        advance clock Editor.previewDelay
        settle view
        buttons view `shouldReturn` 1
        problems view `shouldReturn` Just "1 problem"
        -- The assistant off for the user: no button; errors still on.
        setInput view (inputWith (offForMe assistant))
        settle view
        buttons view `shouldReturn` 0
        problems view `shouldReturn` Just "1 problem"
        -- Errors off for the wiki: the failure is no problem, and the
        -- island still shows its error in the preview.
        setInput view (inputWith (offForAll errors))
        settle view
        buttons view `shouldReturn` 1
        problems view `shouldReturn` Nothing
        -- Both on again, without a reload.
        setInput view (inputWith noPlugins)
        settle view
        buttons view `shouldReturn` 1
        problems view `shouldReturn` Just "1 problem"
      Router.stop router
  where
    slugs :: [Text] -> [Slug]
    slugs = mapMaybe (rightToMaybe . mkSlug)

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
import Data.Aeson (object)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen.Router (Router)
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), noPlugins)
import Wiki.Api.Plugins.External (Contribution (..), ExternalManifest (..), ExternalPlugin (..), ExternalPolicy (..), Permission (..), maxBundleBytes)
import Wiki.App (component, input)
import Wiki.App.Account (Accounts (..), SignInError (..))
import Wiki.App.Assistant (MonadAssistant (..))
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend
  ( MonadLoad (..)
  , MonadWiki (..)
  , Session
  , WikiError (..)
  , session
  )
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Comments (MonadComments)
import Wiki.App.Backend.Encrypted (MonadEncryption)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Backend.Plugins (MonadPlugins (..))
import Wiki.App.Backend.Stars (MonadStars)
import Wiki.App.Backend.WhatsNew (MonadWhatsNew)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.DemoUsers (MonadDemoUsers)
import Wiki.App.Islands (MonadIslands (..))
import Wiki.App.Islands.Monad (ChosenFile (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.Route (Route (..), SettingsSection (..), route)
import Wiki.App.Route qualified as R
import Wiki.App.Security (MonadSecurity)
import Wiki.App.Shell qualified as Shell
import Wiki.App.View.Common (Context (..))
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (id))

spec :: Spec
spec = do
  section
  editor
  external
  externalSettings

assistant, errors :: PluginId
assistant = PluginId "assistant"
errors = PluginId "errors"

choice :: PluginState -> PluginChoice
choice s = PluginChoice {state = s, options = Nothing}

-- | The demo's wiki, the editor signed in.
asEditor :: MemState
asEditor = (demoState demoNow) {current = Just (UserId "user-editor")}

--------------------------------------------------------------------------------
-- The section, in the shell

-- | 'Holding' is what 'Holder' does: hold the plugins' writes (a user's
-- state, the wiki's set) and the deletion of a former user's keys on the
-- device in a gate until the test lets each go, when there is a gate, and
-- fail every read of the plugins, when asked to.
data Holding = Holding
  { gate :: Maybe (Gate Text ())
  , failing :: Bool
  , afterHandled :: Bool
  -- ^ Whether a user's state is written before the gate holds its answer
  -- (the server handled the write, under the cookie it was sent with),
  -- and not after.
  , readHeld :: Bool
  -- ^ Whether the reads of the plugins on are held in the gate too.
  , framesAsked :: Maybe (IORef [Text])
  -- ^ Where to note the address of each external plugin's frame the
  -- editor asks for, in order.
  , filesChosen :: Maybe (IORef [ChosenFile])
  -- ^ The files a file input's change brings, standing in for the
  -- browser's.
  }

-- | 'Holder' is the demo's backend, as 'Holding' says.
newtype Holder m a = Holder (ReaderT Holding m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Holder where
  lift = Holder . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Holder m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Holder m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Holder m)

instance (MonadIslands m, MonadIO m) => MonadIslands (Holder m) where
  openPluginFrame container address said = do
    Holder (asks (\h -> h.framesAsked)) >>= traverse_ (\r -> liftIO (modifyIORef' r (<> [address])))
    lift (openPluginFrame container address said)
  pluginFilesChosen event = Holder (asks (\h -> h.filesChosen)) >>= maybe (lift (pluginFilesChosen event)) (liftIO . readIORef)

instance (MonadUUID m) => MonadUUID (Holder m) where
  generateV4 = lift generateV4

instance (MonadWiki m) => MonadWiki (Holder m)

-- While the reads of the plugins on are held, nothing is read at once, as
-- the HTTP backend reads nothing at once that it has not cached: a read
-- held in place would hold the program's thread.
instance (MonadWiki m) => MonadLoad (Holder m) where
  atOnce program = Holder (asks (\h -> h.readHeld)) >>= bool (Just <$> program) (pure Nothing)
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Holder m)

instance (MonadComments m) => MonadComments (Holder m)

instance (MonadNotifications m) => MonadNotifications (Holder m)

instance (MonadStars m) => MonadStars (Holder m)

instance (MonadWhatsNew m) => MonadWhatsNew (Holder m)

instance (MonadSecurity m) => MonadSecurity (Holder m)

instance (MonadAssistant m, MonadIO m) => MonadAssistant (Holder m) where
  dropDeviceKey u = heldBy ("drop " <> u.text) >> lift (dropDeviceKey u)

instance (MonadVault m) => MonadVault (Holder m)

instance (MonadEncryption m) => MonadEncryption (Holder m)

instance (MonadDemoUsers m) => MonadDemoUsers (Holder m)

instance (MonadPlugins m, MonadIO m) => MonadPlugins (Holder m) where
  pluginsNow = do
    whenM (Holder (asks (\h -> h.readHeld))) (heldBy "the plugins on")
    Holder (asks (\h -> h.failing)) >>= bool (lift pluginsNow) (pure (Left (Unreachable "the network")))
  setPluginState p s =
    Holder (asks (\h -> h.afterHandled)) >>= \case
      True -> lift (setPluginState p s) <* heldBy ("state " <> p.text)
      False -> heldBy ("state " <> p.text) >> lift (setPluginState p s)
  setWikiPluginsOff w = heldBy "the wiki's set" >> lift (setWikiPluginsOff w)
  externalPluginsNow = do
    whenM (Holder (asks (\h -> h.readHeld))) (heldBy "the plugins installed")
    lift externalPluginsNow

-- | Wait, when there is a gate, until the test lets this write go.
heldBy :: (MonadIO m) => Text -> Holder m ()
heldBy what = Holder (asks (\h -> h.gate)) >>= traverse_ (\gate -> liftIO (hold gate what))

-- | The shell over a wiki, at a route, with the settings' sections mounted
-- from the first render; the body gets the view, the router, the wiki,
-- and a way to settle while the gate holds what it holds.
withShellAt
  :: MemState
  -> Holding
  -> Route
  -> (View Shell.Query (Shell.Input (Holder (MemWiki MemDOM))) Void -> Router Route -> IORef MemState -> IO () -> IO a)
  -> IO a
withShellAt = withShellAtWith identity

-- | 'withShellAt', with the shell's input changed.
withShellAtWith
  :: (Shell.Input (Holder (MemWiki MemDOM)) -> Shell.Input (Holder (MemWiki MemDOM)))
  -> MemState
  -> Holding
  -> Route
  -> (View Shell.Query (Shell.Input (Holder (MemWiki MemDOM))) Void -> Router Route -> IORef MemState -> IO () -> IO a)
  -> IO a
withShellAtWith changed st holding at body = do
  wiki <- newMemWiki st
  let run :: Holder (MemWiki MemDOM) a -> MemDOM a
      run (Holder p) = runMemWiki wiki (runReaderT p holding)
      quiet view = maybe (settle view) (settleHolding view . holds) holding.gate
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  Router.redirect router at
  result <- withView run (component @Void) (changed (input router) {Shell.sectionsAtOnce = True}) $ \view -> do
    quiet view
    body view router wiki (quiet view)
  Router.stop router
  pure result

-- | The section mounted in the shell over a wiki.
withSection :: MemState -> (View Shell.Query (Shell.Input (Holder (MemWiki MemDOM))) Void -> IO a) -> IO a
withSection st body = withShellAt st free (UserSettings PluginsSection) (\view _ _ _ -> body view)

-- | Nothing held, nothing failed.
free :: Holding
free = Holding {gate = Nothing, failing = False, afterHandled = False, readHeld = False, framesAsked = Nothing, filesChosen = Nothing}

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

-- | The rows expected: every plugin of the registry, in its order, as
-- its default has it (on or off, its switch enabled), but for those given
-- (whether it shows on, its switch's state, whether the switch is
-- disabled).
rowsWith :: [(PluginId, (Bool, Bool, Bool))] -> [(Text, Bool, Bool, Bool)]
rowsWith given =
  [ (m.id.text, shown, ticked, off)
  | m <- manifests
  , let (shown, ticked, off) = fromMaybe (m.onByDefault, m.onByDefault, False) (Map.lookup m.id (Map.fromList given))
  ]

-- | The user's choices on the server, as the wiki holds them.
choicesOf :: IORef MemState -> Text -> IO (Map PluginId PluginChoice)
choicesOf wiki u = Map.findWithDefault mempty (UserId u) . (\st -> st.pluginChoices) <$> readIORef wiki

-- | Whether the assistant's section says it is off: the shell's
-- 'Wiki.App.Plugin.Registry.pluginsOn' without the assistant.
saysOff :: View q i o -> IO Bool
saysOff view = isJust <$> find view ".settings-plugin-off"

section :: Spec
section = describe "Wiki.App.View.Plugins, in the shell on the in-memory DOM" $ do
  it "lists the registry's plugins, both on by default" $
    withSection asEditor $ \view -> do
      rows view `shouldReturn` rowsWith []
      (findAll view ".plugin-default" >>= traverse N.textContent) `shouldReturn` ["Default: " <> bool "off" "on" m.onByDefault <> "." | m <- manifests]
      -- The registry's plugins, whatever they are: the assistant among them.
      names <- findAll view ".plugin-name" >>= traverse N.textContent
      names `shouldBe` map (\m -> m.name) manifests
      names `shouldSatisfy` elem "Writing assistant"
      -- The assistant's options are its own section; errors has none.
      (findAll view ".plugin-options" >>= traverse (N.attribute "href")) `shouldReturn` [Just "/settings/assistant/"]

  it "shows a plugin the user turned off, with the way back to its default" $
    withSection asEditor {pluginChoices = Map.fromList [(UserId "user-editor", Map.fromList [(assistant, choice PluginOff)])]} $ \view -> do
      rows view `shouldReturn` rowsWith [(assistant, (False, False, False))]
      textOf view ".plugin[data-plugin=assistant] .plugin-to-default" `shouldReturn` "Back to the default (on)"

  it "shows a plugin off for the wiki as off, its switch disabled, with the reason" $
    withSection asEditor {pluginsOff = Set.fromList [errors], pluginChoices = Map.fromList [(UserId "user-editor", Map.fromList [(errors, choice PluginOn)])]} $ \view -> do
      rows view `shouldReturn` rowsWith [(errors, (False, False, True))]
      textOf view ".plugin[data-plugin=errors] .plugin-off-for-wiki" >>= (`shouldSatisfy` T.isPrefixOf "An administrator has turned this plugin off for everyone")

  it "says the plugins could not be read when the read fails" $
    withShellAt asEditor free {failing = True} (UserSettings PluginsSection) $ \view _ _ _ -> do
      rows view `shouldReturn` []
      textOf view ".plugins-error" >>= (`shouldSatisfy` T.isPrefixOf "Your plugins could not be read.")

  it "turns the assistant off and gives it back to its default, the shell following" $
    withShellAt asEditor free (UserSettings PluginsSection) $ \view router wiki quiet -> do
      click view ".plugin[data-plugin=assistant] .plugin-toggle"
      quiet
      rows view `shouldReturn` rowsWith [(assistant, (False, False, False))]
      choicesOf wiki "user-editor" `shouldReturn` Map.fromList [(assistant, choice PluginOff)]
      click view ".plugin[data-plugin=assistant] .plugin-to-default"
      quiet
      rows view `shouldReturn` rowsWith []
      textOf view ".plugin[data-plugin=assistant] .plugin-default" `shouldReturn` "Default: on."
      choicesOf wiki "user-editor" `shouldReturn` mempty
      Router.navigate router (UserSettings AssistantSection)
      quiet
      saysOff view `shouldReturn` False

  -- Codex's second round on #374: the write belongs to the shell, which
  -- outlives the section; a write the section's own worker made died with
  -- the section, and the shell never heard of it.
  it "learns of a choice whose write ends after the section has gone" $ do
    gate <- newGate
    withShellAt asEditor free {gate = Just gate} (UserSettings PluginsSection) $ \view router wiki quiet -> do
      click view ".plugin[data-plugin=assistant] .plugin-toggle"
      quiet
      map (\h -> h.asked) <$> held gate `shouldReturn` ["state assistant"]
      -- To the assistant's own section: the section of plugins goes.
      Router.navigate router (UserSettings AssistantSection)
      quiet
      length <$> findAll view ".plugin" `shouldReturn` 0
      saysOff view `shouldReturn` False
      held gate >>= traverse_ (\h -> answer gate h ())
      quiet
      choicesOf wiki "user-editor" `shouldReturn` Map.fromList [(assistant, choice PluginOff)]
      saysOff view `shouldReturn` True

  it "learns of the administrator's save that ends after the switch has gone" $ do
    gate <- newGate
    withShellAt (demoState demoNow) {current = Just (UserId "user-admin")} free {gate = Just gate} (Admin R.Permissions) $ \view router wiki quiet -> do
      click view ".admin-plugin[data-plugin=assistant] .admin-plugin-off"
      quiet
      map (\h -> h.asked) <$> held gate `shouldReturn` ["the wiki's set"]
      Router.navigate router (UserSettings AssistantSection)
      quiet
      length <$> findAll view ".admin-plugin" `shouldReturn` 0
      saysOff view `shouldReturn` False
      held gate >>= traverse_ (\h -> answer gate h ())
      quiet
      (\st -> st.pluginsOff) <$> readIORef wiki `shouldReturn` Set.fromList [assistant]
      saysOff view `shouldReturn` True

  -- Codex's third round on #374: the section's machine held the plugins
  -- off for the wiki from its read, and told them to the shell with the
  -- user's next choice, over the newer ones the administration's switch
  -- had told.
  it "keeps the wiki's set the administration told when the user then chooses in Plugins" $
    withShellAt (demoState demoNow) {current = Just (UserId "user-admin")} free (UserSettings PluginsSection) $ \view router wiki quiet -> do
      rows view `shouldReturn` rowsWith []
      Router.navigate router (Admin R.Permissions)
      quiet
      click view ".admin-plugin[data-plugin=assistant] .admin-plugin-off"
      quiet
      Router.navigate router (UserSettings PluginsSection)
      quiet
      -- The section shows the assistant off for the wiki, without a read.
      rows view `shouldReturn` rowsWith [(assistant, (False, False, True))]
      click view ".plugin[data-plugin=errors] .plugin-toggle"
      quiet
      choicesOf wiki "user-admin" `shouldReturn` Map.fromList [(errors, choice PluginOff)]
      Router.navigate router (UserSettings AssistantSection)
      quiet
      saysOff view `shouldReturn` True

  -- Codex's fourth round on #374: the section read what is on beside the
  -- administrator's save still out, its read answered the set before the
  -- save and counted as later, and the shell dropped the save's answer.
  -- Now every ask about the plugins is one flight.
  it "keeps the administrator's save when Plugins opens before the save answers" $ do
    gate <- newGate
    withShellAt (demoState demoNow) {current = Just (UserId "user-admin")} free {gate = Just gate} (Admin R.Permissions) $ \view router wiki quiet -> do
      click view ".admin-plugin[data-plugin=assistant] .admin-plugin-off"
      quiet
      map (\h -> h.asked) <$> held gate `shouldReturn` ["the wiki's set"]
      Router.navigate router (UserSettings PluginsSection)
      quiet
      held gate >>= traverse_ (\h -> answer gate h ())
      quiet
      (\st -> st.pluginsOff) <$> readIORef wiki `shouldReturn` Set.fromList [assistant]
      rows view `shouldReturn` rowsWith [(assistant, (False, False, True))]
      Router.navigate router (UserSettings AssistantSection)
      quiet
      saysOff view `shouldReturn` True

  -- Codex's fifth round on #374: the shell heard of another reader, and
  -- waited for the former one's keys to be deleted before it gave the
  -- plugins' state the new session. Meanwhile the answer of the write out
  -- sent the choice that waited its turn, under the new reader's cookie.
  it "sends no choice of the former reader once the shell hears of another, while the former's keys are deleted" $ do
    gate <- newGate
    changes <- HS.create
    editorsSession <- newMemWiki asEditor >>= \w -> runMemDOM (runMemWiki w session)
    let telling i = i {Shell.accounts = Just (accountsTelling changes.emitter)}
    withShellAtWith telling (demoState demoNow) {current = Just (UserId "user-admin")} free {gate = Just gate, afterHandled = True} (UserSettings PluginsSection) $ \view _ wiki quiet -> do
      click view ".plugin[data-plugin=assistant] .plugin-toggle"
      quiet
      -- Errors waits its turn behind the assistant's write.
      click view ".plugin[data-plugin=errors] .plugin-toggle"
      quiet
      map (\h -> h.asked) <$> held gate `shouldReturn` ["state assistant"]
      -- Another tab signs the editor in: the shell hears of it, and holds
      -- at the deletion of the administrator's keys.
      modifyIORef' wiki (\st -> st {current = Just (UserId "user-editor")})
      HS.notify changes.listener editorsSession
      quiet
      sort . map (\h -> h.asked) <$> held gate `shouldReturn` ["drop vault:user-admin", "state assistant"]
      -- The write out answers while the deletion waits.
      held gate >>= traverse_ (\h -> when (h.asked == "state assistant") (answer gate h ()))
      quiet
      map (\h -> h.asked) <$> held gate `shouldReturn` ["drop vault:user-admin"]
      -- The deletion goes on to its end, one slot after another.
      let drain = held gate >>= \hs -> unless (null hs) (traverse_ (\h -> answer gate h ()) hs >> quiet >> drain)
      drain
      choicesOf wiki "user-editor" `shouldReturn` mempty
      choicesOf wiki "user-admin" `shouldReturn` Map.fromList [(assistant, choice PluginOff)]

-- | Sign-in methods that refuse everything, with the changes of session
-- this emitter tells.
accountsTelling :: (Applicative m) => HS.Emitter IO Session -> Accounts m
accountsTelling changes =
  Accounts
    { signIn = \_ _ -> pure (Left refused)
    , signOut = pure (Left no)
    , sessionChanges = pure changes
    , register = \_ -> pure (Left refused)
    , invitationFor = \_ -> pure (Left no)
    , acceptInvitation = \_ _ -> pure (Left refused)
    , identities = pure (Left no)
    , unlinkIdentity = \_ -> pure (Left no)
    , changePassword = \_ _ -> pure (Left refused)
    , confirmSecondFactor = \_ -> pure (Left refused)
    , beginPasskeySignIn = pure (Left refused)
    , finishPasskeySignIn = \_ -> pure (Left refused)
    , beginSecondFactorPasskey = pure (Left refused)
    , finishSecondFactorPasskey = \_ -> pure (Left refused)
    , requestPasswordReset = \_ -> pure (Left refused)
    , resetPassword = \_ _ -> pure (Left no)
    , verifyEmail = \_ -> pure (Left no)
    }
  where
    no = Invalid "This test signs nobody in."
    refused = SignInFailed no

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
          offForMe p = Plugins {offForWiki = mempty, mine = Map.fromList [(p, choice PluginOff)]}
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

-- | An external plugin installed for everyone, in the demo's wiki with the
-- editor signed in.
withExternal :: MemState
withExternal = asEditor {externalPolicy = ExternalWiki, externalWiki = Map.fromList [(doubled.manifest.id, doubled)]}
  where
    doubled =
      ExternalPlugin
        { manifest =
            ExternalManifest
              { id = PluginId "doubled-words"
              , name = "Doubled words"
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

-- | Codex's first round on #381: the shell holds the defaults until the
-- session's read of the plugins on answers, under which a plugin the
-- reader turned off is on; an external plugin, sent the page's text, must
-- not run on them.
external :: Spec
external = describe "Wiki.App.View.Editor's external plugins, in the shell on the in-memory DOM" $ do
  let editing = EditPage (slugs ["blog"]) (slugs ["hello-world"])
      frameOf = "plugin-frame/wiki/doubled-words/0123abcd/diagnostics"
  -- On a clock the test owns and never moves: the frame stood in natively
  -- never says it is ready, and the pause for it stays asleep.
  -- The read is two requests, what is on and then the plugins installed:
  -- no frame before both have answered.
  it "asks for no plugin's frame while either request of the session's read of the plugins is out, and for its frame once both answer" $ withTestClock $ \_ -> do
    gate <- newGate
    asked <- newIORef []
    withShellAt withExternal free {gate = Just gate, readHeld = True, framesAsked = Just asked} editing $ \view _ _ quiet -> do
      _ <- element view ".editor-text"
      waiting <- held gate
      map (\h -> h.asked) waiting `shouldSatisfy` (\hs -> not (null hs) && all (== "the plugins on") hs)
      readIORef asked `shouldReturn` []
      traverse_ (\h -> answer gate h ()) waiting
      quiet
      installing <- held gate
      map (\h -> h.asked) installing `shouldBe` ["the plugins installed"]
      readIORef asked `shouldReturn` []
      traverse_ (\h -> answer gate h ()) installing
      quiet
      readIORef asked `shouldReturn` [frameOf]

  it "asks for none when the read of the plugins on fails" $ do
    asked <- newIORef []
    withShellAt withExternal free {failing = True, framesAsked = Just asked} editing $ \view _ _ _ -> do
      _ <- element view ".editor-text"
      readIORef asked `shouldReturn` []
  where
    slugs :: [Text] -> [Slug]
    slugs = mapMaybe (rightToMaybe . mkSlug)

--------------------------------------------------------------------------------
-- External plugins in the settings

-- | An installed plugin of these tests: its id, its hosts, and whether it
-- is the reader's own.
installedAs :: Text -> [Text] -> Bool -> ExternalPlugin
installedAs pid hosts mine =
  ExternalPlugin
    { manifest =
        ExternalManifest
          { id = PluginId pid
          , name = T.toTitle pid
          , version = "1.2.3"
          , description = "Finds " <> pid <> "."
          , api = 1
          , contributes = Set.singleton Diagnostics
          , permissions = Set.singleton DocumentRead
          , hosts
          }
    , bundle = "b-" <> pid
    , size = 10
    , mine
    , installedAt = UTCTime (fromGregorian 2026 11 25) 0
    }

-- | The demo's wiki, the editor signed in, under this policy, with the
-- wiki's plugin "spell" and the editor's own "mine" and "spell".
withInstalled :: ExternalPolicy -> MemState
withInstalled policy =
  asEditor
    { externalPolicy = policy
    , externalWiki = Map.fromList [(PluginId "spell", installedAs "spell" ["https://spell.example.org"] False)]
    , externalOwn = Map.fromList [(UserId "user-editor", Map.fromList [(PluginId "mine", installedAs "mine" [] True), (PluginId "spell", installedAs "spell" [] True)])]
    }

-- | A file the reader picks, its bytes noted as read when they are.
fileOf :: IORef [Text] -> Text -> ByteString -> ChosenFile
fileOf readsOf name bytes = ChosenFile {name, size = fromIntegral (BS.length bytes), bytes = modifyIORef' readsOf (<> [name]) $> bytes}

-- | A manifest of a plugin of this id.
manifestOf :: Text -> ByteString
manifestOf pid = encodeUtf8 ("{\"manifest\": 1, \"id\": \"" <> pid <> "\", \"name\": \"A <b>new</b> one\", \"version\": \"2.0.0\", \"api\": 1, \"contributes\": {\"diagnostics\": {}}, \"permissions\": [\"document.read\"], \"hosts\": [\"https://new.example.org\"]}")

-- | The shell outside the demo, whose settings offer external plugins.
onServer :: Shell.Input (Holder (MemWiki MemDOM)) -> Shell.Input (Holder (MemWiki MemDOM))
onServer i = i {Shell.demo = Nothing}

-- | Pick these files in the file input the selector names, as the browser
-- tells the view of them.
pick :: View q i o -> IORef [ChosenFile] -> Text -> [ChosenFile] -> IO ()
pick view chosen selector files = do
  writeIORef chosen files
  element view selector >>= N.dispatch "change" (object [])

externalSettings :: Spec
externalSettings = describe "Wiki.App.View.Plugins' external plugins, in the shell on the in-memory DOM" $ do
  it "lists the external plugins after the built-in ones, whose each is, its version, hosts and permissions, and a user's own the wiki's hides" $
    withShellAtWith onServer (withInstalled ExternalUsers) free (UserSettings PluginsSection) $ \view _ _ _ -> do
      rows view >>= (`shouldSatisfy` \rs -> map (\(i, _, _, _) -> i) rs == map (\m -> m.id.text) manifests <> ["spell", "mine", "spell"])
      let at owner pid = ".plugin-external[data-owner=" <> owner <> "][data-plugin=" <> pid <> "] "
      textOf view (at "wiki" "spell" <> ".plugin-about") `shouldReturn` "The wiki's · version 1.2.3"
      textOf view (at "mine" "mine" <> ".plugin-about") `shouldReturn` "Yours · version 1.2.3"
      textOf view (at "wiki" "spell" <> ".plugin-hosts") `shouldReturn` "It may send what it reads to https://spell.example.org, and to no other site."
      textOf view (at "mine" "mine" <> ".plugin-hosts") `shouldReturn` "It may connect to no other site: what it reads stays in your browser."
      textOf view (at "wiki" "spell" <> ".plugin-permissions") `shouldReturn` "It reads the text of the page being edited, while its editor is open."
      -- The user's own "spell" runs not: the wiki's does in its place.
      isJust <$> find view (at "mine" "spell" <> ".plugin-hidden") `shouldReturn` True
      isJust <$> find view (at "mine" "mine" <> ".plugin-hidden") `shouldReturn` False
      -- Only the user's own are theirs to replace and remove.
      isJust <$> find view (at "wiki" "spell" <> ".plugin-remove") `shouldReturn` False
      isJust <$> find view (at "mine" "mine" <> ".plugin-remove") `shouldReturn` True

  it "says external plugins are off under the policy off, and lists none" $
    withShellAtWith onServer (withInstalled ExternalOff) free (UserSettings PluginsSection) $ \view _ _ _ -> do
      textOf view ".plugins-external-off" `shouldReturn` "External plugins are off on this wiki: none runs."
      length <$> findAll view ".plugin-external" `shouldReturn` 0

  it "warns before an upload, with the hosts and permissions of the files picked, and uploads only once confirmed" $ do
    chosen <- newIORef []
    readsOf <- newIORef []
    withShellAtWith onServer (withInstalled ExternalUsers) free {filesChosen = Just chosen} (UserSettings PluginsSection) $ \view _ wiki quiet -> do
      pick view chosen ".plugin-add input[type=file]" [fileOf readsOf "new.json" (manifestOf "new"), fileOf readsOf "new.js" "wikiPlugin.plugin({ diagnostics: () => [] });"]
      quiet
      -- The manifest's words are text: its name's markup is not markup.
      textOf view ".plugin-warning .plugin-warning-title" `shouldReturn` "A <b>new</b> one 2.0.0 (new)"
      length <$> findAll view ".plugin-warning b" `shouldReturn` 0
      textOf view ".plugin-warning .plugin-hosts" `shouldReturn` "It may send what it reads to https://new.example.org, and to no other site."
      textOf view ".plugin-warning .plugin-permissions" `shouldReturn` "It reads the text of the page being edited, while its editor is open."
      -- Nothing is uploaded before the confirmation.
      let own = Map.findWithDefault mempty (UserId "user-editor") . (\st -> st.externalOwn) <$> readIORef wiki
      Map.keys <$> own `shouldReturn` [PluginId "mine", PluginId "spell"]
      click view ".plugin-warning .plugin-confirm"
      quiet
      Map.keys <$> own `shouldReturn` [PluginId "mine", PluginId "new", PluginId "spell"]
      isJust <$> find view ".plugin-external[data-owner=mine][data-plugin=new]" `shouldReturn` True
      length <$> findAll view ".plugin-warning" `shouldReturn` 0

  it "shows a manifest refused in the rule's words, and refuses a file too large without reading it" $ do
    chosen <- newIORef []
    readsOf <- newIORef []
    withShellAtWith onServer (withInstalled ExternalUsers) free {filesChosen = Just chosen} (UserSettings PluginsSection) $ \view _ wiki quiet -> do
      pick view chosen ".plugin-add input[type=file]" [fileOf readsOf "plugin.json" "{\"manifest\": 1}", fileOf readsOf "plugin.js" "x"]
      quiet
      textOf view ".plugin-pick-error" `shouldReturn` "These files cannot be uploaded: A plugin's manifest has the field id."
      readIORef readsOf `shouldReturn` ["plugin.json", "plugin.js"]
      click view ".plugin-pick-drop"
      quiet
      length <$> findAll view ".plugin-pick" `shouldReturn` 0
      writeIORef readsOf []
      let huge = ChosenFile {name = "huge.js", size = fromIntegral maxBundleBytes + 1, bytes = modifyIORef' readsOf (<> ["huge.js"]) $> ""}
      pick view chosen ".plugin-add input[type=file]" [fileOf readsOf "plugin.json" (manifestOf "new"), huge]
      quiet
      textOf view ".plugin-pick-error" `shouldReturn` "These files cannot be uploaded: huge.js has 2097153 bytes: a plugin's bundle is at most 2097152 bytes (2 MiB)."
      readIORef readsOf `shouldReturn` []
      Map.keys . Map.findWithDefault mempty (UserId "user-editor") . (\st -> st.externalOwn) <$> readIORef wiki `shouldReturn` [PluginId "mine", PluginId "spell"]

  it "lets an administrator set the policy and install a plugin for everyone, after the warning, and remove it" $ do
    chosen <- newIORef []
    readsOf <- newIORef []
    withShellAtWith onServer (demoState demoNow) {current = Just (UserId "user-admin")} free {filesChosen = Just chosen} (Admin R.Permissions) $ \view _ wiki quiet -> do
      textOf view ".admin-external-off" >>= (`shouldSatisfy` T.isPrefixOf "External plugins are off")
      click view ".admin-external-policy-users"
      quiet
      (\st -> st.externalPolicy) <$> readIORef wiki `shouldReturn` ExternalUsers
      length <$> findAll view ".admin-external-off" `shouldReturn` 0
      pick view chosen ".admin-external .plugin-add input[type=file]" [fileOf readsOf "new.json" (manifestOf "new"), fileOf readsOf "new.js" "wikiPlugin.plugin({ diagnostics: () => [] });"]
      quiet
      textOf view ".plugin-warning .plugin-warning-text" >>= (`shouldSatisfy` T.isInfixOf "for everyone on this wiki")
      Map.keys . (\st -> st.externalWiki) <$> readIORef wiki `shouldReturn` []
      click view ".plugin-warning .plugin-confirm"
      quiet
      Map.keys . (\st -> st.externalWiki) <$> readIORef wiki `shouldReturn` [PluginId "new"]
      click view ".admin-external-plugin[data-plugin=new] .plugin-remove"
      quiet
      Map.keys . (\st -> st.externalWiki) <$> readIORef wiki `shouldReturn` []
      length <$> findAll view ".admin-external-plugin" `shouldReturn` 0

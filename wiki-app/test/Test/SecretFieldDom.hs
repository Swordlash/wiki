-- | The host's field for a plugin's secret ("Wiki.App.View.SecretField"),
-- the real component on the in-memory DOM, inside the settings of a plugin
-- of the tests (@Test.SecretsPlugin@) with the capabilities the host makes
-- for it ("Wiki.App.Plugin.Host"): over a model of the server's vault
-- (@Test.VaultServer@) and a browser whose cryptography is faked
-- (@Test.FakeVault@). Locked, the field offers the unlock; unlocked, it
-- keeps a value after the plugin's check, never shows it again, and
-- forgets it. The plugin's two fields read again when the vault is
-- unlocked or locked in this browser, by either of them or by anything
-- else, and when a secret is kept by anything else. The rules over every
-- order of acts and answers are the machine's (@Test.SecretField@).
--
-- The writing assistant's settings, on a browser whose copy of the vault
-- is older than the vault, ask the vault once as they open, for all their
-- fields, unless they are on the address the page started at. A key the
-- provider accepts and the vault then refuses to keep leaves the field
-- saying why, and the settings' other controls on again. A change of the
-- assistant's options the server refuses is said where the change was
-- made, the settings or the editor's panel, which show the server's
-- options again: the input is built from the plugins' state
-- ("Wiki.App.View.Plugins.Machine") as the shell and the editor build it.
-- A key kept for one address is not sent to another that the options
-- name: the settings and the editor's panel say for which address it was
-- kept, and the providers, which record what they are asked, are asked
-- nothing with it there.
module Test.SecretFieldDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (Value)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude hiding (find)
import Test.FakeVault
import Test.Harness.MemDom
import Test.Hspec
import Test.SecretsPlugin (endpointName, secretive, secretiveManifest, tokenName)
import Test.VaultServer
import Wiki.Api.Plugins (Plugins (..))
import Wiki.App.Assistant (MonadAssistant (..), Opening (..))
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (anonymousIn)
import Wiki.App.Backend qualified as B
import Wiki.App.Plugin (AgentInput (..), AgentQuery (..), Capabilities (..), Editing (..), Manifest (..), Options (..), OptionsInput (..), OptionsOutput (..), Plugin (..), SecretName (..), Secrets (..))
import Wiki.App.Plugin.Assistant.Agent qualified as AssistantAgent
import Wiki.App.Plugin.Assistant.Options qualified as AssistantOptions
import Wiki.App.Plugin.Host (capabilities)
import Wiki.App.Plugin.Host.Secrets (putSecret, refreshSecrets, removeSecret)
import Wiki.App.Plugin.Registry (assistantId, manifests, optionsFor, refusalFor)
import Wiki.App.Route (Route (..), route)
import Wiki.App.Vault (vaultSlot)
import Wiki.App.View.Plugins.Machine qualified as PluginsMachine
import Wiki.Core.Access (Access (..))
import Wiki.Core.Assistant (ApiKey (..), Provider (..), Ready (..), defaultSettings, withBaseUrl)
import Wiki.Core.Assistant.Split (keptValue, optionsValue, secretOf, serviceFor)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Url (Url (..))
import Wiki.Core.User (Login (..), User (..))
import Wiki.Core.Vault (Item (..), ItemName (..), itemOf)
import Wiki.Core.Vault.Plugin (decodeSecrets, pluginSealingData)

spec :: Spec
spec = describe "Wiki.App.View.SecretField" $ do
  it "unlocks the vault where it is locked, keeps a value after the plugin's check, never shows it again, and forgets it" $
    withSecretive $ \Mounted {view, kept} -> do
      -- Locked: the unlock, and no field for the value.
      void (element view (token ".secret-field-unlock-passkey"))
      isJust <$> find view (token ".secret-field-value") `shouldReturn` False
      textOf view ".secretive-read" `shouldReturn` "unavailable: Locked"

      typeInto view (token ".secret-field-password") "alice's password"
      click view (token ".secret-field-unlock-password")
      settle view
      textOf view (token ".secret-field-state") `shouldReturn` "Not set"
      -- The plugin was told, and read again: nothing is set.
      textOf view ".secretive-read" `shouldReturn` ""

      typeInto view (token ".secret-field-value") "sk-live-123"
      -- While typed, the value is in the field: the check below can fail.
      shown view "sk-live-123" `shouldReturn` True
      click view (token ".secret-field-keep")
      settle view
      textOf view (token ".secret-field-state") `shouldReturn` "Set"
      shown view "sk-live-123" `shouldReturn` False
      textOf view ".secretive-read" `shouldReturn` "token"
      kept `shouldReturn` Just (Map.fromList [(tokenName, "sk-live-123")])

      -- The plugin's check refuses this one: nothing is kept.
      typeInto view (token ".secret-field-value") "bad-token"
      click view (token ".secret-field-keep")
      settle view
      textOf view (token ".secret-field-problem") `shouldReturn` "This token is refused."
      kept `shouldReturn` Just (Map.fromList [(tokenName, "sk-live-123")])

      click view (token ".secret-field-forget")
      settle view
      textOf view (token ".secret-field-state") `shouldReturn` "Not set"
      -- The item stays, with no secret.
      kept `shouldReturn` Just mempty
      outputs view `shouldReturn` ([] :: [OptionsOutput])

  it "reads again in every field when the vault is unlocked or locked here, or a secret is kept elsewhere" $
    withSecretive $ \Mounted {view, run, can} -> do
      void (element view (endpoint ".secret-field-unlock-passkey"))
      -- Unlocked through the token's field: the endpoint's reads again.
      typeInto view (token ".secret-field-password") "alice's password"
      click view (token ".secret-field-unlock-password")
      settle view
      textOf view (token ".secret-field-state") `shouldReturn` "Not set"
      textOf view (endpoint ".secret-field-state") `shouldReturn` "Not set"

      -- Kept through the plugin's capability, not through a field: the
      -- endpoint's field says it is set.
      runMemDOM (run (can.secrets.put endpointName "https://key@example.com")) `shouldReturn` Right ()
      settle view
      textOf view (endpoint ".secret-field-state") `shouldReturn` "Set"
      textOf view (token ".secret-field-state") `shouldReturn` "Not set"

      -- The key let go here (a sign-out in another tab): both are locked.
      runMemDOM (run (dropDeviceKey (vaultSlot alice)))
      settle view
      void (element view (token ".secret-field-unlock-passkey"))
      void (element view (endpoint ".secret-field-unlock-passkey"))

  -- Browser B kept a copy of the assistant's item while it held no key;
  -- device A has kept one since. Opened at another address than the one
  -- the page started at, the settings ask the vault once, and every field
  -- reads the newer copy. At the address the page started at, they ask
  -- nothing, and show the copy: the count shown able to fail.
  it "asks the vault once for both of the assistant's fields as its settings open, and nothing at the address the page started at" $ do
    (opened, fetchedOpened) <- assistantSettingsOnB False
    opened `shouldBe` ["Set", "Not set"]
    fetchedOpened `shouldBe` 1
    (adopted, fetchedAdopted) <- assistantSettingsOnB True
    adopted `shouldBe` ["Not set", "Not set"]
    fetchedAdopted `shouldBe` 0

  it "turns the assistant's controls on again when the provider accepts a typed key and the vault refuses to keep it" $ do
    counter <- newIORef 0
    k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
    server <- newServer (Just (userVault k))
    d <- newDevice counter True
    withVaultKey d alice k
    manifest <- maybe (panic "the assistant is in the registry") pure (head [m | m <- manifests, m.id == assistantId])
    router <- runMemDOM (onDevice d (served server (Router.newRouter "/" Unknown route)))
    let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
        can = capabilities (viewContextAs router session [] 0) manifest
        anthropic selector = "[data-provider=anthropic] " <> selector
        controls = map anthropic [".assistant-service", ".assistant-model", ".assistant-default-effort", ".assistant-test"]
    withView (onDevice d . served server . browsing) AssistantOptions.component OptionsInput {user = Just alice, options = Nothing, refused = Nothing, adopted = True, can} $ \view -> do
      settle view
      -- Another device stores the vault before each of the keep's tries:
      -- every try is refused.
      replicateM_ 3 (beforeNextStore server (serverState server >>= traverse_ (setVault server . snd) . (\st -> st.kept)))
      typeInto view (anthropic ".secret-field-value") "sk-ant-typed"
      click view (anthropic ".secret-field-keep")
      settle view
      textOf view (anthropic ".secret-field-problem") >>= (`shouldSatisfy` T.isInfixOf "could not be changed")
      textOf view (anthropic ".secret-field-state") `shouldReturn` "Not set"
      disabled <- for controls $ \c -> (c,) . isJust . join <$> (find view c >>= traverse (attribute "disabled"))
      disabled `shouldBe` map (,False) controls
      isJust <$> find view (anthropic ".assistant-check") `shouldReturn` False

  -- Codex's sixth round on #382: the plugins' state kept the refusal of
  -- an options write for the row under Settings, Plugins only, and the
  -- settings where the provider was changed went back without a word.
  it "says why a change of the assistant's options was not kept, and shows the server's options again" $
    withAssistant $ \run can -> do
      let given options refused = OptionsInput {user = Just alice, options, refused, adopted = True, can}
      withView run AssistantOptions.component (given Nothing Nothing) $ \view -> do
        settle view
        -- The provider's radio buttons, Anthropic's and OpenAI's, and which
        -- of them are checked.
        let radios = findAll view ".assistant-provider input"
            checkedNow = radios >>= traverse (fmap (\case Just (ScalarBool b) -> b; _ -> False) . property "checked")
        checkedNow `shouldReturn` [True, False]
        radios >>= \case
          [_, openai] -> clickNode openai
          other -> panic ("two providers, not " <> show (length other))
        settle view
        chosen <-
          outputs view <&> \case
            [SaveOptions o] -> o
            other -> panic ("one save of the options, not " <> show other)
        -- The shell's plugins' state takes the choice and writes it, and
        -- gives the settings the options chosen while the write is out.
        let (out, effects) = PluginsMachine.step (PluginsMachine.OptionsChosen assistantId chosen) (PluginsMachine.initialWith (Just Plugins {offForWiki = mempty, mine = mempty}) 0)
            inputOf m = given (optionsFor assistantId (PluginsMachine.held m)) (refusalFor assistantId m.optionsRefused)
        asked <- case effects of
          [PluginsMachine.WriteOptions a p o] | p == assistantId && o == chosen -> pure a
          other -> panic ("one write of the options, not " <> show other)
        setInput view (inputOf out)
        settle view
        checkedNow `shouldReturn` [False, True]
        -- The server refuses it.
        let (refused, _) = PluginsMachine.step (PluginsMachine.OptionsWritten asked assistantId (Left (B.Invalid refusal))) out
        setInput view (inputOf refused)
        settle view
        textOf view ".assistant-options-refused" >>= (`shouldSatisfy` T.isInfixOf refusal)
        checkedNow `shouldReturn` [True, False]
        -- Chosen again: the refusal is no longer said, whatever the write
        -- of the choice answers.
        let (again, _) = PluginsMachine.step (PluginsMachine.OptionsChosen assistantId chosen) refused
        setInput view (inputOf again)
        settle view
        isJust <$> find view ".assistant-options-refused" `shouldReturn` False

  it "says in the editor's panel why a change of the assistant's options was not kept" $
    withAssistant $ \run can -> do
      let given refused = AgentInput {user = Just alice, options = Nothing, refused, editing = Just editing, can}
          editing = Editing {title = "A page", body = "Some text.", selection = Nothing, pending = 0, decisions = mempty, encrypted = False, document = Nothing}
      withView run AssistantAgent.component (given Nothing) $ \view -> do
        settle view
        tell view (Press "assistant" True)
        settle view
        void (element view ".assistant-setup")
        setInput view (given (Just refusal))
        settle view
        textOf view ".assistant-setup" >>= (`shouldSatisfy` T.isInfixOf refusal)
        setInput view (given Nothing)
        settle view
        textOf view ".assistant-setup" >>= (`shouldSatisfy` (not . T.isInfixOf refusal))

  -- Codex's seventh round on #382: a key kept for Anthropic's own service
  -- reached a gateway that options from elsewhere named. The key here was
  -- kept for Anthropic's own service, and the options name a gateway.
  it "says in the assistant's settings that a key kept for another address is not used, and sends it to no other address" $
    withKeptElsewhere $ \run can asked -> do
      let given options = OptionsInput {user = Just alice, options = Just options, refused = Nothing, adopted = True, can}
          anthropic selector = "[data-provider=anthropic] " <> selector
      withView run AssistantOptions.component (given atGateway) $ \view -> do
        settle view
        textOf view (anthropic ".secret-field-state") `shouldReturn` "Set"
        textOf view (anthropic ".assistant-key-elsewhere") >>= (`shouldSatisfy` T.isInfixOf "for another address: https://api.anthropic.com. Enter the key for https://gateway.example/anthropic")
        -- The connection tested at the gateway, without the key.
        click view (anthropic ".assistant-test")
        settle view
        gatewayAsks <- asked
        gatewayAsks `shouldSatisfy` any (\r -> r.baseUrl == Just gateway)
        filter (\r -> r.key == officialKey) gatewayAsks `shouldBe` []
        -- Anthropic's own service chosen: the key is used there.
        setInput view (given (optionsValue defaultSettings))
        settle view
        isJust <$> find view (anthropic ".assistant-key-elsewhere") `shouldReturn` False
        click view (anthropic ".assistant-test")
        settle view
        filter (\r -> r.key == officialKey) <$> asked >>= (`shouldSatisfy` (\rs -> not (null rs) && all (\r -> isNothing r.baseUrl) rs))
        -- The gateway chosen again, and a key typed for it: checked at the
        -- gateway, kept for it, and used there.
        setInput view (given atGateway)
        settle view
        void (element view (anthropic ".assistant-key-elsewhere"))
        typeInto view (anthropic ".secret-field-value") "sk-ant-gateway"
        click view (anthropic ".secret-field-keep")
        settle view
        isJust <$> find view (anthropic ".assistant-key-elsewhere") `shouldReturn` False
        click view (anthropic ".assistant-test")
        settle view
        filter (\r -> r.key == ApiKey "sk-ant-gateway") <$> asked >>= (`shouldSatisfy` (\rs -> not (null rs) && all (\r -> r.baseUrl == Just gateway) rs))

  it "says in the editor's panel that a key kept for another address is not used, and sends it to no other address" $
    withKeptElsewhere $ \run can asked -> do
      let editing = Editing {title = "A page", body = "Some text.", selection = Nothing, pending = 0, decisions = mempty, encrypted = False, document = Nothing}
      withView run AssistantAgent.component AgentInput {user = Just alice, options = Just atGateway, refused = Nothing, editing = Just editing, can} $ \view -> do
        settle view
        tell view (Press "anthropic" True)
        settle view
        textOf view ".assistant-setup" >>= (`shouldSatisfy` T.isInfixOf "for another address: https://api.anthropic.com")
        -- The gateway's list asked for, without the key.
        gatewayAsks <- asked
        gatewayAsks `shouldSatisfy` any (\r -> r.baseUrl == Just gateway)
        filter (\r -> r.key == officialKey) gatewayAsks `shouldBe` []
  where
    token selector = "[data-secret=token] " <> selector
    endpoint selector = "[data-secret=endpoint] " <> selector

-- | The gateway the options of 'withKeptElsewhere' name for Anthropic.
gateway :: Url
gateway = Url "https://gateway.example/anthropic"

-- | Options that send Anthropic's requests to 'gateway'.
atGateway :: Value
atGateway = optionsValue (withBaseUrl Anthropic gateway.text defaultSettings)

-- | The key 'withKeptElsewhere' keeps, for Anthropic's own service.
officialKey :: ApiKey
officialKey = ApiKey "sk-ant-official"

-- | A test on the writing assistant, for Alice, whose Anthropic key is
-- kept for Anthropic's own service: how to run on her browser, the
-- capabilities the host makes for the assistant, and every request the
-- providers were asked, in order.
withKeptElsewhere :: ((forall a. Recording (Served (Fake MemDOM)) a -> MemDOM a) -> Capabilities (Recording (Served (Fake MemDOM))) -> IO [Ready] -> IO ()) -> IO ()
withKeptElsewhere test = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  d <- newDevice counter True
  withVaultKey d alice k
  manifest <- maybe (panic "the assistant is in the registry") pure (head [m | m <- manifests, m.id == assistantId])
  requests <- newIORef []
  let run :: Recording (Served (Fake MemDOM)) a -> MemDOM a
      run (Recording r) = onDevice d (served server (runReaderT r requests))
  runMemDOM (run (putSecret alice manifest (secretOf Anthropic) (keptValue (serviceFor Anthropic defaultSettings) officialKey))) `shouldReturn` Right ()
  router <- runMemDOM (onDevice d (served server (Router.newRouter "/" Unknown route)))
  let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
  test run (capabilities (viewContextAs router session [] 0) manifest) (reverse <$> readIORef requests)

-- | 'Recording' is a browser of the tests where the writing assistant
-- runs, as 'Browsing' is, whose providers record every request they are
-- asked: the key and the address each would have carried.
newtype Recording m a = Recording (ReaderT (IORef [Ready]) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Recording where
  lift = Recording . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Recording m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Recording m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Recording m)

instance (MonadIO m) => MonadUUID (Recording m) where
  generateV4 = liftIO generateV4

instance (MonadAssistant m, MonadIO m) => MonadAssistant (Recording m) where
  assistantHere = pure True

  -- Every provider lists no model, and accepts every key.
  listModels r = recorded r $> Right []
  checkKey r = recorded r $> Nothing
  openTalk opening heard = recorded opening.ready >> lift (openTalk opening heard)

instance (MonadVault m, MonadIO m) => MonadVault (Recording m)

-- | Record a request the providers were asked.
recorded :: (MonadIO m) => Ready -> Recording m ()
recorded r = Recording (ask >>= \ref -> liftIO (modifyIORef' ref (r :)))

alice :: UserId
alice = UserId "user-alice"

-- | The sentence the server refuses the assistant's options with, in the
-- examples.
refusal :: Text
refusal = "A plugin's options are at most 64 KiB."

-- | A test on the writing assistant, for Alice, on a browser that holds
-- the key to her vault: how to run on it, and the capabilities the host
-- makes for the assistant.
withAssistant :: ((forall a. Browsing (Served (Fake MemDOM)) a -> MemDOM a) -> Capabilities (Browsing (Served (Fake MemDOM))) -> IO ()) -> IO ()
withAssistant test = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  d <- newDevice counter True
  withVaultKey d alice k
  manifest <- maybe (panic "the assistant is in the registry") pure (head [m | m <- manifests, m.id == assistantId])
  router <- runMemDOM (onDevice d (served server (Router.newRouter "/" Unknown route)))
  let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
  test (onDevice d . served server . browsing) (capabilities (viewContextAs router session [] 0) manifest)

-- | 'Mounted' is the plugin's settings mounted for Alice, with a vault on
-- the server that this browser has not unlocked: the view; how to run on
-- the browser and the server; the capabilities the plugin was given; and
-- the plugin's secrets as the server's vault holds them.
data Mounted = Mounted
  { view :: View (Const Void) (OptionsInput (Served (Fake MemDOM))) OptionsOutput
  , run :: forall a. Served (Fake MemDOM) a -> MemDOM a
  , can :: Capabilities (Served (Fake MemDOM))
  , kept :: IO (Maybe (Map SecretName Text))
  }

-- | Run a test on the plugin's settings, mounted and settled.
withSecretive :: (Mounted -> IO ()) -> IO ()
withSecretive test = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  -- A browser that holds no key to Alice's vault: it is locked here.
  d <- newDevice counter True
  let run :: Served (Fake MemDOM) a -> MemDOM a
      run = onDevice d . served server
      kept = do
        s <- serverState server
        pure $ do
          (_, v) <- s.kept
          i <- itemOf (ItemName "plugin-secretive") v
          rightToMaybe (fakeOpen k (pluginSealingData "secretive") i.sealed >>= decodeSecrets)
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
      can = capabilities (viewContextAs router session [] 0) secretiveManifest
  surface <- maybe (panic "the plugin has settings") (\(Options o) -> pure o) (secretive @(Served (Fake MemDOM))).options
  withView run surface OptionsInput {user = Just alice, options = Nothing, refused = Nothing, adopted = True, can} $ \view -> do
    settle view
    test Mounted {view, run, can, kept}

-- | The writing assistant's settings mounted on browser B, adopted or
-- not: what each provider's field of a key says (Anthropic, OpenAI), and
-- how many times B fetched the vault while they opened. B holds
-- a copy of the assistant's item with no key in it; device A kept the
-- Anthropic key after B made that copy.
assistantSettingsOnB :: Bool -> IO ([Text], Int)
assistantSettingsOnB adopted = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  a <- newDevice counter True
  b <- newDevice counter True
  withVaultKey a alice k
  withVaultKey b alice k
  manifest <- maybe (panic "the assistant is in the registry") pure (head [m | m <- manifests, m.id == assistantId])
  let onA, onB :: Browsing (Served (Fake MemDOM)) x -> IO x
      onA = runMemDOM . onDevice a . served server . browsing
      onB = runMemDOM . onDevice b . served server . browsing
      anthropic = SecretName "anthropic"
      fetches = (\s -> s.fetches) <$> serverState server
  -- The item made, with no key left in it, and B's copy of it.
  onA (putSecret alice manifest anthropic "sk-ant-old") `shouldReturn` Right ()
  onA (removeSecret alice manifest anthropic) `shouldReturn` Right ()
  onB (refreshSecrets alice manifest)
  onA (putSecret alice manifest anthropic "sk-ant-new") `shouldReturn` Right ()
  router <- runMemDOM (onDevice b (served server (Router.newRouter "/" Unknown route)))
  let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
      can = capabilities (viewContextAs router session [] 0) manifest
  atStart <- fetches
  withView (onDevice b . served server . browsing) AssistantOptions.component OptionsInput {user = Just alice, options = Nothing, refused = Nothing, adopted, can} $ \view -> do
    settle view
    states <- traverse (\p -> textOf view ("[data-secret=" <> p <> "] .secret-field-state")) ["anthropic", "openai"]
    atEnd <- fetches
    pure (states, atEnd - atStart)

-- | 'Browsing' is a browser of the tests where the writing assistant
-- runs, as in a browser ('assistantHere'): its settings read what they
-- show. Its providers accept every key.
newtype Browsing m a = Browsing (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

browsing :: Browsing m a -> m a
browsing (Browsing m) = m

instance MonadTrans Browsing where
  lift = Browsing

deriving newtype instance (MonadDOM m) => MonadDOM (Browsing m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Browsing m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Browsing m)

instance (MonadIO m) => MonadUUID (Browsing m) where
  generateV4 = liftIO generateV4

instance (MonadAssistant m) => MonadAssistant (Browsing m) where
  assistantHere = pure True

  -- Every provider lists no model, and accepts every key.
  listModels _ = pure (Right [])
  checkKey _ = pure Nothing

instance (MonadVault m) => MonadVault (Browsing m)

-- | Whether a value shows anywhere in the view: in its HTML, or as the
-- value of one of its fields.
shown :: View q i o -> Text -> IO Bool
shown view value = do
  page <- html view
  fields <- findAll view "input" >>= traverse (property "value")
  pure (value `T.isInfixOf` page || any (\case Just (ScalarText t) -> value `T.isInfixOf` t; _ -> False) fields)

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
-- fields, unless they are on the address the page started at.
module Test.SecretFieldDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.IORef (atomicModifyIORef', newIORef)
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
import Wiki.App.Assistant (MonadAssistant (..))
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (anonymousIn)
import Wiki.App.Backend qualified as B
import Wiki.App.Plugin (Capabilities (..), Manifest (..), Options (..), OptionsInput (..), OptionsOutput, Plugin (..), SecretName (..), Secrets (..))
import Wiki.App.Plugin.Assistant.Options qualified as AssistantOptions
import Wiki.App.Plugin.Host (capabilities)
import Wiki.App.Plugin.Host.Secrets (putSecret, refreshSecrets, removeSecret)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Route (Route (..), route)
import Wiki.App.Vault (vaultSlot)
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (UserId (..))
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
  where
    token selector = "[data-secret=token] " <> selector
    endpoint selector = "[data-secret=endpoint] " <> selector

alice :: UserId
alice = UserId "user-alice"

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
  withView run surface OptionsInput {user = Just alice, options = Nothing, adopted = True, can} $ \view -> do
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
  withView (onDevice b . served server . browsing) AssistantOptions.component OptionsInput {user = Just alice, options = Nothing, adopted, can} $ \view -> do
    settle view
    states <- traverse (\p -> textOf view ("[data-secret=" <> p <> "] .secret-field-state")) ["anthropic", "openai"]
    atEnd <- fetches
    pure (states, atEnd - atStart)

-- | 'Browsing' is a browser of the tests where the writing assistant
-- runs, as in a browser ('assistantHere'): its settings read what they
-- show.
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

instance (MonadVault m) => MonadVault (Browsing m)

-- | Whether a value shows anywhere in the view: in its HTML, or as the
-- value of one of its fields.
shown :: View q i o -> Text -> IO Bool
shown view value = do
  page <- html view
  fields <- findAll view "input" >>= traverse (property "value")
  pure (value `T.isInfixOf` page || any (\case Just (ScalarText t) -> value `T.isInfixOf` t; _ -> False) fields)

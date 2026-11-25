-- | The move of the writing assistant's legacy settings
-- ("Wiki.App.Assistant.Migrate") in a real browser: legacy records and the
-- vault's legacy item sealed with the browser's AES-GCM, as the assistant
-- kept them before plugins, kept in the browser's IndexedDB, against a
-- model of the server's vault (@Test.VaultServer@). L1 (it follows the
-- vault, whose key is in the assistant's slot only), L2 (sealed with the
-- assistant's own key) and L4 (it follows a vault locked here, with a key
-- typed meanwhile) are each moved to the plugin's secrets, and read back
-- through the plugin's capability; the legacy places are emptied. L3
-- (settings as text, where nothing can seal) is the native tests'
-- (@Test.AssistantMove@).
module Test.Browser.AssistantMove (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef)
import Data.Map.Strict qualified as Map
import Protolude
import Test.Hspec.Halogen
import Test.VaultServer
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), NewIdentity (..))
import Wiki.App.Assistant.Migrate (Outcome (..), migrateAssistant)
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Plugin (Manifest (..), Unavailable)
import Wiki.App.Plugin.Host.Secrets (readSecrets)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Vault (vaultSlot)
import Wiki.Core.Assistant (Provider (..), Settings, defaultSettings, withKey, withModel)
import Wiki.Core.Assistant.Split (keysOf, optionsValue)
import Wiki.Core.Assistant.Vault (DeviceRecord (..), ServerCopy (..), assistantItem, learntOf, sealingData, secretsOf)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), Sealed, Vault (..), identityItem, itemOf, vaultFormat, withItem)
import Wiki.Core.Vault.Plugin (SecretName)

spec :: forall m -> (MonadBrowserTest m, MonadAssistant m, MonadVault m) => Spec
spec m = do
  it "moves an L1 record whose vault key is in the assistant's slot only: the vault's keys in the plugin's item, the legacy item and record gone, the key in the vault's slot" $ runPage $ do
    found <- unsafeIOToPageM (runTest (l1 :: m Found))
    found.outcomes `shouldBe` [Right (Adopt (optionsValue synced)), Right Moved]
    found.secrets `shouldBe` Right (keysOf synced)
    found.legacyLeft `shouldBe` (False, False, False)
    found.vaultKeyKept `shouldBe` True

  it "moves an L2 record sealed with the assistant's own key into the vault" $ runPage $ do
    found <- unsafeIOToPageM (runTest (l2 :: m Found))
    found.outcomes `shouldBe` [Right (Adopt (optionsValue own)), Right Moved]
    found.secrets `shouldBe` Right (keysOf own)
    found.legacyLeft `shouldBe` (False, False, False)

  it "waits on an L4 record while the vault is locked, then moves the vault's keys and the one typed meanwhile" $ runPage $ do
    found <- unsafeIOToPageM (runTest (l4 :: m Found))
    found.outcomes `shouldBe` [Right Waiting, Right (Adopt (optionsValue synced)), Right Moved]
    found.secrets `shouldBe` Right (Map.union (keysOf synced) (keysOf typed))
    found.legacyLeft `shouldBe` (False, False, False)

-- | What a walk found: each run's outcome; the plugin's secrets, read
-- through the host; whether the legacy item, the record and the
-- assistant's key are left; whether the vault's slot holds a key.
data Found = Found
  { outcomes :: [Either Text Outcome]
  , secrets :: Either Unavailable (Map SecretName Text)
  , legacyLeft :: (Bool, Bool, Bool)
  , vaultKeyKept :: Bool
  }

synced, own, typed :: Settings
synced = withModel Anthropic "claude-from-the-vault" (withKey OpenAI (Just "sk-open-vault-ü") (withKey Anthropic (Just "sk-ant-vault") defaultSettings))
own = withModel Anthropic "claude-own" (withKey Anthropic (Just "sk-ant-own") defaultSettings)
typed = withKey Scripted (Just "typed-while-locked") defaultSettings

assistantManifest :: Manifest
assistantManifest = fromMaybe (panic "no assistant") (head (filter (\x -> x.id == assistantId) manifests))

-- | A vault made in this browser: its key, and a server holding it with
-- the user's key pair.
newVault :: (MonadAssistant m, MonadIO m) => m (KeyRef, IORef ServerState)
newVault = do
  k <- newVaultKey False
  made <- either (panic . ("no key pair: " <>)) pure =<< newIdentity k
  forgetKey made.key
  server <- liftIO (newServer (Just Vault {format = vaultFormat, salt = "c2FsdHNhbHRzYWx0c2FsdHNhbHRzYWx0c2FsdHNhbHQ", items = Map.singleton identityItem Item {keyId = Just made.keyId, sealed = made.sealed}, wraps = mempty}))
  pure (k, server)

sealSettings :: (MonadAssistant m) => KeyRef -> Settings -> m Sealed
sealSettings k s = either (panic . ("not sealed: " <>)) identity <$> sealWith k (decodeUtf8 (BSL.toStrict (encode (secretsOf s)))) sealingData

-- | The vault's legacy item, as a syncing assistant stored it, and the
-- copy a following record keeps of the vault.
withLegacyItem :: (MonadAssistant m, MonadVault m, MonadIO m) => IORef ServerState -> KeyRef -> Settings -> m (Sealed, ServerCopy)
withLegacyItem server k s = do
  sealed <- sealSettings k s
  liftIO (serverState server) >>= traverse_ (\(_, v) -> liftIO (setVault server (withItem assistantItem Item {keyId = Nothing, sealed} v))) . (\x -> x.kept)
  served server (fetchVault Nothing) >>= \case
    Right (Fetched stored) -> pure (sealed, ServerCopy {tag = stored.tag, vault = stored.vault})
    _ -> panic "no vault"

writeRecord' :: (MonadAssistant m) => UserId -> DeviceRecord -> m ()
writeRecord' u r = void (keepDeviceRecord u (Just (BSL.toStrict (encode r))))

-- | Runs the move as the shell does, adopting the options it finds as the
-- plugins' state does when the server holds none, until it ends.
moving :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => IORef ServerState -> UserId -> Int -> m [Either Text Outcome]
moving server u runs = go runs Nothing
  where
    go 0 _ = pure []
    go n known = do
      r <- served server (migrateAssistant u (Just known))
      let known' = case r of
            Right (Adopt v) -> known <|> Just v
            _ -> known
      (r :) <$> if r == Right Moved then pure [] else go (n - 1) known'

-- | What is left, read, and the browser's keys and records let go.
ending :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => IORef ServerState -> UserId -> [Either Text Outcome] -> m Found
ending server u outcomes = do
  secrets <- served server (readSecrets u assistantManifest)
  item <- maybe False (isJust . itemOf assistantItem . snd) . (\x -> x.kept) <$> liftIO (serverState server)
  record <- either (const True) isJust <$> deviceRecord u
  assistantKey <- isJust <$> deviceKey u
  vaultKeyKept <- isJust <$> deviceKey (vaultSlot u)
  dropDeviceKey u
  dropDeviceKey (vaultSlot u)
  void (keepDeviceRecord u Nothing)
  void (keepDeviceRecord (vaultSlot u) Nothing)
  pure Found {outcomes, secrets, legacyLeft = (item, record, assistantKey), vaultKeyKept}

l1 :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => m Found
l1 = do
  let u = UserId "user-move-l1"
  (k, server) <- newVault
  (sealed, copy) <- withLegacyItem server k synced
  -- The vault key in the assistant's slot only, as a browser that turned
  -- syncing on before the vault had a slot of its own keeps it.
  _ <- keepDeviceKey u k False
  writeRecord' u DeviceRecord {sealed = Just sealed, server = Just copy, learnt = learntOf synced, askEachTime = False}
  outcomes <- moving server u 4
  ending server u outcomes

l2 :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => m Found
l2 = do
  let u = UserId "user-move-l2"
  (k, server) <- newVault
  _ <- keepDeviceKey (vaultSlot u) k False
  mine <- newVaultKey False
  _ <- keepDeviceKey u mine False
  sealed <- sealSettings mine own
  writeRecord' u DeviceRecord {sealed = Just sealed, server = Nothing, learnt = learntOf own, askEachTime = False}
  outcomes <- moving server u 4
  ending server u outcomes

l4 :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => m Found
l4 = do
  let u = UserId "user-move-l4"
  (k, server) <- newVault
  (_, copy) <- withLegacyItem server k synced
  mine <- newVaultKey False
  _ <- keepDeviceKey u mine False
  sealed <- sealSettings mine typed
  writeRecord' u DeviceRecord {sealed = Just sealed, server = Just copy, learnt = learntOf defaultSettings, askEachTime = False}
  -- Locked: this browser holds no vault key.
  first' <- served server (migrateAssistant u (Just Nothing))
  -- Unlocked, as Security or the field's unlock keeps the key.
  _ <- keepDeviceKey (vaultSlot u) k False
  rest <- moving server u 4
  ending server u (first' : rest)

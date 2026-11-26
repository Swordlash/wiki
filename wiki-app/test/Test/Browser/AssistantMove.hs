-- | The move of the writing assistant's legacy settings
-- ("Wiki.App.Assistant.Migrate") in a real browser: legacy records and the
-- vault's legacy item sealed with the browser's AES-GCM, as the assistant
-- kept them before plugins, kept in the browser's IndexedDB, against a
-- model of the server's vault (@Test.VaultServer@). L1 (it follows the
-- vault, whose key is in the assistant's slot only; and after a reload of
-- a browser that asks for an unlock each time, unlocked through the vault's
-- slot, the assistant's slot empty), L2 (sealed with the
-- assistant's own key) and L4 (it follows a vault locked here, with a key
-- typed meanwhile) are each moved to the plugin's secrets, and read back
-- through the plugin's capability; the legacy places are emptied. L3
-- (settings as text, where nothing can seal) is the native tests'
-- (@Test.AssistantMove@). Each run goes under an owner of the user's
-- keys, as the shell runs it; under an owner let go, the browser writes
-- nothing: no record kept or deleted, no key let go.
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
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), NewIdentity (..), closeKeyOwner, openKeyOwner)
import Wiki.App.Assistant.Migrate (Outcome (..), migrateAssistant)
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Plugin (Manifest (..), Unavailable)
import Wiki.App.Plugin.Host.Secrets (readSecrets)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Vault (VaultDevice (..), readDevice, vaultSlot)
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

  it "moves an L1 record after a reload on \"ask each time\", unlocked through the vault's slot: the copy opens with the vault key, and its \"ask each time\" becomes the vault's" $ runPage $ do
    found <- unsafeIOToPageM (runTest (l1Reloaded :: m Found))
    found.outcomes `shouldBe` [Right (Adopt (optionsValue synced)), Right Moved]
    found.secrets `shouldBe` Right (keysOf synced)
    found.legacyLeft `shouldBe` (False, False, False)
    found.asksEachTime `shouldBe` True

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

  -- What the native tests' fake browser plays (@Test.AssistantMove@'s
  -- sign-out), in the browser's own calls: each checks the owner in the
  -- step that writes.
  it "writes nothing under an owner of keys let go, and writes under one open: a record kept or deleted, a key let go" $ runPage $ do
    (closed, open) <- unsafeIOToPageM (runTest (underOwners :: m (Owned', Owned')))
    closed `shouldBe` Owned' {kept = False, dropped = False, record = Just "\"before\"", keyLeft = True}
    open `shouldBe` Owned' {kept = True, dropped = True, record = Nothing, keyLeft = False}

-- | What a walk found: each run's outcome; the plugin's secrets, read
-- through the host; whether the legacy item, the record and the
-- assistant's key are left; whether the vault's slot holds a key.
data Found = Found
  { outcomes :: [Either Text Outcome]
  , secrets :: Either Unavailable (Map SecretName Text)
  , legacyLeft :: (Bool, Bool, Bool)
  , vaultKeyKept :: Bool
  , asksEachTime :: Bool
  -- ^ Whether the vault asks for an unlock each time in this browser.
  }

-- | What writes under one owner of keys did: whether a record was kept,
-- and then deleted ('dropDeviceRecordIfBy'); the record left; whether the
-- key let go ('dropDeviceKeyBy') is still there.
data Owned' = Owned'
  { kept :: Bool
  , dropped :: Bool
  , record :: Maybe ByteString
  , keyLeft :: Bool
  }
  deriving stock (Eq, Show)

-- | The writes under an owner let go, and then the same under one open.
-- Under the first, the record put before stays and the key stays; under
-- the second, the record is kept, deleted, and the key let go.
underOwners :: (MonadAssistant m, MonadIO m) => m (Owned', Owned')
underOwners = do
  let u = UserId "user-owned"
      slot = UserId "plugin:assistant:user-owned"
  _ <- keepDeviceRecord slot (Just "\"before\"")
  k <- newVaultKey False
  _ <- keepDeviceKey u k False
  forgetKey k
  closed <- liftIO (openKeyOwner u)
  liftIO (closeKeyOwner closed)
  shut <- writes closed u slot "\"before\""
  open <- liftIO (openKeyOwner u)
  opened <- writes open u slot "\"after\""
  liftIO (closeKeyOwner open)
  dropDeviceKey u
  _ <- keepDeviceRecord slot Nothing
  pure (shut, opened)
  where
    writes o u slot expected = do
      kept <- isRight <$> keepDeviceRecordBy o slot (Just "\"after\"")
      dropped <- either (const False) identity <$> dropDeviceRecordIfBy o slot expected
      record <- either (const Nothing) identity <$> deviceRecord slot
      dropDeviceKeyBy o u
      key <- deviceKey u
      traverse_ forgetKey key
      pure Owned' {kept, dropped, record, keyLeft = isJust key}

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

-- | Runs the move as the shell does, each run under an owner of the
-- user's keys let go when it ends, adopting the options it finds as the
-- plugins' state does when the server holds none, until it ends.
moving :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => IORef ServerState -> UserId -> Int -> m [Either Text Outcome]
moving server u runs = go runs Nothing
  where
    go 0 _ = pure []
    go n known = do
      o <- liftIO (openKeyOwner u)
      r <- served server (migrateAssistant o u True (Just known)) <* liftIO (closeKeyOwner o)
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
  asking <- (\d -> d.askEachTime) <$> readDevice u
  dropDeviceKey u
  dropDeviceKey (vaultSlot u)
  void (keepDeviceRecord u Nothing)
  void (keepDeviceRecord (vaultSlot u) Nothing)
  pure Found {outcomes, secrets, legacyLeft = (item, record, assistantKey), vaultKeyKept, asksEachTime = asking}

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

l1Reloaded :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => m Found
l1Reloaded = do
  let u = UserId "user-move-l1-reloaded"
  (k, server) <- newVault
  (sealed, copy) <- withLegacyItem server k synced
  -- The assistant's slot empty, as a reload left it on "ask each time";
  -- the vault key in the vault's slot only, as an unlock keeps it.
  _ <- keepDeviceKey (vaultSlot u) k False
  writeRecord' u DeviceRecord {sealed = Just sealed, server = Just copy, learnt = learntOf synced, askEachTime = True}
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
  o <- liftIO (openKeyOwner u)
  first' <- served server (migrateAssistant o u True (Just Nothing)) <* liftIO (closeKeyOwner o)
  -- Unlocked, as Security or the field's unlock keeps the key.
  _ <- keepDeviceKey (vaultSlot u) k False
  rest <- moving server u 4
  ending server u (first' : rest)

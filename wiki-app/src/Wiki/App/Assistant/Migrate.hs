-- | The move of the writing assistant's legacy settings to the plugin's
-- places (WP5.84 of docs/PLAN.md): the runner of the pure plan
-- "Wiki.Core.Assistant.Migrate". The plan says what is still to do; this
-- module reads what is where, takes the plan's first step, and reads
-- again, until the plan is empty. The shell runs it for the reader at each
-- start of a session and each time the secrets change, until nothing is
-- left to move ("Wiki.App.View.Migration.Machine").
--
-- = Where things go
--
-- The keys go to the assistant's secrets ("Wiki.App.Plugin.Host.Secrets"):
-- the vault's plugin item when the user has a vault, and this browser
-- otherwise. What was learnt from the providers goes to the plugin's cache
-- in this browser. The options are not written here: they are the plugins'
-- state's to write ("Wiki.App.View.Plugins.Machine"), which writes a
-- plugin's options one ask at a time. When the plan comes to them, the run
-- ends with the options to adopt ('Adopt'); the shell gives them to the
-- plugins' state, and a later run, which reads the server's options as
-- written, goes on to empty the legacy places.
--
-- = Safe to stop, and with other browsers and tabs
--
-- Each step is idempotent, and is taken only when what was read before it
-- says it is still needed. A run stopped anywhere leaves a state the next
-- run reads and goes on from. A delete deletes only what was read: the
-- vault's item over the version fetched, if it is still the one read, and
-- this browser's record if it is still the bytes read. Another browser
-- that moves the same vault item meanwhile, or a tab of an earlier version
-- of the wiki that writes the legacy item or record again, is read by the
-- next step.
--
-- The whole run holds the lock of this device's records (within the
-- page) and a lock that the wiki's tabs in this browser share, so that two
-- tabs of this version run one after the other. A tab of an earlier
-- version takes neither: between this run's read of the legacy record and
-- its delete, such a tab can write it again, and what it wrote then is
-- lost. That window is one IndexedDB read long.
module Wiki.App.Assistant.Migrate
  ( Outcome (..)
  , migrateAssistant
  , migrateAssistantHeld
  , found
  , maxSteps
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (Value, eitherDecodeStrict)
import Protolude
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (MonadAssistant (..), holdingAcrossTabs, holdingDeviceRecords)
import Wiki.App.Assistant.Keys (legacyRecordHere, readLegacy)
import Wiki.App.Assistant.Sync (MonadVault (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Manifest (..), PluginId (..))
import Wiki.App.Plugin.Host.Secrets (keepMissingHeld, loadLocal, readSecretsHeld, saveLocal)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Vault (Step, VaultDevice (..), fetched, forgetting, keepKeysAsAsked, keepVaultKey, opensVault, readDevice, vaultKeyHere, vaultSlot, wikiText, writeDevice)
import Wiki.Core.Assistant.Migrate (Found (..), InVault (..), Slot (..), plan)
import Wiki.Core.Assistant.Migrate qualified as Plan
import Wiki.Core.Assistant.Vault (assistantItem, sealingData)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), itemOf, withoutItem)
import Wiki.Core.Vault.Plugin (pluginItem)

-- | 'Outcome' is how a run ended: nothing legacy is left in this browser
-- or in the vault ('Moved'); something is, and waits for what a run
-- cannot do, an unlock of the vault, the server's options to be known, or
-- a part that cannot be opened ('Waiting'); or the plan came to options
-- the server has none of, for the plugins' state to write ('Adopt').
data Outcome = Moved | Waiting | Adopt Value
  deriving stock (Eq, Show)

-- | The most steps a run takes. The plan has eight kinds of step, each
-- taken once; a step another browser undoes meanwhile is taken again.
maxSteps :: Int
maxSteps = 24

-- | Move what is left of the user's legacy settings, given the plugin's
-- options as the server keeps them ('Nothing' until they are known).
migrateAssistant :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Maybe (Maybe Value) -> m (Either Text Outcome)
migrateAssistant u options = holdingDeviceRecords (holdingAcrossTabs (migrationLock u) (migrateAssistantHeld u options))

-- | 'migrateAssistant', for a caller that holds the lock of this device's
-- records already: the tests of two browsers and of two tabs, which run
-- in one process and so share one such lock. The lock of tabs is the
-- caller's too.
migrateAssistantHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Maybe (Maybe Value) -> m (Either Text Outcome)
migrateAssistantHeld u options = runExceptT (go maxSteps)
  where
    go 0 = throwError "The writing assistant's settings could not be moved: they changed at every step."
    go n = do
      f <- found u options
      case plan f of
        [] -> pure (if legacyLeft f then Waiting else Moved)
        Plan.AdoptOptions v : _ -> pure (Adopt v)
        s : _ -> perform u s >> go (n - 1)
    legacyLeft f = f.vault /= NoItem || isJust f.record || f.slot /= NoKey

-- | The name of the lock of tabs a run holds.
migrationLock :: UserId -> Text
migrationLock u = "wiki-assistant-migration:" <> u.text

-- | The assistant's manifest, as the registry has it.
assistantManifest :: (Monad m) => Step m Manifest
assistantManifest = maybe (throwError "The writing assistant is not among the plugins.") pure (find (\m -> m.id == assistantId) manifests)

-- | What a run reads before each step, given the server's options.
found :: (MonadAssistant m, MonadVault m) => UserId -> Maybe (Maybe Value) -> Step m Found
found u options = do
  manifest <- assistantManifest
  syncing <- lift vaultSync
  stored <- if syncing then fetched else pure Nothing
  key <- lift (vaultKeyHere u stored)
  maybe identity forgetting key $ do
    vault <- case (\s -> itemOf assistantItem s.vault) =<< stored of
      Nothing -> pure NoItem
      Just i -> case key of
        Nothing -> pure ItemLocked
        Just k ->
          lift (openWith k i.sealed sealingData) <&> \case
            Right text | Right s <- eitherDecodeStrict (encodeUtf8 text) -> ItemOpened i.sealed s
            _ -> ItemUnopenable
    record <- readLegacy u key
    slot <-
      lift (deviceKey u) >>= \case
        Nothing -> pure NoKey
        Just k -> bool OwnKey VaultKey <$> forgetting k (opens k stored)
    inVaultSlot <- lift (deviceKey (vaultSlot u)) >>= maybe (pure False) (\k -> forgetting k (opens k stored))
    target <- rightToMaybe <$> lift (readSecretsHeld u manifest)
    cached <- isJust <$> lift (loadLocal u assistantId)
    device <- lift (readDevice u)
    pure
      Found
        { vault
        , pluginItem = isJust (stored >>= \s -> pluginItem assistantId.text >>= \n -> itemOf n s.vault)
        , record
        , slot
        , vaultSlot = inVaultSlot
        , target
        , options
        , local = cached
        , askHere = device.askEachTime
        }
  where
    opens k = lift . maybe (pure False) (\s -> opensVault k s.vault)

-- | Take one step of the plan.
perform :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Plan.Step -> Step m ()
perform u = \case
  Plan.KeepAsk -> do
    device <- lift (readDevice u)
    ExceptT (writeDevice u VaultDevice {held = device.held, askEachTime = True, offered = device.offered})
    -- The keys this browser keeps on disk follow it at once.
    syncing <- lift vaultSync
    stored <- if syncing then fetched else pure Nothing
    lift (vaultKeyHere u stored) >>= traverse_ (\v -> forgetting v (keepKeysAsAsked u stored v))
  Plan.CopyVaultKey -> lift (deviceKey u) >>= traverse_ (\k -> forgetting k (keepVaultKey u k))
  Plan.KeepKeys keys -> do
    manifest <- assistantManifest
    ExceptT (keepMissingHeld u manifest keys)
  Plan.KeepLearnt v -> ExceptT (saveLocal u assistantId v)
  -- The plugins' state's to write: the run ends before it.
  Plan.AdoptOptions _ -> pass
  Plan.DropVaultItem sealed ->
    fetched >>= \case
      Just s
        | Just i <- itemOf assistantItem s.vault
        , i.sealed == sealed ->
            lift (storeVault (Just s.tag) (withoutItem assistantItem s.vault)) >>= \case
              Right _ -> pass
              -- Another browser stored a version meanwhile: the next step
              -- reads it.
              Left (Conflict _) -> pass
              Left e -> throwError ("Your vault could not be changed: " <> wikiText e)
      _ -> pass
  Plan.DropRecord bytes ->
    ExceptT (deviceRecord u) >>= \case
      Just now | now == bytes -> ExceptT (keepDeviceRecord u Nothing)
      _ -> pass
  Plan.DropSlotKey -> unlessM (lift (legacyRecordHere u)) (lift (dropDeviceKey u))

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
-- plugin's options one ask at a time. A run takes every other step, and
-- ends with the options to adopt ('Adopt'); the shell gives them to the
-- plugins' state, and a later run, which reads the server's options as
-- written, goes on to empty the legacy places.
--
-- = Each place once
--
-- The keys of a legacy place go to the plugin's item with the place's
-- identity, in one write ('takeSourcesHeld'): the vault's item by its
-- nonce, this browser's record by its hash
-- ('Wiki.App.Assistant.Keys.recordSource'). Whether the
-- place was taken is read in that write, from the item it fetches, so a
-- key forgotten in between (here, in another tab, on another browser) is
-- not put back from a place already taken. The legacy places are emptied
-- once the item, read again, says they were taken.
--
-- = Safe to stop, and with other browsers and tabs
--
-- Each step is idempotent, and is taken only when what was read before it
-- says it is still needed. A run stopped anywhere leaves a state the next
-- run reads and goes on from. A delete deletes only what was read: the
-- vault's item over the version fetched, if it is still the one read, and
-- this browser's record if it is still the bytes read, checked and
-- deleted in one transaction of the browser's store. Another browser
-- that moves the same vault item meanwhile, or a tab of an earlier version
-- of the wiki that writes the legacy item or record again, is read by the
-- next step.
--
-- The whole run holds the lock of this device's records (within the
-- page) and a lock that the wiki's tabs in this browser share, so that two
-- tabs of this version run one after the other. A tab of an earlier
-- version takes neither, and the conditional deletes are what keep its
-- writes. One window is left: an earlier tab that writes a new record
-- sealed with the assistant's key after the record is deleted and before
-- that key is let go ('Wiki.Core.Assistant.Migrate.DropSlotKey', which
-- reads first that no record is there) leaves a record this browser can
-- no longer open, and it is left where it is.
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
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Host.Secrets (loadLocal, readContentsHeld, saveLocal, takeSourcesHeld)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Vault (Step, VaultDevice (..), fetched, forgetting, keepKeysAsAsked, keepVaultKey, opensVault, readDevice, vaultKeyHere, vaultSlot, wikiText, writeDevice)
import Wiki.Core.Assistant.Migrate (Found (..), InVault (..), Slot (..), plan)
import Wiki.Core.Assistant.Migrate qualified as Plan
import Wiki.Core.Assistant.Vault (assistantItem, sealingData)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), itemOf, withoutItem)
import Wiki.Core.Vault.Plugin (Contents (..))

-- | 'Outcome' is how a run ended: nothing legacy is left in this browser
-- or in the vault ('Moved'); something is, and waits for what a run
-- cannot do, an unlock of the vault, the server's options to be known, or
-- a part that cannot be opened ('Waiting'); or the plan came to options
-- the server has none of, for the plugins' state to write ('Adopt'); or
-- this browser keeps nothing legacy, and the vault was not looked at
-- ('NothingHere').
data Outcome = Moved | Waiting | Adopt Value | NothingHere
  deriving stock (Eq, Show)

-- | The most steps a run takes. The plan has eight kinds of step, each
-- taken once; a step another browser undoes meanwhile is taken again.
maxSteps :: Int
maxSteps = 24

-- | Move what is left of the user's legacy settings, given whether to
-- look at the vault when this browser keeps nothing legacy, and the
-- plugin's options as the server keeps them ('Nothing' until they are
-- known). A browser that keeps no legacy record and no key of the
-- assistant's asks the server nothing unless told to: the vault's legacy
-- item, if another browser has not moved it, is looked for at the first
-- change of the secrets here (an unlock, a sign-in), so that a page the
-- server rendered asks nothing more when it starts.
migrateAssistant :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Bool -> Maybe (Maybe Value) -> m (Either Text Outcome)
migrateAssistant u vaultToo options = holdingDeviceRecords (holdingAcrossTabs (migrationLock u) (migrateAssistantHeld u vaultToo options))

-- | 'migrateAssistant', for a caller that holds the lock of this device's
-- records already: the tests of two browsers and of two tabs, which run
-- in one process and so share one such lock. The lock of tabs is the
-- caller's too.
migrateAssistantHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Bool -> Maybe (Maybe Value) -> m (Either Text Outcome)
migrateAssistantHeld u vaultToo options = do
  record <- legacyRecordHere u
  slotKey <- deviceKey u
  traverse_ forgetKey slotKey
  if not vaultToo && not record && isNothing slotKey
    then pure (Right NothingHere)
    else runExceptT (go maxSteps)
  where
    -- The options to adopt are the plugins' state's to write: every other
    -- step is taken, and the run ends with them, if the plan still has
    -- them. The legacy places wait for them to be the server's.
    go 0 = throwError "The writing assistant's settings could not be moved: they changed at every step."
    go n = do
      f <- found u options
      let steps = plan f
      case filter (not . adopting) steps of
        [] -> pure $ case [v | Plan.AdoptOptions v <- steps] of
          v : _ -> Adopt v
          [] -> if legacyLeft f then Waiting else Moved
        s : _ -> perform u s >> go (n - 1)
    adopting = \case
      Plan.AdoptOptions _ -> True
      _ -> False
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
    contents <- rightToMaybe <$> lift (readContentsHeld u manifest)
    cached <- isJust <$> lift (loadLocal u assistantId)
    device <- lift (readDevice u)
    pure
      Found
        { vault
        , record
        , slot
        , vaultSlot = inVaultSlot
        , target = (\c -> c.secrets) <$> contents
        , taken = maybe mempty (\c -> c.taken) contents
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
  Plan.KeepKeys places -> do
    manifest <- assistantManifest
    ExceptT (takeSourcesHeld u manifest places)
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
  -- Deleted only if still the bytes read, in one transaction: a record
  -- an earlier tab writes meanwhile stays, and the next step reads it.
  Plan.DropRecord bytes -> void (ExceptT (dropDeviceRecordIf u bytes))
  Plan.DropSlotKey -> unlessM (lift (legacyRecordHere u)) (lift (dropDeviceKey u))

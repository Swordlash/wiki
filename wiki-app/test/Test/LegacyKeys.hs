{-# LANGUAGE MultiWayIf #-}

-- | The writing assistant's settings as a browser read and wrote them
-- before plugins (WP5.21 of docs/PLAN.md), for the tests: a record of this
-- browser per user, sealed with a key of the assistant's own or with the
-- vault key when it followed the vault, or kept as text where nothing can
-- seal. The application only reads such records, to move them
-- ("Wiki.App.Assistant.Keys", "Wiki.App.Assistant.Migrate"); the tests
-- write them here as an earlier version of the wiki did, and read them
-- back.
module Test.LegacyKeys
  ( Keys (..)
  , readKeys
  , changeKeys
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as BSL
import Protolude
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), holdingDeviceRecords)
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Vault (Step, wikiText)
import Wiki.Core.Assistant (Settings, blankSettings, defaultSettings)
import Wiki.Core.Assistant.Vault
import Wiki.Core.Id (UserId)

--------------------------------------------------------------------------------
-- As the assistant read and wrote it

-- | 'Keys': the assistant's settings on this device, and how they are kept.
data Keys = Keys
  { settings :: Settings
  -- ^ What the assistant uses here. While 'locked', what was typed on
  -- this device since: the vault's own settings wait for a passkey.
  , locked :: Bool
  -- ^ Whether the device follows a vault it has not unlocked.
  , synced :: Bool
  -- ^ Whether the device follows the vault on the server.
  , askEachTime :: Bool
  -- ^ Whether the device keeps the vault key only while the wiki is
  -- open, and asks for the passkey each time it opens.
  }
  deriving stock (Eq, Show)

-- | What this device can do with what it keeps for a user.
data Opened
  = -- | The settings, opened with the key that sealed them.
    Opened KeyRef Settings
  | -- | A vault from the server not yet opened here; the settings typed on
    -- this device meanwhile, with the key they are sealed with, if any.
    Locked (Maybe KeyRef) Settings
  | -- | Nothing sealed that this device can open: nothing set yet.
    Fresh

--------------------------------------------------------------------------------
-- Reading and changing

-- | The settings as this device keeps them, without asking the server.
readKeys :: (MonadAssistant m, MonadUnliftIO m) => UserId -> m (Either Text Keys)
readKeys user = holdingDeviceRecords $ sealing (plainKeys user) $ runExceptT $ do
  record <- current user
  key <- lift (deviceKey user)
  keysOf record <$> open record key

-- | Change the settings, and keep them. Only what was learnt from a
-- provider changes without sealing. Otherwise the settings are sealed
-- again, and, when they sync, stored on the server over the version this
-- device has; a version changed elsewhere meanwhile is fetched, and the
-- change made again on it. A change that the server cannot take is not
-- kept here either, so that the devices do not drift apart. While the
-- vault is locked, a change is kept on this device only, under a key of
-- its own, until the vault is unlocked.
changeKeys :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> (Settings -> Settings) -> m (Either Text Keys)
changeKeys user f = holdingDeviceRecords $ sealing (plainChange user f) $ runExceptT (attempt (2 :: Int))
  where
    attempt tries = do
      record <- current user
      key <- lift (deviceKey user)
      opened <- open record key
      let old = joinSettings (secretsIn opened) record.learnt
          new = f old
          learnt = learntOf new
          secrets = secretsOf new
          kept = learntTo learnt record
      if
        | secrets == secretsOf old -> write user kept $> keysOf kept opened
        | blankSettings new && isNothing record.server -> do
            -- Nothing set and nothing synced: nothing is kept.
            lift (dropDeviceKey user)
            stored user Nothing
            pure (Keys {settings = defaultSettings, locked = False, synced = False, askEachTime = False})
        | otherwise -> case opened of
            Opened k _
              | Just copy <- record.server -> do
                  sealed <- seal k secrets
                  lift (storeVault (Just copy.tag) (withSealed sealed copy.vault)) >>= \case
                    Right fresh -> do
                      let changed = serverTo (Just (ServerCopy fresh.tag fresh.vault)) (sealedTo (sealedOf fresh.vault) kept)
                      write user changed $> keysOf changed (Opened k secrets)
                    Left (Conflict _) | tries > 0 -> do
                      _ <- refreshed user record
                      attempt (tries - 1)
                    Left e -> throwError ("The change was not kept: your keys sync across your devices, and the server did not take it. " <> wikiText e)
              | otherwise -> do
                  sealed <- seal k secrets
                  let changed = sealedTo (Just sealed) kept
                  write user changed $> keysOf changed (Opened k secrets)
            Locked mk _ -> do
              k <- maybe (localKey user) pure mk
              sealed <- seal k secrets
              let changed = sealedTo (Just sealed) kept
              write user changed $> keysOf changed (Locked (Just k) secrets)
            Fresh -> do
              k <- localKey user
              sealed <- seal k secrets
              let changed = sealedTo (Just sealed) kept
              write user changed $> keysOf changed (Opened k secrets)

--------------------------------------------------------------------------------
-- Where nothing can be sealed

-- | The first where this page can seal, the second where it cannot.
sealing :: (MonadAssistant m) => m a -> m a -> m a
sealing plain sealed = sealingHere >>= bool plain sealed

-- | The settings as stored where nothing can be sealed: as text.
plainKeys :: (MonadAssistant m) => UserId -> m (Either Text Keys)
plainKeys user = runExceptT (plainOf <$> plainSettings user)

-- | Change the settings stored as text; nothing set, nothing stored.
plainChange :: (MonadAssistant m) => UserId -> (Settings -> Settings) -> m (Either Text Keys)
plainChange user f = runExceptT $ do
  new <- f <$> plainSettings user
  stored user (if blankSettings new then Nothing else Just (BSL.toStrict (encode new)))
  pure (plainOf new)

plainSettings :: (MonadAssistant m) => UserId -> Step m Settings
plainSettings user =
  ExceptT (deviceRecord user) >>= \case
    Nothing -> pure defaultSettings
    Just bytes -> case readStored bytes of
      Left why -> throwError ("The assistant's settings on this device cannot be read (" <> why <> ").")
      Right (OldSettings s) -> pure s
      Right (Current _) -> throwError "The assistant's settings on this device are sealed, and this page cannot open them: the browser offers no encryption to a page served over plain HTTP. Open this wiki at its HTTPS address."

plainOf :: Settings -> Keys
plainOf settings = Keys {settings, locked = False, synced = False, askEachTime = False}

--------------------------------------------------------------------------------
-- Steps

-- | A device record with one part changed. (Built whole: 'sealed' and
-- 'askEachTime' are fields of other records too, which makes a record
-- update ambiguous.)
sealedTo :: Maybe Sealed -> DeviceRecord -> DeviceRecord
sealedTo sealed r = DeviceRecord {sealed, server = r.server, learnt = r.learnt, askEachTime = r.askEachTime}

serverTo :: Maybe ServerCopy -> DeviceRecord -> DeviceRecord
serverTo copy r = DeviceRecord {sealed = r.sealed, server = copy, learnt = r.learnt, askEachTime = r.askEachTime}

learntTo :: Settings -> DeviceRecord -> DeviceRecord
learntTo learnt r = DeviceRecord {sealed = r.sealed, server = r.server, learnt, askEachTime = r.askEachTime}

askTo :: Bool -> DeviceRecord -> DeviceRecord
askTo askEachTime r = DeviceRecord {sealed = r.sealed, server = r.server, learnt = r.learnt, askEachTime}

-- | What this device keeps for a user: a record, or the old settings as
-- text, sealed into one and stored in their place.
current :: (MonadAssistant m) => UserId -> Step m DeviceRecord
current user =
  ExceptT (deviceRecord user) >>= \case
    Nothing -> pure emptyRecord
    Just bytes -> case readStored bytes of
      Left why -> throwError ("The assistant's settings on this device cannot be read (" <> why <> ").")
      Right (Current record) -> pure record
      Right (OldSettings old) -> do
        k <- localKey user
        sealed <- seal k (secretsOf old)
        let record = learntTo (learntOf old) (sealedTo (Just sealed) emptyRecord)
        write user record
        pure record

-- | A record brought up to date with the server's vault, and stored if it
-- changed: the vault fetched when its tag changed; syncing stopped here
-- when the vault has no settings any more (turned off elsewhere:
-- 'leaveVault').
refreshed :: (MonadAssistant m, MonadVault m) => UserId -> DeviceRecord -> Step m DeviceRecord
refreshed user record =
  lift vaultSync >>= \case
    False -> pure record
    True ->
      lift (fetchVault ((\c -> c.tag) <$> record.server)) >>= \case
        Right (Fetched fresh) | isJust (sealedOf fresh.vault) -> do
          let changed = serverTo (Just (ServerCopy fresh.tag fresh.vault)) record
          write user changed $> changed
        -- No vault, or one without the assistant's settings: they do not
        -- sync (any more).
        Right fetched
          | isJust record.server
          , notSyncing fetched ->
              fst <$> leaveVault user record
        -- The same version, none at all, or no server to ask: as kept.
        _ -> pure record

-- | Stop syncing on this device, once the vault's settings have gone:
-- turned off on another device ('refreshed').
-- Settings this device can open are sealed again with a new key of the
-- assistant's own, kept on this device. The old key is the vault key, and
-- the vault stays when it holds the user's key pair: a copy of it in the
-- assistant's slot, kept on disk, would reopen the vault after a reload
-- even when the vault asks for an unlock each time. Settings this device
-- cannot open (it is still locked) stay what was typed here, with the key
-- it has. The record is stored, and returned with what it opens to.
leaveVault :: (MonadAssistant m) => UserId -> DeviceRecord -> Step m (DeviceRecord, Opened)
leaveVault user record = do
  key <- lift (deviceKey user)
  open record key >>= \case
    Opened old secrets -> do
      own <- localKey user
      lift (forgetKey old)
      sealed <- seal own secrets
      let off = sealedTo (Just sealed) (askTo False (serverTo Nothing record))
      write user off
      pure (off, Opened own secrets)
    _ -> do
      traverse_ (\k -> keep user k True) key
      off <- unsynced key record
      write user off
      (\opened -> (off, opened)) <$> open off key

-- | A record that no longer syncs. The settings it keeps become the
-- vault's, the latest version this device fetched, when the device's key
-- opens them; otherwise (a device still locked) they stay what was typed
-- here. Without this, a device that took changes made elsewhere would go
-- back to the settings of its own last change.
unsynced :: (MonadAssistant m) => Maybe KeyRef -> DeviceRecord -> Step m DeviceRecord
unsynced key record = do
  let off = askTo False (serverTo Nothing record)
  case (followed record, key) of
    (Just sealed, Just k) ->
      lift (openWith k sealed sealingData) <&> \case
        Right _ -> sealedTo (Just sealed) off
        Left _ -> off
    _ -> pure off

-- | Whether the server's answer says that the settings do not sync: no
-- vault, or a vault without the assistant's item (one made for the user's
-- other secrets).
notSyncing :: Fetched -> Bool
notSyncing = \case
  NoVault -> True
  Fetched fresh -> isNothing (sealedOf fresh.vault)
  Unchanged -> False

-- | The vault's sealed settings, in the copy of the vault this device
-- follows, if it follows one.
followed :: DeviceRecord -> Maybe Sealed
followed record = record.server >>= \copy -> sealedOf copy.vault

-- | Open what a device keeps with the key it holds.
open :: (MonadAssistant m) => DeviceRecord -> Maybe KeyRef -> Step m Opened
open record key = case (followed record, key) of
  (Just sealed, Just k) ->
    lift (openWith k sealed sealingData) >>= \case
      Right text -> Opened k <$> decoded text
      Left _ -> Locked (Just k) <$> typedWith k
  (Just _, Nothing) -> pure (Locked Nothing defaultSettings)
  (Nothing, Just k) -> case record.sealed of
    Nothing -> pure (Opened k defaultSettings)
    Just s -> lift (openWith k s sealingData) >>= either (const (pure Fresh)) (fmap (Opened k) . decoded)
  -- Sealed under a key this device no longer has (it was kept for the
  -- page's life only, and syncing went off elsewhere): nothing can open
  -- it, and the next change starts again.
  (Nothing, Nothing) -> pure Fresh
  where
    typedWith k = case record.sealed of
      Nothing -> pure defaultSettings
      Just s -> lift (openWith k s sealingData) >>= either (const (pure defaultSettings)) decoded

keysOf :: DeviceRecord -> Opened -> Keys
keysOf record opened =
  Keys
    { settings = joinSettings (secretsIn opened) record.learnt
    , locked = case opened of
        Locked _ _ -> True
        _ -> False
    , synced = isJust record.server
    , askEachTime = record.askEachTime
    }

secretsIn :: Opened -> Settings
secretsIn = \case
  Opened _ s -> s
  Locked _ s -> s
  Fresh -> defaultSettings

-- | A new vault key for this device alone, kept on it.
localKey :: (MonadAssistant m) => UserId -> Step m KeyRef
localKey user = do
  k <- lift (newVaultKey False)
  keep user k True
  pure k

seal :: (MonadAssistant m) => KeyRef -> Settings -> Step m Sealed
seal k s = ExceptT (sealWith k (decodeUtf8 (BSL.toStrict (encode (secretsOf s)))) sealingData)

keep :: (MonadAssistant m) => UserId -> KeyRef -> Bool -> Step m ()
keep user k onDisk = ExceptT (keepDeviceKey user k onDisk)

decoded :: (Monad m) => Text -> Step m Settings
decoded text = either (\why -> throwError ("The settings in the vault cannot be read (" <> toS why <> ").")) pure (eitherDecodeStrict (encodeUtf8 text))

write :: (MonadAssistant m) => UserId -> DeviceRecord -> Step m ()
write user record = stored user (Just (BSL.toStrict (encode record)))

stored :: (MonadAssistant m) => UserId -> Maybe ByteString -> Step m ()
stored user = ExceptT . keepDeviceRecord user

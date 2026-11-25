{-# LANGUAGE MultiWayIf #-}

-- | The host's side of a plugin's secrets ('Wiki.App.Plugin.Secrets'), and
-- of its cache in this browser ('Wiki.App.Plugin.Local'): reading and
-- changing them for one user and one plugin. "Wiki.App.Plugin.Host" closes
-- these functions over the reader and the plugin's manifest, so that a
-- plugin never names a plugin or a user. A plugin never imports this
-- module.
--
-- = Where the secrets are kept
--
-- A user who has a vault on the server keeps a plugin's secrets in it, in
-- one item per plugin, sealed with the vault key ("Wiki.Core.Vault.Plugin").
-- They follow the user to every device where the vault is unlocked. Where
-- the vault is locked in this browser, the secrets cannot be read or
-- changed: 'readSecrets' answers 'Locked', and the host's field offers the
-- unlock ("Wiki.App.View.SecretField").
--
-- A user with no vault on the server (the demo has no server; a user who
-- signs in with a provider and has not made a vault), and a page that
-- cannot seal (plain HTTP), keep the secrets in this browser: in a record
-- of the assistant's database ('deviceSecretsSlot'), one entry per plugin,
-- sealed with a key of this browser's own where the browser can seal, and
-- in the clear where it cannot. Once the user has a vault that this
-- browser opens, the next change of a plugin's secrets folds what this
-- browser kept for the plugin into the vault, the vault's value winning
-- for a name both have ('Wiki.Core.Vault.Plugin.foldIn'), and the
-- browser's entry goes. Until then, a read gives both, folded the same
-- way.
--
-- = Changing the vault
--
-- A change fetches the vault, opens the plugin's item, changes one name,
-- seals the item again and stores the vault over the version it fetched.
-- When another device stored a version in between, the server refuses
-- ('Conflict'), and the change is made again on the new version, at most
-- 'maxTries' times in all. So two devices that change different names, or
-- different plugins, both keep their change; on one name, the last stored
-- wins. An item that cannot be opened is never written over: the change
-- fails, and says so.
--
-- Before anything is stored, the vault key this browser holds for the user
-- must open the vault fetched (it opens the user's key pair). A vault of
-- another user, whose session the page now holds, is then not opened, and
-- nothing is written to it.
--
-- Each function holds the lock of this device's records
-- ('Wiki.App.Assistant.holdingDeviceRecords') for the whole of it, as the
-- vault's own steps do: within this page, two changes are made one after
-- the other. That lock is the page's only. A change also holds a lock
-- that the wiki's tabs in this browser share ('secretsLock', through
-- 'Wiki.App.Assistant.holdingAcrossTabs'): it reads this browser's record
-- of secrets and its key, waits on WebCrypto, and writes the record back
-- whole, so a change made by another tab in between would be written over,
-- and two tabs would each make the browser's first key. With the lock,
-- the second tab's change waits, and reads what the first wrote. A read
-- writes nothing and takes no such lock. Where the browser has no locks
-- of tabs (a page over plain HTTP), two tabs can still write over each
-- other's change.
--
-- A change done is announced ('Wiki.App.Assistant.announceSecretsChanged')
-- to this page and the wiki's other tabs, so that every field of a secret
-- shown reads again ("Wiki.App.View.SecretField").
module Wiki.App.Plugin.Host.Secrets
  ( -- * Secrets
    readSecrets
  , putSecret
  , removeSecret
  , forgetSecrets
  , maxTries

    -- * With the lock held
  , putSecretHeld
  , removeSecretHeld

    -- * What this browser keeps
  , DeviceEntry (..)
  , deviceSecretsSlot

    -- * The cache
  , loadLocal
  , saveLocal
  , localSlot
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Protolude
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), holdingAcrossTabs, holdingDeviceRecords)
import Wiki.App.Assistant.Sync (MonadVault (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Manifest (..), PluginId (..), SecretName (..), SecretSpec (..), Unavailable (..))
import Wiki.App.Vault (Step, fetched, forgetting, vaultKeyHere, wikiText)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), ItemName (..), Sealed, itemOf, withItem, withoutItem)
import Wiki.Core.Vault.Plugin (decodeSecrets, encodeSecrets, foldIn, pluginItem, pluginSealingData)

--------------------------------------------------------------------------------
-- Secrets

-- | How many times a change is made on the vault, the first included,
-- when other devices store a version of the vault in between.
maxTries :: Int
maxTries = 3

-- | 'Place' is where a plugin's secrets are kept for the user, as this
-- browser finds it: in the user's vault, as fetched now; or in this
-- browser, sealed ('True') or in the clear.
data Place = InVault StoredVault | OnDevice Bool

-- | Where the secrets are kept: in the vault when the user has one and
-- this browser can seal, and otherwise in this browser.
placeOf :: (MonadAssistant m, MonadVault m) => Step m Place
placeOf = do
  sealing <- lift sealingHere
  syncing <- lift vaultSync
  if sealing && syncing
    then maybe (OnDevice True) InVault <$> fetched
    else pure (OnDevice sealing)

-- | The plugin's secrets for the user: every name the manifest declares
-- that has a value.
readSecrets :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> m (Either Unavailable (Map SecretName Text))
readSecrets u manifest = holdingDeviceRecords $ runExceptT $ do
  item <- withExceptT Failed (itemFor manifest)
  withExceptT Failed placeOf >>= \case
    InVault s -> do
      k <- lift (vaultKeyHere u (Just s)) >>= maybe (throwError Locked) pure
      inVault <- withExceptT Failed (forgetting k (openVaultItem k manifest.id item s))
      onDevice <- lift (keptHere u manifest.id item)
      pure (declared manifest (foldIn inVault (maybe mempty snd onDevice)))
    OnDevice _ -> withExceptT Failed (declared manifest . fromMaybe mempty <$> deviceSecrets u manifest.id item)

-- | Keep a value under one of the plugin's names, in the place of the one
-- it had.
putSecret :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> Text -> m (Either Text ())
putSecret u manifest name value = holdingDeviceRecords (putSecretHeld u manifest name value)

-- | Forget the value of one of the plugin's names.
removeSecret :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> m (Either Text ())
removeSecret u manifest name = holdingDeviceRecords (removeSecretHeld u manifest name)

-- | 'putSecret', for a caller that holds the lock of this device's records
-- already. The tests of two devices and of two tabs use it: two devices,
-- or two tabs, are two pages, each with a lock of its own, and a test runs
-- one page's change inside the other's, in one process that has one lock.
-- The lock of tabs ('secretsLock') is taken here all the same.
putSecretHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> Text -> m (Either Text ())
putSecretHeld u manifest name value = changeSecrets u manifest name (Map.insert name value)

-- | 'removeSecret', for a caller that holds the lock already
-- ('putSecretHeld').
removeSecretHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> m (Either Text ())
removeSecretHeld u manifest name = changeSecrets u manifest name (Map.delete name)

-- | 'secretsLock' is the name of the lock of tabs a change of a user's
-- secrets of plugins holds (the module's header says why).
secretsLock :: UserId -> Text
secretsLock u = "wiki-plugin-secrets:" <> u.text

-- | Change the plugin's secrets for one name, after the name is checked
-- against the manifest: nothing is read for a name it does not declare.
-- The change holds the lock of tabs, and is announced once done.
changeSecrets :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> (Map SecretName Text -> Map SecretName Text) -> m (Either Text ())
changeSecrets u manifest name change = announced $ runExceptT $ do
  unless (name `elem` map (\s -> s.name) manifest.secrets) $
    throwError ("The plugin " <> manifest.name <> " has no secret named " <> name.text <> ".")
  item <- itemFor manifest
  ExceptT (holdingAcrossTabs (secretsLock u) (runExceptT (changing 1 item)))
  where
    changing tries item =
      placeOf >>= \case
        OnDevice sealing -> changeOnDevice u manifest.id item sealing change
        InVault s -> do
          k <- lift (vaultKeyHere u (Just s)) >>= maybe (throwError "Your vault is locked in this browser: unlock it first.") pure
          kept <- lift (keptHere u manifest.id item)
          stored <- forgetting k $ do
            current <- openVaultItem k manifest.id item s
            let next = change (foldIn current (maybe mempty snd kept))
            vault <-
              if Map.null next
                then pure (withoutItem item s.vault)
                else (\sealed -> withItem item Item {keyId = Nothing, sealed} s.vault) <$> ExceptT (sealWith k (encodeSecrets next) (pluginSealingData manifest.id.text))
            lift (storeVault (Just s.tag) vault)
          case stored of
            -- What this browser kept for the plugin is in the vault now.
            Right _ -> for_ kept (\(store, _) -> writeStore u (Map.delete item store))
            Left (Conflict _) | tries < maxTries -> changing (tries + 1) item
            Left e -> throwError ("Your vault could not be changed: " <> wikiText e)

-- | Forget every secret of the plugin, in the vault and in this browser.
-- This needs no key: the plugin's item is taken out of the vault whole. It
-- is the way out for a user who can no longer unlock their vault.
forgetSecrets :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> m (Either Text ())
forgetSecrets u manifest = holdingDeviceRecords $ announced $ holdingAcrossTabs (secretsLock u) $ runExceptT $ do
  item <- itemFor manifest
  syncing <- lift vaultSync
  when syncing (forgetting' 1 item)
  store <- readStore u
  when (Map.member item store) (writeStore u (Map.delete item store))
  where
    forgetting' tries item =
      fetched >>= \case
        Just s
          | isJust (itemOf item s.vault) ->
              lift (storeVault (Just s.tag) (withoutItem item s.vault)) >>= \case
                Right _ -> pass
                Left (Conflict _) | tries < maxTries -> forgetting' (tries + 1) item
                Left e -> throwError ("Your vault could not be changed: " <> wikiText e)
        _ -> pass

-- | Announce a change of secrets once it is done
-- ('announceSecretsChanged'), after the lock of tabs is let go.
announced :: (MonadAssistant m) => m (Either Text ()) -> m (Either Text ())
announced act = act >>= \r -> when (isRight r) announceSecretsChanged $> r

-- | The plugin's item in the vault, by the plugin's id.
itemFor :: (Monad m) => Manifest -> Step m ItemName
itemFor manifest = maybe (throwError ("The plugin " <> manifest.name <> " cannot keep secrets: its id is too long.")) pure (pluginItem manifest.id.text)

-- | What the plugin's item in the vault holds: nothing when there is no
-- item; an error when it cannot be opened with the key, or read.
openVaultItem :: (MonadAssistant m) => KeyRef -> PluginId -> ItemName -> StoredVault -> Step m (Map SecretName Text)
openVaultItem k pluginId item s = case itemOf item s.vault of
  Nothing -> pure mempty
  Just i ->
    lift (openWith k i.sealed (pluginSealingData pluginId.text)) >>= \case
      Left _ -> throwError "Your vault holds secrets of this plugin that cannot be opened: they were not sealed for it. Nothing was changed."
      Right text -> ExceptT (pure (decodeSecrets text))

-- | Only the names the manifest declares.
declared :: Manifest -> Map SecretName Text -> Map SecretName Text
declared manifest = Map.filterWithKey (\n _ -> n `elem` map (\s -> s.name) manifest.secrets)

--------------------------------------------------------------------------------
-- What this browser keeps

-- | 'DeviceEntry' is a plugin's secrets as this browser keeps them, where
-- the user has no vault to keep them in: sealed with this browser's key
-- for the user's secrets, with the same additional data as the vault's
-- item; or in the clear, where the page cannot seal.
data DeviceEntry = SealedEntry Sealed | PlainEntry (Map SecretName Text)
  deriving stock (Eq, Show)

-- | Written as @{"sealed": {nonce, ciphertext}}@ or @{"plain": {...}}@.
instance ToJSON DeviceEntry where
  toJSON = \case
    SealedEntry s -> object ["sealed" .= s]
    PlainEntry m -> object ["plain" .= m]

instance FromJSON DeviceEntry where
  parseJSON = withObject "DeviceEntry" $ \o -> (SealedEntry <$> o .: "sealed") <|> (PlainEntry <$> o .: "plain")

-- | The name of the record of the assistant's database where this browser
-- keeps a user's secrets of plugins, by item name ('DeviceEntry'), and of
-- the slot of the key that seals them.
deviceSecretsSlot :: UserId -> UserId
deviceSecretsSlot u = UserId ("secrets:" <> u.text)

-- | What this browser keeps of the user's secrets of plugins, by item.
readStore :: (MonadAssistant m) => UserId -> Step m (Map ItemName DeviceEntry)
readStore u =
  lift (deviceRecord (deviceSecretsSlot u)) >>= \case
    Left why -> throwError why
    Right Nothing -> pure mempty
    Right (Just bytes) -> either (\why -> throwError ("What this browser keeps of your secrets could not be read: " <> toS why)) pure (eitherDecodeStrict bytes)

-- | Keep these entries, in the place of those kept; none deletes the
-- record.
writeStore :: (MonadAssistant m) => UserId -> Map ItemName DeviceEntry -> Step m ()
writeStore u store = ExceptT (keepDeviceRecord (deviceSecretsSlot u) (if Map.null store then Nothing else Just (BSL.toStrict (encode store))))

-- | The plugin's secrets as this browser keeps them, if it keeps any; an
-- error when they cannot be opened or read.
deviceSecrets :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> Step m (Maybe (Map SecretName Text))
deviceSecrets u pluginId item = readStore u >>= traverse (openEntry u pluginId) . Map.lookup item

-- | What this browser keeps, and the plugin's secrets in it, when it
-- keeps some for the plugin that it can open: what a change in the vault
-- folds in. What cannot be opened is left where it is.
keptHere :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> m (Maybe (Map ItemName DeviceEntry, Map SecretName Text))
keptHere u pluginId item =
  rightToMaybe <$> runExceptT (readStore u) >>= \case
    Nothing -> pure Nothing
    Just store -> case Map.lookup item store of
      Nothing -> pure Nothing
      Just entry -> fmap (store,) . rightToMaybe <$> runExceptT (openEntry u pluginId entry)

-- | The secrets an entry holds.
openEntry :: (MonadAssistant m) => UserId -> PluginId -> DeviceEntry -> Step m (Map SecretName Text)
openEntry u pluginId = \case
  PlainEntry m -> pure m
  SealedEntry sealed ->
    lift (deviceKey (deviceSecretsSlot u)) >>= \case
      Nothing -> throwError "This browser no longer holds the key it sealed your secrets with."
      Just k -> forgetting k $ do
        text <- ExceptT (openWith k sealed (pluginSealingData pluginId.text))
        ExceptT (pure (decodeSecrets text))

-- | Change the plugin's secrets as this browser keeps them, sealed where it
-- can seal. What cannot be opened is not written over.
changeOnDevice :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> Bool -> (Map SecretName Text -> Map SecretName Text) -> Step m ()
changeOnDevice u pluginId item sealing change = do
  store <- readStore u
  current <- maybe (pure mempty) (openEntry u pluginId) (Map.lookup item store)
  let next = change current
  entry <-
    if
      | Map.null next -> pure Nothing
      | sealing -> Just . SealedEntry <$> sealHere next
      | otherwise -> pure (Just (PlainEntry next))
  writeStore u (Map.alter (const entry) item store)
  where
    slot = deviceSecretsSlot u
    sealHere next = do
      k <-
        lift (deviceKey slot) >>= \case
          Just k -> pure k
          Nothing -> do
            -- This browser's first secret for the user: a key of its own,
            -- which cannot be exported, kept on disk.
            k <- lift (newVaultKey False)
            lift (keepDeviceKey slot k True) >>= either (\why -> lift (forgetKey k) >> throwError why) (const (pure k))
      forgetting k (ExceptT (sealWith k (encodeSecrets next) (pluginSealingData pluginId.text)))

--------------------------------------------------------------------------------
-- The cache

-- | The name of the record of the assistant's database where this browser
-- keeps a plugin's cache for a user ('Wiki.App.Plugin.Local').
localSlot :: UserId -> PluginId -> UserId
localSlot u p = UserId ("plugin:" <> p.text <> ":" <> u.text)

-- | The plugin's cache for the user, if this browser keeps one it can read.
loadLocal :: (MonadAssistant m) => UserId -> PluginId -> m (Maybe Value)
loadLocal u p =
  deviceRecord (localSlot u p) <&> \case
    Right (Just bytes) -> rightToMaybe (eitherDecodeStrict bytes)
    _ -> Nothing

-- | Keep the plugin's cache for the user, in the place of the one kept.
saveLocal :: (MonadAssistant m) => UserId -> PluginId -> Value -> m (Either Text ())
saveLocal u p v = keepDeviceRecord (localSlot u p) (Just (BSL.toStrict (encode v)))

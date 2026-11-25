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
-- An item is never taken out of the vault, even when its last secret is
-- forgotten: it stays, sealing no secret, with the legacy places its
-- secrets were moved from ("Wiki.Core.Vault.Plugin"'s 'Contents'). This
-- browser's entry for a plugin stays the same way once made. Nothing here
-- empties a plugin's secrets without the vault's key.
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
-- = What this browser holds of the vault
--
-- A page the server rendered asks the server nothing as it starts: the
-- server's render, which runs where no assistant is, never reads the
-- vault, so the vault's answer is not among what the page carries. A
-- read as a surface starts ('readSecretsHere': the editor's assistant, the
-- settings' fields) therefore opens the copy of the plugin's item that
-- this browser keeps ('heldSlot'), sealed as the server has it: every read
-- and every change of the vault here keeps the item it saw. Only where
-- this browser holds no copy, or no key here opens it (a vault made anew
-- by a reset), is the vault fetched. With a copy and no key, the vault is
-- locked here, and nothing is fetched either. A copy can be older than the
-- vault, when another device changed the secrets since; 'refreshSecrets'
-- reads the vault, and announces a change when the copy was older.
--
-- A change done is announced ('Wiki.App.Assistant.announceSecretsChanged')
-- to this page and the wiki's other tabs, so that every field of a secret
-- shown reads again ("Wiki.App.View.SecretField").
module Wiki.App.Plugin.Host.Secrets
  ( -- * Secrets
    readSecrets
  , readSecretsHere
  , readSecretsForField
  , refreshSecrets
  , heldSlot
  , putSecret
  , removeSecret
  , maxTries

    -- * With the lock held
  , readSecretsHeld
  , putSecretHeld
  , removeSecretHeld
  , readContentsHeld
  , takeSourcesHeld

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
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), holdingAcrossTabs, holdingDeviceRecords)
import Wiki.App.Assistant.Sync (MonadVault (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Manifest (..), PluginId (..), SecretName (..), SecretSpec (..), Unavailable (..))
import Wiki.App.Vault (Step, fetched, forgetting, vaultKeyHere, vaultSlot, wikiText)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), ItemName (..), Sealed, itemOf, withItem)
import Wiki.Core.Vault.Plugin (Contents (..), decodeContents, encodeContents, foldContents, foldIn, noContents, pluginItem, pluginSealingData)

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
readSecrets u manifest = holdingDeviceRecords (readSecretsHeld u manifest)

-- | 'readSecrets', for a caller that holds the lock of this device's
-- records already: the move of the assistant's legacy settings
-- ("Wiki.App.Assistant.Migrate"), which reads them between its steps.
readSecretsHeld :: (MonadAssistant m, MonadVault m) => UserId -> Manifest -> m (Either Unavailable (Map SecretName Text))
readSecretsHeld u manifest = fmap (\c -> declared manifest c.secrets) <$> readContentsHeld u manifest

-- | What the plugin's item holds for the user, the legacy places taken
-- with the secrets, as 'readSecrets' reads it: for the move of the
-- assistant's legacy settings, with the lock held.
readContentsHeld :: (MonadAssistant m, MonadVault m) => UserId -> Manifest -> m (Either Unavailable Contents)
readContentsHeld u manifest = runExceptT $ do
  item <- withExceptT Failed (itemFor manifest)
  withExceptT Failed placeOf >>= \case
    InVault s -> do
      -- Kept before the key is looked for: a browser where the vault is
      -- locked knows so from the copy, and asks nothing as a page starts.
      lift (keepHeld u item (HeldVault (itemOf item s.vault)))
      k <- lift (vaultKeyHere u (Just s)) >>= maybe (throwError Locked) pure
      inVault <- withExceptT Failed (forgetting k (openVaultItem k manifest.id item s))
      onDevice <- lift (keptHere u manifest.id item)
      pure (foldContents inVault (maybe noContents snd onDevice))
    OnDevice sealing -> do
      -- The user has no vault: kept, so that a read as a page starts need
      -- not ask the server again.
      when sealing (lift (whenM vaultSync (keepHeld u item HeldNoVault)))
      withExceptT Failed (fromMaybe noContents <$> deviceSecrets u manifest.id item)

-- | The plugin's secrets as a surface reads them as it starts (the
-- module's header says why): from the copy this browser holds of the
-- plugin's item, where a key here opens it; with no copy, 'Locked' where
-- the secrets are kept in a vault and this browser holds no vault key, and
-- otherwise what this browser keeps of its own. It never asks the server,
-- takes no lock and writes nothing. Where the copy is older than the vault, a read of the
-- vault ('readSecrets', 'refreshSecrets') keeps the newer one and
-- announces the change.
readSecretsHere :: (MonadAssistant m, MonadVault m) => UserId -> Manifest -> m (Either Unavailable (Map SecretName Text))
readSecretsHere u manifest =
  fmap (\c -> declared manifest c.secrets) <$> case pluginItem manifest.id.text of
    Nothing -> pure (Right noContents)
    Just item ->
      heldContents u manifest >>= \case
        Just r -> pure r
        Nothing -> do
          inVault <- (&&) <$> sealingHere <*> vaultSync
          keys <- catMaybes <$> sequence [deviceKey (vaultSlot u), deviceKey u]
          traverse_ forgetKey keys
          if inVault && null keys
            then pure (Left Locked)
            else Right . maybe noContents snd <$> keptHere u manifest.id item

-- | 'readSecretsHere', or, where this browser holds no copy of the
-- plugin's item that a key here opens, 'readSecrets': for the host's
-- field of a secret, which says whether it is set and offers the unlock.
readSecretsForField :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> m (Either Unavailable (Map SecretName Text))
readSecretsForField u manifest =
  heldContents u manifest >>= \case
    Just r -> pure (fmap (\c -> declared manifest c.secrets) r)
    Nothing -> readSecrets u manifest

-- | 'Held' is what this browser keeps of the vault for a plugin, as it
-- last saw it: the user has no vault; or the vault, holding this item of
-- the plugin's or none.
data Held = HeldNoVault | HeldVault (Maybe Item)
  deriving stock (Eq)

instance ToJSON Held where
  toJSON = \case
    HeldNoVault -> object ["vault" .= False]
    HeldVault i -> object ["vault" .= True, "item" .= i]

instance FromJSON Held where
  parseJSON = withObject "Held" $ \o ->
    o .: "vault" >>= \case
      False -> pure HeldNoVault
      True -> HeldVault <$> o .: "item"

-- | What the copy this browser holds says of the plugin's secrets: what
-- this browser keeps of its own, where the user has no vault or the vault
-- holds no item of the plugin's; 'Locked' where the vault is there and
-- this browser holds no key; the item's contents, where a key here opens
-- it. 'Nothing' where there is no copy, no key here opens the item, or
-- the secrets are not kept in a vault here.
heldContents :: (MonadAssistant m, MonadVault m) => UserId -> Manifest -> m (Maybe (Either Unavailable Contents))
heldContents u manifest = do
  sealing <- sealingHere
  syncing <- vaultSync
  case pluginItem manifest.id.text of
    Just item
      | sealing && syncing ->
          deviceRecord (heldSlot u item) >>= \case
            Right (Just bytes) -> case eitherDecodeStrict bytes of
              Right HeldNoVault -> Just . Right <$> own item
              Right (HeldVault i) -> do
                keys <- catMaybes <$> sequence [deviceKey (vaultSlot u), deviceKey u]
                case (keys, i) of
                  ([], _) -> pure (Just (Left Locked))
                  (_, Nothing) -> traverse_ forgetKey keys >> (Just . Right <$> own item)
                  (_, Just held) -> do
                    opened <- firstJust keys (\k -> forgettingKey k (rightToMaybe . (>>= decodeContents) <$> openWith k held.sealed (pluginSealingData manifest.id.text)))
                    for opened $ \c -> Right . foldContents c <$> own item
              Left _ -> pure Nothing
            _ -> pure Nothing
    _ -> pure Nothing
  where
    own item = maybe noContents snd <$> keptHere u manifest.id item
    forgettingKey k act = act <* forgetKey k
    firstJust ks f = foldM (\found k -> maybe (f k) (\c -> forgetKey k $> Just c) found) Nothing ks

-- | Read the plugin's secrets from the vault, keeping the copy this
-- browser holds, which announces a change when the copy was older: every
-- surface that shows them reads them again. For when they are needed
-- fresh: the editor's assistant opened.
refreshSecrets :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> m ()
refreshSecrets u manifest = void (readSecrets u manifest)

-- | The name of the record where this browser keeps its copy of a
-- plugin's item for a user, as the vault last held it here ('Held').
heldSlot :: UserId -> ItemName -> UserId
heldSlot u item = UserId ("held:" <> item.text <> ":" <> u.text)

-- | Keep what this browser saw of the vault for the plugin, and announce
-- a change of the secrets when it differs from the copy kept before: a
-- read of the vault that finds what another device changed has every
-- surface read again, the first copy included: what a surface read
-- before there was one was what this browser keeps of its own.
keepHeld :: (MonadAssistant m) => UserId -> ItemName -> Held -> m ()
keepHeld u item h = do
  let bytes = BSL.toStrict (encode h)
  before <- deviceRecord (heldSlot u item)
  unless (before == Right (Just bytes)) $ do
    kept <- keepDeviceRecord (heldSlot u item) (Just bytes)
    when (isRight kept) announceSecretsChanged

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
putSecretHeld u manifest name value = changeSecrets u manifest [name] (\c -> Contents {secrets = Map.insert name value c.secrets, taken = c.taken})

-- | 'removeSecret', for a caller that holds the lock already
-- ('putSecretHeld').
removeSecretHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> SecretName -> m (Either Text ())
removeSecretHeld u manifest name = changeSecrets u manifest [name] (\c -> Contents {secrets = Map.delete name c.secrets, taken = c.taken})

-- | Take the secrets of these legacy places into the plugin's item, each
-- place by its identity, in order: a place the item took before is
-- skipped, and a place taken now fills only the names the item does not
-- hold ('Wiki.Core.Vault.Plugin.foldIn'), and is recorded as taken. For
-- a caller that holds the lock of this device's records: the move of the
-- assistant's legacy settings.
--
-- Whether a place was taken is read from the item this change fetches,
-- on each try: when another browser stored the vault in between, its move
-- and a key forgotten since are read, and a forgotten key is not put back
-- from a place already taken.
takeSourcesHeld :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> [(Text, Map SecretName Text)] -> m (Either Text ())
takeSourcesHeld u manifest sources = changeSecrets u manifest (concatMap (Map.keys . snd) sources) (\c -> foldl' taking c sources)
  where
    taking c (place, values)
      | Set.member place c.taken = c
      | otherwise = Contents {secrets = foldIn c.secrets values, taken = Set.insert place c.taken}

-- | 'secretsLock' is the name of the lock of tabs a change of a user's
-- secrets of plugins holds (the module's header says why).
secretsLock :: UserId -> Text
secretsLock u = "wiki-plugin-secrets:" <> u.text

-- | Change the plugin's secrets for these names, after the names are
-- checked against the manifest: nothing is read for a name it does not
-- declare. The change holds the lock of tabs, and is announced once done.
changeSecrets :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => UserId -> Manifest -> [SecretName] -> (Contents -> Contents) -> m (Either Text ())
changeSecrets u manifest names change = announced $ runExceptT $ do
  for_ names $ \name ->
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
            let next = change (foldContents current (maybe noContents snd kept))
            -- Sealed and kept even with no secret left: the item stays.
            vault <- (\sealed -> withItem item Item {keyId = Nothing, sealed} s.vault) <$> ExceptT (sealWith k (encodeContents next) (pluginSealingData manifest.id.text))
            lift (storeVault (Just s.tag) vault)
          case stored of
            -- What this browser kept for the plugin is in the vault now.
            Right after -> do
              lift (keepHeld u item (HeldVault (itemOf item after.vault)))
              for_ kept (\(store, _) -> writeStore u (Map.delete item store))
            Left (Conflict _) | tries < maxTries -> changing (tries + 1) item
            Left e -> throwError ("Your vault could not be changed: " <> wikiText e)

-- | Announce a change of secrets once it is done
-- ('announceSecretsChanged'), after the lock of tabs is let go.
announced :: (MonadAssistant m) => m (Either Text ()) -> m (Either Text ())
announced act = act >>= \r -> when (isRight r) announceSecretsChanged $> r

-- | The plugin's item in the vault, by the plugin's id.
itemFor :: (Monad m) => Manifest -> Step m ItemName
itemFor manifest = maybe (throwError ("The plugin " <> manifest.name <> " cannot keep secrets: its id is too long.")) pure (pluginItem manifest.id.text)

-- | What the plugin's item in the vault holds: nothing when there is no
-- item; an error when it cannot be opened with the key, or read.
openVaultItem :: (MonadAssistant m) => KeyRef -> PluginId -> ItemName -> StoredVault -> Step m Contents
openVaultItem k pluginId item s = case itemOf item s.vault of
  Nothing -> pure noContents
  Just i ->
    lift (openWith k i.sealed (pluginSealingData pluginId.text)) >>= \case
      Left _ -> throwError "Your vault holds secrets of this plugin that cannot be opened: they were not sealed for it. Nothing was changed."
      Right text -> ExceptT (pure (decodeContents text))

-- | Only the names the manifest declares.
declared :: Manifest -> Map SecretName Text -> Map SecretName Text
declared manifest = Map.filterWithKey (\n _ -> n `elem` map (\s -> s.name) manifest.secrets)

--------------------------------------------------------------------------------
-- What this browser keeps

-- | 'DeviceEntry' is a plugin's secrets as this browser keeps them, where
-- the user has no vault to keep them in: sealed with this browser's key
-- for the user's secrets, with the same additional data as the vault's
-- item; or in the clear, where the page cannot seal.
data DeviceEntry = SealedEntry Sealed | PlainEntry Contents
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
deviceSecrets :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> Step m (Maybe Contents)
deviceSecrets u pluginId item = readStore u >>= traverse (openEntry u pluginId) . Map.lookup item

-- | What this browser keeps, and the plugin's secrets in it, when it
-- keeps some for the plugin that it can open: what a change in the vault
-- folds in. What cannot be opened is left where it is.
keptHere :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> m (Maybe (Map ItemName DeviceEntry, Contents))
keptHere u pluginId item =
  rightToMaybe <$> runExceptT (readStore u) >>= \case
    Nothing -> pure Nothing
    Just store -> case Map.lookup item store of
      Nothing -> pure Nothing
      Just entry -> fmap (store,) . rightToMaybe <$> runExceptT (openEntry u pluginId entry)

-- | The secrets an entry holds.
openEntry :: (MonadAssistant m) => UserId -> PluginId -> DeviceEntry -> Step m Contents
openEntry u pluginId = \case
  PlainEntry m -> pure m
  SealedEntry sealed ->
    lift (deviceKey (deviceSecretsSlot u)) >>= \case
      Nothing -> throwError "This browser no longer holds the key it sealed your secrets with."
      Just k -> forgetting k $ do
        text <- ExceptT (openWith k sealed (pluginSealingData pluginId.text))
        ExceptT (pure (decodeContents text))

-- | Change the plugin's secrets as this browser keeps them, sealed where it
-- can seal. What cannot be opened is not written over. An entry made stays,
-- with no secret left in it, as the vault's item does; a change that
-- leaves nothing where there was no entry makes none.
changeOnDevice :: (MonadAssistant m) => UserId -> PluginId -> ItemName -> Bool -> (Contents -> Contents) -> Step m ()
changeOnDevice u pluginId item sealing change = do
  store <- readStore u
  existing <- traverse (openEntry u pluginId) (Map.lookup item store)
  let next = change (fromMaybe noContents existing)
  unless (isNothing existing && next == noContents) $ do
    entry <-
      if
        | sealing -> SealedEntry <$> sealHere next
        | otherwise -> pure (PlainEntry next)
    writeStore u (Map.insert item entry store)
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
      forgetting k (ExceptT (sealWith k (encodeContents next) (pluginSealingData pluginId.text)))

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

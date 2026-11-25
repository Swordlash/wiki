-- | A browser's half of the vault, faked for the native tests
-- ("Wiki.App.Assistant"'s 'MonadAssistant'): the in-memory document has
-- no WebCrypto. A key is a number. A seal records the key and the
-- additional data beside the text, and an open checks both, so that an
-- item opened with another key, or as another plugin's, fails as AES-GCM
-- would. Nothing is encrypted: what a seal holds is readable, which the
-- browser's test with real AES-GCM covers (@Test.Browser.PluginSecrets@).
--
-- A 'Device' is one browser: its keys by slot and its records, apart from
-- every other device's. The numbers of keys are one test's, shared by its
-- devices, so that two devices can hold the same vault key. Two runs on
-- one device at once are two tabs of that browser: they share its keys,
-- its records and its locks of tabs.
module Test.FakeVault
  ( Device (..)
  , newDevice
  , Fake
  , onDevice
  , fakeSeal
  , fakeOpen
  , userVault
  , keyIdOfTest
  , withVaultKey
  , deviceRecords
  , beforeNextWrite
  , waitingForLock
  , FakeMemDOM
  )
where

import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), TabLock (..), heldHandle, keyRefOf)
import Wiki.App.Assistant.Sync (MonadVault (..))
import Wiki.App.Vault (vaultSlot)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Ciphertext (..), Item (..), KeyId (..), Sealed (..), Unlock (..), Vault (..), WrappedKey (..), identityData, identityItem, vaultFormat)

-- | 'Device' is one browser's keys and records.
data Device = Device
  { keys :: IORef (Map Text Int)
  -- ^ The key kept in each slot, by its number.
  , records :: IORef (Map Text ByteString)
  -- ^ The records of the assistant's database, by name.
  , sealing :: Bool
  -- ^ Whether the browser can seal (a secure page).
  , counter :: IORef Int
  -- ^ The numbers of keys, shared by the test's devices.
  , writeHooks :: IORef [IO ()]
  -- ^ What runs before the next writes of a key or a record, one each.
  , requests :: TVar (Int, Map Int Text)
  -- ^ The requests for locks of tabs ('requestTabLock'), by number, with
  -- the name each asks for; and the last number given.
  , locks :: TVar (Map Text Int)
  -- ^ The locks of tabs held, by name: the request that holds each.
  , waiters :: TVar Int
  -- ^ How many requests wait for a lock of tabs that another holds.
  , changes :: HS.Subscribe IO ()
  -- ^ What 'secretsChanges' gives: a key kept or let go, a secret kept or
  -- forgotten, in any tab of the browser.
  }

-- | A device with nothing kept, numbering its keys with this counter.
newDevice :: IORef Int -> Bool -> IO Device
newDevice counter sealing =
  Device <$> newIORef mempty <*> newIORef mempty <*> pure sealing <*> pure counter <*> newIORef [] <*> newTVarIO (0, mempty) <*> newTVarIO mempty <*> newTVarIO 0 <*> HS.create

-- | Run this before the next write of a key or a record on the device
-- that no hook given before it took: a test's place to act between a
-- tab's read and its write.
beforeNextWrite :: Device -> IO () -> IO ()
beforeNextWrite d act = modifyIORef' d.writeHooks (<> [act])

-- | Wait until a request on the device waits for a lock of tabs, or the
-- condition given holds (the other tab's work ended without waiting).
waitingForLock :: Device -> STM Bool -> IO ()
waitingForLock d ended = atomically $ do
  n <- readTVar d.waiters
  done <- ended
  check (n > 0 || done)

-- | Tell the device's tabs that what the secrets read as changed, as the
-- browser's script does ('secretsChanges').
changed :: Device -> IO ()
changed d = d.changes.listener.notify ()

-- | Run the next hook, before a write.
writing :: Device -> IO ()
writing d = join (atomicModifyIORef' d.writeHooks (\case [] -> ([], pass); h : rest -> (rest, h)))

-- | 'Fake' is a monad whose browser is a 'Device'.
newtype Fake m a = Fake (ReaderT Device m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Fake where
  lift = Fake . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Fake m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Fake m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Fake m)

-- | The fake over the in-memory document.
type FakeMemDOM = Fake MemDOM

-- | Run on this device.
onDevice :: Device -> Fake m a -> m a
onDevice d (Fake r) = runReaderT r d

device :: (Monad m) => Fake m Device
device = Fake ask

fresh :: (MonadIO m) => Fake m Int
fresh = device >>= \d -> liftIO (atomicModifyIORef' d.counter (\n -> (n + 1, n + 1)))

-- | A key's handle.
handleOf :: Int -> KeyRef
handleOf = fromMaybe (panic "a key's number is never 0") . keyRefOf

-- | What a seal with this key and data holds: the key, the data and the
-- text, in the clear.
fakeSeal :: Int -> Text -> Text -> Sealed
fakeSeal k ad text = Sealed {nonce = "AAAAAAAAAAAAAAAA", ciphertext = Ciphertext (decodeUtf8 (BSL.toStrict (encode (k, ad, text))))}

-- | The text of a seal, if it was made with this key and data.
fakeOpen :: Int -> Text -> Sealed -> Either Text Text
fakeOpen k ad s = case eitherDecodeStrict (encodeUtf8 s.ciphertext.text) of
  Right (k', ad', text) | k' == k && ad' == ad -> Right text
  Right (_ :: Int, _ :: Text, _ :: Text) -> Left "The operation failed for an operation-specific reason"
  Left why -> Left (toS why)

-- | The id of the key pair a test's vault holds.
keyIdOfTest :: KeyId
keyIdOfTest = KeyId "test-key-pair"

-- | A user's vault as a sign-in makes it, with this vault key: their key
-- pair sealed in it, and the key wrapped for the password.
userVault :: Int -> Vault
userVault k =
  Vault
    { format = vaultFormat
    , salt = "salt"
    , items = Map.singleton identityItem Item {keyId = Just keyIdOfTest, sealed = fakeSeal k (identityData keyIdOfTest) "the private key"}
    , wraps = Map.singleton ByPassword WrappedKey {nonce = "AAAAAAAAAAAAAAAA", key = Ciphertext (show k)}
    }

-- | A device that holds this vault key for the user: the vault unlocked
-- there.
withVaultKey :: Device -> UserId -> Int -> IO ()
withVaultKey d u k = modifyIORef' d.keys (Map.insert (vaultSlot u).text k)

-- | The device's records.
deviceRecords :: Device -> IO (Map Text ByteString)
deviceRecords d = readIORef d.records

instance (MonadAssistant m, MonadIO m) => MonadAssistant (Fake m) where
  deviceRecord slot = device >>= \d -> liftIO (Right . Map.lookup slot.text <$> readIORef d.records)
  keepDeviceRecord slot record = device >>= \d -> liftIO (writing d >> modifyIORef' d.records (Map.alter (const record) slot.text)) $> Right ()
  dropDeviceRecordIf slot bytes = device >>= \d -> liftIO (writing d >> atomicModifyIORef' d.records (\rs -> if Map.lookup slot.text rs == Just bytes then (Map.delete slot.text rs, Right True) else (rs, Right False)))
  newVaultKeyBy _ _ = handleOf <$> fresh
  sealWith k text ad = pure (Right (fakeSeal (heldHandle k) ad text))
  openWith k sealed ad = pure (fakeOpen (heldHandle k) ad sealed)
  forgetKey _ = pass
  keepDeviceKeyBy _ slot k _ = device >>= \d -> liftIO (writing d >> modifyIORef' d.keys (Map.insert slot.text (heldHandle k)) >> changed d) $> Right ()
  deviceKeyBy _ slot = device >>= \d -> liftIO ((handleOf <$>) . Map.lookup slot.text <$> readIORef d.keys)
  dropDeviceKey slot = device >>= \d -> liftIO (modifyIORef' d.keys (Map.delete slot.text) >> changed d)
  secretsChanges = (\d -> d.changes.emitter) <$> device
  announceSecretsChanged = device >>= liftIO . changed
  requestTabLock name = device >>= \d -> liftIO $ atomically $ stateTVar d.requests (\(n, asked) -> (TabLock (n + 1), (n + 1, Map.insert (n + 1) name asked)))
  tabLockGranted (TabLock n) =
    device >>= \d -> liftIO $ do
      let taking = do
            name <- maybe (panic "a lock of tabs not requested") pure . Map.lookup n . snd =<< readTVar d.requests
            held <- readTVar d.locks
            if Map.member name held then pure False else writeTVar d.locks (Map.insert name n held) $> True
      taken <- atomically taking
      unless taken $ do
        -- Another request holds the lock: this one waits, and says so.
        atomically (modifyTVar' d.waiters (+ 1))
        atomically (taking >>= check >> modifyTVar' d.waiters (subtract 1))
          `onException` atomically (modifyTVar' d.waiters (subtract 1))
  releaseTabLock (TabLock n) =
    device >>= \d -> liftIO $ atomically $ do
      name <- Map.lookup n . snd <$> readTVar d.requests
      modifyTVar' d.requests (second (Map.delete n))
      for_ name (\x -> modifyTVar' d.locks (Map.update (\holder -> if holder == n then Nothing else Just holder) x))
  sealingHere = (\d -> d.sealing) <$> device
  openIdentityBy _ k keyId sealed = case fakeOpen (heldHandle k) (identityData keyId) sealed of
    Right _ -> Right . handleOf <$> fresh
    Left why -> pure (Left why)
  passwordKeyBy _ _ _ = Right . handleOf <$> fresh
  unwrapWithBy _ _ w _ _ = pure (maybe (Left "The operation failed for an operation-specific reason") Right (keyRefOf =<< readMaybe (toS w.key.text :: [Char])))
  wrapWith _ k _ = pure (Right WrappedKey {nonce = "AAAAAAAAAAAAAAAA", key = Ciphertext (show (heldHandle k))})

instance (MonadVault m) => MonadVault (Fake m)

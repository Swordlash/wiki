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
-- devices, so that two devices can hold the same vault key.
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
  , FakeMemDOM
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Halogen.VDom.DOM.Monad (MemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), heldHandle, keyRefOf)
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
  }

-- | A device with nothing kept, numbering its keys with this counter.
newDevice :: IORef Int -> Bool -> IO Device
newDevice counter sealing = Device <$> newIORef mempty <*> newIORef mempty <*> pure sealing <*> pure counter

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
  keepDeviceRecord slot record = device >>= \d -> liftIO (modifyIORef' d.records (Map.alter (const record) slot.text)) $> Right ()
  newVaultKeyBy _ _ = handleOf <$> fresh
  sealWith k text ad = pure (Right (fakeSeal (heldHandle k) ad text))
  openWith k sealed ad = pure (fakeOpen (heldHandle k) ad sealed)
  forgetKey _ = pass
  keepDeviceKeyBy _ slot k _ = device >>= \d -> liftIO (modifyIORef' d.keys (Map.insert slot.text (heldHandle k))) $> Right ()
  deviceKeyBy _ slot = device >>= \d -> liftIO ((handleOf <$>) . Map.lookup slot.text <$> readIORef d.keys)
  dropDeviceKey slot = device >>= \d -> liftIO (modifyIORef' d.keys (Map.delete slot.text))
  sealingHere = (\d -> d.sealing) <$> device
  openIdentityBy _ k keyId sealed = case fakeOpen (heldHandle k) (identityData keyId) sealed of
    Right _ -> Right . handleOf <$> fresh
    Left why -> pure (Left why)
  passwordKeyBy _ _ _ = Right . handleOf <$> fresh
  unwrapWithBy _ _ w _ _ = pure (maybe (Left "The operation failed for an operation-specific reason") Right (keyRefOf =<< readMaybe (toS w.key.text :: [Char])))
  wrapWith _ k _ = pure (Right WrappedKey {nonce = "AAAAAAAAAAAAAAAA", key = Ciphertext (show (heldHandle k))})

instance (MonadVault m) => MonadVault (Fake m)

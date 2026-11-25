{-# LANGUAGE TemplateHaskell #-}

-- | A plugin's secrets in the user's vault, sealed with the browser's real
-- AES-GCM ("Wiki.App.Plugin.Host.Secrets", "Wiki.Core.Vault.Plugin"):
-- kept and read back through the vault, padded so that the ciphertext's
-- length does not tell how many secrets there are, and refused when the
-- item is moved to another plugin's name. The vault on the server is the
-- tests' model (@Test.VaultServer@); the rules of the store, with a fake
-- of the cryptography, are the native suite's (@Test.PluginSecrets@).
--
-- The lock of tabs a change holds ("Wiki.App.Assistant"'s
-- 'holdingAcrossTabs', @jsbits\/tabs.js@) is the browser's Web Lock: a
-- request let go while it waits for another tab is withdrawn, its wait
-- ends failed, never granted, and nothing holds the lock after.
module Test.Browser.PluginSecrets (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.JSBits (Inert (..), JSText, Safety (..), browserJS, inBrowser, toJSText)
import Halogen.JSBits.Promise (Promise (..), wait)
import Protolude hiding (wait)
import Test.Hspec.Halogen
import Test.VaultServer
import Wiki.App.Assistant (MonadAssistant (..), NewIdentity (..))
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Plugin (Manifest (..), SecretName (..), SecretSpec (..), Unavailable (..), builtin)
import Wiki.App.Plugin.Host.Secrets (putSecret, readSecrets)
import Wiki.App.Vault (vaultSlot)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Ciphertext (..), Item (..), ItemName (..), Sealed (..), Vault (..), identityItem, itemOf, vaultFormat, withItem)
import Wiki.Core.Vault.Plugin (paddingBlock, pluginSealingData)

-- The test plays another tab that holds a lock: test/jsbits/test-locks.js.
$( browserJS
     ["jsbits/test-locks.js"]
     [ ("js_lock_holdPromise", "wiki_test_lock_hold", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_lock_let_goPromise", "wiki_test_lock_let_go", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_lock_pendingPromise", "wiki_test_lock_pending", Safe, [t|JSText -> IO (Promise Int)|])
     , ("js_lock_freePromise", "wiki_test_lock_free", Safe, [t|JSText -> IO (Promise Bool)|])
     ]
 )

js_lock_hold :: JSText -> IO ()
js_lock_hold a1 = if inBrowser then wait =<< js_lock_holdPromise a1 else pure inert

js_lock_let_go :: JSText -> IO ()
js_lock_let_go a1 = if inBrowser then wait =<< js_lock_let_goPromise a1 else pure inert

js_lock_pending :: JSText -> IO Int
js_lock_pending a1 = if inBrowser then wait =<< js_lock_pendingPromise a1 else pure inert

js_lock_free :: JSText -> IO Bool
js_lock_free a1 = if inBrowser then wait =<< js_lock_freePromise a1 else pure inert

spec :: forall m -> (MonadBrowserTest m, MonadAssistant m, MonadVault m) => Spec
spec m = do
  it "withdraws a lock of tabs let go while it waits for another tab's: its wait fails, and nothing holds the lock after" $ runPage $ do
    found <- unsafeIOToPageM (runTest (lockLetGoWhileQueued :: m Queued))
    found.waitingBefore `shouldBe` 1
    found.waitEnded `shouldBe` "failed"
    found.waitingAfter `shouldBe` 0
    found.freeAfter `shouldBe` True

  it "keeps a plugin's secrets sealed with AES-GCM, padded, and refuses them as another plugin's" $ runPage $ do
    outcome <- unsafeIOToPageM (runTest (roundTrip :: m Outcome))
    outcome.readBack `shouldBe` Right (Map.fromList [(token, "sk-ü-123"), (other, "two")])
    -- The plaintext is padded to a block, and AES-GCM adds its 16-byte tag.
    outcome.lengths `shouldBe` [unpadded (paddingBlock + 16), unpadded (paddingBlock + 16)]
    outcome.openedAsOwn `shouldBe` True
    outcome.openedAsOther `shouldBe` False
    case outcome.readAsOther of
      Left (Failed _) -> pass
      other' -> expectationFailure ("the item read as another plugin's: " <> show other')

-- | What the round trip found.
data Outcome = Outcome
  { readBack :: Either Unavailable (Map SecretName Text)
  , lengths :: [Int]
  -- ^ The ciphertext's length, after one secret and after two.
  , openedAsOwn :: Bool
  , openedAsOther :: Bool
  , readAsOther :: Either Unavailable (Map SecretName Text)
  }

token, other :: SecretName
token = SecretName "token"
other = SecretName "other"

-- | Two plugins that declare the same names.
notes, mail :: Manifest
notes = (builtin "notes" "Notes" "") {secrets = [SecretSpec {name = token, label = "Token", description = ""}, SecretSpec {name = other, label = "Other", description = ""}]}
mail = (builtin "mail" "Mail" "") {secrets = [SecretSpec {name = token, label = "Token", description = ""}, SecretSpec {name = other, label = "Other", description = ""}]}

-- | A user's vault made in this browser, unlocked here for the page's
-- life; two secrets kept in it; the item moved to the other plugin's name.
roundTrip :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => m Outcome
roundTrip = do
  let u = UserId "user-plugin-secrets-browser"
  k <- newVaultKey False
  made <- either (panic . ("no key pair: " <>)) pure =<< newIdentity k
  forgetKey made.key
  _ <- keepDeviceKey (vaultSlot u) k False
  server <- liftIO (newServer (Just Vault {format = vaultFormat, salt = "c2FsdHNhbHRzYWx0c2FsdHNhbHRzYWx0c2FsdHNhbHQ", items = Map.singleton identityItem Item {keyId = Just made.keyId, sealed = made.sealed}, wraps = mempty}))
  let item = ItemName "plugin-notes"
      sealedNow = liftIO (serverState server) <&> \s -> s.kept >>= itemOf item . snd
      lengthNow = maybe 0 (\i -> T.length i.sealed.ciphertext.text) <$> sealedNow
  _ <- served server (putSecret u notes token "sk-ü-123")
  afterOne <- lengthNow
  _ <- served server (putSecret u notes other "two")
  afterTwo <- lengthNow
  readBack <- served server (readSecrets u notes)
  sealed <- maybe (panic "no item") (\i -> pure i.sealed) =<< sealedNow
  openedAsOwn <- isRight <$> openWith k sealed (pluginSealingData "notes")
  openedAsOther <- isRight <$> openWith k sealed (pluginSealingData "mail")
  liftIO (serverState server) >>= traverse_ (\(_, v) -> liftIO (setVault server (withItem (ItemName "plugin-mail") Item {keyId = Nothing, sealed} v))) . (\s -> s.kept)
  readAsOther <- served server (readSecrets u mail)
  dropDeviceKey (vaultSlot u)
  forgetKey k
  pure Outcome {readBack, lengths = [afterOne, afterTwo], openedAsOwn, openedAsOther, readAsOther}

-- | What the lock of tabs let go while it waited showed: the requests for
-- it the browser queued before and after; how the wait for it ended; and
-- whether the lock was free at once after the other tab let go.
data Queued = Queued
  { waitingBefore :: Int
  , waitEnded :: Text
  , waitingAfter :: Int
  , freeAfter :: Bool
  }

-- | The test holds the lock as another tab would. The wiki asks for it,
-- and a thread waits for the grant. The request is then let go, as the
-- end of a change cancelled while it waits lets it go
-- ('Wiki.App.Assistant.holdingAcrossTabs'): the wait must end at once,
-- failed (its promise settled, so the callbacks on it are freed), and the
-- request must leave the browser's queue, so that once the other tab lets
-- go nothing holds the lock. The browser's queue is read after the
-- request is made, and it answers in the order it was asked.
lockLetGoWhileQueued :: (MonadAssistant m, MonadUnliftIO m) => m Queued
lockLetGoWhileQueued = do
  let name = "wiki-test-lock-let-go"
  liftIO (js_lock_hold (toJSText name))
  l <- requestTabLock name
  waitingBefore <- liftIO (js_lock_pending (toJSText name))
  ended <- liftIO newEmptyMVar
  void $ withRunInIO $ \run ->
    forkFinally (run (tabLockGranted l)) (putMVar ended . either (const "failed") (const ("granted" :: Text)))
  releaseTabLock l
  waitEnded <- liftIO (takeMVar ended)
  waitingAfter <- liftIO (js_lock_pending (toJSText name))
  liftIO (js_lock_let_go (toJSText name))
  freeAfter <- liftIO (js_lock_free (toJSText name))
  pure Queued {waitingBefore, waitEnded, waitingAfter, freeAfter}

-- | The length of so many bytes in base64url without padding.
unpadded :: Int -> Int
unpadded n = (4 * n + 2) `div` 3

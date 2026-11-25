-- | A plugin's secrets in the user's vault, sealed with the browser's real
-- AES-GCM ("Wiki.App.Plugin.Host.Secrets", "Wiki.Core.Vault.Plugin"):
-- kept and read back through the vault, padded so that the ciphertext's
-- length does not tell how many secrets there are, and refused when the
-- item is moved to another plugin's name. The vault on the server is the
-- tests' model (@Test.VaultServer@); the rules of the store, with a fake
-- of the cryptography, are the native suite's (@Test.PluginSecrets@).
module Test.Browser.PluginSecrets (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
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

spec :: forall m -> (MonadBrowserTest m, MonadAssistant m, MonadVault m) => Spec
spec m =
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

-- | The length of so many bytes in base64url without padding.
unpadded :: Int -> Int
unpadded n = (4 * n + 2) `div` 3

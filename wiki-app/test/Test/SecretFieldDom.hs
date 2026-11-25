-- | The host's field for a plugin's secret ("Wiki.App.View.SecretField"),
-- the real component on the in-memory DOM, inside the settings of a plugin
-- of the tests (@Test.SecretsPlugin@) with the capabilities the host makes
-- for it ("Wiki.App.Plugin.Host"): over a model of the server's vault
-- (@Test.VaultServer@) and a browser whose cryptography is faked
-- (@Test.FakeVault@). Locked, the field offers the unlock; unlocked, it
-- keeps a value after the plugin's check, never shows it again, and
-- forgets it. The rules over every order of acts and answers are the
-- machine's (@Test.SecretField@).
module Test.SecretFieldDom (spec) where

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude hiding (find)
import Test.FakeVault
import Test.Harness.MemDom
import Test.Hspec
import Test.SecretsPlugin (secretive, secretiveManifest, tokenName)
import Test.VaultServer
import Wiki.App.Backend (anonymousIn)
import Wiki.App.Backend qualified as B
import Wiki.App.Plugin (Options (..), OptionsInput (..), OptionsOutput, Plugin (..))
import Wiki.App.Plugin.Host (capabilities)
import Wiki.App.Route (Route (..), route)
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Login (..), User (..))
import Wiki.Core.Vault (Item (..), ItemName (..), itemOf)
import Wiki.Core.Vault.Plugin (decodeSecrets, pluginSealingData)

spec :: Spec
spec = describe "Wiki.App.View.SecretField" $
  it "unlocks the vault where it is locked, keeps a value after the plugin's check, never shows it again, and forgets it" $ do
    counter <- newIORef 0
    k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
    server <- newServer (Just (userVault k))
    -- A browser that holds no key to Alice's vault: it is locked here.
    d <- newDevice counter True
    let run :: Served (Fake MemDOM) a -> MemDOM a
        run = onDevice d . served server
        kept = do
          s <- serverState server
          pure $ do
            (_, v) <- s.kept
            i <- itemOf (ItemName "plugin-secretive") v
            rightToMaybe (fakeOpen k (pluginSealingData "secretive") i.sealed >>= decodeSecrets)
    router <- runMemDOM (run (Router.newRouter "/" Unknown route))
    let session = (anonymousIn Open) {B.user = Just User {id = alice, login = Login "alice", displayName = "Alice", avatar = Nothing}}
        can = capabilities (viewContextAs router session [] 0) secretiveManifest
    surface <- maybe (panic "the plugin has settings") (\(Options o) -> pure o) (secretive @(Served (Fake MemDOM))).options
    withView run surface OptionsInput {user = Just alice, options = Nothing, can} $ \view -> do
      settle view
      -- Locked: the unlock, and no field for the value.
      void (element view ".secret-field-unlock-passkey")
      isJust <$> find view ".secret-field-value" `shouldReturn` False
      textOf view ".secretive-read" `shouldReturn` "unavailable: Locked"

      typeInto view ".secret-field-password" "alice's password"
      click view ".secret-field-unlock-password"
      settle view
      textOf view ".secret-field-state" `shouldReturn` "Not set"
      -- The plugin was told, and read again: nothing is set.
      textOf view ".secretive-read" `shouldReturn` ""

      typeInto view ".secret-field-value" "sk-live-123"
      -- While typed, the value is in the field: the check below can fail.
      shown view "sk-live-123" `shouldReturn` True
      click view ".secret-field-keep"
      settle view
      textOf view ".secret-field-state" `shouldReturn` "Set"
      shown view "sk-live-123" `shouldReturn` False
      textOf view ".secretive-read" `shouldReturn` "token"
      kept `shouldReturn` Just (Map.fromList [(tokenName, "sk-live-123")])

      -- The plugin's check refuses this one: nothing is kept.
      typeInto view ".secret-field-value" "bad-token"
      click view ".secret-field-keep"
      settle view
      textOf view ".secret-field-problem" `shouldReturn` "This token is refused."
      kept `shouldReturn` Just (Map.fromList [(tokenName, "sk-live-123")])

      click view ".secret-field-forget"
      settle view
      textOf view ".secret-field-state" `shouldReturn` "Not set"
      kept `shouldReturn` Nothing
      outputs view `shouldReturn` ([] :: [OptionsOutput])
  where
    alice = UserId "user-alice"

-- | Whether a value shows anywhere in the view: in its HTML, or as the
-- value of one of its fields.
shown :: View q i o -> Text -> IO Bool
shown view value = do
  page <- html view
  fields <- findAll view "input" >>= traverse (property "value")
  pure (value `T.isInfixOf` page || any (\case Just (ScalarText t) -> value `T.isInfixOf` t; _ -> False) fields)

-- | A plugin of the tests, which keeps two secrets, a token and an
-- endpoint, with the plugins' capability of secrets ("Wiki.App.Plugin"'s
-- 'Secrets'). It is
-- written as a built-in plugin is: it imports the plugins' interface,
-- Halogen and wiki-core, and nothing else of the application's. It is in
-- no registry; a test mounts its settings with the host's capabilities
-- (@Test.SecretFieldDom@).
--
-- Its settings show the host's field for each secret, and the names of
-- the secrets it reads back, never their values. Its check refuses a
-- value that starts with "bad".
module Test.SecretsPlugin
  ( secretive
  , secretiveManifest
  , tokenName
  , endpointName
  )
where

import Data.Map.Strict qualified as Map
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Wiki.App.Plugin

-- | The names of the plugin's two secrets.
tokenName, endpointName :: SecretName
tokenName = SecretName "token"
endpointName = SecretName "endpoint"

-- | The plugin's manifest: it declares the token and the endpoint.
secretiveManifest :: Manifest
secretiveManifest =
  Manifest
    { id = PluginId "secretive"
    , name = "Secretive"
    , description = "A plugin of the tests that keeps a token."
    , onByDefault = True
    , secrets =
        [ SecretSpec {name = tokenName, label = "Token", description = "The token of a service of the tests."}
        , SecretSpec {name = endpointName, label = "Endpoint", description = "The address of the service, with a key in it."}
        ]
    }

-- | The plugin: its settings and nothing else.
secretive :: (Typeable m, MonadIO m) => Plugin m
secretive = Plugin {manifest = secretiveManifest, diagnostics = Nothing, agent = Nothing, pageKind = Nothing, options = Just (Options settings)}

type Slots = ("secret" .== H.Slot (Const Void) FieldOutput FieldSlot)

data State m = State
  { granted :: Capabilities m
  -- ^ The capabilities, as the host gave them last.
  , read :: Maybe (Either Unavailable [SecretName])
  -- ^ The names of the secrets read back last, or why none could be.
  }

data Action m = Initialize | Receive (OptionsInput m) | FromField FieldOutput

settings :: forall m. (Typeable m, MonadIO m) => H.Component (Const Void) (OptionsInput m) OptionsOutput m
settings =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {granted = input.can, read = Nothing}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize, H.receive = Just . Receive}
      }
  where
    render :: State m -> H.ComponentHTML (Action m) Slots m
    render st =
      HH.section
        [HP.class_ (HH.ClassName "secretive")]
        ( secretField "secret" st.granted.secrets FieldInput {name = tokenName, check = checkToken, binding = ""} FromField
            : secretField "secret" st.granted.secrets FieldInput {name = endpointName, check = checkToken, binding = ""} FromField
            : [HH.p [HP.class_ (HH.ClassName "secretive-read")] [HH.text (either (\u -> "unavailable: " <> show u) (T.intercalate ", " . map (\n -> n.text)) r)] | Just r <- [st.read]]
        )

    checkToken :: Text -> m (Either Text Text)
    checkToken value = pure (if "bad" `T.isPrefixOf` value then Left "This token is refused." else Right value)

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Slots OptionsOutput m ()
    handleAction = \case
      Initialize -> readBack
      Receive input -> modify (\st -> st {granted = input.can})
      FromField _ -> readBack

    -- The secrets read back: their names.
    readBack = do
      can <- gets (\st -> st.granted)
      r <- H.async can.secrets.get
      modify (\st -> st {read = Just (Map.keys <$> r)})

-- | The writing assistant's settings ("Wiki.Core.Assistant"'s 'Settings')
-- cut into the three places a plugin keeps things (decision 69 and WP5.84
-- of docs/PLAN.md), and put together again.
--
-- * The options ('optionsValue'): the provider the assistant asks, its
--   mode (Ask or Agent), the page's cap, and for each provider the model,
--   the effort a conversation starts with and the service's base URL. The
--   wiki's server keeps them for the user, in the clear, so they follow
--   the user to every device.
-- * The secrets ('keysOf'): one API key per provider, under the provider's
--   name ('secretOf'), each with the service it was kept for ('keptValue').
--   They are kept sealed in the user's vault ("Wiki.Core.Vault.Plugin").
-- * What was learnt from the providers ('learntValue'): each provider's
--   list of models and whether it could be reached. It is a cache of this
--   browser, kept in the clear and seen by no other device. A list of
--   models can be far longer than the server takes for options.
--
-- 'fromParts' puts the three together again. Before this split, the vault
-- sealed the options with the keys, in one item ("Wiki.Core.Assistant.Vault").
--
-- = A key and its service
--
-- A key is only ever sent to the service it was kept for. The options and
-- the keys are kept apart, and change apart: the options may name another
-- address after a key was kept (a service chosen on another device, the
-- options of another browser adopted by the move of legacy settings). So
-- each key is kept with its 'Service': the provider, and the address its
-- requests went to when the key was checked and kept, the provider's own
-- included ('Wiki.Core.Assistant.officialUrl'). 'fromParts' gives the
-- settings a key only where its service is the one the options name for
-- the provider; otherwise the provider has no key there, and
-- 'keyElsewhere' says for which address the key was kept, for a surface to
-- say so ('elsewhereText'). The field of a key binds its check to the
-- same 'Service' ('serviceText'), so the service the key was checked
-- against is the one it is kept for.
module Wiki.Core.Assistant.Split
  ( -- * The secrets
    secretOf
  , assistantSecrets
  , keysOf
  , Service (..)
  , serviceFor
  , serviceText
  , keptValue
  , keptKey
  , keyElsewhere
  , elsewhereText

    -- * The options
  , optionsValue
  , blankOptions

    -- * What was learnt
  , learntValue

    -- * Together
  , fromParts
  )
where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither, parseJSON, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Core.Assistant (ApiKey (..), Mode (..), Provider (..), ProviderSettings (..), Settings (..), addressOf, apiKeyFrom, defaultSettings, officialUrl, providerFromKey, providerKey, providerLabel, settingsOf, withKey)
import Wiki.Core.Assistant.Vault (joinSettings, learntOf)
import Wiki.Core.Url (Url (..))
import Wiki.Core.Vault.Plugin (SecretName (..))

--------------------------------------------------------------------------------
-- The secrets

-- | The name of the secret that holds a provider's API key: the
-- provider's own name, as the settings write it ('providerKey').
secretOf :: Provider -> SecretName
secretOf p = SecretName (providerKey p)

-- | Every secret the assistant keeps: one key per provider.
assistantSecrets :: [SecretName]
assistantSecrets = map secretOf [minBound .. maxBound]

-- | The API keys of the settings, by secret's name, each as its secret
-- holds it: with the service these same settings send the provider's
-- requests to ('keptValue'). The move of legacy settings takes each
-- legacy place's keys with it, so a key moves with the address its own
-- place held for the provider.
keysOf :: Settings -> Map SecretName Text
keysOf s = Map.fromList [(secretOf p, keptValue (serviceFor p s) k) | (p, ProviderSettings (Just k) _ _ _ _ _) <- Map.toList s.byProvider]

-- | 'Service' is a service a provider's requests go to: the provider, and
-- the address ('Wiki.Core.Assistant.addressOf'), the provider's own
-- included. An API key is kept for one service, and sent to no other.
data Service = Service {provider :: Provider, address :: Url}
  deriving stock (Eq, Show)

-- | The service the settings send a provider's requests to.
serviceFor :: Provider -> Settings -> Service
serviceFor p s = Service {provider = p, address = addressOf p (settingsOf p s)}

-- | The text a key's field binds its check to ('Wiki.App.Plugin''s
-- @binding@): the service, as one text. Two services give two texts.
serviceText :: Service -> Text
serviceText svc = providerKey svc.provider <> " " <> svc.address.text

-- | The value of a provider's secret: the key with the service it was kept
-- for, as a JSON object, @{"key", "provider", "address"}@.
keptValue :: Service -> ApiKey -> Text
keptValue svc k = decodeUtf8 (BSL.toStrict (encode (object ["key" .= k.text, "provider" .= providerKey svc.provider, "address" .= svc.address.text])))

-- | The key a provider's secret holds, with the service it was kept for:
-- the key made as every key is ('Wiki.Core.Assistant.apiKeyFrom'). A
-- secret that is not 'keptValue''s object is a key in the plain form that
-- earlier builds kept, for a service nobody recorded. It is read as kept
-- for the provider's own service ('Wiki.Core.Assistant.officialUrl'),
-- never for another address: a key typed against a gateway then reaches
-- at worst the provider itself, and the provider's own key never reaches
-- a gateway. 'Nothing' where no key is left.
keptKey :: Provider -> Text -> Maybe (Service, ApiKey)
keptKey p stored = case eitherDecodeStrict (encodeUtf8 stored) of
  Right v | Just (key, provider, address) <- parseMaybe fields v -> (,) <$> (Service <$> providerFromKey provider <*> pure (Url (T.dropWhileEnd (== '/') (T.strip address)))) <*> apiKeyFrom key
  _ -> (Service {provider = p, address = officialUrl p},) <$> apiKeyFrom stored
  where
    fields = withObject "KeptKey" $ \o -> (,,) <$> o .: "key" <*> o .: "provider" <*> o .: "address"

-- | The address a provider's key was kept for, where the secrets hold a
-- key for the provider and the options name another service: that key is
-- not used ('fromParts'), and a surface says why ('elsewhereText').
keyElsewhere :: Maybe Value -> Map SecretName Text -> Provider -> Maybe Url
keyElsewhere options keys p = case keptKey p =<< Map.lookup (secretOf p) keys of
  Just (svc, _) | svc /= serviceFor p (optionsIn options) -> Just svc.address
  _ -> Nothing

-- | What a surface says where a provider's key was kept for another
-- address than the one the options name: the address the key is for, and
-- what to do.
elsewhereText :: Provider -> Url -> Url -> Text
elsewhereText p kept chosen = "The " <> providerLabel p <> " key kept is for another address: " <> kept.text <> ". Enter the key for " <> chosen.text <> ", or choose that address."

--------------------------------------------------------------------------------
-- The options

-- | The options of the settings, as the server keeps them for the user:
-- one JSON object, as the settings are written ('Wiki.Core.Assistant''s
-- JSON) without the keys and without what was learnt. A provider appears
-- only when a model, an effort or a base URL is set for it.
optionsValue :: Settings -> Value
optionsValue s =
  object $
    ("provider" .= providerKey s.provider)
      : ["mode" .= ("ask" :: Text) | s.chatMode == Ask]
        <> ["pageCap" .= n | Just n <- [s.pageCap]]
        <> [ Key.fromText (providerKey p) .= object (["model" .= m | Just m <- [model]] <> ["defaultEffort" .= e | Just e <- [effort]] <> ["baseUrl" .= u | Just u <- [baseUrl]])
           | (p, ProviderSettings _ model effort baseUrl _ _) <- Map.toList s.byProvider
           , isJust model || isJust effort || isJust baseUrl
           ]

-- | Whether options hold nothing but the defaults: such options need not
-- be kept.
blankOptions :: Value -> Bool
blankOptions v = v == optionsValue defaultSettings

--------------------------------------------------------------------------------
-- What was learnt

-- | What was learnt from the providers, as this browser keeps it: the
-- settings' JSON of 'learntOf', as the device's record held it before the
-- split ("Wiki.Core.Assistant.Vault"'s @learnt@).
learntValue :: Settings -> Value
learntValue = toJSON . learntOf

--------------------------------------------------------------------------------
-- Together

-- | The settings made whole from the options the server keeps (none: the
-- defaults), the keys, by secret's name, and what this browser learnt
-- (none: nothing learnt). Options that cannot be read count as none; keys
-- or anything learnt that options carry are not taken from them.
--
-- A provider gets its key only where the key was kept for the service the
-- options name for it ('keptKey', 'serviceFor'); otherwise it has none
-- (the module's header says why). Every request is made from these
-- settings, so the key and the address it goes to are read together.
fromParts :: Maybe Value -> Map SecretName Text -> Maybe Value -> Settings
fromParts options keys learnt = joinSettings (withKeys (optionsIn options)) (learntIn learnt)
  where
    learntIn = maybe defaultSettings (either (const defaultSettings) learntOf . parseEither parseJSON)
    withKeys s = foldr keyed s [minBound .. maxBound]
    -- Made as every key is ('withKey', 'Wiki.Core.Assistant.apiKeyFrom'):
    -- a key stored with spaces around it, by an earlier build or moved
    -- from a legacy record, is the key that was checked.
    keyed p s = case keptKey p =<< Map.lookup (secretOf p) keys of
      Just (svc, k) | svc == serviceFor p s -> withKey p (Just k.text) s
      _ -> s

-- | The settings the options hold, with no key and nothing learnt: the
-- defaults where there are none or they cannot be read.
optionsIn :: Maybe Value -> Settings
optionsIn = maybe defaultSettings (either (const defaultSettings) stripped . parseEither parseJSON)
  where
    stripped s = s {byProvider = Map.map (\(ProviderSettings _ m e b _ _) -> ProviderSettings Nothing m e b Nothing False) s.byProvider}

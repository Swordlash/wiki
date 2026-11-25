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
--   name ('secretOf'). They are kept sealed in the user's vault
--   ("Wiki.Core.Vault.Plugin").
-- * What was learnt from the providers ('learntValue'): each provider's
--   list of models and whether it could be reached. It is a cache of this
--   browser, kept in the clear and seen by no other device. A list of
--   models can be far longer than the server takes for options.
--
-- 'fromParts' puts the three together again. Before this split, the vault
-- sealed the options with the keys, in one item ("Wiki.Core.Assistant.Vault").
module Wiki.Core.Assistant.Split
  ( -- * The secrets
    secretOf
  , assistantSecrets
  , keysOf

    -- * The options
  , optionsValue
  , blankOptions

    -- * What was learnt
  , learntValue

    -- * Together
  , fromParts
  )
where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Map.Strict qualified as Map
import Protolude
import Wiki.Core.Assistant (ApiKey (..), Mode (..), Provider (..), ProviderSettings (..), Settings (..), defaultSettings, providerKey, settingsOf)
import Wiki.Core.Assistant.Vault (joinSettings, learntOf)
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

-- | The API keys of the settings, by secret's name.
keysOf :: Settings -> Map SecretName Text
keysOf s = Map.fromList [(secretOf p, k.text) | (p, ProviderSettings (Just k) _ _ _ _ _) <- Map.toList s.byProvider]

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
fromParts :: Maybe Value -> Map SecretName Text -> Maybe Value -> Settings
fromParts options keys learnt = joinSettings (withKeys (optionsIn options)) (learntIn learnt)
  where
    optionsIn = maybe defaultSettings (either (const defaultSettings) stripped . parseEither parseJSON)
    stripped s = s {byProvider = Map.map (\(ProviderSettings _ m e b _ _) -> ProviderSettings Nothing m e b Nothing False) s.byProvider}
    learntIn = maybe defaultSettings (either (const defaultSettings) learntOf . parseEither parseJSON)
    withKeys s = foldr keyed s [minBound .. maxBound]
    keyed p s = case Map.lookup (secretOf p) keys of
      Nothing -> s
      Just k ->
        let ProviderSettings _ m e b f u = settingsOf p s
         in s {byProvider = Map.insert p (ProviderSettings (Just (ApiKey k)) m e b f u) s.byProvider}

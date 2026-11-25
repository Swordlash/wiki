-- | A plugin's secrets in the user's vault ("Wiki.Core.Vault"; decision
-- 69 and WP5.84 of docs/PLAN.md): the names a plugin keeps its secrets
-- under, the vault item that holds them, and the text that item seals.
-- This module is the format, pure; the application reads and writes the
-- items ("Wiki.App.Plugin.Host.Secrets").
--
-- = One item per plugin
--
-- Every secret of one plugin is in one item of the vault, named
-- @plugin-@ and the plugin's id ('pluginItem'). The item seals a JSON
-- object from secret name to value ('encodeSecrets'). The object is padded
-- with spaces to a multiple of 'paddingBlock' bytes, so that the length of
-- the ciphertext the server stores does not tell how many secrets a
-- plugin keeps, nor how long they are.
--
-- An item is sealed with additional data that names its plugin
-- ('pluginSealingData'). An item copied to another plugin's name then fails
-- to open: one plugin's item cannot be read as another's.
--
-- One item per plugin, and not one per secret, keeps the vault within the
-- server's limit of 16 items, and needs no separator between a plugin's id
-- and a secret's name in an item's name.
module Wiki.Core.Vault.Plugin
  ( -- * A secret's name
    SecretName (..)
  , secretNameOk
  , maxSecretNameLength

    -- * A plugin's item
  , pluginItem
  , maxPluginIdForSecrets
  , pluginSealingData
  , pluginDataPrefix

    -- * What an item seals
  , encodeSecrets
  , decodeSecrets
  , paddingBlock
  , foldIn
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), eitherDecodeStrict, encode)
import Data.Aeson.Types (FromJSONKeyFunction (..), toJSONKeyText)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Core.Vault (ItemName (..))

-- | 'SecretName' is the name a plugin gives one of its secrets, such as
-- @anthropic@ for the writing assistant's key for Anthropic. A plugin
-- declares its names in its manifest, and can read and write those
-- names only. A name is 1 to 'maxSecretNameLength' lower-case letters,
-- digits and hyphens ('secretNameOk').
newtype SecretName = SecretName {text :: Text}
  deriving stock (Eq, Ord, Show)

instance ToJSON SecretName where
  toJSON n = toJSON n.text

instance FromJSON SecretName where
  parseJSON v = parseJSON v >>= \t -> if secretNameOk (SecretName t) then pure (SecretName t) else fail ("not a secret's name: " <> toS t)

instance ToJSONKey SecretName where
  toJSONKey = toJSONKeyText (\n -> n.text)

instance FromJSONKey SecretName where
  fromJSONKey = FromJSONKeyTextParser (\t -> if secretNameOk (SecretName t) then pure (SecretName t) else fail ("not a secret's name: " <> toS t))

-- | The longest name of a secret.
maxSecretNameLength :: Int
maxSecretNameLength = 32

-- | Whether a name can be a secret's: 1 to 'maxSecretNameLength'
-- lower-case ASCII letters, digits and hyphens.
secretNameOk :: SecretName -> Bool
secretNameOk n = not (T.null n.text) && T.length n.text <= maxSecretNameLength && T.all (\c -> isAsciiLower c || isDigit c || c == '-') n.text

-- | The longest id of a plugin that keeps secrets. The item's name is
-- @plugin-@ and the id, and the server takes item names of at most 32
-- characters.
maxPluginIdForSecrets :: Int
maxPluginIdForSecrets = 32 - T.length "plugin-"

-- | The name of the vault item that holds a plugin's secrets, by the
-- plugin's id: @plugin-@ and the id. 'Nothing' for an id that cannot be
-- part of an item's name: empty, longer than 'maxPluginIdForSecrets', or
-- with a character other than a lower-case letter, a digit or a hyphen.
pluginItem :: Text -> Maybe ItemName
pluginItem pluginId
  | T.null pluginId || T.length pluginId > maxPluginIdForSecrets = Nothing
  | not (T.all (\c -> isAsciiLower c || isDigit c || c == '-') pluginId) = Nothing
  | otherwise = Just (ItemName ("plugin-" <> pluginId))

-- | The start of the additional data AES-GCM authenticates with a plugin's
-- item: the format's name and version, and what the item is. The plugin's
-- id follows it ('pluginSealingData').
pluginDataPrefix :: Text
pluginDataPrefix = "wiki-vault/2 plugin "

-- | The additional data a plugin's item is sealed with, by the plugin's
-- id. An item sealed for one plugin fails to open as another plugin's.
pluginSealingData :: Text -> Text
pluginSealingData pluginId = pluginDataPrefix <> pluginId

-- | The size, in bytes, that the text an item seals is padded to a
-- multiple of.
paddingBlock :: Int
paddingBlock = 256

-- | The text a plugin's item seals: its secrets as a JSON object, padded
-- with spaces to a multiple of 'paddingBlock' bytes of UTF-8.
encodeSecrets :: Map SecretName Text -> Text
encodeSecrets secrets = decodeUtf8 (bytes <> BS.replicate padding 0x20)
  where
    bytes = BSL.toStrict (encode secrets)
    padding = (paddingBlock - BS.length bytes `mod` paddingBlock) `mod` paddingBlock

-- | The secrets an item's text holds ('encodeSecrets'), or why it holds
-- none. The padding is ignored.
decodeSecrets :: Text -> Either Text (Map SecretName Text)
decodeSecrets t = first (\why -> "A plugin's secrets that could not be read: " <> toS why) (eitherDecodeStrict (encodeUtf8 (T.stripEnd t)))

-- | Secrets kept in two places, put together into one: the first's, and
-- the second's under the names the first does not have. Where both have a
-- name, the first wins: the first is where the secrets are kept from now
-- on (the vault), and the second what is folded into it (what this browser
-- kept before the user had a vault).
foldIn :: Map SecretName Text -> Map SecretName Text -> Map SecretName Text
foldIn = Map.union

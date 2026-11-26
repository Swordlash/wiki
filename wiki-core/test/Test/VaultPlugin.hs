-- | A plugin's secrets as the vault keeps them ("Wiki.Core.Vault.Plugin"):
-- the names, the item and its additional data, the padding, and the fold
-- of what a browser kept into what the vault keeps.
module Test.VaultPlugin (spec) where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Test.Cases (coverage)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wiki.Core.Vault (ItemName (..))
import Wiki.Core.Vault.Plugin

spec :: Spec
spec = describe "Wiki.Core.Vault.Plugin" $ do
  names
  items
  padding
  folding

names :: Spec
names = describe "a secret's name" $ do
  it "is lower-case letters, digits and hyphens, 1 to 32 of them" $ do
    map (secretNameOk . SecretName) ["anthropic", "a", "key-2", T.replicate 32 "k"] `shouldBe` [True, True, True, True]
    map (secretNameOk . SecretName) ["", "Anthropic", "api key", "key_2", "clé", T.replicate 33 "k"] `shouldBe` [False, False, False, False, False, False]

  it "is refused when an item's text names an invalid one" $
    decodeSecrets "{\"Not a name\":\"value\"}" `shouldSatisfy` isLeft

items :: Spec
items = describe "a plugin's item" $ do
  prop "is named for its plugin, in the server's rule for an item's name" $
    forAll genPluginId $ \p ->
      coverage . cover 10 (T.length p > maxPluginIdForSecrets) "too long" $
        case pluginItem p of
          Just n -> T.length p <= maxPluginIdForSecrets .&&. n === ItemName ("plugin-" <> p) .&&. serverTakes n
          Nothing -> property (T.length p > maxPluginIdForSecrets)

  it "is no item for an id that cannot be part of an item's name" $
    map pluginItem ["", "Assistant", "a b", "a_b"] `shouldBe` [Nothing, Nothing, Nothing, Nothing]

  prop "is sealed with data that tells one plugin from every other" $
    forAll genPluginId $ \p -> forAll genPluginId $ \q ->
      (p /= q) ==> pluginSealingData p =/= pluginSealingData q

  it "is sealed with data of the vault's format, naming the plugin" $
    pluginSealingData "assistant" `shouldBe` "wiki-vault/2 plugin assistant"

padding :: Spec
padding = describe "what an item seals" $ do
  prop "is the secrets it was made of, whatever their letters" $
    forAll genSecrets $ \s ->
      coverage . cover 20 (any (T.any (not . isAscii)) (Map.elems s)) "letters beyond ASCII" $
        decodeSecrets (encodeSecrets s) === Right s

  prop "is a multiple of the block in bytes, and at most a block longer than the secrets" $
    forAll genSecrets $ \s ->
      let sealed = BS.length (encodeUtf8 (encodeSecrets s))
          bare = BS.length (encodeUtf8 (T.stripEnd (encodeSecrets s)))
       in coverage . cover 10 (sealed > paddingBlock) "longer than a block" $
            sealed `mod` paddingBlock === 0 .&&. sealed - bare < paddingBlock .&&. sealed > 0

  it "is as many bytes for one short secret as for three" $
    BS.length (encodeUtf8 (encodeSecrets (Map.fromList [(SecretName "a", "x")])))
      `shouldBe` BS.length (encodeUtf8 (encodeSecrets (Map.fromList [(SecretName "a", "xyz"), (SecretName "b", "é"), (SecretName "c", "sk-123")])))

  it "is a block for no secrets" $
    BS.length (encodeUtf8 (encodeSecrets mempty)) `shouldBe` paddingBlock

  prop "is the contents it was made of, the legacy places taken with the secrets" $
    forAll genSecrets $ \s -> forAll (Set.fromList <$> listOf (elements ["item:abc", "record:0f1e", "record:99"])) $ \taken ->
      coverage . cover 20 (not (Set.null taken)) "places taken" $
        decodeContents (encodeContents Contents {secrets = s, taken}) === Right Contents {secrets = s, taken}

  it "reads an item written before the places taken were kept as one that took none" $
    decodeContents "{\"token\":\"sk-1\"}   " `shouldBe` Right Contents {secrets = Map.fromList [(SecretName "token", "sk-1")], taken = mempty}

  it "keeps the places taken under a key no secret's name can be" $
    secretNameOk (SecretName takenKey) `shouldBe` False

folding :: Spec
folding = describe "folding what a browser kept into the vault" $
  prop "keeps every name of both, and the vault's value where both have one" $
    forAll genSecrets $ \vault -> forAll genSecrets $ \device ->
      let folded = foldIn vault device
       in coverage . cover 20 (not (Map.null (Map.intersection vault device))) "a name in both" $
            Map.keysSet folded === Map.keysSet vault <> Map.keysSet device
              .&&. conjoin [Map.lookup n folded === Just v | (n, v) <- Map.toList vault]
              .&&. conjoin [Map.lookup n folded === Just v | (n, v) <- Map.toList device, Map.notMember n vault]

-- | Whether the server takes a name for an item: one to 32 lower-case
-- letters, digits and hyphens ("Wiki.Server.Vault"'s @itemNameOk@).
serverTakes :: ItemName -> Bool
serverTakes n = not (T.null n.text) && T.length n.text <= 32 && T.all (\c -> isAsciiLower c || isDigit c || c == '-') n.text

-- | A plugin's id as the server takes one: a lower-case letter, then up to
-- 63 lower-case letters, digits and hyphens; short ones more often.
genPluginId :: Gen Text
genPluginId = do
  first' <- elements ['a' .. 'z']
  n <- frequency [(4, choose (0, 20)), (1, choose (21, 63))]
  rest <- vectorOf n (elements (['a' .. 'z'] <> ['0' .. '9'] <> ['-']))
  pure (T.pack (first' : rest))

-- | A few secrets, under a few names, so that two maps often share one,
-- with values of any letters.
genSecrets :: Gen (Map SecretName Text)
genSecrets = do
  n <- choose (0, 6)
  Map.fromList <$> vectorOf n ((,) <$> (SecretName <$> elements ["anthropic", "openai", "scripted", "token", "key-2"]) <*> genValue)
  where
    genValue = T.pack <$> listOf (frequency [(6, elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_ \"\\")), (2, elements "éłżß€漢字🙂"), (1, arbitraryUnicodeChar)])

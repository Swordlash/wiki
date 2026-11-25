-- | The writing assistant's settings cut into options, secrets and what
-- was learnt, and put together again ("Wiki.Core.Assistant.Split").
module Test.AssistantSplit (spec, genSettings) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wiki.Core.Assistant
import Wiki.Core.Assistant.Split
import Wiki.Core.Url (Url (..))
import Wiki.Core.Vault.Plugin (SecretName (..))

spec :: Spec
spec = describe "Wiki.Core.Assistant.Split" $ do
  prop "puts the settings together again from their options, their keys and what was learnt" $
    forAll genSettings $ \s ->
      fromParts (Just (optionsValue s)) (keysOf s) (Just (learntValue s)) === s

  prop "keeps no key and nothing learnt in the options" $
    forAll genSettings $ \s ->
      let shown = show (optionsValue s) :: Text
       in conjoin
            [ counterexample (toS k) (not (k `T.isInfixOf` shown))
            | k <- Map.elems (keysOf s)
            ]
            .&&. not ("models" `T.isInfixOf` shown)
            .&&. not ("unreachable" `T.isInfixOf` shown)

  -- The settings' field keeps the key its check made from the text typed
  -- ('readyWith'), and a key stored by an earlier build, or moved from a
  -- legacy record, is the text as typed: either way, the key read is the
  -- key the check tried, so the requests send the credential that was
  -- checked, or the check refused it. U+2003 around a key passes the
  -- check, and in a request's Authorization header it throws.
  prop "reads a provider's key as the key its check tried, whatever text was stored" $
    forAll genSettings $ \s -> forAll (elements [minBound .. maxBound]) $ \p -> forAll genTyped $ \typed ->
      let checked = (\r -> r.key) <$> readyWith typed p s
          readFrom stored = (settingsOf p (fromParts (Just (optionsValue s)) (Map.singleton (secretOf p) stored) Nothing)).key
       in counterexample (show typed) $
            (readFrom typed === checked)
              .&&. (readFrom (maybe typed (\k -> k.text) checked) === checked)

  it "names each provider's key by the provider" $
    assistantSecrets `shouldBe` [SecretName "anthropic", SecretName "openai", SecretName "scripted"]

  it "takes nothing but options from the options, and the defaults from options it cannot read" $ do
    let sneaky = object ["provider" .= ("openai" :: Text), "openai" .= object ["key" .= ("sk-from-options" :: Text), "model" .= ("gpt-x" :: Text), "unreachable" .= True]]
        s = fromParts (Just sneaky) mempty Nothing
    s.provider `shouldBe` OpenAI
    (settingsOf OpenAI s).key `shouldBe` Nothing
    (settingsOf OpenAI s).unreachable `shouldBe` False
    (settingsOf OpenAI s).model `shouldBe` Just (ModelName "gpt-x")
    fromParts (Just (String "not options")) mempty Nothing `shouldBe` defaultSettings
    fromParts Nothing mempty Nothing `shouldBe` defaultSettings

  it "says when options are the defaults, which need not be kept" $ do
    blankOptions (optionsValue defaultSettings) `shouldBe` True
    blankOptions (optionsValue (withKey Anthropic (Just "sk-ant") defaultSettings)) `shouldBe` True
    blankOptions (optionsValue (withMode Ask defaultSettings)) `shouldBe` False
    case optionsValue (withKey Anthropic (Just "sk-ant") defaultSettings) of
      Object o -> KeyMap.member "anthropic" o `shouldBe` False
      other -> expectationFailure (show other)

-- | A text pasted into a key's field: a key, or nothing, with white space
-- around it that a copy adds, the wide spaces of Unicode included.
genTyped :: Gen Text
genTyped = do
  let space = listOf (elements [" ", "\t", "\n", "\r\n", "\x2003", "\xa0"])
  lead <- T.concat <$> space
  key <- oneof [pure "", ("sk-" <>) . T.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))]
  trail <- T.concat <$> space
  pure (lead <> key <> trail)

-- | Settings as the settings page and the editor make them: keys trimmed
-- and not empty, efforts the provider offers, whole seconds.
genSettings :: Gen Settings
genSettings = do
  provider <- elements [minBound .. maxBound]
  chatMode <- elements [Ask, Agent]
  pageCap <- oneof [pure Nothing, Just <$> chooseInt (minPageCap, maxPageCap)]
  ps <- sublistOf [minBound .. maxBound]
  byProvider <- Map.fromList <$> traverse (\p -> (p,) <$> genProvider p) ps
  pure Settings {provider, byProvider, chatMode, pageCap}
  where
    genProvider p =
      ProviderSettings
        <$> oneof [pure Nothing, Just . ApiKey <$> word "sk-"]
        <*> oneof [pure Nothing, Just . ModelName <$> word "model-"]
        <*> oneof [pure Nothing, Just <$> elements (efforts p)]
        <*> oneof [pure Nothing, Just . Url <$> word "https://gateway.example/"]
        <*> oneof [pure Nothing, Just <$> genList]
        <*> arbitrary
    word prefix = (prefix <>) . T.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))
    genList = do
      listed <- listOf (Listed <$> (ModelName <$> word "m-") <*> oneof [pure Nothing, Just <$> word "Model "] <*> oneof [pure Nothing, Just <$> choose (0, 2000000000)])
      day <- choose (0, 3000)
      secs <- choose (0, 86399 :: Integer)
      pure ModelList {listed, fetchedAt = UTCTime (fromGregorian 2026 1 1 `addDays'` day) (secondsToDiffTime secs)}
    addDays' d n = toEnum (fromEnum d + n)

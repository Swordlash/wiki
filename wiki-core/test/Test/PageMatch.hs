module Test.PageMatch (spec) where

import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, chooseInt, elements, forAll, listOf, shuffle, vectorOf, (===))
import Wiki.Core.Id
import Wiki.Core.PageMatch
import Wiki.Core.Slug (Slug, mkSlug, slugText)

slug :: Text -> Slug
slug = either (panic . show) identity . mkSlug

-- | A page found in a space at a path, by its title.
hit :: Text -> Text -> [Text] -> TitleHit
hit spaceSlug title pagePath =
  TitleHit
    { page = PageId (T.intercalate "/" (spaceSlug : pagePath) <> ":" <> title)
    , space = SpaceId spaceSlug
    , spacePath = [slug spaceSlug]
    , pagePath = map slug pagePath
    , title
    }

-- | The titles found for a query, as written.
titlesFor :: TitleQuery -> [TitleHit] -> [Text]
titlesFor q = map (\h -> h.title) . rankTitles q

-- | Pages with titles and paths that share starts, words and letters,
-- some equal but for case or accents, in a few spaces.
genHits :: Gen [TitleHit]
genHits = do
  n <- chooseInt (0, 30)
  vectorOf n $ do
    space <- elements ["docs", "team", "team-ops", "blog"]
    title <- elements ["Onboarding", "onboarding", "ONBOARDING", "Önboarding", "On-call", "On call rota", "Deploys", "Déploiement", "Release notes", "Notes", "A note", "Zebra"]
    path <- listOf (elements ["guides", "on-call", "notes", "deploys", "zebra", "x"])
    final <- elements ["one", "two", "three", "onboarding", "rota"]
    pure (hit space title (take 3 path <> [final]))

genQuery :: Gen TitleQuery
genQuery = do
  letters <- elements ["on", "ON", "onb", "ön", "on c", "call", "de", "dé", "not", "es", "ze", "z", " no ", "o", "", "guides/on", "rota"]
  limit <- chooseInt (-2, 30)
  pure TitleQuery {letters, except = Nothing, limit}

spec :: Spec
spec = describe "Wiki.Core.PageMatch" $ do
  describe "matchRank" $ do
    it "ranks a title's start first, then a later word or a path's segment, then anything inside" $ do
      let rank t p = matchRank (typedLetters t) (matchable "Duty rotation" (map slug p))
      rank "duty" [] `shouldBe` Just 1
      rank "  DUTY " [] `shouldBe` Just 1
      rank "rot" [] `shouldBe` Just 2
      rank "on c" ["on-call"] `shouldBe` Just 2
      rank "tat" [] `shouldBe` Just 3
      rank "ster" ["ops", "rosters"] `shouldBe` Just 3
      rank "xyz" [] `shouldBe` Nothing
      rank "" [] `shouldBe` Just 1

    it "compares in lower case, and does not fold accents" $ do
      matchRank (typedLetters "ÖN") (matchable "önboarding" []) `shouldBe` Just 1
      matchRank (typedLetters "on") (matchable "Önboarding" []) `shouldBe` Nothing
      matchRank (typedLetters "on") (matchable "Önboarding" [slug "onboarding"]) `shouldBe` Just 2

  describe "rankTitles" $ do
    it "orders by rank, then by title in lower case, then as written, then by the space's path" $
      titlesFor
        (titleQuery "on")
        [hit "team" "On-call" ["on-call"], hit "docs" "Release notes" ["release-notes"], hit "docs" "onboarding" ["b"], hit "docs" "Onboarding" ["a"], hit "blog" "A notion" ["a-notion"]]
        `shouldBe` ["On-call", "Onboarding", "onboarding", "A notion"]

    it "orders equal titles by their spaces' paths, then their own" $
      map (\h -> map slugText h.spacePath <> map slugText h.pagePath) (rankTitles (titleQuery "glossary") [hit "team" "Glossary" ["glossary"], hit "docs" "Glossary" ["z", "glossary"], hit "docs" "Glossary" ["glossary"]])
        `shouldBe` [["docs", "glossary"], ["docs", "z", "glossary"], ["team", "glossary"]]

    it "finds nothing for fewer than two letters once trimmed, and refuses more than the most" $ do
      titlesFor (titleQuery " o ") [hit "docs" "Onboarding" ["onboarding"]] `shouldBe` []
      titlesFor (titleQuery "on") [hit "docs" "Onboarding" ["onboarding"]] `shouldBe` ["Onboarding"]
      titleRefusal (titleQuery (T.replicate maxLetters "a")) `shouldBe` Nothing
      titleRefusal (titleQuery (T.replicate (maxLetters + 1) "a")) `shouldSatisfy` isJust

    prop "gives the same pages in the same order whatever order the candidates come in" $
      forAll genQuery $ \q -> forAll genHits $ \hs -> forAll (shuffle hs) $ \shuffled ->
        rankTitles q shuffled === rankTitles q hs

    prop "gives every candidate that matches, best first, cut at the limit and at the most" $
      forAll genQuery $ \q -> forAll genHits $ \hs -> do
        let found = rankTitles q hs
            typed = typedLetters q.letters
            matching = [h | h <- hs, isJust (matchRank typed (matchable h.title h.pagePath))]
            key h = (matchRank typed (matchable h.title h.pagePath), T.toLower h.title, h.title, map slugText h.spacePath, map slugText h.pagePath, h.page.text)
        if T.length (T.strip (T.toLower q.letters)) < minLetters || q.limit <= 0
          then found === []
          else found === take (min maxTitleHits q.limit) (sortOn key matching)

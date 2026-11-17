-- | The editor's preview's parse of its draft ("Wiki.App.View.Preview"):
-- what a move of the cursor to another slide of a long presentation
-- costs, counted in the bytes it allocates rather than timed, so that a
-- slow or a busy machine changes nothing (decision 81 of docs/PLAN.md).
--
-- Each move of the cursor to another slide changes only which card the
-- preview shows. The preview parsed the whole deck again for it, and on a
-- deck of sixty slides of a hundred lines each parse held the page about a
-- second in the browser (c6f0ae82). The browser example that held that by
-- a bound in milliseconds failed on a slow machine; the parse is pure, so
-- its cost is held here instead.
module Test.Preview (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import System.Mem (getAllocationCounter, setAllocationCounter)
import Test.Hspec
import Wiki.App.View.Preview (Parsed (..), parse, reparsed)
import Wiki.Core.Slides (Deck (..))

spec :: Spec
spec = describe "Wiki.App.View.Preview's parse of its draft" $ do
  it "keeps its parse of a long deck when only another slide is shown, and parses a new text once" $ do
    let kept = parse longDeck
        typed = longDeck <> "\nOne more line."
    -- The deck parsed, and both texts made, before anything is counted.
    _ <- evaluate (forced kept + T.length typed)
    again <- allocated (forced (reparsed longDeck kept))
    fresh <- allocated (forced (reparsed typed kept))
    -- The same text: the parse kept, which costs next to nothing (a
    -- comparison of the two texts). A new text: one parse, which is what a
    -- move to another slide cost before, and costs over 100 MB natively.
    again `shouldSatisfy` (< 1_000_000)
    fresh `shouldSatisfy` (> 100 * again)
    (forced (reparsed typed kept) > 0) `shouldBe` True

-- | The bytes allocated in forcing a value: what its work costs, counted
-- rather than timed.
allocated :: Int -> IO Int64
allocated value = do
  setAllocationCounter 0
  _ <- evaluate value
  negate <$> getAllocationCounter

-- | A number that is known only once the deck is cut into slides and the
-- logins it mentions are found: the whole parse, forced.
forced :: Parsed -> Int
forced p = length p.deck.slides + Set.size p.mentioned + T.length p.text

-- | A presentation of sixty slides of a hundred lines, about 300,000
-- characters: paragraphs, a list, a code block and a formula on each, as
-- the browser example's ("Test.Browser.SlideEditor").
longDeck :: Text
longDeck = T.intercalate "\n" (["# Long", ""] <> concatMap slide [1 .. 60 :: Int])
  where
    slide n =
      (<> [""]) . take 99 $
        ["## Slide " <> show n, ""]
          <> para n 0 12
          <> para n 12 12
          <> map ("- " <>) (sentences n 24 10)
          <> ["", "```haskell"]
          <> ["step" <> show i <> " x = x * " <> show n <> " + " <> show i | i <- [0 .. 9 :: Int]]
          <> ["```", ""]
          <> ["$$\\int_0^{" <> show n <> "} x^2 \\, dx = \\frac{" <> show n <> "^3}{3}$$", ""]
          <> concat [para n k 12 | k <- [34, 46 .. 94]]
    para n start count = sentences n start count <> [""]
    sentences n start count = ["Line " <> show k <> " of slide " <> show n <> ": the quick brown fox jumps over the lazy dog." | k <- [start .. start + count - 1 :: Int]]

-- | What "Wiki.App.View.Latest" adds to haskell-halogen's "Halogen.Latest":
-- 'superseded'. The tickets themselves ('takes', 'supersede', 'asking',
-- 'answered', 'madeAfter', 'stillAnswerable') are the library's, and its
-- own suite checks them over every order of up to five asks. 'takesPage' is
-- checked in "Test.Ticking".
module Test.Latest (spec) where

import Protolude
import Test.Hspec
import Wiki.App.View.Latest

spec :: Spec
spec = describe "Wiki.App.View.Latest" $ do
  it "superseded makes its change and drops the answer to every ask out, in one step" $ do
    let (first', a1) = newAsk noAsks
        (second', a2) = newAsk a1
        (cut, shown) = superseded fst (\a (_, s) -> (a, s)) (second (const "newer")) (a2, "older" :: Text)
    shown `shouldBe` "newer"
    takes first' cut `shouldBe` Nothing
    takes second' cut `shouldBe` Nothing
    let (third, a3) = newAsk cut
    isJust (takes third a3) `shouldBe` True

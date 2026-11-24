-- | The registry of plugins ("Wiki.App.Plugin.Registry"): what the
-- plugins that are on contribute; and the errors of formulas and diagrams
-- ("Wiki.App.Plugin.Errors"), whose diagnostics are the problems the
-- editor marked for the islands' failures before the plugins
-- ('Wiki.Core.Markdown.Problems.allProblems').
module Test.Plugins (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.QuickCheck
import Wiki.App.Plugin (Diagnostics (..), Drawn (..), Manifest (..), Plugin (..), PluginId (..), builtin)
import Wiki.App.Plugin.Errors (errors, islandFailures)
import Wiki.App.Plugin.Registry (Contributed (..), builtinIds, contributed, plugins)
import Wiki.Core.Markdown.Problems (Checked (..), IslandPlace (..), Problem (..), ProblemKind (..), Severity (..), allProblems, checkText)
import Wiki.Core.Markdown.Render (IslandKey (..))
import Wiki.Core.PageKind (PageKind (..))

-- | A plugin of the tests, whose diagnostics are one warning naming it.
saying :: Text -> Plugin
saying name =
  Plugin
    { manifest = builtin name name ""
    , diagnostics = Just (Diagnostics (const [Problem {from = 1, to = 1, kind = Marker, severity = Warning, message = name}]))
    }

-- | The messages of what some plugins diagnose.
heard :: Set PluginId -> [Plugin] -> [Text]
heard chosenIds given = [p.message | d <- (contributed chosenIds given).diagnostics, p <- d.diagnose (Drawn "" mempty mempty)]

-- | What a text's islands look like to the editor: the text checked, and
-- the failures given.
drawnOf :: Text -> Map IslandKey Text -> (Checked, Drawn)
drawnOf text failures =
  let checked = checkText Document text
   in (checked, Drawn {text, islands = checked.islands, failures})

-- | A page of formulas, diagrams, pictures and the parser's own problems,
-- in any order.
page :: Gen Text
page = T.intercalate "\n\n" <$> listOf (elements blocks)
  where
    blocks =
      [ "# Title"
      , "Text with $x^2$ and $y_$ inline."
      , "$$\n\\frac{1}{2\n$$"
      , "```mermaid\ngraph TD\nA-->B\n```"
      , "```mermaid\nnot a diagram\n```"
      , "```tikz\n\\draw (0,0) -- (1,1);\n```"
      , "[[!spoiler]]{a}(A)"
      , "> [!TIP]Title"
      , "- $a$ and $a$ again"
      , "Plain text."
      ]

-- | Failures of some of a text's islands, and of keys the text does not
-- have (an island the preview still shows from an older text).
failuresOf :: Checked -> Gen (Map IslandKey Text)
failuresOf checked = do
  kept <- sublistOf (Map.keys checked.islands)
  stale <- listOf (IslandKey <$> elements ["gone", "$z$"] <*> choose (0, 3))
  Map.fromList <$> traverse (\k -> (k,) <$> elements ["KaTeX parse error", "Parse error on line 1"]) (kept <> stale)

spec :: Spec
spec = describe "plugins" $ do
  describe "contributed" $ do
    it "gives every built-in plugin's contribution when every plugin is on" $
      length (contributed builtinIds plugins).diagnostics `shouldBe` 1
    it "gives nothing of a plugin that is off" $ do
      length (contributed (Set.delete (PluginId "errors") builtinIds) plugins).diagnostics `shouldBe` 0
      length (contributed Set.empty plugins).diagnostics `shouldBe` 0
    it "ignores an id that no plugin has" $
      length (contributed (Set.fromList [PluginId "nothing"]) plugins).diagnostics `shouldBe` 0
    it "keeps the plugins' order, whatever the set's" $ do
      let given = [saying "b", saying "a", saying "c"]
      heard (Set.fromList [PluginId "a", PluginId "b", PluginId "c"]) given `shouldBe` ["b", "a", "c"]
      heard (Set.fromList [PluginId "c", PluginId "b"]) given `shouldBe` ["b", "c"]

  describe "the errors of formulas and diagrams" $ do
    it "are the islands' problems the editor marked before the plugins" $
      property $
        forAll page $ \text ->
          let checked = checkText Document text
           in forAll (failuresOf checked) $ \failed ->
                let (_, drawn) = drawnOf text failed
                 in sort (checked.problems <> islandFailures drawn) === allProblems checked failed
    it "mark a formula a renderer reported failed, on its lines, until it is reported drawn" $ do
      let text = "# T\n\nFirst.\n\nText $x^$ here.\n"
          checked = checkText Document text
          formula = [k | (k, place) <- Map.toList checked.islands, place.kind == Formula]
      length formula `shouldBe` 1
      let (_, failed) = drawnOf text (Map.fromList [(k, "KaTeX parse error") | k <- formula])
      [(p.from, p.to, p.kind, p.severity, p.message) | d <- (contributed builtinIds plugins).diagnostics, p <- d.diagnose failed]
        `shouldBe` [(5, 5, Formula, Error, "KaTeX parse error")]
      let (_, cleared) = drawnOf text mempty
      [p | d <- (contributed builtinIds plugins).diagnostics, p <- d.diagnose cleared] `shouldBe` []
    it "are none for a failure whose island the text checked does not have" $
      islandFailures (snd (drawnOf "Plain.\n" (Map.fromList [(IslandKey "$x$" 0, "KaTeX parse error")]))) `shouldBe` []
    it "are the plugin called errors" $
      errors.manifest.id `shouldBe` PluginId "errors"

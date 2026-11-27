-- | The registry of plugins ("Wiki.App.Plugin.Registry"): what the
-- plugins that are on contribute; the errors of formulas and diagrams
-- ("Wiki.App.Plugin.Errors"), whose diagnostics are the problems the
-- editor marked for the islands' failures before the plugins
-- ('Wiki.Core.Markdown.Problems.allProblems'); and the writing
-- assistant's buttons ("Wiki.App.Plugin.Assistant.Agent"), as the editor
-- drew them before the plugins.
module Test.Plugins (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (runMemDOM)
import Protolude
import Test.Hspec
import Test.QuickCheck
import Wiki.Api.Plugins (PluginChoice (..), PluginState (..), Plugins (..), noPlugins)
import Wiki.App.Plugin (AgentButton (..), Child (..), Diagnostics (..), Drawn (..), Face (..), KindView (..), Manifest (..), Plugin (..), PluginId (..), builtin, linkTo)
import Wiki.App.Plugin.Assistant.Agent (buttonsOf)
import Wiki.App.Plugin.Calendar (dayName, days)
import Wiki.App.Plugin.Errors (errors, islandFailures)
import Wiki.App.Plugin.Registry (Contributed (..), builtinIds, contributed, isOn, manifests, onAmong, optionsOf, plugins)
import Wiki.App.Route (Route (..), route)
import Wiki.Core.Assistant (Provider (..), Settings, chooseProvider, defaultSettings, serviceName, settingsOf, withBaseUrl, withKey, withReached)
import Wiki.Core.Id (PageId (..), SpaceId (..))
import Wiki.Core.Markdown.Problems (Checked (..), IslandPlace (..), Problem (..), ProblemKind (..), Severity (..), allProblems, checkText)
import Wiki.Core.Markdown.Render (IslandKey (..))
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (PageKind (..), calendarKind, knownKinds)
import Wiki.Core.Slug (mkSlug)

-- | A plugin of the tests, whose diagnostics are one warning naming it.
saying :: Text -> Plugin IO
saying name =
  Plugin
    { manifest = builtin name name ""
    , diagnostics = Just (Diagnostics (const [Problem {from = 1, to = 1, kind = Marker, severity = Warning, message = name}]))
    , agent = Nothing
    , pageKind = Nothing
    , options = Nothing
    }

-- | The messages of what some plugins diagnose.
heard :: Set PluginId -> [Plugin IO] -> [Text]
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

-- | The built-in plugins, in the monad of the tests.
builtins :: [Plugin IO]
builtins = plugins

spec :: Spec
spec = describe "plugins" $ do
  describe "which plugins are on (isOn, pluginsOn)" $ do
    -- Every combination of the three things that decide: the wiki's off
    -- set, the user's state (none kept, or one of the three), and the
    -- plugin's default. The expected column is written out, not computed.
    let table :: [(Bool, Maybe PluginState, Bool, Bool)]
        table =
          [ -- off for the wiki, the user's state, on by default, on
            (True, Nothing, True, False)
          , (True, Nothing, False, False)
          , (True, Just PluginDefault, True, False)
          , (True, Just PluginDefault, False, False)
          , (True, Just PluginOn, True, False)
          , (True, Just PluginOn, False, False)
          , (True, Just PluginOff, True, False)
          , (True, Just PluginOff, False, False)
          , (False, Nothing, True, True)
          , (False, Nothing, False, False)
          , (False, Just PluginDefault, True, True)
          , (False, Just PluginDefault, False, False)
          , (False, Just PluginOn, True, True)
          , (False, Just PluginOn, False, True)
          , (False, Just PluginOff, True, False)
          , (False, Just PluginOff, False, False)
          ]
        manifestOf byDefault = (builtin "p" "P" "") {onByDefault = byDefault}
        known wikiOff s =
          Plugins
            { offForWiki = Set.fromList [PluginId "p" | wikiOff] <> Set.fromList [PluginId "other"]
            , mine = Map.fromList ([(PluginId "p", PluginChoice {state = x, options = Nothing}) | Just x <- [s]] <> [(PluginId "other", PluginChoice {state = PluginOn, options = Nothing})])
            }
    it "follows its table over every combination" $ do
      length table `shouldBe` 2 * 4 * 2
      for_ table $ \(wikiOff, s, byDefault, expected) ->
        (wikiOff, s, byDefault, isOn (known wikiOff s) (manifestOf byDefault)) `shouldBe` (wikiOff, s, byDefault, expected)
    it "has every built-in plugin on by default, and on with nothing read" $ do
      map (\m -> m.onByDefault) manifests `shouldBe` [True, True, True]
      onAmong manifests noPlugins `shouldBe` builtinIds
    it "turns each built-in plugin off for the wiki, or for the user alone" $ do
      onAmong manifests noPlugins {offForWiki = Set.fromList [PluginId "errors"]} `shouldBe` Set.fromList [PluginId "assistant", PluginId "calendar"]
      onAmong manifests noPlugins {mine = Map.fromList [(PluginId "assistant", PluginChoice {state = PluginOff, options = Nothing})]} `shouldBe` Set.fromList [PluginId "errors", PluginId "calendar"]

  describe "optionsOf"
    $ it "gives a plugin's options whether the plugin is on or off"
    $
    -- 'contributed' takes the set of plugins on; the options take none.
    map fst (optionsOf builtins) `shouldBe` [PluginId "assistant"]

  describe "contributed" $ do
    it "gives every built-in plugin's contribution when every plugin is on" $ do
      let given = contributed builtinIds builtins
      length given.diagnostics `shouldBe` 1
      (\(m, _) -> m.id) <$> given.agent `shouldBe` Just (PluginId "assistant")
      map fst (optionsOf builtins) `shouldBe` [PluginId "assistant"]
    it "gives nothing of a plugin that is off" $ do
      length (contributed (Set.delete (PluginId "errors") builtinIds) builtins).diagnostics `shouldBe` 0
      length (contributed (Set.delete (PluginId "assistant") builtinIds) builtins).diagnostics `shouldBe` 1
      isJust (contributed (Set.delete (PluginId "assistant") builtinIds) builtins).agent `shouldBe` False
      isJust (contributed (Set.delete (PluginId "errors") builtinIds) builtins).agent `shouldBe` True
      length (contributed Set.empty builtins).diagnostics `shouldBe` 0
    it "gives the calendar's kind of page while the calendar is on, and not while it is off" $ do
      let kindsOf chosen = [(m.id, k.kind, k.label) | (m, k) <- (contributed chosen builtins).pageKinds]
      kindsOf builtinIds `shouldBe` [(PluginId "calendar", calendarKind, "Calendar")]
      kindsOf (Set.delete (PluginId "calendar") builtinIds) `shouldBe` []
      kindsOf (Set.fromList [PluginId "calendar"]) `shouldBe` [(PluginId "calendar", calendarKind, "Calendar")]
      length (contributed (Set.delete (PluginId "calendar") builtinIds) builtins).diagnostics `shouldBe` 1
    it "brings only kinds the backends take" $
      [k.kind | p <- builtins, Just k <- [p.pageKind], k.kind `notElem` knownKinds] `shouldBe` []
    it "ignores an id that no plugin has" $
      length (contributed (Set.fromList [PluginId "nothing"]) builtins).diagnostics `shouldBe` 0
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
      [(p.from, p.to, p.kind, p.severity, p.message) | d <- (contributed builtinIds builtins).diagnostics, p <- d.diagnose failed]
        `shouldBe` [(5, 5, Formula, Error, "KaTeX parse error")]
      let (_, cleared) = drawnOf text mempty
      [p | d <- (contributed builtinIds builtins).diagnostics, p <- d.diagnose cleared] `shouldBe` []
    it "are none for a failure whose island the text checked does not have" $
      islandFailures (snd (drawnOf "Plain.\n" (Map.fromList [(IslandKey "$x$" 0, "KaTeX parse error")]))) `shouldBe` []
    it "are the plugin called errors" $
      errors.manifest.id `shouldBe` PluginId "errors"

  describe "the calendar's days" $ do
    let summary title template position =
          PageSummary
            { id = PageId ("p" <> show (position :: Int))
            , space = SpaceId "s"
            , parent = Just (PageId "calendar")
            , title
            , slug = either (panic . show) identity (mkSlug ("p" <> show position))
            , createdAt = UTCTime (fromGregorian 2026 11 1) 0
            , updatedAt = UTCTime (fromGregorian 2026 11 1) 0
            , position
            , template
            , kind = Document
            }
        -- The pages under a calendar, by title and whether each is a
        -- template, in the space's order, each linking home.
        under titles = do
          router <- runMemDOM (Router.newRouter "/" Unknown route)
          pure [Child {page = summary title template position, link = linkTo router Home} | (position, (title, template)) <- zip [0 ..] titles]
        listed titles = (\cs -> [(d, c.page.title) | (d, c) <- days cs]) <$> under titles
    it "are the pages titled with a date, newest first" $
      listed [("2026-11-02", False), ("2026-11-27", False), (" 2025-12-31 ", False)]
        `shouldReturn` [(fromGregorian 2026 11 27, "2026-11-27"), (fromGregorian 2026 11 2, "2026-11-02"), (fromGregorian 2025 12 31, " 2025-12-31 ")]
    it "leave out a page whose title is no date, and a template" $
      listed [("Notes", False), ("2026-11-27 planning", False), ("2026-02-30", False), ("2026-11-26", True), ("2026-11-25", False)]
        `shouldReturn` [(fromGregorian 2026 11 25, "2026-11-25")]
    it "keep the space's order for two pages of one date" $ do
      cs <- under [("2026-11-27", False), ("2026-11-27", False)]
      [c.page.position | (_, c) <- days cs] `shouldBe` [0, 1]
    it "name a day in words" $
      dayName (fromGregorian 2026 11 27) `shouldBe` "Friday, 27 November 2026"

  describe "the writing assistant's buttons" $ do
    let neutral = AgentButton {key = "assistant", label = "Assistant", title = "Assistant", ariaLabel = "Assistant", selected = True, unreachable = False, face = ChatGlyph}
        claude = withKey Anthropic (Just "a-key") defaultSettings
        shown :: Settings -> [(Text, Text, Text, Bool, Bool)]
        shown s = [(b.key, b.title, b.ariaLabel, b.selected, b.unreachable) | b <- buttonsOf False (Just (Right s))]
    it "are none before the settings are read" $
      buttonsOf False Nothing `shouldBe` []
    it "are one button, \"Assistant\", without settings or without a key" $ do
      buttonsOf False (Just (Left "Sign in to use the assistant.")) `shouldBe` [neutral]
      buttonsOf False (Just (Right defaultSettings)) `shouldBe` [neutral]
    it "are one per provider with a key, the provider in use selected" $ do
      shown claude `shouldBe` [("anthropic", "Chat with Claude", "Chat with Claude", True, False)]
      let both = withKey OpenAI (Just "o-key") claude
      shown both `shouldBe` [("anthropic", "Chat with Claude", "Chat with Claude", True, False), ("openai", "Chat with ChatGPT", "Chat with ChatGPT", False, False)]
      shown (chooseProvider OpenAI both) `shouldBe` [("anthropic", "Chat with Claude", "Chat with Claude", False, False), ("openai", "Chat with ChatGPT", "Chat with ChatGPT", True, False)]
    it "say why a provider that could not be reached is dimmed" $ do
      let blocked = withReached Anthropic False claude
          cannot = "Cannot reach " <> serviceName Anthropic (settingsOf Anthropic blocked) <> " from this network"
      shown blocked `shouldBe` [("anthropic", cannot, "Chat with Claude (" <> cannot <> ")", True, True)]
    it "show each provider's face" $ do
      [b.face | b <- buttonsOf False (Just (Right claude))] `shouldSatisfy` \case
        [Logo _ (Just _)] -> True
        _ -> False
      [b.face | b <- buttonsOf False (Just (Right (withKey OpenAI (Just "o-key") defaultSettings)))] `shouldSatisfy` \case
        [Logo _ Nothing] -> True
        _ -> False
      let gateway = withBaseUrl OpenAI "http://localhost:1234/v1" defaultSettings
      [(b.key, b.label, b.face) | b <- buttonsOf False (Just (Right gateway))] `shouldBe` [("openai", "Chat through " <> serviceName OpenAI (settingsOf OpenAI gateway), ChatGlyph)]
      [(b.key, b.label, b.face) | b <- buttonsOf True (Just (Right defaultSettings))] `shouldBe` [("scripted", "Chat with the scripted provider", Letter "S")]

-- | External plugins on the application's side ("Wiki.App.Plugin.External"):
-- which installed plugins run, the protocol's messages and the problems a
-- plugin's answer stands for, and the editor's machine of frames
-- ("Wiki.App.Plugin.External.Machine"), whose rules are a property over
-- generated orders of inputs (the plugins installed among them, as the
-- shell's plugins' state reads and changes them) and a model plugin that
-- answers in time, late, out of order, never, or with junk.
module Test.PluginsExternal (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Protolude
import Test.Hspec
import Test.QuickCheck
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), noPlugins)
import Wiki.Api.Plugins.External (Contribution (..), ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy (..), Permission (..), framePath, noExternalPlugins, reservedPluginIds)
import Wiki.App.Backend.Http.Cache (emptyCache)
import Wiki.App.Backend.Http.Cache qualified as Cache
import Wiki.App.Backend.Http.Preload (decodePreload, encodePreload)
import Wiki.App.Plugin (Drawn (..))
import Wiki.App.Plugin qualified as Plugin
import Wiki.App.Plugin.External (Heard (..), diagnosticsOf, runnable)
import Wiki.App.Plugin.External.Frame (FrameSaid (..), saidOf)
import Wiki.App.Plugin.External.Machine (Effect (..), Event (..), Frame (..), Input (..), Model (..), Out (..), Phase (..), Stopped (..), initial, step, wanted)
import Wiki.App.Plugin.External.Protocol (FromPlugin (..), Reported (..), fromPlugin, maxFrameMessage, maxMessageLength, maxProblems, problemsIn, utf16Length)
import Wiki.App.Plugin.Registry (builtinIds, externalOn)
import Wiki.Core.Markdown.Problems (Problem (..), ProblemKind (..), Severity (..))

spec :: Spec
spec = describe "Wiki.App.Plugin.External" $ do
  registry
  protocol
  machine

--------------------------------------------------------------------------------
-- Plugins

installedAt :: UTCTime
installedAt = UTCTime (fromGregorian 2026 11 25) 0

-- | An installed plugin of the tests: its id and name, its bundle's hash,
-- and whether it is the reader's own.
plugin :: Text -> Text -> Bool -> ExternalPlugin
plugin pid bundle mine =
  ExternalPlugin
    { manifest =
        ExternalManifest
          { id = PluginId pid
          , name = T.toTitle pid
          , version = "1.0.0"
          , description = ""
          , api = 1
          , contributes = Set.singleton Diagnostics
          , permissions = Set.singleton DocumentRead
          , hosts = []
          }
    , bundle
    , size = 10
    , mine
    , installedAt
    }

choosing :: [(Text, PluginState)] -> Plugins
choosing cs = Plugins {offForWiki = mempty, mine = Map.fromList [(PluginId p, PluginChoice {state = s, options = Nothing}) | (p, s) <- cs]}

ids :: [ExternalPlugin] -> [Text]
ids = map (\p -> p.manifest.id.text)

registry :: Spec
registry = describe "which installed plugins run" $ do
  it "has the server reserve the id of every plugin of the registry" $
    builtinIds `Set.difference` reservedPluginIds `shouldBe` Set.empty

  it "runs the wiki's while the policy is not off, a user's own only while users may add theirs, and the wiki's in the place of a user's of the same id" $ do
    let wiki = plugin "spell" "a1" False
        own = plugin "mine" "b1" True
        shadowed = plugin "spell" "c1" True
        all' = [wiki, own, shadowed]
    ids (runnable ExternalPlugins {policy = ExternalOff, external = all'}) `shouldBe` []
    ids (runnable ExternalPlugins {policy = ExternalWiki, external = all'}) `shouldBe` ["spell"]
    (map (\p -> p.bundle) . runnable) ExternalPlugins {policy = ExternalUsers, external = all'} `shouldBe` ["a1", "b1"]
    -- One installed under an id a built-in plugin took since runs under
    -- no policy, the wiki's or a user's own.
    let taken = [plugin "calendar" "d1" False, plugin "calendar" "e1" True]
    for_ [ExternalOff, ExternalWiki, ExternalUsers] $ \policy ->
      (map (\p -> p.bundle) . runnable) ExternalPlugins {policy, external = all' <> taken} `shouldBe` map (\p -> p.bundle) (runnable ExternalPlugins {policy, external = all'})

  it "runs no user's own plugin under a reserved id, with no plugin of the wiki's of that id to run in its place" $
    (map (\p -> p.bundle) . runnable) ExternalPlugins {policy = ExternalUsers, external = [plugin "mine" "b1" True, plugin "calendar" "f1" True]} `shouldBe` ["b1"]

  it "is on by default, and off as the user or the administrator turned it off, by the rule of the built-in plugins" $ do
    let installed = ExternalPlugins {policy = ExternalUsers, external = [plugin "spell" "a1" False, plugin "mine" "b1" True]}
    ids (externalOn noPlugins installed) `shouldBe` ["spell", "mine"]
    ids (externalOn (choosing [("spell", PluginOff)]) installed) `shouldBe` ["mine"]
    ids (externalOn (choosing [("spell", PluginDefault), ("mine", PluginOn)]) installed) `shouldBe` ["spell", "mine"]
    ids (externalOn noPlugins {offForWiki = Set.fromList [PluginId "mine"]} installed) `shouldBe` ["spell"]

  it "is carried by the page an editor is prerendered into, and by no other" $ do
    let read' = ExternalPlugins {policy = ExternalWiki, external = [plugin "spell" "a1" False]}
        carrying = emptyCache {Cache.externalPlugins = Just (Right read')}
    ((\c -> c.externalPlugins) <$> decodePreload (encodePreload carrying)) `shouldBe` Right (Just (Right read'))
    encodePreload emptyCache `shouldNotSatisfy` T.isInfixOf "externalPlugins"

  it "counts what a plugin heard for exactly the text checked" $ do
    let problem = Problem {from = 1, to = 1, kind = FromPlugin, severity = Warning, message = "Spell: x"}
        heard = Map.fromList [(PluginId "spell", Heard {text = "x", problems = [problem]})]
        drawnOf t = Drawn {text = t, islands = mempty, failures = mempty}
    concatMap (\d -> d.diagnose (drawnOf "x")) (diagnosticsOf heard) `shouldBe` [problem]
    concatMap (\d -> d.diagnose (drawnOf "x ")) (diagnosticsOf heard) `shouldBe` []
    -- The type is the interface's, as a built-in plugin's diagnostics are.
    length (diagnosticsOf heard :: [Plugin.Diagnostics]) `shouldBe` 1

--------------------------------------------------------------------------------
-- The protocol

-- | A text of lines, letters, x's and characters written with two UTF-16
-- units.
genText :: Gen Text
genText = T.pack <$> listOf (elements "ab x\n\n😀𝔸é")

-- | The line (from 1) of the character at an index, counted independently
-- of the protocol: the line breaks before it.
lineOfChar :: Text -> Int -> Int
lineOfChar text i = 1 + T.count "\n" (T.take i text)

-- | The UTF-16 offset of the character at an index.
offsetOf :: Text -> Int -> Int
offsetOf text i = utf16Length (T.take i text)

protocol :: Spec
protocol = describe "the protocol, version 1" $ do
  it "reads the plugin's messages, and refuses what the protocol does not have" $ do
    fromPlugin (object ["type" .= ("ready" :: Text), "extra" .= True]) `shouldBe` Right Ready
    fromPlugin (object ["type" .= ("failed" :: Text), "message" .= ("boom" :: Text)]) `shouldBe` Right (Failed "boom")
    fromPlugin (object ["type" .= ("problems" :: Text), "re" .= (3 :: Int), "problems" .= [object ["from" .= (0 :: Int), "to" .= (1 :: Int), "severity" .= ("error" :: Text), "message" .= ("m" :: Text)]]])
      `shouldBe` Right (Problems 3 [Reported {from = 0, to = 1, severity = Error, message = "m"}])
    for_
      [ object ["type" .= ("hello" :: Text)]
      , object ["type" .= ("problems" :: Text), "re" .= (1.5 :: Double), "problems" .= ([] :: [Value])]
      , object ["type" .= ("problems" :: Text), "re" .= (1 :: Int), "problems" .= [object ["from" .= (0 :: Int), "to" .= (1 :: Int), "severity" .= ("fatal" :: Text), "message" .= ("m" :: Text)]]]
      , object ["type" .= ("problems" :: Text), "re" .= (1 :: Int)]
      , Aeson.String "ready"
      ]
      $ \v -> fromPlugin v `shouldSatisfy` isLeft

  it "reads what the frame's JavaScript says" $ do
    saidOf "m{\"type\":\"ready\"}" `shouldBe` Message ready
    saidOf "n" `shouldBe` Navigated
    saidOf "x" `shouldBe` Unasked
    saidOf "u" `shouldBe` Unreadable
    saidOf "mnot json" `shouldBe` Unreadable
    saidOf "" `shouldBe` Unreadable
    saidOf "nx" `shouldBe` Unreadable

  it "has the runtime in the frame and the frame's JavaScript keep the program's limits" $ do
    runtime <- decodeUtf8 <$> BS.readFile "../web/plugin/runtime.js"
    host <- decodeUtf8 <$> BS.readFile "jsbits/plugin-frames.js"
    let states source line = (line, line `T.isInfixOf` source) `shouldBe` (line, True)
    states runtime ("const MAX_PROBLEMS = " <> show maxProblems <> ";")
    states runtime ("const MAX_MESSAGE_LENGTH = " <> show maxMessageLength <> ";")
    states runtime ("const MESSAGE_LENGTH = " <> show maxFrameMessage <> ";")
    states host ("const wikiPluginMessageLength = " <> show maxFrameMessage <> ";")

  it "parses a plugin's message only up to the bound, and refuses a longer one before parsing it" $ do
    let padded k = "{\"type\":\"ready\",\"pad\":\"" <> T.replicate k "a" <> "\"}"
        fits = maxFrameMessage - T.length (padded 0)
        isMessage = \case Message _ -> True; _ -> False
    T.length (padded fits) `shouldBe` maxFrameMessage
    saidOf ("m" <> padded fits) `shouldSatisfy` isMessage
    saidOf ("m" <> padded (fits + 1)) `shouldBe` Unreadable
    -- A message one character too long is refused whatever it holds, and
    -- one nested as deep as the bound lets it is parsed.
    saidOf ("m" <> T.replicate (maxFrameMessage + 1) "[") `shouldBe` Unreadable
    let deepest = maxFrameMessage `div` 2
    saidOf ("m" <> T.replicate deepest "[" <> T.replicate deepest "]") `shouldSatisfy` isMessage

  it "puts a problem on the lines of the characters it marks, counting offsets in UTF-16 units" $
    forAll genText $ \text -> forAll (choose (0, T.length text)) $ \i -> forAll (choose (i, T.length text)) $ \j ->
      let reported = Reported {from = offsetOf text i, to = offsetOf text j, severity = Warning, message = "here"}
          -- The last character marked, or the first when none is.
          lastLine = if j > i then lineOfChar text (j - 1) else lineOfChar text i
       in problemsIn "Plug" text [reported]
            === Right [Problem {from = lineOfChar text i, to = lastLine, kind = FromPlugin, severity = Warning, message = "Plug: here"}]

  it "refuses an offset inside a character of two units, past the end, or backwards, and never clamps it" $ do
    let text = "a😀b\nc"
        at f t = problemsIn "P" text [Reported {from = f, to = t, severity = Error, message = "m"}]
    utf16Length text `shouldBe` 6
    at 0 6 `shouldBe` Right [Problem {from = 1, to = 2, kind = FromPlugin, severity = Error, message = "P: m"}]
    at 2 3 `shouldSatisfy` isLeft
    at 1 2 `shouldSatisfy` isLeft
    at 0 7 `shouldSatisfy` isLeft
    at 3 1 `shouldSatisfy` isLeft
    at (-1) 1 `shouldSatisfy` isLeft
    -- A range ending with its line's break is on that line alone.
    at 4 5 `shouldBe` Right [Problem {from = 1, to = 1, kind = FromPlugin, severity = Error, message = "P: m"}]

  it "refuses more than 500 problems, and a message empty or over 300 characters" $ do
    let saying m = Reported {from = 0, to = 0, severity = Warning, message = m}
    problemsIn "P" "" (replicate 500 (saying "m")) `shouldSatisfy` isRight
    problemsIn "P" "" (replicate 501 (saying "m")) `shouldSatisfy` isLeft
    problemsIn "P" "" [saying " "] `shouldSatisfy` isLeft
    problemsIn "P" "" [saying (T.replicate 300 "m")] `shouldSatisfy` isRight
    problemsIn "P" "" [saying (T.replicate 301 "m")] `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
-- The machine

-- | The model plugin: a warning on each x, at its UTF-16 offsets.
reportsOf :: Text -> [Reported]
reportsOf text = [Reported {from = offsetOf text i, to = offsetOf text (i + 1), severity = Warning, message = "an x"} | (i, c) <- zip [0 ..] (T.unpack text), c == 'x']

-- | The problems the model plugin's answer stands for, worked out
-- without the protocol: an x's line.
expectedFor :: Text -> Text -> [Problem]
expectedFor name text = [Problem {from = lineOfChar text i, to = lineOfChar text i, kind = FromPlugin, severity = Warning, message = name <> ": an x"} | (i, c) <- zip [0 ..] (T.unpack text), c == 'x']

problemsMessage :: Int -> [Reported] -> Value
problemsMessage re reported =
  object
    [ "type" .= ("problems" :: Text)
    , "re" .= re
    , "problems" .= [object ["from" .= r.from, "to" .= r.to, "severity" .= ("warning" :: Text), "message" .= r.message] | r <- reported]
    ]

ready :: Value
ready = object ["type" .= ("ready" :: Text)]

-- | 'Step' is what may happen next in a world.
data Step
  = -- | The author's text, as checked, becomes one of 'texts'.
    Type Int
  | -- | The reader turns a plugin on or off.
    Toggle Int
  | -- | The page is, or is not, in an encrypted space.
    Encrypt Bool
  | -- | Another session, whose read of the reader's choices has not
    -- answered yet.
    NewSession
  | -- | The session's read of the reader's choices and of the plugins
    -- installed answers, or a read of them fails.
    ChoicesRead Bool
  | -- | The administrator turns a plugin off, or on, for everyone.
    WikiOff Int
  | -- | The server replaces a plugin with another version, elsewhere: the
    -- editor learns of it at the session's next read.
    Replace Int
  | -- | The reader replaces a plugin with another version in the settings
    -- of this tab: the input's plugins installed change at once.
    ReplaceHere Int
  | -- | The reader removes a plugin in the settings of this tab.
    RemoveHere Int
  | -- | The administrator sets the policy in this tab: off ('True'), or
    -- users may add theirs.
    PolicyHere Bool
  | -- | A frame's plugin says it is ready.
    Ready' Int
  | -- | A frame's plugin answers its ask out, rightly.
    Answer Int
  | -- | A frame's plugin answers under a number it was never sent, or an
    -- older one.
    AnswerStale Int
  | -- | A frame's plugin reports an offset past the text.
    AnswerOutside Int
  | -- | A frame's plugin sends what the protocol does not have.
    Junk Int
  | -- | A frame goes to another page.
    Navigate Int
  | -- | A frame's wait ends: its plugin did not start, or answer, in time.
    Timeout Int
  | -- | A frame's plugin sends more messages than it was asked for, which
    -- the JavaScript cuts short.
    Flood Int
  | -- | A frame closed earlier says something.
    FromClosed
  deriving stock (Show)

instance Arbitrary Step where
  arbitrary =
    frequency
      [ (12, Type <$> choose (0, length texts - 1))
      , (4, Toggle <$> choose (0, 1))
      , (1, Encrypt <$> arbitrary)
      , (2, pure NewSession)
      , (4, ChoicesRead <$> frequency [(4, pure True), (1, pure False)])
      , (2, WikiOff <$> choose (0, 1))
      , (1, Replace <$> choose (0, 1))
      , (2, ReplaceHere <$> choose (0, 1))
      , (1, RemoveHere <$> choose (0, 1))
      , (1, PolicyHere <$> arbitrary)
      , (10, Ready' <$> choose (0, 2))
      , (20, Answer <$> choose (0, 2))
      , (1, AnswerStale <$> choose (0, 2))
      , (1, AnswerOutside <$> choose (0, 2))
      , (1, Junk <$> choose (0, 2))
      , (1, Navigate <$> choose (0, 2))
      , (1, Timeout <$> choose (0, 2))
      , (1, Flood <$> choose (0, 2))
      , (1, pure FromClosed)
      ]

texts :: [Text]
texts = ["", "x", "ax\nbx", "😀x\nx😀", "no problem here", "x\n\n𝔸x"]

-- | 'Opened' is a frame as the world sees it: made and not closed.
data Opened = Opened
  { address :: Text
  , isReady :: Bool
  , asks :: [(Int, Text)]
  -- ^ The asks sent and not answered: ticket and text.
  , sent :: [(Int, Text)]
  -- ^ Every ask sent, ever.
  , waiting :: Maybe Int
  -- ^ The ticket of the wait under the frame's number, while one runs.
  }

-- | 'World' is the machine with what is around it: the server, the frames
-- open, what the editor was told, and what went wrong.
data World = World
  { model :: Model
  , server :: [ExternalPlugin]
  , open :: Map Int Opened
  , closed :: Set Int
  , told :: Map PluginId Heard
  , given :: Input
  -- ^ The input last given to the machine.
  , seen :: Set Text
  -- ^ What the run reached, for its coverage.
  , wrong :: [Text]
  , stepper :: Event -> Model -> (Model, [Effect])
  }

-- | The plugins the server has at first.
firstServer :: [ExternalPlugin]
firstServer = [plugin "spell" "s1" False, plugin "mine" "m1" True]

-- | The plugins installed as a read of them answers.
installedAs :: [ExternalPlugin] -> ExternalPlugins
installedAs ps = ExternalPlugins {policy = ExternalUsers, external = ps}

firstInput :: Input
firstInput = Input {session = 0, known = noPlugins, installed = installedAs firstServer, choicesRead = True, text = Nothing, encrypted = False}

newWorld :: (Event -> Model -> (Model, [Effect])) -> World
newWorld stepper =
  happen
    (Given firstInput)
    World {model = initial firstInput, server = firstServer, open = mempty, closed = mempty, told = mempty, given = firstInput, seen = mempty, wrong = [], stepper}

-- | An event stepped, its effects carried out against the world, and the
-- rules checked.
happen :: Event -> World -> World
happen event w =
  let (model, effects) = w.stepper event w.model
   in checked (foldl' perform w {model} effects)

perform :: World -> Effect -> World
perform w = \case
  Open n t address
    | Map.member n w.open || Set.member n w.closed -> w {wrong = ("frame " <> show n <> " opened twice") : w.wrong}
    | otherwise -> w {open = Map.insert n Opened {address, isReady = False, asks = [], sent = [], waiting = Just t} w.open}
  Send n t message -> case Map.lookup n w.open of
    Nothing -> w {wrong = ("an ask sent to frame " <> show n <> ", which is not open") : w.wrong}
    Just o ->
      let text = case Aeson.fromJSON message of
            Aeson.Success (m :: Map Text Value) | Just (Aeson.String s) <- Map.lookup "text" m -> s
            _ -> "?"
          also =
            [x | not o.isReady, x <- ["an ask sent before the plugin was ready"]]
              <> [x | not (null o.asks), x <- ["a second ask out at once"]]
              <> [x | isJust o.waiting, x <- ["a wait started over another"]]
              <> [x | not w.given.choicesRead, x <- ["the page's text sent while this session's choices are unknown"]]
              <> [x | not (onFor w.given (pluginAt o.address)), x <- ["the page's text sent to a plugin off for the reader or the wiki"]]
       in w {open = Map.insert n o {asks = o.asks <> [(t, text)], sent = o.sent <> [(t, text)], waiting = Just t} w.open, wrong = also <> w.wrong}
  StopWaiting n -> w {open = Map.adjust (\o -> o {waiting = Nothing}) n w.open}
  Close n
    | Map.member n w.open -> w {open = Map.delete n w.open, closed = Set.insert n w.closed}
    | otherwise -> w {wrong = ("frame " <> show n <> " closed and not open") : w.wrong}
  Tell heard -> w {told = heard}

-- | The rules, after each event.
checked :: World -> World
checked w = w {wrong = problems <> w.wrong, seen = w.seen <> reached}
  where
    reached =
      Set.fromList
        ( ["heard" | any (\h -> not (null h.problems)) m.heard]
            <> ["stopped" | not (Map.null m.stopped)]
            <> ["asking" | any (\f -> case f.phase of Asking _ -> True; _ -> False) m.frames]
            <> ["two running" | Map.size m.frames == 2]
        )
    m = w.model
    -- What should run, by the input the world gave.
    shouldRun = wanted m {input = w.given}
    framesHeld = Map.fromList [(f.number, f) | f <- Map.elems m.frames]
    problems =
      [ "the frames open are not the frames the model holds: " <> show (Map.keys w.open) <> " " <> show (Map.keys framesHeld)
      | Map.keysSet w.open /= Map.keysSet framesHeld
      ]
        <> [ "a frame runs a plugin not wanted: " <> f.plugin.manifest.id.text
           | f <- Map.elems m.frames
           , f.plugin `notElem` shouldRun
           ]
        <> ["a frame open in an encrypted space" | w.given.encrypted, not (Map.null w.open)]
        <> ["a frame open while this session's choices are unknown" | not w.given.choicesRead, not (Map.null w.open)]
        <> [ "a frame open for a plugin off for the reader or the wiki: " <> pluginAt o.address
           | o <- Map.elems w.open
           , not (onFor w.given (pluginAt o.address))
           ]
        <> [ "a stopped plugin has a frame: " <> p.text
           | p <- Map.keys m.stopped
           , Map.member p m.frames
           ]
        <> [ "frame " <> show n <> " is open at the wrong address"
           | (n, o) <- Map.toList w.open
           , Just f <- [Map.lookup n framesHeld]
           , o.address /= framePath f.plugin
           ]
        <> ["the editor was not told what was heard" | w.told /= m.heard]
        <> [ "what was heard of " <> p.text <> " is not the model plugin's answer for the text it was sent"
           | (p, h) <- Map.toList m.heard
           , Just f <- [Map.lookup p m.frames]
           , h.problems /= expectedFor f.plugin.manifest.name h.text || h.text `notElem` maybe [] (map snd . (\o -> o.sent)) (Map.lookup f.number w.open)
           ]
        <> ["what was heard of a plugin that does not run" | not (Map.keysSet m.heard `Set.isSubsetOf` Map.keysSet m.frames)]

-- | The id of the plugin a frame's address is of (@plugin-frame/<wiki or
-- mine>/<id>/<bundle>/diagnostics@).
pluginAt :: Text -> Text
pluginAt address = fromMaybe "" (atMay (T.splitOn "/" address) 2)

-- | Whether a plugin is on by the input, worked out without the
-- registry: an external plugin is on unless the wiki or the reader turned
-- it off.
onFor :: Input -> Text -> Bool
onFor input p =
  not (Set.member (PluginId p) input.known.offForWiki)
    && maybe True (\c -> c.state /= PluginOff) (Map.lookup (PluginId p) input.known.mine)

-- | The frame at an index among those open, if any.
frameAt :: Int -> World -> Maybe (Int, Opened)
frameAt i w = atMay (Map.toList w.open) i

play :: World -> Step -> World
play w = \case
  Type i -> given (\x -> Input {session = x.session, known = x.known, installed = x.installed, choicesRead = x.choicesRead, text = atMay texts i, encrypted = x.encrypted})
  Toggle i -> case atMay ["spell", "mine"] i of
    Just p ->
      let isOff = maybe False (\c -> c.state == PluginOff) (Map.lookup (PluginId p) w.given.known.mine)
       in given (\x -> x {known = Plugins {offForWiki = x.known.offForWiki, mine = (choosing (filter ((/= p) . fst) (current x) <> [(p, if isOff then PluginOn else PluginOff)])).mine}})
    Nothing -> w
  Encrypt e -> given (\x -> x {encrypted = e})
  -- The shell holds the defaults, with the wiki's set as it last knew it
  -- and no plugins installed, until the new session's read answers.
  NewSession -> given (\x -> x {session = x.session + 1, known = Plugins {offForWiki = x.known.offForWiki, mine = mempty}, installed = noExternalPlugins, choicesRead = False})
  -- The read answers what the server has; a read that fails changes
  -- nothing held.
  ChoicesRead ok -> given (\x -> if ok then x {installed = installedAs w.server, choicesRead = True} else x {choicesRead = False})
  WikiOff i -> case atMay ["spell", "mine"] i of
    Just p ->
      let flipped off = if Set.member (PluginId p) off then Set.delete (PluginId p) off else Set.insert (PluginId p) off
       in given (\x -> x {known = Plugins {offForWiki = flipped x.known.offForWiki, mine = x.known.mine}})
    Nothing -> w
  Replace i -> w {server = replaced i w.server}
  -- What the reader changes in the settings reaches the editor in the same
  -- step, as the server's answer the plugins' state takes.
  ReplaceHere i -> heldHere (\x -> x {external = replaced i x.external}) w {server = replaced i w.server}
  RemoveHere i -> case atMay w.server i of
    Just gone ->
      let without = filter (\p -> p.manifest.id /= gone.manifest.id)
       in heldHere (\x -> x {external = without x.external}) w {server = without w.server}
    Nothing -> w
  PolicyHere off -> heldHere (\x -> x {policy = if off then ExternalOff else ExternalUsers}) w
  Ready' i -> onFrame i $ \(n, o) -> happen (Said n (Message ready)) w {open = Map.insert n o {isReady = True} w.open}
  Answer i -> onFrame i $ \(n, o) -> case o.asks of
    (t, text) : rest -> happen (Said n (Message (problemsMessage t (reportsOf text)))) w {open = Map.insert n o {asks = rest} w.open}
    [] -> w
  AnswerStale i -> onFrame i $ \(n, o) ->
    -- An answer under a number never sent, or under an older one, with no
    -- problems.
    happen (Said n (Message (problemsMessage (maybe 9999 fst (lastMay (filter (`notElem` o.asks) o.sent))) []))) w
  AnswerOutside i -> onFrame i $ \(n, o) -> case o.asks of
    (t, text) : _ -> happen (Said n (Message (problemsMessage t [Reported {from = 0, to = utf16Length text + 1, severity = Warning, message = "past"}]))) w
    [] -> w
  Junk i -> onFrame i $ \(n, _) -> happen (Said n (Message (object ["type" .= ("hello" :: Text)]))) w
  Navigate i -> onFrame i $ \(n, _) -> happen (Said n Navigated) w
  Flood i -> onFrame i $ \(n, _) -> happen (Said n Unasked) w
  Timeout i -> onFrame i $ \(n, o) -> maybe w (\t -> happen (Waited n t (Right ())) w) o.waiting
  FromClosed -> case Set.lookupMax w.closed of
    Just n -> happen (Said n (Message ready)) (happen (Said n (Message (problemsMessage 0 []))) w)
    Nothing -> w
  where
    given change = let input = change w.given in happen (Given input) w {given = input}
    -- A change made in this tab, held once the session's read answered.
    heldHere change w' = if w'.given.choicesRead then let input = w'.given {installed = change w'.given.installed} in happen (Given input) w' {given = input} else w'
    replaced i = zipWith (\n p -> if n == i then p {bundle = p.bundle <> "'"} else p) [0 :: Int ..]
    current x = [(p.text, c.state) | (p, c) <- Map.toList x.known.mine]
    onFrame i f = maybe w f (frameAt i w)

-- | Every plugin that runs answers whatever it is asked, rightly, until
-- nothing is asked: the reads, the readiness, the asks.
drain :: World -> World
drain w0 = go (50 :: Int) w0
  where
    go 0 w = w {wrong = "the plugins never stopped being asked" : w.wrong}
    go k w
      | Just i <- findIndex (\(_, o) -> not o.isReady) (Map.toList w.open) = go (k - 1) (play w (Ready' i))
      | Just i <- findIndex (\(_, o) -> not (null o.asks)) (Map.toList w.open) = go (k - 1) (play w (Answer i))
      | otherwise = w

-- | What went wrong in a run: during it, and once every plugin that runs
-- has answered everything (each then heard for the text checked), and
-- after the editor closed (every frame closed).
runWith :: (Event -> Model -> (Model, [Effect])) -> [Step] -> [Text]
runWith stepper steps =
  let w = drain (foldl' play (newWorld stepper) steps)
      settledWrong =
        [ "a plugin that runs has not heard the text checked: " <> p.text
        | (p, f) <- Map.toList w.model.frames
        , Just text <- [w.given.text]
        , ((\h -> h.text) <$> Map.lookup p w.model.heard) /= Just text
        , isJust (Map.lookup f.number w.open)
        ]
      -- The editor is gone: nothing is told any more, and every frame is
      -- closed.
      finished = let (model, effects) = w.stepper Finished w.model in foldl' perform w {model} effects
   in reverse finished.wrong <> settledWrong <> ["a frame left open after the editor closed" | not (Map.null finished.open)]

fails :: (Event -> Model -> (Model, [Effect])) -> [Step] -> Text -> Expectation
fails stepper steps expected = runWith stepper steps `shouldSatisfy` any (expected `T.isInfixOf`)

machine :: Spec
machine = describe "the editor's machine of external plugins" $ do
  it "keeps one frame for each plugin that runs, one ask out a frame, takes answers only under the ticket of the ask out, and closes what it opened" $
    -- Half of the runs start with the plugins running and asked, so that
    -- the rest of the run is spent on what happens to them.
    let runs = (<>) <$> elements [[], [Type 1, Ready' 0, Ready' 1]] <*> (choose (10, 80) >>= vector)
     in withNumTests 2000 . checkCoverage . forAll runs $ \steps ->
          let wrong = runWith step steps
              -- The runs reach what the rules are about.
              played = foldl' play (newWorld step) steps
           in cover 30 (Set.member "heard" played.seen) "a plugin found problems in a text"
                . cover 30 (Set.member "stopped" played.seen) "a plugin was stopped"
                . cover 50 (Set.member "asking" played.seen) "an ask was out"
                . cover 50 (Set.member "two running" played.seen) "two plugins ran"
                $ counterexample (show wrong) (null wrong)

  it "is told apart from a machine that breaks each rule" $ do
    let early = [Type 1, Ready' 0, Ready' 1]
    -- Taking any answer, as if under the ticket out.
    fails
      ( \e m -> case e of
          Said n (Message v) | Right (Problems _ r) <- fromPlugin v, Just f <- find (\f -> f.number == n) (Map.elems m.frames), Asking out <- f.phase -> step (Said n (Message (problemsMessage out.ticket r))) m
          _ -> step e m
      )
      (early <> [Type 2, AnswerStale 0])
      "is not the model plugin's answer"
    -- Keeping the frames of plugins turned off.
    fails (\e m -> second (filter (\case Close _ -> False; _ -> True)) (step e m)) (early <> [Toggle 0]) "not the frames the model holds"
    -- Forgetting the ask out, so that a newer text goes out beside it.
    fails (\e m -> first (\m' -> m' {frames = Map.map (\f -> case f.phase of Asking _ -> f {phase = Idle Nothing}; _ -> f) m'.frames}) (step e m)) (early <> [Type 2]) "a second ask out at once"
    -- Running plugins in an encrypted space.
    fails (\e m -> case e of Given i -> step (Given i {encrypted = False}) m; _ -> step e m) (early <> [Encrypt True]) "encrypted"
    -- Running plugins on the defaults held before the session's choices
    -- are read.
    fails
      (\e m -> case e of Given i -> step (Given i {choicesRead = True, installed = installedAs firstServer}) m; _ -> step e m)
      (early <> [NewSession, Ready' 0])
      "while this session's choices are unknown"
    -- Ignoring the plugins off for the wiki.
    fails
      (\e m -> case e of Given i -> step (Given i {known = Plugins {offForWiki = mempty, mine = i.known.mine}}) m; _ -> step e m)
      (early <> [WikiOff 1, Type 2])
      "off for the reader or the wiki"
    -- Keeping the frame of a plugin removed in this tab, or of one that no
    -- longer runs under the policy set in this tab.
    fails (\e m -> case e of Given i -> step (Given i {installed = m.input.installed}) m; _ -> step e m) (early <> [RemoveHere 0]) "a frame runs a plugin not wanted"
    fails (\e m -> case e of Given i -> step (Given i {installed = m.input.installed}) m; _ -> step e m) (early <> [PolicyHere True]) "a frame runs a plugin not wanted"
    -- Not closing the frames when the editor closes.
    fails (\e m -> case e of Finished -> (m, []); _ -> step e m) early "left open"

  it "stops a plugin that does not answer in time, says why, and does not start it again while the same version is on" $ do
    let w = foldl' play (newWorld step) [Type 1, Ready' 0, Ready' 1, Timeout 0, Type 2]
    w.wrong `shouldBe` []
    map (\s -> (s.plugin.manifest.id.text, s.reason)) (Map.elems w.model.stopped) `shouldBe` [("mine", "it did not answer within 10 seconds")]
    Map.keys w.model.frames `shouldBe` [PluginId "spell"]
    -- Another version runs again.
    let w' = foldl' play w [Replace 1, NewSession, ChoicesRead True]
    sort (Map.keys w'.model.frames) `shouldBe` [PluginId "mine", PluginId "spell"]

  it "counts a plugin's problems for the text it was sent, and asks for the newest text once the answer has come" $ do
    let w = foldl' play (newWorld step) [Type 1, Ready' 0, Answer 0, Type 2]
    w.wrong `shouldBe` []
    Map.toList (map (\h -> (h.text, h.problems)) w.model.heard) `shouldBe` [(PluginId "mine", ("x", expectedFor "Mine" "x"))]
    -- The newest text is out, and the answer for it replaces the last.
    let w' = play w (Answer 0)
    map (\h -> h.text) (Map.elems w'.model.heard) `shouldBe` ["ax\nbx"]
    map (\h -> map (\p -> p.from) h.problems) (Map.elems w'.model.heard) `shouldBe` [[1, 2]]

  it "runs no plugin before the session's read answers, and each installed once it does" $ do
    let w = foldl' play (newWorld step) [NewSession]
    Map.null w.model.frames `shouldBe` True
    let w' = play w (ChoicesRead True)
    Map.size w'.model.frames `shouldBe` 2

  it "follows a plugin replaced, removed, or the policy set in this tab, in the step the input changes" $ do
    let w = foldl' play (newWorld step) [Type 1, Ready' 0, Ready' 1]
        frameOf p x = (\f -> (f.number, f.plugin.bundle)) <$> Map.lookup (PluginId p) x.model.frames
    -- Another version: its frame closed and a new one made, at once.
    let replaced = play w (ReplaceHere 0)
    replaced.wrong `shouldBe` []
    (snd <$> frameOf "spell" replaced) `shouldBe` Just "s1'"
    (fst <$> frameOf "spell" replaced) `shouldNotBe` (fst <$> frameOf "spell" w)
    -- Removed: its frame closed.
    let removed = play w (RemoveHere 0)
    Map.keys removed.model.frames `shouldBe` [PluginId "mine"]
    -- The policy off: none runs; on again, both.
    let off = play w (PolicyHere True)
    Map.null off.open `shouldBe` True
    Map.size (play off (PolicyHere False)).open `shouldBe` 2

  it "runs no plugin until the session's choices are read, closes every frame when they are not, and keeps them closed when the read fails" $ do
    let w = foldl' play (newWorld step) [Type 1, Ready' 0, Ready' 1, NewSession]
    w.wrong `shouldBe` []
    Map.null w.open `shouldBe` True
    let failed = play w (ChoicesRead False)
    Map.null failed.open `shouldBe` True
    let read' = play failed (ChoicesRead True)
    Map.size read'.open `shouldBe` 2
    read'.wrong `shouldBe` []

  it "stops a plugin that sends what it was not asked for: a flood, or an answer to no ask out" $ do
    let early = [Type 1, Ready' 0, Ready' 1]
        reasons w = map (\s -> s.reason) (Map.elems w.model.stopped)
    reasons (foldl' play (newWorld step) (early <> [Flood 0])) `shouldBe` ["it sent a message it was not asked for"]
    reasons (foldl' play (newWorld step) (early <> [Answer 0, AnswerStale 0])) `shouldBe` ["it sent an answer it was not asked for"]

-- | Which plugins are on, as the settings change it: the in-memory
-- backend's rules ("Wiki.App.Backend.Mem.State"'s 'setPluginStateIn' and
-- 'setWikiPluginsIn', the demo's and the native tests' server), the
-- section's machine ("Wiki.App.View.Plugins.Machine") on generated runs,
-- and the administration's switch ("Wiki.App.View.WikiPlugins.Machine").
--
-- = The model of the server
--
-- The runs' server is the in-memory state itself, with two readers, the
-- demo's editor and its administrator. Its rules are @wiki-server@'s
-- ("Wiki.Server.Storage.Plugins", "Wiki.Server.Plugins"): the id's form,
-- a first choice refused once a user has choices kept for 100 plugins, a
-- plugin given back to its default with no options no longer kept, the
-- plugins off for the wiki kept apart from the users' choices, and the
-- administrator's set read and set by an administrator only. The first
-- group of this spec checks each against the server's words. Besides the
-- section, the wiki changes elsewhere: the administrator turns a plugin
-- off or on for everyone ('WikiOff'), another tab of the reader sets a
-- state ('OtherTab'), or fills the reader's choices up to the bound
-- ('Fill'), so that a write is refused. An answer can be lost on the way
-- after the server handled the ask ('Lose'): the section hears
-- 'Unreachable'.
--
-- Each ask has three moments: the section sends it, the server handles it
-- (a read takes what is on then; a write changes it and answers), and the
-- answer comes back. The server handles asks in the order sent, and the
-- answers come back in any order ('Handle', 'Deliver').
module Test.PluginsSettings (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.QuickCheck (Gen, chooseInt, counterexample, elements, forAllShrinkShow, frequency, listOf, shrinkList, withNumTests)
import Test.QuickCheck qualified as QC
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), WikiPlugins (..), maxPluginsPerUser, pluginIdRule)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Mem.State (MemState (..), pluginsIn, setPluginStateIn, setWikiPluginsIn, wikiPluginsIn)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.View.Latest (madeAfter)
import Wiki.App.View.Plugins.Machine
import Wiki.App.View.WikiPlugins.Machine qualified as Wiki
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Permission (Grants (..))

spec :: Spec
spec = describe "plugins in the settings" $ do
  memRules
  machine
  wikiSwitch

--------------------------------------------------------------------------------
-- The in-memory backend's rules

-- | The demo's wiki with someone signed in.
as :: Text -> MemState
as u = (demoState demoNow) {current = Just (UserId ("user-" <> u))}

sure :: (Show e) => Either e a -> a
sure = either (panic . show) identity

assistant, errors :: PluginId
assistant = PluginId "assistant"
errors = PluginId "errors"

choice :: PluginState -> PluginChoice
choice s = PluginChoice {state = s, options = Nothing}

memRules :: Spec
memRules = describe "in the in-memory backend, by the server's rules" $ do
  it "keeps a user's state by plugin, and nothing for a plugin given back to its default" $ do
    let (st1, c) = sure (setPluginStateIn assistant PluginOff (as "editor"))
    c `shouldBe` choice PluginOff
    (\p -> p.mine) <$> pluginsIn st1 `shouldBe` Right (Map.fromList [(assistant, choice PluginOff)])
    -- Another user's choices are their own.
    (\p -> p.mine) <$> pluginsIn st1 {current = Just (UserId "user-admin")} `shouldBe` Right mempty
    let (st2, back) = sure (setPluginStateIn assistant PluginDefault st1)
    back `shouldBe` choice PluginDefault
    (\p -> p.mine) <$> pluginsIn st2 `shouldBe` Right mempty
    st2.pluginChoices `shouldBe` mempty

  it "refuses as the server does: an id that is not one, nobody signed in, a first choice past the bound" $ do
    leftToMaybe (setPluginStateIn assistant PluginOn (as "editor") {current = Nothing}) `shouldBe` Just Forbidden
    leftToMaybe (setPluginStateIn (PluginId "Not one") PluginOn (as "editor")) `shouldBe` Just (Invalid pluginIdRule)
    -- The id first, as the server reads the address before anything else.
    leftToMaybe (setPluginStateIn (PluginId "Not one") PluginOn (as "editor") {current = Nothing}) `shouldBe` Just (Invalid pluginIdRule)
    let full = foldl' (\st k -> fst (sure (setPluginStateIn (PluginId ("p" <> show k)) PluginOn st))) (as "editor") [1 .. maxPluginsPerUser]
    leftToMaybe (setPluginStateIn assistant PluginOn full)
      `shouldBe` Just (Invalid "You have choices kept for 100 plugins already, the most a user has: give one of them back to its default, with no options, first.")
    -- A plugin kept already may change, and one given back makes room.
    isRight (setPluginStateIn (PluginId "p1") PluginOff full) `shouldBe` True
    let (room, _) = sure (setPluginStateIn (PluginId "p1") PluginDefault full)
    isRight (setPluginStateIn assistant PluginOn room) `shouldBe` True

  it "answers what is on to anyone who may read the wiki, and refuses a closed wiki to nobody" $ do
    let nobody = (as "editor") {current = Nothing}
    pluginsIn nobody `shouldBe` Right Plugins {offForWiki = mempty, mine = mempty}
    pluginsIn nobody {grants = nobody.grants {access = Closed}} `shouldBe` Left Forbidden

  it "turns a plugin off for the wiki, for an administrator only, keeping each user's choice" $ do
    let (st1, _) = sure (setPluginStateIn errors PluginOn (as "editor"))
    leftToMaybe (setWikiPluginsIn WikiPlugins {off = Set.fromList [errors]} st1) `shouldBe` Just Forbidden
    leftToMaybe (wikiPluginsIn st1) `shouldBe` Just Forbidden
    let (st2, w) = sure (setWikiPluginsIn WikiPlugins {off = Set.fromList [errors]} st1 {current = Just (UserId "user-admin")})
    w `shouldBe` WikiPlugins {off = Set.fromList [errors]}
    sure (wikiPluginsIn st2) `shouldBe` w
    let editor = st2 {current = Just (UserId "user-editor")}
    pluginsIn editor `shouldBe` Right Plugins {offForWiki = Set.fromList [errors], mine = Map.fromList [(errors, choice PluginOn)]}
    -- Back on for the wiki: the editor's choice applies again.
    let (st3, _) = sure (setWikiPluginsIn WikiPlugins {off = mempty} st2)
    pluginsIn st3 {current = Just (UserId "user-editor")} `shouldBe` Right Plugins {offForWiki = mempty, mine = Map.fromList [(errors, choice PluginOn)]}

  it "refuses the wiki's set as the server does: an id that is not one, more than 100" $ do
    let admin = as "admin"
    leftToMaybe (setWikiPluginsIn WikiPlugins {off = Set.fromList [PluginId "-x"]} admin) `shouldBe` Just (Invalid pluginIdRule)
    leftToMaybe (setWikiPluginsIn WikiPlugins {off = Set.fromList [PluginId ("p" <> show k) | k <- [1 .. 101 :: Int]]} admin)
      `shouldBe` Just (Invalid "The wiki turns off at most 100 plugins.")
    isRight (setWikiPluginsIn WikiPlugins {off = Set.fromList [PluginId ("p" <> show k) | k <- [1 .. 100 :: Int]]} admin) `shouldBe` True

--------------------------------------------------------------------------------
-- Runs of the section

-- | The plugins of the runs: the registry's.
ids :: [PluginId]
ids = map (\m -> m.id) manifests

-- | The reader a session's number names: the two take turns.
readerOf :: Int -> Text
readerOf s = if even s then "editor" else "admin"

-- | 'Answer' is what the server answered an ask.
data Answer
  = Read (Either WikiError Plugins)
  | Wrote (Either WikiError PluginChoice)
  deriving stock (Show)

-- | The server's handling of an ask sent with a reader's cookie.
handled :: Text -> Effect -> MemState -> (MemState, Maybe Answer)
handled u e st0 = case e of
  Load _ -> (st0, Just (Read (pluginsIn st)))
  Write _ p s -> case setPluginStateIn p s st of
    Left err -> (st0, Just (Wrote (Left err)))
    Right (st', c) -> (st' {current = st0.current}, Just (Wrote (Right c)))
  Tell _ -> (st0, Nothing)
  where
    st = st0 {current = Just (UserId ("user-" <> u))}

-- | 'Step' is one thing that happens in a run. A step that cannot happen in
-- the state it comes to is skipped, so that a run shrinks to any part.
data Step
  = -- | The reader chooses a state for the plugin at this place.
    Click Int PluginState
  | -- | The server handles the oldest ask it has not handled.
    Handle
  | -- | The handled answer at this place among those handled comes back.
    Deliver Int
  | -- | The administrator turns the plugin at this place off for everyone
    -- ('True'), or on, elsewhere.
    WikiOff Int Bool
  | -- | Another tab of the reader sets the plugin at this place.
    OtherTab Int PluginState
  | -- | Another tab fills the reader's choices up to the bound.
    Fill
  | -- | The answer to the next ask handled is lost on the way.
    Lose
  | -- | The reader leaves the section and opens it again: the shell keeps
    -- the model, and a write out goes on.
    Reopen
  | -- | Another reader signs in.
    SwitchUser
  | Finish
  deriving stock (Eq, Show)

genRun :: Gen [Step]
genRun =
  QC.scale (* 2) . listOf . frequency $
    [ (8, Click <$> place <*> elements [minBound .. maxBound])
    , (8, pure Handle)
    , (8, Deliver <$> frequency [(6, pure 0), (1, pure 1)])
    , (2, WikiOff <$> place <*> elements [True, False])
    , (1, OtherTab <$> place <*> elements [minBound .. maxBound])
    , (1, pure Fill)
    , (1, pure Lose)
    , (1, pure Reopen)
    , (1, pure SwitchUser)
    , (1, pure Finish)
    ]
  where
    place = chooseInt (0, length ids - 1)

shrinkRun :: [Step] -> [[Step]]
shrinkRun = shrinkList $ \case
  Deliver i -> Deliver <$> [0 .. i - 1]
  _ -> []

-- | An ask the section made.
data Out = Out
  { effect :: Effect
  , reader :: Text
  , inSession :: Int
  , answer :: Maybe Answer
  , changesSeen :: Int
  -- ^ How many changes elsewhere (and answers lost) there had been when
  -- the server handled it.
  }
  deriving stock (Show)

-- | A run under way.
data World = World
  { section :: Model
  , backend :: MemState
  , outs :: [Out]
  , turn :: Int
  , finished :: Bool
  , losing :: Bool
  -- ^ Whether the next answer handled is lost.
  , changes :: Int
  -- ^ The changes made elsewhere, and the answers lost, so far.
  , fresh :: Bool
  -- ^ Whether the section has taken, in this session, a read the server
  -- handled after every change elsewhere and every answer lost: from then
  -- on, what it knows is the server's.
  , clicked :: [(Int, PluginId, PluginState)]
  -- ^ The states chosen, with the session they were chosen in.
  , told :: Maybe Plugins
  -- ^ What the section last told the shell it holds ('Tell').
  , lastWrite :: Map PluginId Bool
  -- ^ Whether the last answer to a write of each plugin taken in this
  -- session was a success.
  , seen :: [Text]
  , trace :: [Text]
  , faults :: [Text]
  }

start :: World
start = afterEvent "mounted" Started World {section = initial 0, backend = as "editor", outs = [], turn = 0, finished = False, losing = False, changes = 0, fresh = False, clicked = [], told = Nothing, lastWrite = mempty, seen = [], trace = [], faults = []}

play :: World -> Step -> World
play w = \case
  Click i s -> case atMay ids i of
    Just p
      | not w.finished
      , Just (Right known) <- w.section.known
      , not (Set.member p known.offForWiki) ->
          afterEvent ("chooses " <> p.text <> " " <> show s) (Chosen p s) w {clicked = (w.turn, p, s) : w.clicked}
    _ -> w
  Handle -> case break (isNothing . (\o -> o.answer)) w.outs of
    (earlier, o : later') ->
      let (backend, a) = handled o.reader o.effect w.backend
          lost = w.losing
          a' = if lost then lostAnswer o.effect else a
          changes = if lost then w.changes + 1 else w.changes
       in noted ("the server handles " <> show o.effect <> if lost then ", its answer lost" else "") w {backend, outs = earlier <> [o {answer = a', changesSeen = changes}] <> later', losing = False, changes, fresh = w.fresh && not lost}
    _ -> w
  Deliver i -> case atMay [(k, o, a) | (k, o) <- zip [0 :: Int ..] w.outs, Just a <- [o.answer]] i of
    Just (k, o, a) ->
      let w' = w {outs = [x | (j, x) <- zip [0 ..] w.outs, j /= k]}
          ours = o.inSession == w.turn
       in case (o.effect, a) of
            (Load t, Read r) ->
              let w2 = w' {fresh = w'.fresh || (ours && isRight r && o.changesSeen == w.changes), seen = ["off for the wiki, read" | ours, Right k' <- [r], not (Set.null k'.offForWiki)] <> w'.seen}
               in afterEvent "a read comes back" (Loaded t r) w2
            (Write t p _, Wrote r) ->
              let tag = case r of
                    Right _ -> "a write taken"
                    Left (Invalid _) -> "a write refused"
                    Left _ -> "a write lost"
               in afterEvent "a write comes back" (Written t p r) w' {seen = [tag | ours] <> w'.seen, lastWrite = if ours then Map.insert p (isRight r) w'.lastWrite else w'.lastWrite}
            _ -> w'
    Nothing -> w
  WikiOff i isOff -> case atMay ids i of
    Just p ->
      let st = w.backend
          off' = (if isOff then Set.insert else Set.delete) p st.pluginsOff
       in elsewhere ("the administrator turns " <> p.text <> if isOff then " off" else " on") w {backend = st {pluginsOff = off'}}
    Nothing -> w
  OtherTab i s -> case atMay ids i of
    Just p -> case setPluginStateIn p s w.backend {current = Just (UserId ("user-" <> readerOf w.turn))} of
      Right (st', _) -> elsewhere ("another tab sets " <> p.text <> " " <> show s) w {backend = st' {current = w.backend.current}}
      Left _ -> w
    Nothing -> w
  Fill ->
    let u = UserId ("user-" <> readerOf w.turn)
        st = w.backend
        others = Map.fromList [(PluginId ("other" <> show k), choice PluginOn) | k <- [1 .. maxPluginsPerUser]]
     in elsewhere "another tab fills the choices" w {backend = st {pluginChoices = Map.insertWith (\_ old -> Map.union old (Map.fromList (take (maxPluginsPerUser - Map.size old) (Map.toList others)))) u others st.pluginChoices}}
  Lose -> noted "the next answer will be lost" w {losing = True}
  Reopen
    | w.finished -> w
    | otherwise -> afterEvent "opens the section again" Opened w {seen = ["opened again with an ask out" | any (\o -> o.inSession == w.turn) w.outs] <> w.seen}
  SwitchUser
    | w.finished -> w
    | otherwise ->
        let next = w.turn + 1
         in -- The shell gives the new session; the section, open, asks.
            afterEvent "the section, open, asks" Opened
              . afterEvent ("reader " <> readerOf next <> " signs in") (Given next)
              $ w {turn = next, fresh = False, lastWrite = mempty, seen = ["a switch with an ask out" | any (\o -> o.inSession == w.turn) w.outs] <> w.seen}
  Finish -> afterEvent "goes away" Finished w {finished = True}
  where
    lostAnswer = \case
      Load _ -> Just (Read (Left (Unreachable "lost")))
      Write {} -> Just (Wrote (Left (Unreachable "lost")))
      Tell _ -> Nothing

-- | A change made elsewhere: what the section knows may be old until its
-- next read.
elsewhere :: Text -> World -> World
elsewhere what w = noted what w {changes = w.changes + 1, fresh = False}

noted :: Text -> World -> World
noted what w = w {trace = what : w.trace}

-- | The world after the section took an event, its effects out, and the
-- rules checked.
afterEvent :: Text -> Event -> World -> World
afterEvent what event w =
  checked
    . noted (what <> if null effects then "" else " -> " <> show effects)
    $ w
      { section
      , outs = outs'
      , told = lastMay [k | Tell k <- effects] <|> w.told
      , faults =
          w.faults
            <> [what <> ": " <> show effects <> " after the section went away" | w.finished, not (null effects)]
            <> [what <> ": two asks out at once: " <> show (map (\o -> o.effect) live) | length live > 1]
            <> [what <> ": writes " <> p.text <> " " <> show s <> ", not chosen in this session" | Write _ p s <- effects, (w.turn, p, s) `notElem` w.clicked]
            <> [what <> ": tells the shell with no answer taken" | not (null [() | Tell _ <- effects]), not (answered event)]
            -- The section opened again finds the shell's model as it is,
            -- a write out with it, and asks nothing once it has read.
            <> [what <> ": asks anew on opening, what is on read already: " <> show effects | Opened <- [event], isJust (w.section.known >>= rightToMaybe), not (null effects)]
      }
  where
    (section, effects) = step event w.section
    outs' = w.outs <> [Out {effect = e, reader = readerOf w.turn, inSession = w.turn, answer = Nothing, changesSeen = 0} | e <- effects, isJust (askOf e)]
    live = [o | o <- outs', Just t <- [askOf o.effect], t `madeAfter` section.sessionFrom, o.inSession == w.turn]
    answered = \case
      Written {} -> True
      Loaded {} -> True
      _ -> False

-- | The rules, checked after every step: once the section has read what
-- the server holds since the last change elsewhere, and nothing it asked
-- is out or waits, it shows exactly what the server holds for its reader,
-- with no switch left showing a write that is not out.
checked :: World -> World
checked w = w {faults = w.faults <> if w.finished then [] else settledRule, seen = ["checked settled after a write" | quiet, w.fresh, not w.finished, "a write taken" `elem` w.seen] <> w.seen}
  where
    quiet = null [o | o <- w.outs, o.inSession == w.turn] && null w.section.queued
    serverNow = pluginsIn w.backend {current = Just (UserId ("user-" <> readerOf w.turn))}
    settledRule =
      [ "settled, but shows " <> show w.section.known <> " where the server has " <> show serverNow
      | quiet
      , w.fresh
      , w.section.known /= Just serverNow
      ]
        <> [ "settled, but its switches show writes not out: " <> show w.section.writing
           | quiet
           , not (Map.null w.section.writing)
           ]
        -- What it holds, once read, the shell has been told: the editor
        -- runs the plugins the switches show.
        <> [ "holds " <> show held <> ", but told the shell " <> show w.told
           | Just held <- [w.section.known >>= rightToMaybe]
           , w.told /= Just held
           ]
        -- A refusal stays only while the last answer about its plugin is
        -- one.
        <> [ "shows a refusal for " <> p.text <> ", whose last write succeeded"
           | (p, True) <- Map.toList w.lastWrite
           , Map.member p w.section.refused
           ]

runOf :: [Step] -> World
runOf = foldl' play start

machine :: Spec
machine = describe "the section's machine, on generated runs" $ do
  it "keeps every rule on any run" $
    QC.property . QC.checkCoverage . withNumTests 20000 $
      forAllShrinkShow genRun shrinkRun show $ \run ->
        let w = runOf run
            reached tag = tag `elem` w.seen
         in QC.cover 15 (reached "a write taken") "a write taken"
              . QC.cover 3 (reached "checked settled after a write") "checked settled after a write"
              . QC.cover 3 (reached "opened again with an ask out") "opened again with an ask out"
              . QC.cover 1 (reached "a write refused") "a write refused"
              . QC.cover 2 (reached "a write lost") "a write lost"
              . QC.cover 10 (reached "off for the wiki, read") "off for the wiki, read"
              . QC.cover 10 (reached "a switch with an ask out") "a switch with an ask out"
              $ counterexample (toS (T.unlines (reverse w.trace) <> "\n" <> T.unlines w.faults)) (null w.faults)

  it "sends one ask at a time: a switch flipped while the read is out waits, and the later choice of a plugin takes its place" $ do
    let w = runOf [Click 0 PluginOff]
    -- Nothing is shown before the first read: nothing to click.
    length w.outs `shouldBe` 1
    let w1 = runOf [Handle, Deliver 0, Click 0 PluginOff, Click 1 PluginOff, Click 0 PluginOn]
    map (\o -> o.effect) w1.outs `shouldSatisfy` ((== 1) . length)
    [(p, s) | (_, p, s) <- w1.section.queued] `shouldBe` [(PluginId "errors", PluginOff), (PluginId "assistant", PluginOn)]

  it "shows the state chosen while its write is out, and the server's once it answered" $ do
    let w = runOf [Handle, Deliver 0, Click 0 PluginOff]
    shownState w.section assistant `shouldBe` PluginOff
    let w1 = foldl' play w [Handle, Deliver 0]
    shownState w1.section assistant `shouldBe` PluginOff
    w1.section.writing `shouldBe` mempty
    stateOf (sure (sure (maybeToRight () w1.section.known))) assistant `shouldBe` PluginOff

  it "drops a write chosen before another reader signed in" $ do
    let w = runOf [Handle, Deliver 0, Click 0 PluginOff, Click 1 PluginOff, SwitchUser, Handle, Deliver 0, Handle, Deliver 0]
    -- The editor's first write was out and done; the second waited, and
    -- went with the editor's session.
    Map.lookup (UserId "user-editor") w.backend.pluginChoices `shouldBe` Just (Map.fromList [(assistant, choice PluginOff)])
    Map.lookup (UserId "user-admin") w.backend.pluginChoices `shouldBe` Nothing
    w.faults `shouldBe` []

  it "is shown able to fail: a section that keeps an answer older than a write breaks the rule" $ do
    let w = runOf [Handle, Deliver 0, Click 0 PluginOff, Handle, Deliver 0]
        stale = w {section = w.section {known = Just (Right Plugins {offForWiki = mempty, mine = mempty})}}
    (checked stale).faults `shouldSatisfy` (not . null)

--------------------------------------------------------------------------------
-- The administration's switch

-- | The one effect of a step that has one.
onlyEffect :: [a] -> a
onlyEffect = \case
  [x] -> x
  _ -> panic "not one effect"

wikiSwitch :: Spec
wikiSwitch = describe "the administration's switch" $ do
  let ticket e = fromMaybe (panic "no ask") (Wiki.askOf e)
      run' :: [Wiki.Event] -> Wiki.Model -> (Wiki.Model, [Wiki.Effect])
      run' es m0 = foldl' (\(m, fx) e -> let (m', fx') = Wiki.step e m in (m', fx <> fx')) (m0, []) es
      -- The view mounted in session 0, its first read answered with a set.
      readWith off =
        let (m1, load) = second onlyEffect (Wiki.step Wiki.Started (Wiki.initial 0))
            (m2, fx) = Wiki.step (Wiki.Loaded (ticket load) (Right WikiPlugins {off})) m1
         in (m2, fx)
  it "reads the set and tells the shell, saves the set ticked, and tells the shell once the server took it" $ do
    let (m2, told) = readWith mempty
    told `shouldBe` [Wiki.Tell mempty]
    let (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
    save `shouldSatisfy` \case
      Wiki.Save _ s -> s == Set.fromList [errors]
      _ -> False
    Wiki.shownOff m3 `shouldBe` Just (Set.fromList [errors])
    let (m4, fx) = Wiki.step (Wiki.Saved (ticket save) (Right WikiPlugins {off = Set.fromList [errors]})) m3
    fx `shouldBe` [Wiki.Tell (Set.fromList [errors])]
    m4.off `shouldBe` Just (Right (Set.fromList [errors]))
    m4.wantedOff `shouldBe` Nothing

  it "saves once at a time, the last set wanted, and keeps ids it does not list" $ do
    let (m2, _) = readWith (Set.fromList [PluginId "elsewhere"])
        (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
        (m4, more) = run' [Wiki.Ticked assistant True, Wiki.Ticked errors False] m3
    more `shouldBe` []
    m4.queued `shouldBe` Just (0, Set.fromList [PluginId "elsewhere", assistant])
    let (_, next) = Wiki.step (Wiki.Saved (ticket save) (Left (Unreachable "lost"))) m4
    -- The set that waits goes next; the read the failure wants follows it.
    next `shouldSatisfy` \case
      [Wiki.Save _ s] -> s == Set.fromList [PluginId "elsewhere", assistant]
      _ -> False

  it "shows a refusal, reads again, and tells the shell when that read finds the save done" $ do
    let (m2, _) = readWith mempty
        (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
        (m4, load) = second onlyEffect (Wiki.step (Wiki.Saved (ticket save) (Left (Unreachable "lost"))) m3)
    m4.problem `shouldBe` Just (Unreachable "lost")
    Wiki.shownOff m4 `shouldBe` Just mempty
    -- The save was done all the same: the read says so, and the shell hears it.
    let (m5, fx) = Wiki.step (Wiki.Loaded (ticket load) (Right WikiPlugins {off = Set.fromList [errors]})) m4
    fx `shouldBe` [Wiki.Tell (Set.fromList [errors])]
    m5.off `shouldBe` Just (Right (Set.fromList [errors]))

  it "clears a refusal once a later save succeeds" $ do
    let (m2, _) = readWith mempty
        (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
        -- Ticked again while the save is out: it waits.
        (m4, _) = Wiki.step (Wiki.Ticked assistant True) m3
        (m5, again) = second onlyEffect (Wiki.step (Wiki.Saved (ticket save) (Left (Invalid "no"))) m4)
    m5.problem `shouldBe` Just (Invalid "no")
    let (m6, _) = Wiki.step (Wiki.Saved (ticket again) (Right WikiPlugins {off = Set.fromList [errors, assistant]})) m5
    m6.problem `shouldBe` Nothing

  it "drops a save queued by the session before, and sends nothing for it" $ do
    let (m2, _) = readWith mempty
        (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
        (m4, _) = Wiki.step (Wiki.Ticked assistant True) m3
        -- Another administrator signs in, and the view, open, asks.
        (m5', given) = Wiki.step (Wiki.Given 1) m4
        (m5, fresh) = Wiki.step Wiki.Opened m5'
    given `shouldBe` []
    fresh `shouldSatisfy` \case
      [Wiki.Load _] -> True
      _ -> False
    m5.queued `shouldBe` Nothing
    -- The former session's save answers: nothing is sent, nothing taken.
    let (m6, fx) = Wiki.step (Wiki.Saved (ticket save) (Right WikiPlugins {off = Set.fromList [errors]})) m5
    fx `shouldBe` []
    m6.off `shouldBe` Nothing

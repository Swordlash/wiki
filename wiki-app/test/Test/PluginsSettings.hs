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
-- Beside the section, the reader works the administration's switch
-- ('SwitchOpen', 'SwitchTick'), whose machine the shell runs too; the
-- server refuses it to the editor. The runs keep the shell's one value of
-- what is on as the shell does: what the reads with the session answered,
-- and the part each machine's answers told, merged in ('toldInto', the
-- switch's set). The rule is that, once nothing is out, that value is the
-- server's: the last answer about each part wins, whichever machine took
-- it.
--
-- Each ask has three moments: the shell sends it, the server handles it
-- (a read takes what is on then; a write changes it and answers), and the
-- answer comes back. The server handles asks in the order sent, the
-- section's and the switch's alike, and the answers come back in any
-- order ('Handle', 'Deliver').
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
import Wiki.App.View.Latest (Ask, madeAfter)
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
readerOf s = if even s then "admin" else "editor"

-- | 'Asked' is an ask the shell sent: one of the section's, or one of the
-- administration's switch's. Both go to the one server, which handles them
-- in the order sent.
data Asked
  = Personal Effect
  | Switch Wiki.Effect
  deriving stock (Show)

-- | 'Answer' is what the server answered an ask.
data Answer
  = Read (Either WikiError Plugins)
  | Wrote (Either WikiError PluginChoice)
  | SetRead (Either WikiError WikiPlugins)
  | SetSaved (Either WikiError WikiPlugins)
  deriving stock (Show)

-- | The server's handling of an ask sent with a reader's cookie.
handled :: Text -> Asked -> MemState -> (MemState, Maybe Answer)
handled u asked st0 = case asked of
  Personal (Load _) -> (st0, Just (Read (pluginsIn st)))
  Personal (Write _ p s) -> case setPluginStateIn p s st of
    Left err -> (st0, Just (Wrote (Left err)))
    Right (st', c) -> (st' {current = st0.current}, Just (Wrote (Right c)))
  Personal (Tell _ _) -> (st0, Nothing)
  Switch (Wiki.Load _) -> (st0, Just (SetRead (wikiPluginsIn st)))
  Switch (Wiki.Save _ off) -> case setWikiPluginsIn WikiPlugins {off} st of
    Left err -> (st0, Just (SetSaved (Left err)))
    Right (st', w) -> (st' {current = st0.current}, Just (SetSaved (Right w)))
  Switch (Wiki.Tell _ _) -> (st0, Nothing)
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
  | -- | The reader opens the administration's switch.
    SwitchOpen
  | -- | The reader ticks the box "off for everyone" of the plugin at this
    -- place, or clears it, in the administration's switch.
    SwitchTick Int Bool
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
    , (2, pure SwitchOpen)
    , (4, SwitchTick <$> place <*> elements [True, False])
    , (1, pure SwitchUser)
    , (1, pure Finish)
    ]
  where
    place = chooseInt (0, length ids - 1)

shrinkRun :: [Step] -> [[Step]]
shrinkRun = shrinkList $ \case
  Deliver i -> Deliver <$> [0 .. i - 1]
  _ -> []

-- | An ask the shell made, for the section or the switch.
data Out = Out
  { effect :: Asked
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
  , switch :: Wiki.Model
  -- ^ The administration's switch's machine, which the shell runs too.
  , shell :: Held
  -- ^ What is on as the shell holds it: what the reads with the session
  -- answered, and what the two machines told since, each part from the
  -- latest ask about it, as the shell merges them.
  , sectionSent :: Maybe (Ask, Int)
  , switchSent :: Maybe (Ask, Int)
  -- ^ Each machine's last ask, with the number the shell gave it.
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
  , lastWrite :: Map PluginId Bool
  -- ^ Whether the last answer to a write of each plugin taken in this
  -- session was a success.
  , seen :: [Text]
  , trace :: [Text]
  , faults :: [Text]
  }

start :: World
start = afterEvent "mounted" Started World {section = initial 0, switch = Wiki.initial 0, shell = heldFrom (sure (pluginsIn (as "admin"))), sectionSent = Nothing, switchSent = Nothing, backend = as "admin", outs = [], turn = 0, finished = False, losing = False, changes = 0, fresh = False, clicked = [], lastWrite = mempty, seen = [], trace = [], faults = []}

play :: World -> Step -> World
play w = \case
  Click i s -> case atMay ids i of
    Just p
      | not w.finished
      , Just (Right _) <- w.section.known
      , not (Set.member p w.shell.plugins.offForWiki) ->
          afterEvent ("chooses " <> p.text <> " " <> show s) (Chosen p s) w {clicked = (w.turn, p, s) : w.clicked}
    _ -> w
  Handle -> case break (isNothing . (\o -> o.answer)) w.outs of
    (earlier, o : later') ->
      let (backend, a) = handled o.reader o.effect w.backend
          lost = w.losing
          a' = if lost then lostAnswer o.effect else a
          -- A save or a write of an earlier session, done now, is a change
          -- the shell hears of no more than one made elsewhere.
          former = o.inSession /= w.turn && backend /= w.backend
          changes = if lost || former then w.changes + 1 else w.changes
       in noted ("the server handles " <> show o.effect <> if lost then ", its answer lost" else "") w {backend, outs = earlier <> [o {answer = a', changesSeen = changes}] <> later', losing = False, changes, fresh = w.fresh && not lost && not former}
    _ -> w
  Deliver i -> case atMay [(k, o, a) | (k, o) <- zip [0 :: Int ..] w.outs, Just a <- [o.answer]] i of
    Just (k, o, a) ->
      let w' = w {outs = [x | (j, x) <- zip [0 ..] w.outs, j /= k]}
          ours = o.inSession == w.turn
       in case (o.effect, a) of
            (Personal (Load t), Read r) ->
              let w2 = w' {fresh = w'.fresh || (ours && isRight r && o.changesSeen == w.changes), seen = ["off for the wiki, read" | ours, Right k' <- [r], not (Set.null k'.offForWiki)] <> w'.seen}
               in afterEvent "a read comes back" (Loaded t r) w2
            (Personal (Write t p _), Wrote r) ->
              let tag = case r of
                    Right _ -> "a write taken"
                    Left (Invalid _) -> "a write refused"
                    Left _ -> "a write lost"
               in afterEvent "a write comes back" (Written t p r) w' {seen = [tag | ours] <> ["a write taken after a save" | ours, isRight r, "a save taken" `elem` w'.seen] <> w'.seen, lastWrite = if ours then Map.insert p (isRight r) w'.lastWrite else w'.lastWrite}
            (Switch (Wiki.Load t), SetRead r) -> afterSwitch "the wiki's set comes back" (Wiki.Loaded t r) w'
            (Switch (Wiki.Save t _), SetSaved r) -> afterSwitch "a save comes back" (Wiki.Saved t r) w' {seen = ["a save taken" | ours, isRight r] <> w'.seen}
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
  SwitchOpen
    | w.finished -> w
    | otherwise -> afterSwitch "opens the switch" Wiki.Opened w
  SwitchTick i isOff -> case atMay ids i of
    Just p | not w.finished -> afterSwitch ("ticks " <> p.text <> if isOff then " off for everyone" else " on for everyone") (Wiki.Ticked p isOff) w
    _ -> w
  Reopen
    | w.finished -> w
    | otherwise -> afterEvent "opens the section again" Opened w {seen = ["opened again with an ask out" | any (\o -> o.inSession == w.turn) w.outs] <> w.seen}
  SwitchUser
    | w.finished -> w
    | otherwise ->
        let next = w.turn + 1
         in -- The shell gives the new session; the section, open, asks.
            -- The shell reads what is on with the new session.
            afterEvent "the section, open, asks" Opened
              . afterSwitch "the switch hears the session" (Wiki.Given next)
              . afterEvent ("reader " <> readerOf next <> " signs in") (Given next)
              $ w {turn = next, fresh = False, lastWrite = mempty, shell = worldRead (newReader w.shell), seen = ["a switch with an ask out" | any (\o -> o.inSession == w.turn) w.outs] <> w.seen}
  Finish -> afterEvent "goes away" Finished w {finished = True}
  where
    -- The shell's read with the session, numbered among the asks about
    -- the plugins, answered at once.
    worldRead held =
      let (n, held') = sending held
       in readAnswered n (rightToMaybe (pluginsIn w.backend {current = Just (UserId ("user-" <> readerOf (w.turn + 1)))})) held'
    lostAnswer = \case
      Personal (Load _) -> Just (Read (Left (Unreachable "lost")))
      Personal (Write {}) -> Just (Wrote (Left (Unreachable "lost")))
      Personal (Tell _ _) -> Nothing
      Switch (Wiki.Load _) -> Just (SetRead (Left (Unreachable "lost")))
      Switch (Wiki.Save {}) -> Just (SetSaved (Left (Unreachable "lost")))
      Switch (Wiki.Tell _ _) -> Nothing

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
      , shell = shell'
      , sectionSent = sent'
      , faults =
          w.faults
            <> [what <> ": " <> show effects <> " after the section went away" | w.finished, not (null effects)]
            <> [what <> ": two asks out at once: " <> show (map (\o -> o.effect) live) | length live > 1]
            <> [what <> ": tells the shell the plugins off for the wiki after a write: " <> show t | Written {} <- [event], t@(Tell _ (Everything _)) <- effects]
            <> [what <> ": writes " <> p.text <> " " <> show s <> ", not chosen in this session" | Write _ p s <- effects, (w.turn, p, s) `notElem` w.clicked]
            <> [what <> ": tells the shell with no answer taken" | not (null [() | Tell _ _ <- effects]), not (answered event)]
            -- The section opened again finds the shell's model as it is,
            -- a write out with it, and asks nothing once it has read.
            <> [what <> ": asks anew on opening, what is on read already: " <> show effects | Opened <- [event], isJust (w.section.known >>= rightToMaybe), not (null effects)]
      }
  where
    (section, effects) = step event w.section
    -- As the shell carries the effects, in order: an ask numbered as it
    -- is sent, a told part merged in by its ask's number.
    (shell', sent') = foldl' carried (w.shell, w.sectionSent) effects
    carried (held, sent) = \case
      Tell asked told
        | Just (sentAs, n) <- sent, sentAs == asked -> (answeredWith n told held, sent)
        | otherwise -> (held, sent)
      e -> case askOf e of
        Just asked -> let (n, held') = sending held in (held', Just (asked, n))
        Nothing -> (held, sent)
    outs' = w.outs <> [Out {effect = Personal e, reader = readerOf w.turn, inSession = w.turn, answer = Nothing, changesSeen = 0} | e <- effects, isJust (askOf e)]
    live = [o | o <- outs', Personal e <- [o.effect], Just t <- [askOf e], t `madeAfter` section.sessionFrom, o.inSession == w.turn]
    answered = \case
      Written {} -> True
      Loaded {} -> True
      _ -> False

-- | The world after the administration's switch took an event: its asks
-- out, and what it told the shell merged in, as the shell does.
afterSwitch :: Text -> Wiki.Event -> World -> World
afterSwitch what event w =
  checked
    . noted (what <> if null effects then "" else " -> " <> show effects)
    $ w
      { switch
      , outs = w.outs <> [Out {effect = Switch e, reader = readerOf w.turn, inSession = w.turn, answer = Nothing, changesSeen = 0} | e <- effects, isJust (Wiki.askOf e)]
      , shell = shell'
      , switchSent = sent'
      , faults = w.faults <> [what <> ": two saves or reads out at once" | length live > 1]
      }
  where
    (switch, effects) = Wiki.step event w.switch
    (shell', sent') = foldl' carried (w.shell, w.switchSent) effects
    carried (held, sent) = \case
      Wiki.Tell asked off
        | Just (sentAs, n) <- sent, sentAs == asked -> (offAnswered n off held, sent)
        | otherwise -> (held, sent)
      e -> case Wiki.askOf e of
        Just asked -> let (n, held') = sending held in (held', Just (asked, n))
        Nothing -> (held, sent)
    live = [o | o <- w.outs <> [Out {effect = Switch e, reader = "", inSession = w.turn, answer = Nothing, changesSeen = 0} | e <- effects, isJust (Wiki.askOf e)], Switch e <- [o.effect], Just t <- [Wiki.askOf e], t `madeAfter` switch.sessionFrom, o.inSession == w.turn]

-- | The rules, checked after every step: once the section has read what
-- the server holds since the last change elsewhere, and nothing it asked
-- is out or waits, it shows exactly what the server holds for its reader,
-- with no switch left showing a write that is not out.
checked :: World -> World
checked w = w {faults = w.faults <> if w.finished then [] else settledRule, seen = ["checked settled after a write" | quiet, w.fresh, not w.finished, "a write taken" `elem` w.seen] <> w.seen}
  where
    quiet = null [o | o <- w.outs, o.inSession == w.turn] && null w.section.queued && isNothing w.switch.queued
    serverNow = pluginsIn w.backend {current = Just (UserId ("user-" <> readerOf w.turn))}
    settledRule =
      [ "settled, but shows the choices " <> show ((\k -> k.mine) <$> (w.section.known >>= rightToMaybe)) <> " where the server has " <> show serverNow
      | quiet
      , w.fresh
      , ((\k -> k.mine) <$> (w.section.known >>= rightToMaybe)) /= ((\k -> k.mine) <$> rightToMaybe serverNow)
      ]
        -- The shell's one value: the last answer about each part wins,
        -- whichever machine took it.
        <> [ "settled, but the shell holds " <> show w.shell.plugins <> " where the server has " <> show serverNow
           | quiet
           , w.fresh
           , Right w.shell.plugins /= serverNow
           ]
        <> [ "settled, but its switches show writes not out: " <> show w.section.writing
           | quiet
           , not (Map.null w.section.writing)
           ]
        -- The user's choices it holds, once read, the shell has been told:
        -- the editor runs the plugins the switches show.
        <> [ "holds the choices " <> show held.mine <> ", but the shell " <> show w.shell.plugins.mine
           | Just held <- [w.section.known >>= rightToMaybe]
           , w.shell.plugins.mine /= held.mine
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
              . QC.cover 1 (reached "checked settled after a write") "checked settled after a write"
              . QC.cover 3 (reached "opened again with an ask out") "opened again with an ask out"
              . QC.cover 1 (reached "a write refused") "a write refused"
              . QC.cover 2 (reached "a write lost") "a write lost"
              . QC.cover 10 (reached "off for the wiki, read") "off for the wiki, read"
              . QC.cover 10 (reached "a switch with an ask out") "a switch with an ask out"
              . QC.cover 1 (reached "a write taken after a save") "a write taken after a save"
              $ counterexample (toS (T.unlines (reverse w.trace) <> "\n" <> T.unlines w.faults)) (null w.faults)

  it "sends one ask at a time: a switch flipped while the read is out waits, and the later choice of a plugin takes its place" $ do
    let w = runOf [Click 0 PluginOff]
    -- Nothing is shown before the first read: nothing to click.
    length w.outs `shouldBe` 1
    let w1 = runOf [Handle, Deliver 0, Click 0 PluginOff, Click 1 PluginOff, Click 0 PluginOn]
    length w1.outs `shouldBe` 1
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
    -- The administrator's first write was out and done; the second waited, and
    -- went with the administrator's session.
    Map.lookup (UserId "user-admin") w.backend.pluginChoices `shouldBe` Just (Map.fromList [(assistant, choice PluginOff)])
    Map.lookup (UserId "user-editor") w.backend.pluginChoices `shouldBe` Nothing
    w.faults `shouldBe` []

  -- Codex's third round on #374, as a run: the administrator reads the
  -- section, turns the assistant off for everyone, then turns errors off
  -- for themselves.
  it "keeps the wiki's set the switch told when the section's next write is answered" $ do
    let w = runOf [Handle, Deliver 0, SwitchOpen, Handle, Deliver 0, SwitchTick 0 True, Handle, Deliver 0, Click 1 PluginOff, Handle, Deliver 0]
    w.shell.plugins.offForWiki `shouldBe` Set.fromList [assistant]
    w.shell.plugins.mine `shouldBe` Map.fromList [(errors, choice PluginOff)]
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
    told `shouldSatisfy` \case [Wiki.Tell _ off] -> off == mempty; _ -> False
    let (m3, save) = second onlyEffect (Wiki.step (Wiki.Ticked errors True) m2)
    save `shouldSatisfy` \case
      Wiki.Save _ s -> s == Set.fromList [errors]
      _ -> False
    Wiki.shownOff m3 `shouldBe` Just (Set.fromList [errors])
    let (m4, fx) = Wiki.step (Wiki.Saved (ticket save) (Right WikiPlugins {off = Set.fromList [errors]})) m3
    fx `shouldSatisfy` \case [Wiki.Tell _ off] -> off == Set.fromList [errors]; _ -> False
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
    fx `shouldSatisfy` \case [Wiki.Tell _ off] -> off == Set.fromList [errors]; _ -> False
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

-- | Which plugins are on, as the settings change it: the in-memory
-- backend's rules ("Wiki.App.Backend.Mem.State"'s 'setPluginStateIn' and
-- 'setWikiPluginsIn', the demo's and the native tests' server), and the
-- plugins' state ("Wiki.App.View.Plugins.Machine"), the one machine the
-- shell keeps for the settings' section "Plugins", the administration's
-- switch and the context's plugins, on generated runs.
--
-- = The model of the server
--
-- The runs' server is the in-memory state itself, with two readers, the
-- demo's editor and its administrator. Its rules are @wiki-server@'s
-- ("Wiki.Server.Storage.Plugins", "Wiki.Server.Plugins"): the id's form,
-- a first choice refused once a user has choices kept for 100 plugins, a
-- plugin given back to its default with no options no longer kept, the
-- plugins off for the wiki kept apart from the users' choices, and the
-- administrator's set set by an administrator only. The first group of
-- this spec checks each against the server's words. Besides the machine's
-- asks, the wiki changes elsewhere: another administrator turns a plugin
-- off or on for everyone ('WikiOff'), another tab of the reader sets a
-- state ('OtherTab'), or fills the reader's choices up to the bound
-- ('Fill'), so that a write is refused. An answer can be lost on the way
-- after the server handled the ask ('Lose'): the machine hears
-- 'Unreachable'.
--
-- The reader flips switches in the section ('Click') and boxes in the
-- administration ('Tick'), the shell loads the session again ('Reload'),
-- a view opens ('OpenView'), and another reader signs in ('SwitchUser');
-- the server refuses the editor's saves of the wiki's set.
--
-- Each ask has three moments: the shell sends it, the server handles it
-- (a read takes what is on then; a write changes it and answers), and the
-- answer comes back. The server handles asks in the order sent, and the
-- answers come back in any order ('Handle', 'Deliver').
--
-- = The HTTP backend's cache
--
-- The runs go once over the in-memory backend, which caches nothing, and
-- once over the HTTP backend's cache ("Wiki.App.Backend.Http.Cache"),
-- with its own functions: the first load's read is in it, as the page the
-- server prerendered carries it; a read is answered from it at once when
-- it holds what is on, and otherwise stores its answer if the cache did
-- not change since the read was sent; a write's answer, failed or not,
-- and a sign-in empty it. Changes elsewhere do not reach it.
--
-- = The rules
--
-- One ask out at a time, across reads, writes and saves, and across
-- sessions; no write that was not asked for in the session it goes out
-- in; no answer of an earlier session changes what the views show; no
-- read anew when a view opens on what is read. Once nothing is out, waits
-- or is wanted: a read was sent after every load of the session and every
-- answer of a write or a save; if a read handled after every change
-- elsewhere was taken, the value is the server's; no switch or box shows
-- a change that is not out. A refusal is not shown once the last answer
-- about its plugin, or the last save's, is a success. Each rule is shown
-- able to fail, with a fault put in the step.
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
import Wiki.App.Backend.Http.Cache (Cache (plugins), Reading (..), Stored (cache), afterWrite, changedBy, emptyCache, kept, storedAnswer, storedOf)
import Wiki.App.Backend.Mem.State (MemState (..), pluginsIn, setPluginStateIn, setWikiPluginsIn, wikiPluginsIn)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.View.Latest (madeAfter)
import Wiki.App.View.Plugins.Machine
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
-- Runs of the plugins' state

-- | The plugins of the runs: the registry's.
ids :: [PluginId]
ids = map (\m -> m.id) manifests

-- | The reader a session's number names: the two take turns, the
-- administrator first, to whom the server lets the saves of the plugins
-- off for the wiki through.
readerOf :: Int -> Text
readerOf s = if even s then "admin" else "editor"

-- | 'Answer' is what the server answered an ask.
data Answer
  = Read (Either WikiError Plugins)
  | Wrote (Either WikiError PluginChoice)
  | SetSaved (Either WikiError WikiPlugins)
  deriving stock (Show)

-- | The server's handling of an ask sent with a reader's cookie.
handled :: Text -> Effect -> MemState -> (MemState, Answer)
handled u e st0 = case e of
  Load _ -> (st0, Read (pluginsIn st))
  WriteState _ p s -> case setPluginStateIn p s st of
    Left err -> (st0, Wrote (Left err))
    Right (st', c) -> (st' {current = st0.current}, Wrote (Right c))
  SaveOff _ off -> case setWikiPluginsIn WikiPlugins {off} st of
    Left err -> (st0, SetSaved (Left err))
    Right (st', w) -> (st' {current = st0.current}, SetSaved (Right w))
  where
    st = st0 {current = Just (UserId ("user-" <> u))}

-- | 'Step' is one thing that happens in a run. A step that cannot happen in
-- the state it comes to is skipped, so that a run shrinks to any part.
data Step
  = -- | The reader chooses a state for the plugin at this place in the
    -- settings' section.
    Click Int PluginState
  | -- | The reader ticks the box "off for everyone" of the plugin at this
    -- place, or clears it, in the administration.
    Tick Int Bool
  | -- | The server handles the oldest ask it has not handled.
    Handle
  | -- | The handled answer at this place among those handled comes back.
    Deliver Int
  | -- | The shell loads the session again (after a save of a page, say).
    Reload
  | -- | A view of the plugins opens.
    OpenView
  | -- | Another administrator turns the plugin at this place off for
    -- everyone ('True'), or on.
    WikiOff Int Bool
  | -- | Another tab of the reader sets the plugin at this place.
    OtherTab Int PluginState
  | -- | Another tab fills the reader's choices up to the bound.
    Fill
  | -- | The answer to the next ask handled is lost on the way.
    Lose
  | -- | Another reader signs in.
    SwitchUser
  deriving stock (Eq, Show)

genRun :: Gen [Step]
genRun =
  QC.scale (* 2) . listOf . frequency $
    [ (8, Click <$> place <*> elements [minBound .. maxBound])
    , (4, Tick <$> place <*> elements [True, False])
    , (10, pure Handle)
    , (10, Deliver <$> frequency [(6, pure 0), (1, pure 1)])
    , (3, pure Reload)
    , (1, pure OpenView)
    , (2, WikiOff <$> place <*> elements [True, False])
    , (2, OtherTab <$> place <*> elements [minBound .. maxBound])
    , (1, pure Fill)
    , (1, pure Lose)
    , (1, pure SwitchUser)
    ]
  where
    place = chooseInt (0, length ids - 1)

shrinkRun :: [Step] -> [[Step]]
shrinkRun = shrinkList $ \case
  Deliver i -> Deliver <$> [0 .. i - 1]
  _ -> []

-- | An ask the shell made for the plugins' state.
data Out = Out
  { effect :: Effect
  , reader :: Text
  , inSession :: Int
  , answer :: Maybe Answer
  , changesSeen :: Int
  -- ^ How many changes elsewhere (and answers lost) there had been when
  -- the server handled it, or, for a read the cache answered, when the
  -- server handled the read whose answer the cache kept.
  , sentWith :: Maybe Stored
  -- ^ Over the HTTP backend's cache, for a read sent to the server: the
  -- cache as it was when the read was sent ('storedAnswer').
  , fromCache :: Bool
  -- ^ Whether the HTTP backend's cache answered it, without the server.
  , owedSeen :: Int
  -- ^ How many reads were owed when it was sent ('owed').
  }
  deriving stock (Show)

-- | A run under way.
data World = World
  { model :: Model
  , stepper :: Event -> Model -> (Model, [Effect])
  -- ^ The machine's step: 'step', or a fault put in to show a rule able
  -- to fail.
  , backend :: MemState
  , tab :: Maybe Stored
  -- ^ The HTTP backend's cache, when the run is over it; 'Nothing' for a
  -- run over the in-memory backend, which caches nothing.
  , cacheSeen :: Int
  -- ^ The changes elsewhere seen by the read whose answer the cache keeps.
  , outs :: [Out]
  , turn :: Int
  , losing :: Bool
  -- ^ Whether the next answer handled is lost.
  , changes :: Int
  -- ^ The changes made elsewhere, and the answers lost, so far.
  , fresh :: Bool
  -- ^ Whether the model has taken, in this session, a read the server
  -- handled after every change elsewhere and every answer lost: from then
  -- on, what it knows is the server's.
  , owed :: Int
  -- ^ How many reads the model owes: one for each load of the session,
  -- and one for each answer of a write or a save of this session, which
  -- says one part of what is on only.
  , paid :: Int
  -- ^ The most reads owed when a read whose answer came was sent.
  , chosen :: [(Int, Write)]
  -- ^ The writes the reader asked for, with the session they were asked
  -- in.
  , lastWrite :: Map PluginId Bool
  -- ^ Whether the last answer to a write of each plugin taken in this
  -- session was a success.
  , lastSave :: Maybe Bool
  -- ^ Whether the last answer to a save taken in this session was one.
  , otherTab :: Bool
  -- ^ Whether another tab of the reader made a choice in this run.
  , learntOther :: Bool
  -- ^ Whether the model took a read handled after the other tab's choice
  -- and every other change.
  , seen :: [Text]
  , trace :: [Text]
  , faults :: [Text]
  }

-- | The shell started, its first load read with it, over the in-memory
-- backend.
start :: World
start = startOver Nothing

-- | The shell started over the HTTP backend's cache, which holds the
-- first load's read, as the page the server prerendered carries it.
startCached :: World
startCached = startOver (Just (storedOf emptyCache))

startOver :: Maybe Stored -> World
startOver tab =
  World
    { model = initialWith (rightToMaybe firstRead) 0
    , stepper = step
    , backend = as "admin"
    , tab = (\t -> t {cache = (t.cache) {plugins = Just firstRead}}) <$> tab
    , cacheSeen = 0
    , outs = []
    , turn = 0
    , losing = False
    , changes = 0
    , fresh = True
    , owed = 0
    , paid = 0
    , chosen = []
    , lastWrite = mempty
    , lastSave = Nothing
    , otherTab = False
    , learntOther = False
    , seen = []
    , trace = []
    , faults = []
    }
  where
    firstRead = pluginsIn (as "admin")

play :: World -> Step -> World
play w = \case
  Click i s -> case (atMay ids i, knownNow w.model) of
    (Just p, Just known)
      | not (Set.member p known.offForWiki) ->
          afterEvent ("chooses " <> p.text <> " " <> show s) (Chosen p s) w {chosen = (w.turn, StateWrite p s) : w.chosen, seen = ["a choice after a read learnt another tab's" | w.learntOther] <> w.seen}
    _ -> w
  Tick i isOff -> case (atMay ids i, shownOff w.model) of
    (Just p, Just now') ->
      let next = (if isOff then Set.insert else Set.delete) p now'
       in afterEvent ("ticks " <> p.text <> if isOff then " off for everyone" else " on for everyone") (Ticked p isOff) w {chosen = (w.turn, OffWrite next) : w.chosen}
    _ -> w
  Handle -> case break (isNothing . (\o -> o.answer)) w.outs of
    (earlier, o : later') ->
      let (backend, a) = handled o.reader o.effect w.backend
          lost = w.losing
          a' = if lost then lostAnswer o.effect else a
          changes = if lost then w.changes + 1 else w.changes
       in noted ("the server handles " <> show o.effect <> if lost then ", its answer lost" else "") w {backend, outs = earlier <> [o {answer = Just a', changesSeen = changes}] <> later', losing = False, changes, fresh = w.fresh && not lost}
    _ -> w
  Deliver i -> case atMay [(k, o, a) | (k, o) <- zip [0 :: Int ..] w.outs, Just a <- [o.answer]] i of
    Just (k, o, a) ->
      let ours = o.inSession == w.turn
          -- The HTTP backend has done its part before the machine hears
          -- the answer: a read's answer stored, the cache emptied by a
          -- write, whatever it answered.
          w' = arrived o a w {outs = [x | (j, x) <- zip [0 ..] w.outs, j /= k]}
          w2 = case (o.effect, a) of
            (Load t, Read r) ->
              let upToDate = ours && isRight r && o.changesSeen == w.changes
               in afterEvent (if o.fromCache then "a read comes back from the cache" else "a read comes back") (Loaded t r) $
                    w'
                      { fresh = w'.fresh || upToDate
                      , paid = if ours then max w'.paid o.owedSeen else w'.paid
                      , learntOther = w'.learntOther || (upToDate && w'.otherTab)
                      , seen = ["off for the wiki, read" | ours, Right k' <- [r], not (Set.null k'.offForWiki)] <> ["a read from the cache" | o.fromCache] <> w'.seen
                      }
            (WriteState t p _, Wrote r) ->
              let tag = case r of
                    Right _ -> "a write taken"
                    Left (Invalid _) -> "a write refused"
                    Left _ -> "a write lost"
               in afterEvent "a write comes back" (Written t p r) $
                    w'
                      { seen = [tag | ours] <> ["a write after a save" | ours, isRight r, "a save taken" `elem` w'.seen] <> w'.seen
                      , lastWrite = if ours then Map.insert p (isRight r) w'.lastWrite else w'.lastWrite
                      , owed = if ours then w'.owed + 1 else w'.owed
                      }
            (SaveOff t _, SetSaved r) ->
              afterEvent "a save comes back" (Saved t r) $
                w'
                  { seen = ["a save taken" | ours, isRight r] <> w'.seen
                  , lastSave = if ours then Just (isRight r) else w'.lastSave
                  , owed = if ours then w'.owed + 1 else w'.owed
                  }
            _ -> w'
       in -- An answer to an ask of an earlier session changes nothing the
          -- views show; its only effect is the next ask, in its turn.
          w2 {faults = w2.faults <> ["the answer to " <> show o.effect <> ", of an earlier session, changed what is shown" | not ours, shown w2.model /= shown w.model]}
    Nothing -> w
  Reload ->
    afterEvent "the shell loads the session" Refresh $
      w
        { owed = w.owed + 1
        , seen = ["a load with an ask out" | not (null w.outs)] <> ["a load with a save out" | any (isSave . (\o -> o.effect)) w.outs] <> w.seen
        }
  OpenView -> afterEvent "a view opens" Opened w {seen = ["a view opened with an ask out" | not (null w.outs)] <> w.seen}
  WikiOff i isOff -> case atMay ids i of
    Just p ->
      let st = w.backend
          off' = (if isOff then Set.insert else Set.delete) p st.pluginsOff
       in elsewhere ("another administrator turns " <> p.text <> if isOff then " off" else " on") w {backend = st {pluginsOff = off'}}
    Nothing -> w
  OtherTab i s -> case atMay ids i of
    Just p -> case setPluginStateIn p s w.backend {current = Just (UserId ("user-" <> readerOf w.turn))} of
      Right (st', _) -> elsewhere ("another tab sets " <> p.text <> " " <> show s) w {backend = st' {current = w.backend.current}, otherTab = True, learntOther = False}
      Left _ -> w
    Nothing -> w
  Fill ->
    let u = UserId ("user-" <> readerOf w.turn)
        st = w.backend
        others = Map.fromList [(PluginId ("other" <> show k), choice PluginOn) | k <- [1 .. maxPluginsPerUser]]
     in elsewhere "another tab fills the choices" w {backend = st {pluginChoices = Map.insertWith (\_ old -> Map.union old (Map.fromList (take (maxPluginsPerUser - Map.size old) (Map.toList others)))) u others st.pluginChoices}}
  Lose -> noted "the next answer will be lost" w {losing = True}
  SwitchUser ->
    let next = w.turn + 1
     in -- The shell gives the new session, and loads it. The sign-in
        -- empties the HTTP backend's cache.
        afterEvent "the shell loads the new session" Refresh
          . (\w' -> w' {owed = w'.owed + 1})
          . afterEvent ("reader " <> readerOf next <> " signs in") (Given next)
          $ w
            { turn = next
            , fresh = False
            , tab = changedBy (const emptyCache) <$> w.tab
            , lastWrite = mempty
            , lastSave = Nothing
            , otherTab = False
            , learntOther = False
            , seen = ["a switch with an ask out" | not (null w.outs)] <> w.seen
            }
  where
    lostAnswer = \case
      Load _ -> Read (Left (Unreachable "lost"))
      WriteState {} -> Wrote (Left (Unreachable "lost"))
      SaveOff {} -> SetSaved (Left (Unreachable "lost"))
    isSave = \case
      SaveOff {} -> True
      _ -> False

-- | What the views show of the model: what is known, the changes not done,
-- and the refusals.
shown :: Model -> (Maybe (Either WikiError Plugins), Map PluginId PluginState, Maybe (Set PluginId), Map PluginId WikiError, Maybe WikiError, [(Int, Write)])
shown m = (m.known, m.writing, m.wantedOff, m.refused, m.problem, m.queued)

-- | The HTTP backend's cache after an answer arrived, before the machine
-- hears it: a read sent to the server stores its answer, unless the cache
-- changed since it was sent; a write empties the cache.
arrived :: Out -> Answer -> World -> World
arrived o a w = case (w.tab, a) of
  (Nothing, _) -> w
  (Just t, Read r)
    | o.fromCache -> w
    | Just sent <- o.sentWith
    , kept r ->
        let t' = storedAnswer PlainRead sent (\c -> c {plugins = Just r}) t
         in w {tab = Just t', cacheSeen = if t' /= t then o.changesSeen else w.cacheSeen}
    | otherwise -> w
  (Just t, _) -> w {tab = Just (changedBy afterWrite t)}

-- | A change made elsewhere: what the model knows may be old until its
-- next read.
elsewhere :: Text -> World -> World
elsewhere what w = noted what w {changes = w.changes + 1, fresh = False}

noted :: Text -> World -> World
noted what w = w {trace = what : w.trace}

-- | The world after the model took an event, its asks out, and the rules
-- checked. Over the HTTP backend's cache, a read the cache can answer is
-- answered at once, without the server.
afterEvent :: Text -> Event -> World -> World
afterEvent what event w =
  checked
    . noted (what <> if null effects then "" else " -> " <> show effects)
    $ w
      { model
      , outs = outs'
      , faults =
          w.faults
            -- One ask out at a time, across reads, writes and saves, and
            -- across sessions.
            <> [what <> ": two asks out at once: " <> show (map (\o -> o.effect) outs') | length outs' > 1]
            <> [what <> ": writes " <> show x <> ", not asked for in this session" | x <- writesOf effects, (w.turn, x) `notElem` w.chosen]
            -- A view opened again finds the state as it is, a write out
            -- with it, and asks nothing once it has read.
            <> [what <> ": asks anew on opening, what is on read already: " <> show effects | Opened <- [event], isJust (knownNow w.model), not (null effects)]
      }
  where
    (model, effects) = w.stepper event w.model
    outs' = w.outs <> [sent e | e <- effects, isJust (askOf e)]
    sent e =
      let o = Out {effect = e, reader = readerOf w.turn, inSession = w.turn, answer = Nothing, changesSeen = 0, sentWith = Nothing, fromCache = False, owedSeen = w.owed}
       in case (e, w.tab) of
            (Load _, Just t) -> case t.cache.plugins of
              Just r -> o {answer = Just (Read r), changesSeen = w.cacheSeen, fromCache = True}
              Nothing -> o {sentWith = Just t}
            _ -> o
    writesOf = mapMaybe $ \case
      WriteState _ p s -> Just (StateWrite p s)
      SaveOff _ off -> Just (OffWrite off)
      Load _ -> Nothing

-- | The rules, checked after every step. Once nothing the model asked is
-- out, waits or is wanted: it has read since every load of the session and
-- every answer of a write or a save; if it has read what the server holds
-- since the last change elsewhere, it holds exactly what the server holds
-- for its reader; and no switch or box shows a change that is not out. A
-- refusal stays only while the last answer about its plugin, or the last
-- save's, is one.
checked :: World -> World
checked w = w {faults = w.faults <> settledRule, seen = ["checked settled after a write" | quiet, w.fresh, "a write taken" `elem` w.seen] <> w.seen}
  where
    quiet = null w.outs && null w.model.queued && not w.model.wanted
    serverNow = pluginsIn w.backend {current = Just (UserId ("user-" <> readerOf w.turn))}
    settledRule =
      [ "settled, but no read was sent after the last load or answer that owed one"
      | quiet
      , w.paid < w.owed
      ]
        <> [ "settled, but holds " <> show w.model.known <> " where the server has " <> show serverNow
           | quiet
           , w.fresh
           , w.model.known /= Just serverNow
           ]
        <> [ "settled, but shows changes not out: " <> show (w.model.writing, w.model.wantedOff)
           | quiet
           , not (Map.null w.model.writing) || isJust w.model.wantedOff
           ]
        <> [ "shows a refusal for " <> p.text <> ", whose last write succeeded"
           | (p, True) <- Map.toList w.lastWrite
           , Map.member p w.model.refused
           ]
        <> [ "shows the refusal of a save, after a save succeeded"
           | w.lastSave == Just True
           , isJust w.model.problem
           ]

runOf :: [Step] -> World
runOf = foldl' play start

-- | A run with a fault put in the machine's step.
runFaulty :: (Event -> Model -> (Model, [Effect])) -> [Step] -> World
runFaulty faulty = foldl' play start {stepper = faulty}

machine :: Spec
machine = describe "the plugins' state, on generated runs" $ do
  for_ [("", start, []), (", over the HTTP backend's cache", startCached, ["a read from the cache"])] $ \(how, w0, more) ->
    it ("keeps every rule on any run" <> how) $
      QC.property . QC.checkCoverage . withNumTests 20000 $
        forAllShrinkShow genRun shrinkRun show $ \run ->
          let w = foldl' play w0 run
              reached tag = tag `elem` w.seen
              covers =
                [ (15, "a write taken")
                , (5, "a save taken")
                , (3, "a write after a save")
                , (3, "checked settled after a write")
                , (10, "a load with an ask out")
                , (3, "a load with a save out")
                , (1, "a view opened with an ask out")
                , (1, "a write refused")
                , (2, "a write lost")
                , (10, "off for the wiki, read")
                , (10, "a switch with an ask out")
                , (2, "a choice after a read learnt another tab's")
                ]
                  <> [(5, tag) | tag <- more]
           in foldr (\(n, tag) -> QC.cover n (reached tag) (toS tag)) (counterexample (toS (T.unlines (reverse w.trace) <> "\n" <> T.unlines w.faults)) (null w.faults)) covers

  it "sends one ask at a time: a switch flipped while a read is out waits, and the later choice of a plugin takes its place" $ do
    let w = runOf [Reload, Click 0 PluginOff, Click 1 PluginOff, Click 0 PluginOn]
    length w.outs `shouldBe` 1
    w.model.queued `shouldBe` [(0, StateWrite errors PluginOff), (0, StateWrite assistant PluginOn)]

  it "shows the state chosen while its write is out, and the server's once it answered and was read again" $ do
    let w = runOf [Click 0 PluginOff]
    shownState w.model assistant `shouldBe` PluginOff
    let w1 = foldl' play w [Handle, Deliver 0, Handle, Deliver 0]
    w1.model.writing `shouldBe` mempty
    stateOf (sure (maybeToRight () (knownNow w1.model))) assistant `shouldBe` PluginOff
    w1.outs `shouldSatisfy` null

  it "drops a write chosen before another reader signed in, and reads for the new one after the write out answered" $ do
    let w = runOf [Click 0 PluginOff, Click 1 PluginOff, SwitchUser]
    -- The new session's read waits for the write out.
    map (\o -> o.effect) w.outs `shouldSatisfy` \case
      [WriteState _ p PluginOff] -> p == assistant
      _ -> False
    let w1 = foldl' play w [Handle, Deliver 0, Handle, Deliver 0]
    -- The administrator's first write was out and done; the second waited,
    -- and went with the administrator's session.
    Map.lookup (UserId "user-admin") w1.backend.pluginChoices `shouldBe` Just (Map.fromList [(assistant, choice PluginOff)])
    Map.lookup (UserId "user-editor") w1.backend.pluginChoices `shouldBe` Nothing
    knownNow w1.model `shouldBe` Just Plugins {offForWiki = mempty, mine = mempty}
    w1.faults `shouldBe` []

  it "keeps the plugins off for the wiki across a sign-in, until the new reader's read answers" $ do
    let w = runOf [Tick 1 True, Handle, Deliver 0, Handle, Deliver 0, SwitchUser]
    w.model.known `shouldBe` Nothing
    (held w.model).offForWiki `shouldBe` Set.fromList [errors]
    (held w.model).mine `shouldBe` mempty
    let w1 = foldl' play w [Lose, Handle, Deliver 0]
    w1.model.known `shouldBe` Just (Left (Unreachable "lost"))
    (held w1.model).offForWiki `shouldBe` Set.fromList [errors]

  -- Codex's fourth round on #374. The administrator saves "off for
  -- everyone", and the shell loads the session, or a view opens, before
  -- the save answers: the read waits for the save's answer, and sees the
  -- save, from the server and not from the cache the save empties.
  for_ [("", start), (", over the HTTP backend's cache", startCached)] $ \(how, w0) ->
    it ("reads what a save out changed, not beside it" <> how) $ do
      let w = foldl' play w0 [Tick 0 True, OpenView, Reload]
      length w.outs `shouldBe` 1
      let w1 = foldl' play w [Handle, Deliver 0, Handle, Deliver 0]
      w1.outs `shouldSatisfy` null
      (\k -> k.offForWiki) <$> knownNow w1.model `shouldBe` Just (Set.fromList [assistant])
      (held w1.model).offForWiki `shouldBe` Set.fromList [assistant]
      w1.faults `shouldBe` []

  -- The other: a load of the session learns a choice made in another tab,
  -- and the section's next choice keeps it, there being one value.
  it "keeps a choice another tab made, which a load learnt, when the section chooses next" $ do
    let w = runOf [OtherTab 0 PluginOff, Reload, Handle, Deliver 0]
    (\k -> k.mine) <$> knownNow w.model `shouldBe` Just (Map.fromList [(assistant, choice PluginOff)])
    let w1 = foldl' play w [Click 1 PluginOff, Handle, Deliver 0]
    (\k -> k.mine) <$> knownNow w1.model `shouldBe` Just (Map.fromList [(assistant, choice PluginOff), (errors, choice PluginOff)])
    let w2 = foldl' play w1 [Handle, Deliver 0]
    (\k -> k.mine) <$> knownNow w2.model `shouldBe` Just (Map.fromList [(assistant, choice PluginOff), (errors, choice PluginOff)])
    w2.faults `shouldBe` []

  -- Each rule, with a fault put in the step, and a run that shows it: so
  -- that a rule that says something stays as it was is known able to fail.
  describe "is shown able to fail, rule by rule" $ do
    let fails faulty run rule = do
          let w = runFaulty faulty run
          w.faults `shouldSatisfy` any (T.isInfixOf rule)
          -- And the same run with the real step keeps every rule.
          (runOf run).faults `shouldBe` []
    it "one ask at a time: a load that reads beside the save out" $
      fails (\e m -> case e of Refresh -> step e m {flight = Nothing}; _ -> step e m) [Tick 0 True, Reload] "two asks out at once"
    it "a write chosen in this session: a write queued before the sign-in, sent after it" $
      fails (\e m -> case e of Given n -> first (\m' -> m' {queued = [(n, x) | (_, x) <- m.queued]}) (step e m); _ -> step e m) [Click 0 PluginOff, Click 1 PluginOff, SwitchUser, Handle, Deliver 0] "not asked for in this session"
    it "nothing from an earlier session: a lost write's refusal shown to the next reader" $
      fails
        ( \e m -> case e of
            Written t p (Left err) | not (t `madeAfter` m.sessionFrom) -> first (\m' -> m' {refused = Map.insert p err m'.refused}) (step e m)
            _ -> step e m
        )
        [Click 0 PluginOff, Lose, SwitchUser, Handle, Deliver 0]
        "of an earlier session, changed what is shown"
    it "a read after a write: a write's answer taken with no read after it" $
      fails
        ( \e m -> case (e, step e m) of
            (Written {}, (m', [Load _])) -> (m' {flight = Nothing}, [])
            (_, r) -> r
        )
        [Click 0 PluginOff, Handle, Deliver 0]
        "no read was sent after"
    it "a read after a load: a load of the session that reads nothing" $
      fails (\e m -> case e of Refresh -> (m, []); _ -> step e m) [Reload] "no read was sent after"
    it "the server's value once settled: a read's answer not taken" $
      fails (\e m -> case e of Loaded {} -> first (\m' -> m' {known = m.known}) (step e m); _ -> step e m) [OtherTab 0 PluginOff, Reload, Handle, Deliver 0] "where the server has"
    it "a refusal cleared by a later success: a write's success that keeps it" $
      fails
        (\e m -> case e of Written {} -> first (\m' -> m' {refused = Map.union m'.refused m.refused}) (step e m); _ -> step e m)
        [Click 0 PluginOff, Click 0 PluginOn, Lose, Handle, Deliver 0, Handle, Deliver 0]
        "whose last write succeeded"
    it "no read anew on opening: a view that reads what is read already" $
      fails (\e m -> case e of Opened -> step Refresh m; _ -> step e m) [OpenView] "asks anew on opening"

-- | The one effect of a step that has one.
onlyEffect :: [a] -> a
onlyEffect = \case
  [x] -> x
  _ -> panic "not one effect"

-- | The administration's switch, step by step: its events are the
-- plugins' state's.
wikiSwitch :: Spec
wikiSwitch = describe "the administration's switch, on the plugins' state" $ do
  let ticket e = fromMaybe (panic "no ask") (askOf e)
      read0 = initialWith (Just Plugins {offForWiki = mempty, mine = mempty}) 0
  it "saves the set ticked, shows it while out, and reads after the save" $ do
    let (m1, save) = second onlyEffect (step (Ticked errors True) read0)
    save `shouldSatisfy` \case
      SaveOff _ s -> s == Set.fromList [errors]
      _ -> False
    shownOff m1 `shouldBe` Just (Set.fromList [errors])
    let (m2, load) = second onlyEffect (step (Saved (ticket save) (Right WikiPlugins {off = Set.fromList [errors]})) m1)
    load `shouldSatisfy` \case
      Load _ -> True
      _ -> False
    (\k -> k.offForWiki) <$> knownNow m2 `shouldBe` Just (Set.fromList [errors])
    m2.wantedOff `shouldBe` Nothing

  it "saves once at a time, the last set wanted, and keeps ids it does not list" $ do
    let start' = initialWith (Just Plugins {offForWiki = Set.fromList [PluginId "elsewhere"], mine = mempty}) 0
        (m1, save) = second onlyEffect (step (Ticked errors True) start')
        (m2, more) = step (Ticked assistant True) m1
        (m3, more') = step (Ticked errors False) m2
    more <> more' `shouldBe` []
    m3.queued `shouldBe` [(0, OffWrite (Set.fromList [PluginId "elsewhere", assistant]))]
    let (_, next) = step (Saved (ticket save) (Left (Unreachable "lost"))) m3
    next `shouldSatisfy` \case
      [SaveOff _ s] -> s == Set.fromList [PluginId "elsewhere", assistant]
      _ -> False

  it "clears a refusal once a later save succeeds" $ do
    let (m1, save) = second onlyEffect (step (Ticked errors True) read0)
        (m2, _) = step (Ticked assistant True) m1
        (m3, again) = second onlyEffect (step (Saved (ticket save) (Left (Invalid "no"))) m2)
    m3.problem `shouldBe` Just (Invalid "no")
    let (m4, _) = step (Saved (ticket again) (Right WikiPlugins {off = Set.fromList [errors, assistant]})) m3
    m4.problem `shouldBe` Nothing

  it "drops a save queued by the session before, and sends nothing for it" $ do
    let (m1, save) = second onlyEffect (step (Ticked errors True) read0)
        (m2, _) = step (Ticked assistant True) m1
        (m3, given) = step (Given 1) m2
    given `shouldBe` []
    m3.queued `shouldBe` []
    let (m4, fx) = step (Saved (ticket save) (Right WikiPlugins {off = Set.fromList [errors]})) m3
    fx `shouldBe` []
    m4.known `shouldBe` Nothing

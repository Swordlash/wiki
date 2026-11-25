-- | 'Model' is everything the application knows of which plugins are on,
-- and 'step' is the one place it changes (decision 82 of docs/PLAN.md):
-- the one value of what is on ('known': the plugins off for the wiki and
-- the user's own states), what the reader asked to change and is not done
-- yet, and why a change was refused. The shell keeps it ("Wiki.App.Shell")
-- and carries out its effects, so that an ask goes on after the view that
-- made it is gone. Every view of the plugins draws it: the editor, through
-- the context ('Wiki.App.Plugin.Registry.pluginsOn'), the settings'
-- section "Plugins" ("Wiki.App.View.Plugins") and the administration's
-- switch ("Wiki.App.View.WikiPlugins"), whose acts are its events. A
-- plugin's own surfaces (its agent in the editor, its settings) change
-- the plugin's options through it too ('OptionsChosen').
--
-- = One ask at a time
--
-- Every ask about the plugins is one flight: the read the shell wants with
-- each load of the session and the spaces ('Refresh'), the read a view
-- wants when it opens on nothing read ('Opened'), the user's writes of
-- their states, and the administrator's saves of the plugins off for the
-- wiki, and the user's writes of a plugin's options. While one of them is
-- out, no other is sent ('flight'), so the
-- server answers them in the order sent and a later answer is never an
-- older one. A write chosen meanwhile waits its turn, in the order chosen
-- ('queued'); a later choice for the same plugin, later options for the
-- same plugin, or a later set of the plugins off for the wiki, takes the
-- place of the one that waits. A read
-- wanted meanwhile is sent when the ask out answers, failed or not, after
-- what waits ('wanted').
--
-- = What is known
--
-- A read answers with the whole value. A write, of a state or of options,
-- answers with the user's choice for the one plugin written, and a save
-- with the plugins off for
-- the wiki: each is put in by what it answers, and a read follows, since
-- neither answers with the whole value. A write refused is shown on its
-- plugin's row ('refused'), a save refused beside the boxes ('problem'),
-- until a later one succeeds; a read follows them too, since a write whose
-- answer was lost may have been done. A read made after a write is sent
-- after the write's answer came: the HTTP backend's cache, which a write
-- empties when it answers, never answers it with what the write changed.
--
-- = A plugin's options
--
-- While a write of a plugin's options is out or waits, the plugin is given
-- the options last chosen ('writingOptions', 'held'), as the section's
-- switch shows the state last chosen ('writing', 'shownState'): a plugin
-- that set its options sees them at once, and does not see the server's
-- older ones come back with a read in between. Once the last write
-- answers, the plugin is given what the server keeps.
--
-- Options found by the move of a plugin's legacy settings
-- ("Wiki.App.Assistant.Migrate") are adopted only when the server keeps
-- none for the plugin and none were chosen since ('OptionsAdopted'): other
-- options were moved by another browser, or chosen by the user. Before
-- what is on is read in the session, they wait for the read
-- ('adopting').
--
-- = Who is signed in
--
-- The model is the signed-in user's, by the session's number
-- ("Wiki.App.View.Common"'s @sessionNumber@), which the shell gives it
-- each time it changes ('Given'). The step that takes a new one forgets
-- what is known, what waits its turn and what was refused, and puts the
-- asks out behind it: the shell's load of the new session then reads. An
-- answer to an ask of the earlier session changes nothing ('ofSession'),
-- and a write chosen in it never goes out. The ask of the earlier session
-- still out stays the flight all the same: the new session's read is sent
-- after its answer, so that the server has handled it, and the read sees
-- what it did. Until the new session's read answers, what is on is the
-- defaults with the plugins off for the wiki as the earlier session knew
-- them ('offBefore', 'held'): the wiki's set is the same for every reader.
module Wiki.App.View.Plugins.Machine
  ( Model (..)
  , Event (..)
  , Effect (..)
  , Write (..)
  , initial
  , initialWith
  , step
  , knownNow
  , held
  , stateOf
  , shownState
  , shownOptions
  , shownOff
  , askOf
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginId, PluginState (..), Plugins (..), WikiPlugins (..))
import Wiki.App.Backend (WikiError)
import Wiki.App.View.Latest (Ask, Asks, madeAfter, newAsk, noAsks, supersede, takes)

-- | 'Model' is the plugins' state.
data Model = Model
  { known :: Maybe (Either WikiError Plugins)
  -- ^ What the server keeps, as the last answers said it, or why it could
  -- not be read; 'Nothing' until the first answer in the session.
  , asks :: Asks
  -- ^ The asks sent, and which one's answer was taken last.
  , flight :: Maybe Ask
  -- ^ The last ask sent, until its answer comes, of this session or an
  -- earlier one. While it is out, no other is sent.
  , wanted :: Bool
  -- ^ Whether a read was wanted while that ask was out.
  , queued :: [(Int, Write)]
  -- ^ The writes chosen while an ask was out, in the order chosen, each
  -- with the session it was chosen in.
  , writing :: Map PluginId PluginState
  -- ^ The state last chosen for each plugin whose write is out or waits.
  , writingOptions :: Map PluginId (Maybe Value)
  -- ^ The options last chosen for each plugin whose write of options is
  -- out or waits: a value, or 'Nothing' to clear them.
  , wantedOff :: Maybe (Set PluginId)
  -- ^ The plugins off for the wiki last ticked, while their save is out or
  -- waits.
  , refused :: Map PluginId WikiError
  -- ^ Why the last write for a plugin failed, of its state or its options.
  , problem :: Maybe WikiError
  -- ^ Why the last save of the plugins off for the wiki failed.
  , session :: Int
  -- ^ The number of the session the model was last given.
  , sessionFrom :: Asks
  -- ^ The asks as they were when that session began.
  , offBefore :: Set PluginId
  -- ^ The plugins off for the wiki as the earlier session last knew them,
  -- for what is on until this session has read them ('held').
  , adopting :: Map PluginId Value
  -- ^ The options to adopt for each plugin once what is on is read in
  -- this session ('OptionsAdopted').
  }
  deriving stock (Eq, Show)

-- | 'Write' is a change sent to the server, or waiting its turn.
data Write
  = -- | The user's state for a plugin.
    StateWrite PluginId PluginState
  | -- | The plugins off for the wiki, the whole set.
    OffWrite (Set PluginId)
  | -- | The user's options for a plugin: a value, or 'Nothing' to clear
    -- them.
    OptionsWrite PluginId (Maybe Value)
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the plugins' state.
data Event
  = -- | The shell loads the session: read what is on, in turn.
    Refresh
  | -- | A view of the plugins was opened, or is open in a new session: a
    -- read, unless what is on was read in this session already.
    Opened
  | -- | The shell was given the number of the session signed in.
    Given Int
  | -- | The answer to the read made under this ticket.
    Loaded Ask (Either WikiError Plugins)
  | -- | The user chose a state for a plugin: on, off, or its default.
    Chosen PluginId PluginState
  | -- | The answer to the write for this plugin sent under this ticket:
    -- the user's choice for it after the write.
    Written Ask PluginId (Either WikiError PluginChoice)
  | -- | A plugin's surface set the plugin's options: a value, or 'Nothing'
    -- to clear them.
    OptionsChosen PluginId (Maybe Value)
  | -- | The answer to the write of a plugin's options sent under this
    -- ticket: the user's choice for it after the write.
    OptionsWritten Ask PluginId (Either WikiError PluginChoice)
  | -- | The move of a plugin's legacy settings, started in the session of
    -- this number, found these options: kept if the server keeps none for
    -- the plugin and none were chosen since.
    OptionsAdopted Int PluginId Value
  | -- | The administrator ticked a plugin's box "off for everyone"
    -- ('True'), or cleared it.
    Ticked PluginId Bool
  | -- | The answer to the save sent under this ticket.
    Saved Ask (Either WikiError WikiPlugins)
  deriving stock (Show)

-- | 'Effect' is what a step asks the shell to do.
data Effect
  = -- | Read what is on; the answer comes back as 'Loaded'.
    Load Ask
  | -- | Set the user's state for a plugin; the answer comes back as
    -- 'Written'.
    WriteState Ask PluginId PluginState
  | -- | Set the user's options for a plugin, or clear them ('Nothing');
    -- the answer comes back as 'OptionsWritten'.
    WriteOptions Ask PluginId (Maybe Value)
  | -- | Set the plugins off for the wiki; the answer comes back as 'Saved'.
    SaveOff Ask (Set PluginId)
  deriving stock (Eq, Show)

-- | The plugins' state for a session, before anything is read.
initial :: Int -> Model
initial = initialWith Nothing

-- | The plugins' state for a session, with what a read before the program
-- started answered (the shell's first load), if it did.
initialWith :: Maybe Plugins -> Int -> Model
initialWith read' session =
  Model
    { known = Right <$> read'
    , asks = noAsks
    , flight = Nothing
    , wanted = False
    , queued = []
    , writing = mempty
    , writingOptions = mempty
    , wantedOff = Nothing
    , refused = mempty
    , problem = Nothing
    , session
    , sessionFrom = noAsks
    , offBefore = mempty
    , adopting = mempty
    }

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Refresh -> reading m
  Opened
    | isNothing (knownNow m) -> reading m
    | otherwise -> (m, [])
  Given session
    | session /= m.session ->
        let behind = supersede m.asks
         in ((initial session) {asks = behind, sessionFrom = behind, flight = m.flight, offBefore = (held m).offForWiki}, [])
    | otherwise -> (m, [])
  Loaded asked answer
    | ofSession asked m ->
        let (m1, effects) = landed asked (taking asked answer m)
         in second (effects <>) (adoptWaiting m1)
    | otherwise -> landed asked m
  Chosen p wanted'
    | shownState m p == wanted' -> (m, [])
    | otherwise -> writingNow (StateWrite p wanted') m {writing = Map.insert p wanted' m.writing, refused = Map.delete p m.refused}
  Written asked _ _
    | not (ofSession asked m) -> landed asked m
  Written asked p answer ->
    let waits = any (\(_, w) -> w `writes` p) m.queued
     in wrote asked p answer m {writing = if waits then m.writing else Map.delete p m.writing}
  OptionsChosen p wanted'
    | shownOptions m p == wanted' -> (m, [])
    | otherwise -> writingNow (OptionsWrite p wanted') m {writingOptions = Map.insert p wanted' m.writingOptions, refused = Map.delete p m.refused}
  OptionsWritten asked _ _
    | not (ofSession asked m) -> landed asked m
  OptionsWritten asked p answer ->
    let waits = any (\(_, w) -> w `writesOptions` p) m.queued
     in wrote asked p answer m {writingOptions = if waits then m.writingOptions else Map.delete p m.writingOptions}
  OptionsAdopted session p v
    | session /= m.session -> (m, [])
    | isNothing (knownNow m) -> (m {adopting = Map.insert p v m.adopting}, [])
    | otherwise -> adopt p v m
  Ticked p isOff -> case shownOff m of
    Just now'
      | Set.member p now' /= isOff ->
          let next = (if isOff then Set.insert else Set.delete) p now'
           in writingNow (OffWrite next) m {wantedOff = Just next, problem = Nothing}
    _ -> (m, [])
  Saved asked _
    | not (ofSession asked m) -> landed asked m
  Saved asked answer ->
    let waits = any (\(_, w) -> isOffWrite w) m.queued
        m1 = if waits then m else m {wantedOff = Nothing}
        m2 = case (answer, takes asked m1.asks) of
          (Right w, Just after) -> m1 {asks = after, known = fmap (\k -> k {offForWiki = w.off}) <$> m1.known, problem = Nothing}
          (Right _, Nothing) -> m1
          (Left e, _) -> m1 {problem = Just e}
     in -- The answer is the set alone: a read follows, after what waits.
        landed asked m2 {wanted = True}

-- | Options found by a move adopted for a plugin: written, unless the
-- plugin has options, the server's or chosen since ('shownOptions').
adopt :: PluginId -> Value -> Model -> (Model, [Effect])
adopt p v m
  | isJust (shownOptions m p) = (m, [])
  | otherwise = writingNow (OptionsWrite p (Just v)) m {writingOptions = Map.insert p (Just v) m.writingOptions, refused = Map.delete p m.refused}

-- | The options waiting for what is on to be read, adopted once it is.
adoptWaiting :: Model -> (Model, [Effect])
adoptWaiting m
  | isNothing (knownNow m) || Map.null m.adopting = (m, [])
  | otherwise = Map.foldrWithKey (\p v (x, effects) -> second (effects <>) (adopt p v x)) (m {adopting = mempty}, []) m.adopting

-- | The answer to a write for a plugin, of its state or its options, of
-- this session, taken into the model.
wrote :: Ask -> PluginId -> Either WikiError PluginChoice -> Model -> (Model, [Effect])
wrote asked p answer m1 =
  let m2 = case (answer, takes asked m1.asks) of
        -- A success clears the plugin's refusal: the last answer about it
        -- is not one.
        (Right choice, Just after) -> m1 {asks = after, known = withChoice p choice <$> m1.known, refused = Map.delete p m1.refused}
        (Right _, Nothing) -> m1
        (Left e, _) -> m1 {refused = Map.insert p e m1.refused}
   in -- The answer is the one plugin's: a read follows, after what waits.
      landed asked m2 {wanted = True}

-- | What the server keeps, as the model holds it, once read.
knownNow :: Model -> Maybe Plugins
knownNow m = m.known >>= rightToMaybe

-- | What is on as the rest of the application sees it (the context's
-- plugins): what is known, or, before a read of this session answered or
-- when none could, the defaults with the plugins off for the wiki that an
-- earlier session knew. Each plugin whose write of options is out or
-- waits has the options last chosen in place of the server's, which is
-- what its surfaces are given ('Wiki.App.Plugin.Registry.optionsFor').
held :: Model -> Plugins
held m = withChosen (fromMaybe Plugins {offForWiki = m.offBefore, mine = mempty} (knownNow m))
  where
    withChosen k = k {mine = Map.foldrWithKey chosenFor k.mine m.writingOptions}
    chosenFor p o mine = putChoice p PluginChoice {state = maybe PluginDefault (\c -> c.state) (Map.lookup p mine), options = o} mine

-- | The user's state for a plugin, as the server keeps it: the default
-- when they made no choice.
stateOf :: Plugins -> PluginId -> PluginState
stateOf known p = maybe PluginDefault (\c -> c.state) (Map.lookup p known.mine)

-- | The state a plugin's switch shows: the one chosen while its write is
-- out or waits, and otherwise the server's.
shownState :: Model -> PluginId -> PluginState
shownState m p = fromMaybe (maybe PluginDefault (`stateOf` p) (knownNow m)) (Map.lookup p m.writing)

-- | A plugin's options as its surfaces are given them: the ones last
-- chosen while their write is out or waits, and otherwise the server's.
shownOptions :: Model -> PluginId -> Maybe Value
shownOptions m p = (\c -> c.options) =<< Map.lookup p (held m).mine

-- | The plugins off for the wiki the administrator's boxes show: the set
-- last ticked while its save is out or waits, and otherwise the server's;
-- 'Nothing' before it was read.
shownOff :: Model -> Maybe (Set PluginId)
shownOff m = m.wantedOff <|> ((\k -> k.offForWiki) <$> knownNow m)

-- | What is known with the user's choice for a plugin put in, by its id.
-- A choice of the default with no options is none, as the server keeps it:
-- the plugin is taken out. A read that failed holds nothing to put it in.
withChoice :: PluginId -> PluginChoice -> Either WikiError Plugins -> Either WikiError Plugins
withChoice p choice = fmap (\k -> k {mine = putChoice p choice k.mine})

-- | The user's choices with the choice for a plugin put in, or taken out
-- when it is the default with no options, as the server keeps them.
putChoice :: PluginId -> PluginChoice -> Map PluginId PluginChoice -> Map PluginId PluginChoice
putChoice p choice
  | choice.state /= PluginDefault || isJust choice.options = Map.insert p choice
  | otherwise = Map.delete p

-- | Whether a write is one of this plugin's state.
writes :: Write -> PluginId -> Bool
writes w p = case w of
  StateWrite q _ -> q == p
  OffWrite _ -> False
  OptionsWrite _ _ -> False

-- | Whether a write is one of this plugin's options.
writesOptions :: Write -> PluginId -> Bool
writesOptions w p = case w of
  OptionsWrite q _ -> q == p
  StateWrite {} -> False
  OffWrite _ -> False

isOffWrite :: Write -> Bool
isOffWrite = \case
  OffWrite _ -> True
  StateWrite {} -> False
  OptionsWrite {} -> False

-- | The answer to a read taken, if no answer to a later ask was. A failure
-- is shown only where nothing is held: what is held is kept.
taking :: Ask -> Either WikiError Plugins -> Model -> Model
taking asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case answer of
    Right _ -> m {asks = after, known = Just answer}
    Left _ -> case m.known of
      Just (Right _) -> m {asks = after}
      _ -> m {asks = after, known = Just answer}

-- | The state after the answer to an ask came, taken or not, failed or
-- not: when it was the ask out, the next is sent, the first write waiting
-- of this session, or else the read wanted.
landed :: Ask -> Model -> (Model, [Effect])
landed asked m
  | m.flight /= Just asked = (m, [])
  | otherwise = case [w | (n, w) <- m.queued, n == m.session] of
      w : rest -> send w m {flight = Nothing, queued = [(m.session, x) | x <- rest]}
      []
        | m.wanted -> sendRead m {flight = Nothing, queued = []}
        | otherwise -> (m {flight = Nothing, queued = []}, [])

-- | Whether an ask was made in the session the model is in.
ofSession :: Ask -> Model -> Bool
ofSession asked m = asked `madeAfter` m.sessionFrom

-- | Whether an ask is out, of this session or an earlier one.
inFlight :: Model -> Bool
inFlight m = isJust m.flight

-- | A read begun, or wanted while an ask is out.
reading :: Model -> (Model, [Effect])
reading m
  | inFlight m = (m {wanted = True}, [])
  | otherwise = sendRead m

-- | A write begun, or put in its turn while an ask is out, in the place of
-- one that waits for the same thing.
writingNow :: Write -> Model -> (Model, [Effect])
writingNow w m
  | inFlight m = (m {queued = [x | x@(_, q) <- m.queued, not (same q)] <> [(m.session, w)]}, [])
  | otherwise = send w m
  where
    same q = case (q, w) of
      (StateWrite a _, StateWrite b _) -> a == b
      (OffWrite _, OffWrite _) -> True
      (OptionsWrite a _, OptionsWrite b _) -> a == b
      _ -> False

sendRead :: Model -> (Model, [Effect])
sendRead m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket, wanted = False}, [Load ticket])

send :: Write -> Model -> (Model, [Effect])
send w m =
  let (ticket, after) = newAsk m.asks
   in ( m {asks = after, flight = Just ticket}
      ,
        [ case w of
            StateWrite p s -> WriteState ticket p s
            OffWrite off -> SaveOff ticket off
            OptionsWrite p o -> WriteOptions ticket p o
        ]
      )

-- | The ticket an effect was sent under.
askOf :: Effect -> Maybe Ask
askOf = \case
  Load a -> Just a
  WriteState a _ _ -> Just a
  WriteOptions a _ _ -> Just a
  SaveOff a _ -> Just a

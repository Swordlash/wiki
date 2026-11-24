-- | 'Model' is what the administration's switch of plugins
-- ("Wiki.App.View.WikiPlugins") knows, and 'step' is the one place it
-- changes (decision 82 of docs/PLAN.md). Every input is an 'Event': a
-- checkbox ticked, an answer of the backend, the view going away. Every
-- output is an 'Effect': read the plugins off for the wiki, save them,
-- tell the shell that they changed.
--
-- A read and a save are one flight: while one of them is out, no other is
-- sent ('flight'). A save replaces the whole set on the server, so only
-- the last set wanted matters: a box ticked while an ask is out changes
-- the set that waits ('queued'), which is saved when the ask out answers,
-- failed or not. The boxes show the set last wanted until its save has
-- answered ('wantedOff'). A save refused is shown, and a read follows: a
-- save whose answer was lost may have been done. An answer is taken only
-- if no answer to a later ask was taken already ("Wiki.App.View.Latest").
--
-- The view is mounted for one user ("Wiki.App.Shell" keys it by the
-- user), so it has no change of session to follow.
module Wiki.App.View.WikiPlugins.Machine
  ( Model (..)
  , Event (..)
  , Effect (..)
  , initial
  , step
  , shownOff
  , askOf
  )
where

import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginId, WikiPlugins (..))
import Wiki.App.Backend (WikiError)
import Wiki.App.View.Latest (Ask, Asks, newAsk, noAsks, takes)

-- | 'Model' is the view's state.
data Model = Model
  { off :: Maybe (Either WikiError (Set PluginId))
  -- ^ The plugins off for the wiki as the server answered, or why they
  -- could not be read; 'Nothing' until the first answer.
  , asks :: Asks
  , flight :: Maybe Ask
  -- ^ The last ask sent, until its answer comes.
  , wanted :: Bool
  -- ^ Whether a read was wanted while that ask was out.
  , queued :: Maybe (Set PluginId)
  -- ^ The set to save once the ask out has answered.
  , wantedOff :: Maybe (Set PluginId)
  -- ^ The set last wanted, while its save is out or waits.
  , problem :: Maybe WikiError
  -- ^ Why the last save failed.
  , closed :: Bool
  }
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the view.
data Event
  = Started
  | Finished
  | -- | The answer to the read made under this ticket.
    Loaded Ask (Either WikiError WikiPlugins)
  | -- | The administrator ticked a plugin's box "off for everyone"
    -- ('True'), or cleared it.
    Ticked PluginId Bool
  | -- | The answer to the save sent under this ticket.
    Saved Ask (Either WikiError WikiPlugins)
  deriving stock (Show)

-- | 'Effect' is what a step asks the component to do.
data Effect
  = Load Ask
  | Save Ask (Set PluginId)
  | -- | Tell the shell that the plugins off for the wiki changed.
    Tell
  deriving stock (Eq, Show)

initial :: Model
initial = Model {off = Nothing, asks = noAsks, flight = Nothing, wanted = False, queued = Nothing, wantedOff = Nothing, problem = Nothing, closed = False}

step :: Event -> Model -> (Model, [Effect])
step event m
  | m.closed = (m, [])
  | otherwise = case event of
      Started -> reading m
      Finished -> (m {closed = True}, [])
      Loaded asked answer -> landed asked (taking asked (fmap (\w -> w.off) answer) m)
      Ticked p isOff -> case shownOff m of
        Just now'
          | Set.member p now' /= isOff ->
              let next = (if isOff then Set.insert else Set.delete) p now'
               in saving next m {wantedOff = Just next, problem = Nothing}
        _ -> (m, [])
      Saved asked answer ->
        let m1 = if isJust m.queued then m else m {wantedOff = Nothing}
            (m2, fx1) = case answer of
              Right w -> case takes asked m1.asks of
                Just after -> (m1 {asks = after, off = Just (Right w.off)}, [Tell])
                Nothing -> (m1, [])
              Left e -> reading m1 {problem = Just e}
            (m3, fx2) = landed asked m2
         in (m3, fx1 <> fx2)

-- | The set the boxes show: the one last wanted while its save is out or
-- waits, and otherwise the server's; 'Nothing' before it was read.
shownOff :: Model -> Maybe (Set PluginId)
shownOff m = m.wantedOff <|> (m.off >>= rightToMaybe)

taking :: Ask -> Either WikiError (Set PluginId) -> Model -> Model
taking asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case (answer, m.off) of
    (Left _, Just (Right _)) -> m {asks = after}
    _ -> m {asks = after, off = Just answer}

landed :: Ask -> Model -> (Model, [Effect])
landed asked m
  | m.flight /= Just asked = (m, [])
  | Just next <- m.queued = send next m {flight = Nothing, queued = Nothing}
  | m.wanted = sendRead m {flight = Nothing}
  | otherwise = (m {flight = Nothing}, [])

inFlight :: Model -> Bool
inFlight m = maybe False (\a -> isJust (takes a m.asks)) m.flight

reading :: Model -> (Model, [Effect])
reading m
  | inFlight m = (m {wanted = True}, [])
  | otherwise = sendRead m

saving :: Set PluginId -> Model -> (Model, [Effect])
saving next m
  | inFlight m = (m {queued = Just next}, [])
  | otherwise = send next m

sendRead :: Model -> (Model, [Effect])
sendRead m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket, wanted = False}, [Load ticket])

send :: Set PluginId -> Model -> (Model, [Effect])
send next m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket}, [Save ticket next])

askOf :: Effect -> Maybe Ask
askOf = \case
  Load a -> Just a
  Save a _ -> Just a
  Tell -> Nothing

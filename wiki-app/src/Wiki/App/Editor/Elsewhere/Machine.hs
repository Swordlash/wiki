-- | The editor's lookup of other spaces' pages after @[[@ (#301), as a
-- machine: a pure 'step' over 'Event's, which the editor
-- ("Wiki.App.View.Editor") runs with "Halogen.Machine" and whose 'Effect's
-- it carries out. Nothing of Halogen is imported here, so the rules are
-- tested over any order of events and answers ("Test.Elsewhere").
--
-- CodeMirror asks for the group "In other spaces" of the list after @[[@
-- with a question of its own (@wiki-complete-elsewhere@, in
-- @web/src/editor.js@), beside the question of the list of the space's own
-- pages, which the editor answers at once. The editor reads the letters
-- typed after @[[@ from its text ("Wiki.App.Editor.Complete"'s
-- 'Wiki.App.Editor.Complete.linkLetters') and tells the machine
-- ('Asked'). The machine waits a pause ('Pause'), so that a lookup goes
-- out once the typing stops, then asks the server for the pages of the
-- other spaces whose title matches the letters ('Lookup', the backend's
-- @findPagesByTitle@), and replies to the question with the pages found
-- ('Reply'). The editor writes the reply as a list, against the text it
-- holds then.
--
-- The rules:
--
-- * Every question gets one reply, and a question is replied to before a
--   newer one is: a question replaces the one waiting, which is replied to
--   with nothing.
-- * No lookup from a page whose text is encrypted, or being encrypted or
--   decrypted ('sealed'): the letters are the page's text, which the
--   server must not read. Nor for fewer than
--   'Wiki.Core.PageMatch.minLetters' letters.
-- * One lookup out at a time. A question whose pause ends while one is out
--   waits for it, and is asked when it answers.
-- * A lookup's answer is matched to the question it was sent for by its
--   'Ticket', never by its letters: the answer to another question (the
--   same letters typed again, or asked by another reader) is dropped.
-- * A change of the reader's session, of the space, or of whether the page
--   is sealed changes the model in the step that takes it ('Inputs'): the
--   question waiting is replied to with nothing, the answer of a lookup
--   still out then matches no question, and the list shown is closed
--   ('Close'), since its pages were found for the reader before.
module Wiki.App.Editor.Elsewhere.Machine
  ( -- * The model
    Model (..)
  , Question (..)
  , Ticket (..)
  , initial
  , elsewhereShown

    -- * The step
  , Event (..)
  , Effect (..)
  , step
  )
where

import Protolude
import Wiki.App.Editor.Complete (Request)
import Wiki.Core.Id (SpaceId)
import Wiki.Core.PageMatch (TitleHit, TitleQuery (..), defaultTitleHits, looksFor)

-- | 'Ticket' is the number of one lookup sent: its answer comes back with
-- it, and is the reply to the question whose ticket it is. No two lookups
-- of one editor have the same ticket.
newtype Ticket = Ticket Int
  deriving stock (Eq, Ord, Show)

-- | 'Question' is CodeMirror's question waiting for its reply: its number,
-- the cursor's place it was asked at, the letters typed after @[[@, the
-- ticket of the lookup sent for it once one is sent, and whether its pause
-- has ended.
data Question = Question
  { qid :: Int
  , request :: Request
  , letters :: Text
  , ticket :: Maybe Ticket
  , paused :: Bool
  }
  deriving stock (Eq, Show)

-- | 'Model' is what the machine knows: the reader's session (the shell's
-- number of it, 'Wiki.App.View.Common.Context''s @sessionNumber@), the
-- space of the page edited, which the lookup leaves out, and whether the
-- page is sealed (encrypted, or being encrypted or decrypted); the next
-- lookup's ticket; the question waiting for its reply, if one is; and the
-- ticket of the lookup out, if one is.
data Model = Model
  { session :: Int
  , space :: SpaceId
  , sealed :: Bool
  , next :: Ticket
  , waiting :: Maybe Question
  , out :: Maybe Ticket
  }
  deriving stock (Eq, Show)

-- | The model of an editor that has asked nothing yet.
initial :: Int -> SpaceId -> Bool -> Model
initial session space sealed = Model {session, space, sealed, next = Ticket 0, waiting = Nothing, out = Nothing}

-- | How many pages of other spaces the list shows at most.
elsewhereShown :: Int
elsewhereShown = defaultTitleHits

data Event
  = -- | CodeMirror's question: its number, the cursor's place, and the
    -- letters typed after @[[@ where a link to a page opens a list there
    -- ('Nothing' where none opens).
    Asked Int Request (Maybe Text)
  | -- | The pause after the last question has ended.
    Paused
  | -- | The answer of the lookup with this ticket: the pages found, or
    -- 'Nothing' when the lookup failed.
    Found Ticket (Maybe [TitleHit])
  | -- | The editor's input, as it is now: the reader's session, the space,
    -- and whether the page is sealed. Told at every change of input; one
    -- that changes none of the three changes nothing.
    Inputs Int SpaceId Bool
  | -- | The editor goes.
    Finished
  deriving stock (Eq, Show)

data Effect
  = -- | Start the pause, after which 'Paused' comes.
    Pause
  | -- | Stop the pause, if one runs: its 'Paused' never comes.
    StopPause
  | -- | Ask the server for the pages, under this ticket.
    Lookup Ticket TitleQuery
  | -- | Reply to the question with this number, asked at this place: the
    -- pages found, or 'Nothing' for none to show.
    Reply Int Request (Maybe [TitleHit])
  | -- | Close the list the editor shows, in the editor and in the rows that
    -- share its list, with every entry of it: no key chooses one of them
    -- after. Told on every change of the input, whatever is shown: a list
    -- shown then was made for the input before (the pages of other spaces
    -- for the reader before, or the space's own pages from the input
    -- before), and a change of the reader, the space or the sealing while
    -- a list is open is rare enough that closing a list that could have
    -- stayed costs nothing worth a rule of its own.
    Close
  deriving stock (Eq, Show)

step :: Event -> Model -> (Model, [Effect])
step event model = case event of
  Asked qid request found -> case found of
    Just letters
      | not model.sealed
      , looksFor (queryFor model letters) ->
          ( model {waiting = Just Question {qid, request, letters, ticket = Nothing, paused = False}}
          , dropWaiting model <> [StopPause, Pause]
          )
    _ -> (model {waiting = Nothing}, dropWaiting model <> [StopPause, Reply qid request Nothing])
  Paused -> case model.waiting of
    Just q | isNothing q.ticket -> send model {waiting = Just q {paused = True}}
    _ -> (model, [])
  Found ticket hits ->
    let freed = model {out = if model.out == Just ticket then Nothing else model.out}
     in case freed.waiting of
          Just q
            | q.ticket == Just ticket -> (freed {waiting = Nothing}, [Reply q.qid q.request hits])
          _ -> send freed
  Inputs session space sealed
    | (session, space, sealed) == (model.session, model.space, model.sealed) -> (model, [])
    | otherwise -> (model {session, space, sealed, waiting = Nothing}, dropWaiting model <> [StopPause, Close])
  Finished -> (model {waiting = Nothing}, [StopPause])

-- | The reply to the question waiting, if one is, which another replaces:
-- nothing.
dropWaiting :: Model -> [Effect]
dropWaiting model = [Reply q.qid q.request Nothing | Just q <- [model.waiting]]

-- | Send the lookup of the question waiting, if its pause has ended, no
-- lookup was sent for it yet, and none is out.
send :: Model -> (Model, [Effect])
send model = case model.waiting of
  Just q
    | q.paused
    , isNothing q.ticket
    , isNothing model.out ->
        let Ticket n = model.next
         in ( model {next = Ticket (n + 1), out = Just model.next, waiting = Just q {ticket = Just model.next}}
            , [Lookup model.next (queryFor model q.letters)]
            )
  _ -> (model, [])

-- | The lookup of these letters: the pages of every space the reader may
-- view but this one, at most 'elsewhereShown'.
queryFor :: Model -> Text -> TitleQuery
queryFor model letters = TitleQuery {letters, except = Just model.space, limit = elsewhereShown}

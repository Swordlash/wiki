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
-- out once the typing stops. Then it asks the backend whether the server
-- takes the page's text as plain now ('Check', the backend's @plainNow@),
-- and only on a yes ('Checked') asks the server for the pages of the
-- other spaces whose title matches the letters ('Lookup', the backend's
-- @findPagesByTitle@). It replies to the question with the pages found
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
--   server must not read. The editor's input may be older than another
--   tab's change of the space, so the backend is asked again before each
--   lookup ('Check'), and a no replies with nothing. The check is an ask
--   of its own, which the machine sees: a question given up, replaced, or
--   dropped by a change of input while its check is out sends no lookup
--   when the check answers. No lookup for fewer than
--   'Wiki.Core.PageMatch.minLetters' letters, nor for more than the server
--   takes ('Wiki.Core.PageMatch.titleRefusal'); such a question is
--   replied to with nothing at once, with no pause.
-- * A question CodeMirror gave up before its lookup was sent is not looked
--   up ('Abandoned').
-- * One ask out at a time, a check or a lookup. A question whose pause
--   ends while one is out waits for it, and is checked when it answers.
-- * An answer is matched to the question it was asked for by its
--   'Ticket', never by its letters: the answer to another question (the
--   same letters typed again, or asked by another reader) is dropped.
-- * A change of the reader's session, of the space, of whether the page is
--   sealed, or of the spaces the editor holds changes the model in the
--   step that takes it ('Inputs'): the question waiting is replied to with
--   nothing, the answer of an ask still out then matches no question, and
--   the list shown is closed ('Close'), since its pages were found for the
--   input before.
module Wiki.App.Editor.Elsewhere.Machine
  ( -- * The model
    Model (..)
  , Input (..)
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
import Wiki.App.Editor.Complete (HeldSpace, Request)
import Wiki.Core.Id (SpaceId)
import Wiki.Core.PageMatch (TitleHit, TitleQuery (..), defaultTitleHits, looksFor, titleRefusal)

-- | 'Ticket' is the number of one question's check and lookup: their
-- answers come back with it, and are about the question whose ticket it
-- is. No two questions of one editor get the same ticket.
newtype Ticket = Ticket Int
  deriving stock (Eq, Ord, Show)

-- | 'Question' is CodeMirror's question waiting for its reply: its number,
-- the cursor's place it was asked at, the letters typed after @[[@, the
-- ticket of its check (and of its lookup after a yes) once the check is
-- sent, and whether its pause has ended.
data Question = Question
  { qid :: Int
  , request :: Request
  , letters :: Text
  , ticket :: Maybe Ticket
  , paused :: Bool
  }
  deriving stock (Eq, Show)

-- | 'Input' is what the editor tells the machine of its input: the
-- reader's session (the shell's number of it,
-- 'Wiki.App.View.Common.Context''s @sessionNumber@), the space of the page
-- edited, which the lookup leaves out, whether the page is sealed
-- (encrypted, or being encrypted or decrypted), and the spaces the editor
-- holds, by id, which the reply's pages are offered against
-- ('Wiki.App.Editor.Complete.elsewhereSource').
data Input = Input
  { session :: Int
  , space :: SpaceId
  , sealed :: Bool
  , spaces :: Map SpaceId HeldSpace
  }
  deriving stock (Eq, Show)

-- | 'Model' is what the machine knows: the input; the next question's
-- ticket; the question waiting for its reply, if one is; and the ticket of
-- the ask out (a check or a lookup), if one is.
data Model = Model
  { input :: Input
  , next :: Ticket
  , waiting :: Maybe Question
  , out :: Maybe Ticket
  }
  deriving stock (Eq, Show)

-- | The model of an editor that has asked nothing yet.
initial :: Input -> Model
initial input = Model {input, next = Ticket 0, waiting = Nothing, out = Nothing}

-- | How many pages of other spaces the list shows at most.
elsewhereShown :: Int
elsewhereShown = defaultTitleHits

data Event
  = -- | CodeMirror's question: its number, the cursor's place, and the
    -- letters typed after @[[@ where a link to a page opens a list there
    -- ('Nothing' where none opens).
    Asked Int Request (Maybe Text)
  | -- | CodeMirror gave up the question with this number: the text changed,
    -- the list was closed, or the text lost the focus.
    Abandoned Int
  | -- | The pause after the last question has ended.
    Paused
  | -- | The answer of the check with this ticket: whether the server takes
    -- the page's text as plain now. A refusal, or a check that failed,
    -- answers no.
    Checked Ticket Bool
  | -- | The answer of the lookup with this ticket: the pages found, or
    -- 'Nothing' when the lookup failed.
    Found Ticket (Maybe [TitleHit])
  | -- | The editor's input, as it is now. Told at every change of input;
    -- one equal to the input before changes nothing.
    Inputs Input
  | -- | The editor goes.
    Finished
  deriving stock (Eq, Show)

data Effect
  = -- | Start the pause, after which 'Paused' comes.
    Pause
  | -- | Stop the pause, if one runs: its 'Paused' never comes.
    StopPause
  | -- | Ask whether the server takes the page's text as plain now, under
    -- this ticket. Nothing of the letters goes with it.
    Check Ticket
  | -- | Ask the server for the pages, under this ticket. This request
    -- carries the letters.
    Lookup Ticket TitleQuery
  | -- | Reply to the question with this number, asked at this place: the
    -- pages found, or 'Nothing' for none to show.
    Reply Int Request (Maybe [TitleHit])
  | -- | Close the list the editor shows, in the editor and in the rows that
    -- share its list, with every entry of it: no key chooses one of them
    -- after. Told on every change of the input, whatever is shown: a list
    -- shown then was made for the input before (the pages of other spaces
    -- for the reader or the spaces before, or the space's own pages from
    -- the input before), and a change of input while a list is open is
    -- rare enough that closing a list that could have stayed costs nothing
    -- worth a rule of its own.
    Close
  deriving stock (Eq, Show)

step :: Event -> Model -> (Model, [Effect])
step event model = case event of
  Asked qid request found -> case found of
    Just letters
      | not model.input.sealed
      , let query = queryFor model letters
      , looksFor query
      , isNothing (titleRefusal query) ->
          ( model {waiting = Just Question {qid, request, letters, ticket = Nothing, paused = False}}
          , dropWaiting model <> [StopPause, Pause]
          )
    _ -> (model {waiting = Nothing}, dropWaiting model <> [StopPause, Reply qid request Nothing])
  -- Replied to with nothing, so that CodeMirror's asker forgets the
  -- question: the same letters asked again are a new question. A check or
  -- a lookup already out stays out, and its answer, for no question then,
  -- is dropped.
  Abandoned qid -> case model.waiting of
    Just q | q.qid == qid -> (model {waiting = Nothing}, [StopPause | isNothing q.ticket] <> [Reply q.qid q.request Nothing])
    _ -> (model, [])
  Paused -> case model.waiting of
    Just q | isNothing q.ticket -> send model {waiting = Just q {paused = True}}
    _ -> (model, [])
  -- A yes for the question still waiting sends its lookup at once, in
  -- this step, under the same ticket, which stays out. A no replies with
  -- nothing. An answer for a question gone frees the turn for the
  -- question waiting, if its pause has ended.
  Checked ticket plain ->
    let freed = free ticket model
     in case model.waiting of
          Just q
            | q.ticket == Just ticket ->
                if plain
                  then (model, [Lookup ticket (queryFor model q.letters)])
                  else (freed {waiting = Nothing}, [Reply q.qid q.request Nothing])
          _ -> send freed
  Found ticket hits ->
    let freed = free ticket model
     in case freed.waiting of
          Just q
            | q.ticket == Just ticket -> (freed {waiting = Nothing}, [Reply q.qid q.request hits])
          _ -> send freed
  Inputs input
    | input == model.input -> (model, [])
    | otherwise -> (model {input, waiting = Nothing}, dropWaiting model <> [StopPause, Close])
  Finished -> (model {waiting = Nothing}, [StopPause])

-- | The model with the ask of this ticket no longer out.
free :: Ticket -> Model -> Model
free ticket model = model {out = if model.out == Just ticket then Nothing else model.out}

-- | The reply to the question waiting, if one is, which another replaces:
-- nothing.
dropWaiting :: Model -> [Effect]
dropWaiting model = [Reply q.qid q.request Nothing | Just q <- [model.waiting]]

-- | Send the check of the question waiting, if its pause has ended, no
-- check was sent for it yet, and no ask is out.
send :: Model -> (Model, [Effect])
send model = case model.waiting of
  Just q
    | q.paused
    , isNothing q.ticket
    , isNothing model.out ->
        let Ticket n = model.next
         in ( model {next = Ticket (n + 1), out = Just model.next, waiting = Just q {ticket = Just model.next}}
            , [Check model.next]
            )
  _ -> (model, [])

-- | The lookup of these letters: the pages of every space the reader may
-- view but this one, at most 'elsewhereShown'.
queryFor :: Model -> Text -> TitleQuery
queryFor model letters = TitleQuery {letters, except = Just model.input.space, limit = elsewhereShown}

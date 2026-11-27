-- | 'Model' is the threads of one page as the page view shows them
-- ("Wiki.App.View.Page", with "Wiki.App.View.Comments" drawing them), and
-- 'step' is the one place they change (WP5.121 of docs/PLAN.md, decision
-- 82). Every input is an 'Event': the page view showing a page or going
-- elsewhere, the answer to a read of the threads, a message of the live
-- channel about the page, a write the reader asked for in the comments
-- view, and its answer, and the answer to a lookup of people. Every
-- output is an 'Effect': read the threads, send a write, or look up the
-- people the page and its threads mention.
--
-- = Where it sits
--
-- The model is a part of the page view's state, stepped by its own
-- events: the page view turns what concerns the threads (its fetch taken,
-- a change of route, the feed's messages about the page, a write the
-- comments view asks for) into events, runs 'step' in one step of its
-- program ("Halogen.Machine"), and carries out the effects; a read, a
-- write or a lookup of people is forked and its answer comes back as one
-- more event. The rest of the page view (its fetch, the ticks, the
-- chain's check) is as it was. The comments view holds what
-- is being written, asks for a write ('Write'), and is told its answer by
-- the page view.
--
-- = The people mentioned
--
-- The users the page's text and its comments mention (@\@login@) are the
-- model's too ('people'), so that the step that changes the threads also
-- asks for the people it lacks ('LookUp'), and the step that takes the
-- answer folds it in ('Found'). An answer about people is true whenever
-- it comes, for the reader and the space it was asked for: one that comes
-- late, or after the comment that mentioned the login is gone, is folded
-- in all the same, as it shows nothing wrong. When the reader or the space
-- may be another ('PeopleAgain', in the step the page view takes its new
-- input), what was known is forgotten, the lookups are asked in the
-- input's space from then on, and an answer asked for before then is
-- told apart by its number ('lookups') and dropped. A login whose lookup failed is asked for again at the next
-- change of the threads or of the page, not at once.
--
-- = The avatars shown
--
-- The addresses of the avatars the page view shows (the page's writers,
-- the comments' authors, the readers) are one map, the model's
-- ('avatars'), and every answer about avatars is an event of the
-- machine: the lookup it asks for its comments' authors at the steps
-- that change the threads ('Avatars', answered by 'Avatared'), and the
-- page view's own lookup of everyone it shows, when it takes a page or
-- hears who reads it ('Seen'). An answer sets the address of each person
-- it was asked for and found with one, and removes that of each person it
-- was asked for and found without one: someone asked for with no avatar
-- has none ('seenIn'). The last event about a person wins, in the order
-- the events are stepped. Each lookup has its tickets
-- ("Wiki.App.View.Latest"), so that its own late answer does not undo a
-- later one; between the two lookups the order is that of their answers'
-- arrival. Two lookups out at once whose snapshots of one person differ
-- (a comment's author as a read of the threads named them, and the same
-- person as a reader just now) can land in the wrong order, and the next
-- change of either puts it right. Nothing in either answer says which
-- snapshot is newer, and no order of the server's lies between them, as
-- none lay between two reads of the threads out at once before one read
-- was kept in flight.
--
-- = What an answer says
--
-- The machine infers nothing about what an answer covers from what its
-- threads and comments hold: no id, time or order says whether a read's
-- or a write's answer was made before or after a change the live channel
-- told. The live channel tells every change in the order it was made, and
-- its answer to the page's subscription is the threads as they were when
-- it subscribed, before every change it tells after. So:
--
-- * an event the channel tells is applied to the threads shown, a thread
--   or a reply added only when no thread or comment has its id;
-- * one read of the page's threads is out at a time: a read wanted while
--   one whose answer can still be taken is out (the page read again, the
--   read that follows a write, a reload, an encrypted page's change) is
--   asked when that answer comes, failed or not, and several wanted
--   meanwhile are one ('reading'). The server handles a read sent after
--   another's answer came after that one, so a read's answer taken is
--   never older than the one taken before it, whatever order the server
--   would handle reads out together in;
-- * the subscription's answer replaces the threads and puts every read out
--   behind it;
-- * a read's answer is taken only if no answer to a later read was
--   ("Wiki.App.View.Latest"), and only for the page shown; what the
--   channel told since the read was asked is applied to it again, in the
--   order told, as the threads shown took it when it came ('taking',
--   'remembered'). A failed read is shown only where no threads
--   are; threads kept after it are marked behind ('behind'), and the
--   comments view says so, with a button to read them again;
-- * a write's answer is taken only on the visit of the page it was sent
--   on. When the channel told of what it made or changed since it was
--   sent, the threads have it already; otherwise it is applied only when
--   the threads are as they were when it was sent (another write's answer
--   applied since changed them too), and a read asked after
--   it follows, which puts it in its place ('wrote'). What the answer does
--   beside the threads is the machine's too, and only then: the comments
--   view that asked is told it ('Answered'), and a thread opened in the
--   session of quoting still open ends it and is chosen ('opened'). A
--   session is numbered ('Quoting'): the same quote chosen again is
--   another.
--
-- The quote being commented on and the thread chosen are the model's
-- ('quoting', 'focused'): the reader's own changes to them are events
-- ('Quoted', 'GaveUp', 'Closed', 'Chose'), and the page view renders them
-- from here. So the step that takes an answer sees what it would
-- overwrite, and nothing else writes them.
module Wiki.App.View.Comments.Machine
  ( Model (..)
  , Writing (..)
  , Quoting (..)
  , Threads (..)
  , Request (..)
  , Event (..)
  , Effect (..)
  , initial
  , step
  , lacking
  , readyThreads
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude hiding (State)
import Wiki.Api.Events (PageEvent (..), applyEvent)
import Wiki.App.Backend (WikiError)
import Wiki.App.View.Latest (Ask, Asks, madeAfter, newAsk, noAsks, stillAnswerable, supersede, takes)
import Wiki.Core.Anchor (Anchor)
import Wiki.Core.Comment (Comment (..), Thread (..))
import Wiki.Core.Id (CommentId, CommentThreadId, PageId, UserId)
import Wiki.Core.Notification (mentionedIn)
import Wiki.Core.Space (Space (encryption))
import Wiki.Core.User (Avatar)
import Wiki.Core.Version (Author (avatar, id))

-- | A page's threads, as far as they have been read.
data Threads
  = ThreadsLoading
  | ThreadsFailed WikiError
  | ThreadsReady [Thread]
  deriving stock (Eq, Show)

-- | 'Model' is what the page view knows of the threads of the page it
-- shows.
data Model = Model
  { page :: Maybe PageId
  -- ^ The page shown, whose threads these are; 'Nothing' while the page
  -- view has none.
  , comments :: Bool
  -- ^ Whether the page shown has comments: not a home page, nor an older
  -- version ("Wiki.App.View.Page"'s @commentsHere@).
  , space :: Maybe Space
  -- ^ The space of the page shown, which the people are looked up in: the
  -- page view's input's, from the step that takes it ('PeopleAgain'), so
  -- that a lookup is never asked in a space the page has left (a plain
  -- space that became encrypted, whose logins must not reach the server).
  , encrypted :: Bool
  -- ^ Whether the page is in an encrypted space: the live channel then
  -- tells only that its threads changed, and they are read again.
  , threads :: Threads
  , asks :: Asks
  -- ^ The reads of the threads made, and which one's answer is shown.
  , written :: Int
  -- ^ How many writes were sent: the number of the latest.
  , writes :: Map Int Writing
  -- ^ The writes sent on this visit of the page that have not been
  -- answered, by number.
  , heard :: [(PageEvent, Asks)]
  -- ^ What the live channel told on this visit, the newest first, with
  -- the reads as they were then, while a read is out whose answer may be
  -- older ('taking').
  , flight :: Maybe (Ask, PageId)
  -- ^ The last read of the threads asked, with the page it reads, until
  -- its answer comes. While its answer can still be taken, no other read
  -- of that page is asked ('reading').
  , wanted :: Bool
  -- ^ Whether a read was wanted while that one was out: it is asked when
  -- that one's answer comes.
  , quoting :: Maybe Quoting
  -- ^ The quote of the page the reader is commenting on, in its session:
  -- a new thread about it is being written, in the panel beside the page.
  , sessions :: Int
  -- ^ How many sessions of quoting were opened: the number of the latest.
  , focused :: Maybe CommentThreadId
  -- ^ The thread the reader has chosen, by clicking its quote in the page
  -- or the thread itself, or that the reader's write opened.
  , behind :: Bool
  -- ^ Whether the threads shown are known to be behind the server's: a
  -- read whose answer was needed failed, and no read's or subscription's
  -- answer was taken since. The comments view says so, with a button to
  -- read them again.
  , closed :: Bool
  -- ^ Whether the page view has gone away: after that nothing changes and
  -- nothing is asked.
  , mentioned :: Set Text
  -- ^ The logins the text of the page shown mentions.
  , people :: Map Text Author
  -- ^ The users found among the logins looked up, by login.
  , lookedUp :: Set Text
  -- ^ Every login whose lookup answered, found or not.
  , lookingUp :: Set Text
  -- ^ The logins asked for whose answer has not come: not asked again
  -- meanwhile.
  , lookups :: Int
  -- ^ Which lookups count: the number 'PeopleAgain' raises. An answer to
  -- a lookup made under another number is dropped.
  , avatars :: Map UserId Text
  -- ^ The addresses of the avatars the page view shows, by person, as the
  -- events about them last set them (see "The avatars shown").
  , avatarAsks :: Asks
  -- ^ The machine's lookups of its authors' avatars, and which one's
  -- answer was taken last.
  }
  deriving stock (Eq, Show)

-- | 'Quoting' is one session of commenting on a quote: from the reader's
-- choice of the quote to its thread sent and answered, or the quote given
-- up. The same quote chosen again is another session, with a form of its
-- own: the number tells them apart, as the anchor cannot.
data Quoting = Quoting
  { session :: Int
  , anchor :: Anchor
  }
  deriving stock (Eq, Show)

-- | 'Writing' is a write sent on this visit of the page, until its answer
-- comes.
data Writing = Writing
  { page :: PageId
  , session :: Maybe Int
  -- ^ For a thread about a quote, the session of quoting it was written
  -- in.
  , told :: [PageEvent]
  -- ^ What the live channel told about the page since the write was sent.
  , quiet :: Bool
  -- ^ Whether the threads shown are as they were when the write was sent:
  -- nothing told, no read's answer taken, no subscription answered, no
  -- other write's answer applied.
  }
  deriving stock (Eq, Show)

-- | 'Request' is a write to a page's threads that the reader asks for in
-- the comments view.
data Request
  = -- | Open a thread, about the quote of a session of quoting or about
    -- the whole page, with its first comment. The session is the one the
    -- comments view's form was opened in, as its input named it.
    OpenThread (Maybe Quoting) Text
  | Reply CommentThreadId Text
  | EditComment CommentThreadId CommentId Text
  | -- | Delete a comment. The only comment of a thread stands for the
    -- thread.
    DeleteComment CommentThreadId CommentId
  | DeleteThread CommentThreadId
  | -- | Resolve a thread about a quote, or reopen it.
    Resolve CommentThreadId Bool
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the threads shown. A write is
-- asked for from a place @a@ (the comments view that asked), which its
-- 'Send' carries back.
data Event a
  = -- | The page view is mounted.
    Started
  | -- | The page view goes away.
    Finished
  | -- | The page view took a page it fetched (a new page, or the page
    -- shown read again): whether it has comments, its space, and its
    -- text, for the people it mentions.
    Fetched PageId Bool Space Text
  | -- | The page view went to another page or version: nothing is shown
    -- until it is fetched.
    Left'
  | -- | The answer to the read made under this ticket, for this page.
    Loaded Ask PageId (Either WikiError [Thread])
  | -- | The reader asked for a write to this page's threads.
    Write a PageId Request
  | -- | The answer to the write sent under this number, from the place
    -- that asked for it ('Send' gives both): the event the write made, or
    -- why it failed.
    Wrote a Int Request (Either WikiError PageEvent)
  | -- | The live channel's event on a page.
    Heard PageId PageEvent
  | -- | The live channel's answer to the page's subscription: its threads
    -- as they are now.
    Replaced PageId [Thread]
  | -- | The reader asked to read the threads again (after a failure).
    Reload
  | -- | The reader comments on this quote of the page: the panel opens with
    -- a new thread about it, and no thread chosen.
    Quoted Anchor
  | -- | The reader gave up the new thread about the quote.
    GaveUp
  | -- | The panel beside the page closed: the quote is given up.
    Closed
  | -- | The reader chose this thread.
    Chose CommentThreadId
  | -- | The page view took an input that may change who the people are:
    -- another page, another reader (the wiki changed), or another space,
    -- or the same space changed (made encrypted). The people are looked up
    -- again, in this space, the input's, from this step on.
    PeopleAgain Space
  | -- | The answer to the lookup of these logins made under this number:
    -- the users found among them.
    Found Int [Text] (Either WikiError (Map Text Author))
  | -- | The answer to the lookup of the avatars of these authors made
    -- under this ticket, or 'Nothing' when it failed.
    Avatared Ask [UserId] (Maybe (Map UserId Text))
  | -- | The page view's own lookup of avatars answered, and taken under
    -- its ticket: the people it was asked for, and the addresses found.
    Seen [UserId] (Map UserId Text)
  deriving stock (Eq, Show)

-- | 'Effect' is what a step asks the page view to do.
data Effect a
  = -- | Read the page's threads; the answer comes back as 'Loaded'.
    Load Ask PageId
  | -- | Send this write under this number. The answer comes back as
    -- 'Wrote'.
    Send a Int PageId Request
  | -- | Tell the comments view in this place the answer to the write it
    -- asked for: its form emptied, or why the write failed shown. Asked
    -- only on the visit of the page the write was sent on.
    Answered a Request (Maybe WikiError)
  | -- | Look up these logins in this space, under this number; the answer
    -- comes back as 'Found'.
    LookUp Space Int [Text]
  | -- | Look up the addresses of these avatars, under this ticket; the
    -- answer comes back as 'Avatared'.
    Avatars Ask [(UserId, Maybe Avatar)]
  deriving stock (Eq, Show)

-- | The threads before the page view is mounted: for the page it shows, if
-- any, with whether it has comments, its space, its text, and the threads
-- read at once if the backend could. The people they mention are looked
-- up once the view is mounted ('Started'), or at once by the page view,
-- which gives the answer as 'Found' ('lacking' says which).
initial :: Maybe PageId -> Bool -> Space -> Text -> Maybe (Either WikiError [Thread]) -> Model
initial page comments space text read' =
  Model
    { page
    , comments
    , space = Just space
    , encrypted = isJust space.encryption
    , threads = maybe ThreadsLoading (either ThreadsFailed ThreadsReady) (read' >>= \r -> r <$ guard comments)
    , asks = noAsks
    , written = 0
    , writes = mempty
    , heard = []
    , flight = Nothing
    , wanted = False
    , quoting = Nothing
    , sessions = 0
    , focused = Nothing
    , behind = False
    , closed = False
    , mentioned = if isJust page then mentionedIn text else mempty
    , people = mempty
    , lookedUp = mempty
    , lookingUp = mempty
    , lookups = 0
    , avatars = mempty
    , avatarAsks = noAsks
    }

-- | The threads after an event, and what the page view is asked to do.
step :: Event a -> Model -> (Model, [Effect a])
step event m
  | m.closed = (m, [])
  | otherwise = case event of
      Started -> lookingFirst $ case m.threads of
        ThreadsLoading -> reading m
        _ -> (m, [])
      Finished -> (m {closed = True}, [])
      -- Another page is another visit: the answers to the writes sent on
      -- the last one are not taken.
      Fetched page comments space text ->
        let mentioned = mentionedIn text
            encrypted = isJust space.encryption
         in lookingFirst (reading (if m.page == Just page then m {comments, space = Just space, encrypted, mentioned} else (elsewhere m) {page = Just page, comments, space = Just space, encrypted, mentioned}))
      Left' -> ((elsewhere m) {page = Nothing, threads = ThreadsLoading, asks = supersede m.asks, mentioned = mempty}, [])
      Quoted a -> let k = m.sessions + 1 in (m {quoting = Just Quoting {session = k, anchor = a}, sessions = k, focused = Nothing}, [])
      GaveUp -> (m {quoting = Nothing}, [])
      Closed -> (m {quoting = Nothing}, [])
      Chose tid -> (m {focused = Just tid}, [])
      PeopleAgain space -> (m {space = Just space, encrypted = isJust space.encryption, people = mempty, lookedUp = mempty, lookingUp = mempty, lookups = m.lookups + 1}, [])
      Found n logins answer
        | n /= m.lookups -> (m, [])
        | otherwise ->
            let asked = Set.fromList logins
             in case answer of
                  Right found -> (m {people = found <> m.people, lookedUp = m.lookedUp <> asked, lookingUp = m.lookingUp `Set.difference` asked}, [])
                  Left _ -> (m {lookingUp = m.lookingUp `Set.difference` asked}, [])
      -- An answer under an older ticket than the one taken last, or a
      -- failed one, changes nothing.
      Avatared asked authors answer -> case (answer, takes asked m.avatarAsks) of
        (Just found, Just after) -> (m {avatars = seenIn authors found m.avatars, avatarAsks = after}, [])
        _ -> (m, [])
      Seen asked found -> (m {avatars = seenIn asked found m.avatars}, [])
      -- The ticket is taken whichever page the answer is for; its threads
      -- only for the page shown.
      Loaded asked page answer -> case takes asked m.asks of
        Just after ->
          let m' = taking asked page answer m {asks = after}
              (m'', fx) = landed asked (if m'.threads /= m.threads then unquiet m' else m')
           in lookingFirst (m'', fx)
        Nothing -> lookingFirst (landed asked m)
      Write place page request ->
        let n = m.written + 1
            -- A thread about a quote is of the session its request names.
            session = case request of
              OpenThread (Just q) _ -> Just q.session
              _ -> Nothing
            writing = Writing {page, session, told = [], quiet = True}
         in (m {written = n, writes = if m.page == Just page then Map.insert n writing m.writes else m.writes}, [Send place n page request])
      -- The answer of a write sent on another visit, of this page or
      -- another, changes nothing and asks nothing: the forms it came from
      -- were emptied when the page changed.
      Wrote place n request answer -> case Map.lookup n m.writes of
        Just w
          | m.page == Just w.page ->
              let (m', fx) = wrote w answer (opened w answer m {writes = Map.delete n m.writes})
                  -- A thread of a session of quoting ended since: its form
                  -- is the newer session's now, and stays as it is.
                  formGone = isRight answer && isJust w.session && w.session /= ((\q -> q.session) <$> m.quoting)
               in (m', fx <> [Answered place request (leftToMaybe answer) | not formGone])
        _ -> (m {writes = Map.delete n m.writes}, [])
      Heard page e
        | m.page /= Just page -> (m, [])
        | e == ThreadsChanged -> reading m
        | otherwise -> lookingFirst (m {threads = applied e m.threads, heard = heardNow (remembered e m.threads) m, writes = map (\w -> w {told = e : w.told, quiet = False}) m.writes}, [])
      Replaced page ts
        | m.page /= Just page -> (m, [])
        | m.encrypted -> reading m
        -- A read wanted before it is not asked: the subscription's answer
        -- is the threads as they were when the page was subscribed to, and
        -- every change after that comes after it on the channel.
        | otherwise -> lookingFirst (unquiet m {threads = ThreadsReady ts, asks = supersede m.asks, heard = [], wanted = False, behind = False, focused = mfilter (\f -> any (\t -> t.id == f) ts) m.focused}, [])
      Reload -> reading m

-- | The model on another visit, or none: what was kept about the last
-- one's reads and writes forgotten.
elsewhere :: Model -> Model
elsewhere m = m {writes = mempty, heard = [], wanted = False, behind = False, quoting = Nothing, focused = Nothing}

-- | The model after the answer to a write sent on this visit: a thread
-- opened in the session of quoting still open ends it, and is chosen. A
-- thread of a session ended since changes neither, also when the reader
-- comments on the same quote again.
opened :: Writing -> Either WikiError PageEvent -> Model -> Model
opened w answer m = case answer of
  Right (ThreadOpened t) | isJust w.session, w.session == ((\q -> q.session) <$> m.quoting) -> m {quoting = Nothing, focused = Just t.id}
  _ -> m

-- | The writes out, with the threads shown changed since they were sent.
unquiet :: Model -> Model
unquiet m = m {writes = map (\w -> w {quiet = False}) m.writes}

-- | The answer to a write sent on this visit, taken. Nothing says where
-- the write's commit falls among what the channel told, so:
--
-- * when the channel told of the thread or the comment the write made or
--   changed since the write was sent ('about'), the threads have it in its
--   place already: nothing more;
-- * otherwise, when the threads shown are as they were when the write was
--   sent, its event is applied, so that the reader sees it at once. The
--   threads then differ from those the other writes out were sent on
--   ('unquiet'): the server may have handled another write before this
--   one or after it, and nothing in that write's answer says which, so
--   that answer is not applied (two edits of one comment, the later one's answer taken
--   first, then the earlier one's, would show the older text);
-- * either way, a read asked after the answer follows ('reading', which
--   asks it once the read out, if any, is answered): its answer was read
--   after the write was made, and it puts the write in its place among
--   what others made meanwhile. A write that failed may have been made
--   too, and is followed by a read as well.
wrote :: Writing -> Either WikiError PageEvent -> Model -> (Model, [Effect a])
wrote w answer m = case answer of
  Right e
    | any (about e) w.told -> (m, [])
    | w.quiet ->
        let m' = m {threads = applied e m.threads, heard = heardNow (remembered e m.threads) m}
         in lookingFirst (reading (if m'.threads /= m.threads then unquiet m' else m'))
  _ -> reading m

-- | Whether an event the channel told is about what a write's answer made
-- or changed: the write's own event told back, or a later change to the
-- same thread or comment, or its deletion.
about :: PageEvent -> PageEvent -> Bool
about answer told = case answer of
  ThreadOpened t -> onThread t.id
  CommentAdded c -> onComment c
  CommentEdited c -> told == answer || gone c
  CommentDeleted tid cid -> told == answer || told == CommentDeleted tid cid || told == ThreadDeleted tid
  ThreadDeleted tid -> told == ThreadDeleted tid
  ThreadResolved tid _ -> told == answer || told == ThreadDeleted tid
  _ -> False
  where
    onThread tid = case told of
      ThreadOpened t -> t.id == tid
      CommentAdded c -> c.thread == tid
      CommentEdited c -> c.thread == tid
      CommentDeleted t _ -> t == tid
      ThreadResolved t _ -> t == tid
      ThreadDeleted t -> t == tid
      _ -> False
    onComment c = case told of
      CommentAdded x -> x.id == c.id
      CommentEdited x -> x.id == c.id
      _ -> gone c
    gone c = case told of
      CommentDeleted _ cid -> cid == c.id
      ThreadDeleted tid -> tid == c.thread
      _ -> False

-- | The model after the answer to a read came, taken or not, failed or
-- not: when it was the read out, the read wanted meanwhile, if any, is
-- asked now.
landed :: Ask -> Model -> (Model, [Effect a])
landed asked m = case m.flight of
  Just (out, _)
    | out == asked ->
        if m.wanted
          then reading m {flight = Nothing, wanted = False}
          else (m {flight = Nothing}, [])
  _ -> (m, [])

-- | The answer to a read taken: for the page shown, its threads, with
-- what the live channel told since the read was asked applied again, in
-- the order told. The answer may have been read before or after each of
-- those changes was made, and nothing in it says which. Applied again, an
-- addition the answer holds is kept as the answer has it, one it lacks
-- is added as the threads showed it when it was told ('remembered'), a
-- deletion is one again, and an edit or a resolution is as told, which a
-- later one still on its way corrects. An addition the answer lacks may
-- also be one deleted since, whose deletion the channel has still to tell
-- or lost with the connection: it is shown until that deletion, or the
-- subscription's answer after the channel opens again, comes. A failure
-- is shown only where no threads are: threads shown are kept, and marked
-- behind the server's ('behind'), until a read's or a subscription's
-- answer is taken.
taking :: Ask -> PageId -> Either WikiError [Thread] -> Model -> Model
taking asked page answer m
  | m.page /= Just page = m'
  | otherwise = case (answer, m.threads) of
      (Right ts, _) -> m' {behind = False, threads = ThreadsReady (foldr applyEvent ts [e | (e, at) <- m.heard, not (asked `madeAfter` at)])}
      (Left _, ThreadsReady _) -> m' {behind = True}
      (Left e, _) -> m' {behind = False, threads = ThreadsFailed e}
  where
    m' = m {heard = stillWanted m.asks m.heard}

-- | An event of the channel, or of a write's answer, as the threads shown
-- took it, to be applied again to a read's answer ('taking'). A thread
-- opened or a reply added whose id the threads already show changed
-- nothing when it came: the threads show that thread or comment as
-- something taken before it left it, which may be newer than the event
-- (the answer to the reader's edit, taken before the channel told of the
-- thread's opening). So it is remembered as the threads show it, by its
-- id: applied again to an answer that lacks it, it shows what was shown,
-- never the text it was told with. Every other event changed the threads
-- as it says when it came, and is remembered as it is.
remembered :: PageEvent -> Threads -> PageEvent
remembered e threads = case e of
  ThreadOpened t | Just t' <- find (\x -> x.id == t.id) shown -> ThreadOpened t'
  CommentAdded c | Just c' <- find (\x -> x.id == c.id) [x | t <- shown, x <- t.comments] -> CommentAdded c'
  _ -> e
  where
    shown = case threads of
      ThreadsReady ts -> ts
      _ -> []

-- | The channel's event kept, if a read is out whose answer it may be newer
-- than.
heardNow :: PageEvent -> Model -> [(PageEvent, Asks)]
heardNow e m = stillWanted m.asks ((e, m.asks) : m.heard)

-- | What the channel told that an answer still to be taken may be older
-- than: told while a read whose answer can still be taken was out.
stillWanted :: Asks -> [(PageEvent, Asks)] -> [(PageEvent, Asks)]
stillWanted now = filter (\(_, at) -> stillAnswerable at now)

-- | A read of the threads begun, for the page shown, or wanted: while a
-- read of that page is out whose answer can still be taken, no other is
-- asked; it is marked wanted, and asked when that answer comes
-- ('landed'). A read whose answer can no longer be taken (another visit,
-- the subscription's answer since) holds nothing back. A page with no
-- comments has none, at once, under its own ticket.
reading :: Model -> (Model, [Effect a])
reading m = case m.page of
  Nothing -> (m, [])
  Just page
    | m.comments, Just (out, p) <- m.flight, p == page, isJust (takes out m.asks) -> (m {wanted = True}, [])
    | m.comments -> (m {asks = after, flight = Just (ticket, page), wanted = False}, [Load ticket page])
    | otherwise -> case takes ticket after of
        Just taken -> lookingFirst (m {asks = taken, threads = ThreadsReady []}, [])
        Nothing -> (m {asks = after}, [])
    where
      (ticket, after) = newAsk m.asks

-- | The threads after an event, if they are read.
applied :: PageEvent -> Threads -> Threads
applied e = \case
  ThreadsReady ts -> ThreadsReady (applyEvent e ts)
  other -> other

-- | The step's model and effects, with a lookup of the people it lacks,
-- if it lacks any ('lacking'), and of the avatars of the threads'
-- authors, before them: first, as the page view did them before the
-- step's read or write.
lookingFirst :: (Model, [Effect a]) -> (Model, [Effect a])
lookingFirst (m, fx) =
  let (m', look) = lookUp m
      (m'', faces) = avatarsLookedUp m'
   in (m'', look <> faces <> fx)

-- | A lookup of the people the model lacks, begun: they are being looked
-- up from then on. Nothing for none, or with no page shown.
lookUp :: Model -> (Model, [Effect a])
lookUp m = case (m.space, lacking m) of
  (Just space, logins@(_ : _)) -> (m {lookingUp = m.lookingUp <> Set.fromList logins}, [LookUp space m.lookups logins])
  _ -> (m, [])

-- | A lookup of the avatars of the threads' authors, under a new ticket,
-- with a page shown.
avatarsLookedUp :: Model -> (Model, [Effect a])
avatarsLookedUp m
  | isNothing m.page = (m, [])
  | otherwise =
      let (ticket, after) = newAsk m.avatarAsks
       in (m {avatarAsks = after}, [Avatars ticket [(c.author.id, c.author.avatar) | t <- readyThreads m, c <- t.comments]])

-- | The logins the page's text and its threads mention that are neither
-- looked up nor being looked up. None while no page is shown.
lacking :: Model -> [Text]
lacking m
  | isNothing m.page = []
  | otherwise = Set.toList (mentions `Set.difference` (m.lookedUp <> m.lookingUp))
  where
    mentions = m.mentioned <> foldMap mentionedIn [c.body | t <- readyThreads m, c <- t.comments]

-- | The addresses of avatars after an answer about them: each person
-- asked for gets the address found, or none when none was found; everyone
-- else's stays.
seenIn :: [UserId] -> Map UserId Text -> Map UserId Text -> Map UserId Text
seenIn asked found avatars = foldr (\u -> Map.alter (const (Map.lookup u found)) u) avatars asked

-- | The threads shown, if they are read.
readyThreads :: Model -> [Thread]
readyThreads m = case m.threads of
  ThreadsReady ts -> ts
  _ -> []

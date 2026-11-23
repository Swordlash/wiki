-- | 'Model' is what the panel of API keys ("Wiki.App.View.ApiKeys") knows,
-- and 'step' is the one place it changes (decision 82 of docs/PLAN.md).
-- Every input is an 'Event': a click or a key typed, an answer of the
-- backend, a change of the panel's input, the panel going away. Every
-- output is an 'Effect' as data: read the keys, make one, revoke one, copy
-- the key shown. The component runs 'step' in one step of its program and
-- carries out the effects; an answer comes back as one more event.
--
-- = One ask at a time
--
-- A read of the list, a revoke and a make are one flight: while one of
-- them is out, no other is sent ('flight'). A revoke or a make clicked
-- meanwhile waits its turn, in the order clicked ('queued'); a read wanted
-- meanwhile is sent when the ask out answers, failed or not, after what
-- waits, and several wanted are one ('wanted'). The server handles an ask
-- sent after another's answer came after that one, so a later list is
-- never an older one.
--
-- = Which list is shown
--
-- A read and a revoke both answer with the user's whole list. An answer is
-- taken only if no answer to a later ask was taken already
-- ("Wiki.App.View.Latest"), so an older list never replaces a newer one.
-- A make answers with the one key made: it is added to the list shown by
-- its id, and a read asked after that answer follows, whose list replaces
-- everything. The panel infers nothing from how many keys a list holds:
-- whether 20 keys work already is the server's to say, in its refusal.
--
-- What any answer says was revoked stays shown revoked ('known'), whichever
-- list is taken: the server never takes a revoke back.
--
-- = The key, once
--
-- The whole key is in the model from the step that takes the make's answer
-- to the step that closes its box, or the panel going away ('shown'). It
-- is kept as 'MadeApiKey', whose 'Show' hides it, and nothing here writes it
-- anywhere. A make's answer from an earlier session is not shown: the key
-- is the other user's.
--
-- = Who is signed in
--
-- The panel is the signed-in user's, by the session's number
-- ("Wiki.App.View.Common"'s @sessionNumber@). When it changes, the step
-- that takes it forgets the list, the key shown, the form and what waits
-- its turn, puts the asks out behind it, and reads again. An answer to an
-- ask of the earlier session changes nothing ('ofSession').
module Wiki.App.View.ApiKeys.Machine
  ( Model (..)
  , Form (..)
  , Choice (..)
  , Expiry (..)
  , Draft (..)
  , Write (..)
  , Event (..)
  , Effect (..)
  , KeyState (..)
  , initial
  , step
  , blankForm
  , listed
  , keyState
  , ordered
  , busy
  , choiceValue
  , choiceOf
  , askOf
  )
where

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day, UTCTime, defaultTimeLocale, parseTimeM)
import Protolude
import Wiki.Api.ApiKeys (ApiKeyId, ApiKeyInfo (..), KeyRight (..), MadeApiKey (..))
import Wiki.App.Account (SignInError)
import Wiki.App.Backend (WikiError)
import Wiki.App.View.Latest (Ask, Asks, madeAfter, newAsk, noAsks, supersede, takes)
import Wiki.Core.User (Password (..))

-- | 'Model' is the panel's state, without what only the component holds
-- (the field the key is shown in, for the copy).
data Model = Model
  { keys :: Maybe (Either WikiError [ApiKeyInfo])
  -- ^ The user's keys as the panel shows them, the newest first, or why
  -- they could not be read; 'Nothing' until the first answer.
  , now :: Maybe UTCTime
  -- ^ When the latest answer came: what a key's expiry is compared with.
  , asks :: Asks
  -- ^ The reads, revokes and makes sent, and which one's answer is shown.
  , flight :: Maybe Ask
  -- ^ The last ask sent, until its answer comes. While its answer can still
  -- be taken, no other is sent.
  , wanted :: Bool
  -- ^ Whether a read was wanted while that ask was out.
  , queued :: [(Int, Write)]
  -- ^ The revokes and makes clicked while an ask was out, in the order
  -- clicked, each with the session it was clicked in.
  , known :: Map ApiKeyId UTCTime
  -- ^ The keys some answer said were revoked, with when: shown revoked
  -- whatever list is taken.
  , revoking :: Set ApiKeyId
  -- ^ The keys whose revoke is out or waits its turn.
  , confirming :: Maybe ApiKeyId
  -- ^ The key whose revoke is asked once more before it is sent.
  , problem :: Maybe WikiError
  -- ^ Why the last revoke failed.
  , form :: Form
  , asksPassword :: Maybe Bool
  -- ^ Whether making a key asks for the current password: the user has
  -- one. 'Nothing' until known, when the field shows, as for a passkey.
  , making :: Bool
  -- ^ Whether a make is out or waits its turn.
  , refusal :: Maybe SignInError
  -- ^ Why the last make was refused, shown beside the form.
  , badDay :: Bool
  -- ^ Whether the last make was not sent because its day is not one.
  , shown :: Maybe MadeApiKey
  -- ^ The key just made, whole, until its box is closed.
  , copied :: Maybe Bool
  -- ^ Whether the copy of the key shown worked, once tried.
  , session :: Int
  -- ^ The number of the session the panel was last given.
  , sessionFrom :: Asks
  -- ^ The asks as they were when that session began.
  , administers :: Bool
  -- ^ Whether the user is an administrator: only then is the right
  -- @admin@ offered.
  , closed :: Bool
  -- ^ Whether the panel has gone away: no event changes anything after.
  }
  deriving stock (Eq, Show)

-- | 'Form' is what the user typed and chose to make a key.
data Form = Form
  { keyName :: Text
  , keyRight :: KeyRight
  , choice :: Choice
  , day :: Text
  -- ^ The day typed, as a date input gives it (@2027-06-30@), for
  -- 'OnDay'.
  , typed :: Password
  -- ^ The current password typed. It goes with one attempt, however it
  -- ends, and its 'Show' hides it.
  }
  deriving stock (Eq, Show)

-- | When a new key stops working, as the form offers it.
data Choice = In30Days | In90Days | InAYear | NeverExpires | OnDay
  deriving stock (Eq, Show, Enum, Bounded)

-- | 'Expiry' is a key's expiry as chosen: a number of days from the
-- moment it is made, never, or the end of a day in the user's time zone.
-- The component turns it into a moment when it sends the make: only the
-- browser knows the time zone.
data Expiry = AfterDays Int | Never | EndOf Day
  deriving stock (Eq, Show)

-- | 'Draft' is a key to make, as sent: the backend's 'NewApiKey' but for
-- its expiry, not yet a moment.
data Draft = Draft
  { name :: Text
  , right :: KeyRight
  , expiry :: Expiry
  , password :: Maybe Password
  }
  deriving stock (Eq, Show)

-- | 'Write' is a write that waits its turn.
data Write = RevokeKey ApiKeyId | MakeKey Draft
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the panel.
data Event
  = -- | The panel is mounted.
    Started
  | -- | The panel goes away.
    Finished
  | -- | The panel was given the number of the session signed in, and
    -- whether its user is an administrator.
    Given Int Bool
  | -- | The answer to the read made under this ticket, with when it came.
    Loaded Ask UTCTime (Either WikiError [ApiKeyInfo])
  | -- | Whether the session's user has a password, as asked in that
    -- session.
    ProofKnown Int (Either WikiError Bool)
  | NameTyped Text
  | RightChosen KeyRight
  | ChoiceMade Choice
  | DayTyped Text
  | -- | The current password typed; its 'Show' hides it.
    PasswordTyped Password
  | -- | The user submitted the form.
    MakeClicked
  | -- | The answer to the make sent under this ticket, with when it came.
    Made Ask UTCTime (Either SignInError MadeApiKey)
  | -- | The user clicked Revoke on a key: asked once more.
    RevokeClicked ApiKeyId
  | -- | The user said no.
    RevokeCancelled
  | -- | The user said yes.
    RevokeConfirmed
  | -- | The answer to the revoke of this key sent under this ticket: the
    -- user's keys after it.
    Revoked Ask ApiKeyId UTCTime (Either WikiError [ApiKeyInfo])
  | -- | The user clicked Copy beside the key shown.
    CopyClicked
  | -- | Whether the copy of this key worked.
    Copied ApiKeyId Bool
  | -- | The user closed the key's box.
    KeyClosed
  deriving stock (Show)

-- | 'Effect' is what a step asks the component to do.
data Effect
  = -- | Read the user's keys; the answer comes back as 'Loaded'.
    Load Ask
  | -- | Ask whether the user has a password, in this session; the answer
    -- comes back as 'ProofKnown'.
    LoadProof Int
  | -- | Make a key; the answer comes back as 'Made'.
    Make Ask Draft
  | -- | Revoke a key; the answer comes back as 'Revoked'.
    Revoke Ask ApiKeyId
  | -- | Copy the key shown, whose id this is; the answer comes back as
    -- 'Copied'.
    Copy ApiKeyId
  deriving stock (Eq, Show)

-- | 'KeyState' is whether a key works, as the list says.
data KeyState = Works | Expired | RevokedKey
  deriving stock (Eq, Show)

-- | The panel before it is mounted, for a session and whether its user
-- administers.
initial :: Int -> Bool -> Model
initial session administers =
  Model
    { keys = Nothing
    , now = Nothing
    , asks = noAsks
    , flight = Nothing
    , wanted = False
    , queued = []
    , known = mempty
    , revoking = mempty
    , confirming = Nothing
    , problem = Nothing
    , form = blankForm
    , asksPassword = Nothing
    , making = False
    , refusal = Nothing
    , badDay = False
    , shown = Nothing
    , copied = Nothing
    , session
    , sessionFrom = noAsks
    , administers
    , closed = False
    }

-- | The form as it opens: no name, the right to read, 90 days.
blankForm :: Form
blankForm = Form {keyName = "", keyRight = ReadRight, choice = In90Days, day = "", typed = Password ""}

-- | The panel after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m
  | m.closed = (m, [])
  | otherwise = case event of
      Started -> (LoadProof m.session :) <$> reading m
      -- The key leaves the model with the panel.
      Finished -> (m {closed = True, shown = Nothing}, [])
      Given session administers
        | session /= m.session ->
            let behind = supersede m.asks
                fresh = (initial session administers) {asks = behind, sessionFrom = behind}
             in (LoadProof session :) <$> reading fresh
        | otherwise -> (m {administers, form = allowed administers m.form}, [])
      Loaded asked at answer
        | ofSession asked m -> landed asked (taking asked answer (learning answer m {now = later at m.now}))
        | otherwise -> landed asked m
      ProofKnown session answer
        | session == m.session, Right has <- answer -> (m {asksPassword = Just has}, [])
        | otherwise -> (m, [])
      NameTyped t -> (m {form = m.form {keyName = t}}, [])
      RightChosen r
        | r /= AdminRight || m.administers -> (m {form = m.form {keyRight = r}}, [])
        | otherwise -> (m, [])
      ChoiceMade c -> (m {form = m.form {choice = c}, badDay = False}, [])
      DayTyped t -> (m {form = m.form {day = t}, badDay = False}, [])
      PasswordTyped p -> (m {form = m.form {typed = p}}, [])
      MakeClicked
        | m.making -> (m, [])
        | otherwise -> case expiryOf m.form of
            -- A day that is not one: nothing is sent, and the form says so.
            Nothing -> (m {refusal = Nothing, badDay = True, form = m.form {typed = Password ""}}, [])
            Just expiry ->
              let typed = m.form.typed.text
                  draft =
                    Draft
                      { name = m.form.keyName
                      , right = m.form.keyRight
                      , expiry
                      , password = if T.null typed || m.asksPassword == Just False then Nothing else Just (Password typed)
                      }
                  -- The password goes with this one attempt.
                  cleared = m {making = True, refusal = Nothing, badDay = False, form = m.form {typed = Password ""}}
               in writing (MakeKey draft) cleared
      Made asked _ _
        | not (ofSession asked m) -> landed asked m
      Made asked at answer ->
        let m1 = m {making = False, now = later at m.now}
            (m2, fx1) = case answer of
              Left e -> (m1 {refusal = Just e}, [])
              -- The key made is added to the list by its id, and a read
              -- asked after this answer follows: its list is exact.
              Right made' ->
                let m' = m1 {shown = Just made', copied = Nothing, form = blankForm, keys = fmap (withMade made'.info) <$> m1.keys, asks = maybe m1.asks identity (takes asked m1.asks)}
                 in reading m'
            (m3, fx2) = landed asked m2
         in (m3, fx1 <> fx2)
      RevokeClicked i
        | works m i && not (i `Set.member` m.revoking) -> (m {confirming = Just i}, [])
        | otherwise -> (m, [])
      RevokeCancelled -> (m {confirming = Nothing}, [])
      RevokeConfirmed -> case m.confirming of
        Just i
          | works m i && not (i `Set.member` m.revoking) ->
              writing (RevokeKey i) m {confirming = Nothing, problem = Nothing, revoking = Set.insert i m.revoking}
        _ -> (m {confirming = Nothing}, [])
      Revoked asked _ _ _
        | not (ofSession asked m) -> landed asked m
      Revoked asked i at answer ->
        let m1 = learning answer m {revoking = Set.delete i m.revoking, now = later at m.now}
            (m2, fx1) = case answer of
              Right _ -> (taking asked answer m1, [])
              -- A revoke that failed may have been done all the same (the
              -- answer lost on the way): a read follows.
              Left e -> reading m1 {problem = Just e}
            (m3, fx2) = landed asked m2
         in (m3, fx1 <> fx2)
      CopyClicked -> case m.shown of
        Just made' -> (m {copied = Nothing}, [Copy made'.info.id])
        Nothing -> (m, [])
      Copied i ok
        | Just made' <- m.shown, made'.info.id == i -> (m {copied = Just ok}, [])
        | otherwise -> (m, [])
      KeyClosed -> (m {shown = Nothing, copied = Nothing}, [])

-- | The later of a time and the time known.
later :: UTCTime -> Maybe UTCTime -> Maybe UTCTime
later t = Just . maybe t (max t)

-- | The form with the right @admin@ taken back from a user who no longer
-- administers.
allowed :: Bool -> Form -> Form
allowed administers f
  | f.keyRight == AdminRight && not administers = f {keyRight = ReadRight}
  | otherwise = f

-- | The expiry the form chose, if its day is one.
expiryOf :: Form -> Maybe Expiry
expiryOf f = case f.choice of
  In30Days -> Just (AfterDays 30)
  In90Days -> Just (AfterDays 90)
  InAYear -> Just (AfterDays 365)
  NeverExpires -> Just Never
  OnDay -> EndOf <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" (toS (T.strip f.day))

-- | Whether a key is listed as working.
works :: Model -> ApiKeyId -> Bool
works m i = any (\k -> k.id == i && keyState m.now k == Works) (listed m)

-- | The answer to a read or a revoke taken, if no answer to a later ask
-- was. A failure is shown only where no list is held: a list held is kept.
taking :: Ask -> Either WikiError [ApiKeyInfo] -> Model -> Model
taking asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case answer of
    Right ks -> m {asks = after, keys = Just (Right (map (withKnown m.known) ks))}
    Left _ -> case m.keys of
      Just (Right _) -> m {asks = after}
      _ -> m {asks = after, keys = Just answer}

-- | What an answer says was revoked learnt, and shown on the list as it
-- is, whether the answer is taken or not.
learning :: Either WikiError [ApiKeyInfo] -> Model -> Model
learning answer m = case answer of
  Right ks ->
    let known = Map.union m.known (Map.fromList [(k.id, t) | k <- ks, Just t <- [k.revokedAt]])
     in m {known, keys = fmap (map (withKnown known)) <$> m.keys}
  Left _ -> m

-- | A key shown revoked if an answer said so.
withKnown :: Map ApiKeyId UTCTime -> ApiKeyInfo -> ApiKeyInfo
withKnown known k = case (k.revokedAt, Map.lookup k.id known) of
  (Nothing, Just t) -> k {revokedAt = Just t}
  _ -> k

-- | The list with a key made added by its id, first, if it does not hold
-- it.
withMade :: ApiKeyInfo -> [ApiKeyInfo] -> [ApiKeyInfo]
withMade info ks
  | any (\k -> k.id == info.id) ks = ks
  | otherwise = info : ks

-- | The panel after the answer to an ask came, taken or not, failed or
-- not: when it was the ask out, the next is sent, the first write waiting
-- of this session, or else the read wanted.
landed :: Ask -> Model -> (Model, [Effect])
landed asked m
  | m.flight /= Just asked = (m, [])
  | otherwise = case [w | (s, w) <- m.queued, s == m.session] of
      w : rest -> send w m {flight = Nothing, queued = [(m.session, x) | x <- rest]}
      []
        | m.wanted -> sendRead m {flight = Nothing, queued = []}
        | otherwise -> (m {flight = Nothing, queued = []}, [])

-- | Whether an ask was made in the session the panel is in.
ofSession :: Ask -> Model -> Bool
ofSession asked m = asked `madeAfter` m.sessionFrom

-- | Whether an ask is out whose answer can still be taken.
inFlight :: Model -> Bool
inFlight m = maybe False (\a -> isJust (takes a m.asks)) m.flight

-- | A read begun, or wanted while an ask is out.
reading :: Model -> (Model, [Effect])
reading m
  | inFlight m = (m {wanted = True}, [])
  | otherwise = sendRead m

-- | A write begun, or put in its turn while an ask is out.
writing :: Write -> Model -> (Model, [Effect])
writing w m
  | inFlight m = (m {queued = m.queued <> [(m.session, w)]}, [])
  | otherwise = send w m

sendRead :: Model -> (Model, [Effect])
sendRead m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket, wanted = False}, [Load ticket])

send :: Write -> Model -> (Model, [Effect])
send w m =
  let (ticket, after) = newAsk m.asks
   in ( m {asks = after, flight = Just ticket}
      ,
        [ case w of
            RevokeKey i -> Revoke ticket i
            MakeKey d -> Make ticket d
        ]
      )

-- | The keys the list shows, as the panel holds them.
listed :: Model -> [ApiKeyInfo]
listed m = foldMap (fromRight []) m.keys

-- | Whether a key works at a time: not revoked, and not expired. With no
-- time known yet, an expiry counts as not past.
keyState :: Maybe UTCTime -> ApiKeyInfo -> KeyState
keyState now k
  | isJust k.revokedAt = RevokedKey
  | Just e <- k.expiresAt, Just t <- now, e <= t = Expired
  | otherwise = Works

-- | The keys in the order the list shows them: those that work first, then
-- the revoked and expired ones, each the newest first as the server lists
-- them.
ordered :: Model -> [ApiKeyInfo]
ordered m = let (working, others) = partition (\k -> keyState m.now k == Works) (listed m) in working <> others

-- | Whether the form's button is disabled: a make is out or waits.
busy :: Model -> Bool
busy m = m.making

-- | A choice as the form's select names it.
choiceValue :: Choice -> Text
choiceValue = \case
  In30Days -> "30"
  In90Days -> "90"
  InAYear -> "365"
  NeverExpires -> "never"
  OnDay -> "day"

-- | The choice a select's value names.
choiceOf :: Text -> Maybe Choice
choiceOf v = find (\c -> choiceValue c == v) [minBound .. maxBound]

-- | The ticket an effect was sent under, if it is an ask of the flight.
askOf :: Effect -> Maybe Ask
askOf = \case
  Load a -> Just a
  Make a _ -> Just a
  Revoke a _ -> Just a
  _ -> Nothing

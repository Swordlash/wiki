-- | The user's API keys: the in-memory backend's rules
-- ("Wiki.App.Backend.Mem.State"'s 'makeApiKeyIn', the demo's and the
-- native tests' server), and the panel's machine
-- ("Wiki.App.View.ApiKeys.Machine") on generated runs.
--
-- = The model of the server
--
-- The runs' server is the in-memory state itself, with two readers, the
-- demo's administrator and its editor: 'makeApiKeyIn' and
-- 'revokeApiKeyIn' take the server's rules from @wiki-server@'s
-- @Wiki.Server.ApiKeys@ (the checks in its order, the 20 keys that work,
-- the 100 characters of a name, the right @admin@ for an administrator
-- only, a past expiry refused, the default name, the newest first then by
-- id, a revoked key listed and its time kept, another user's key not
-- found), and the first group of this spec checks each against the
-- server's words. What the state does not hold is modelled here, from
-- @wiki-server@'s @Api.hs@ (@makeApiKey@ and @reauthenticated@): the
-- password, checked before any rule, a wrong one counted, and after five
-- wrong ones in a row the answer "too many attempts" (the server's limit
-- is its sign-in form's; the number is the model's, and the view reads
-- none). A key's last use is written by programs, not by the view: the
-- model's server marks one used at any time ('Use'). Keys are also made
-- and revoked outside the panel, by the console or in another tab
-- ('MakeElsewhere', which can reach the 20 keys that work, and
-- 'RevokeElsewhere'), and sign-ins fail elsewhere up to the limit
-- ('FailElsewhere'). Time goes forward in steps ('Tick'), so keys expire
-- while the panel shows them. Not modelled: a user removed, whose keys
-- go with them (the panel is then not shown to anybody), and a user who
-- stops being an administrator (the panel's input changes, and nothing
-- of the list does).
--
-- Each ask has three moments: the panel sends it, the server handles it
-- (a read takes the list as it is then; a make or a revoke changes it and
-- answers), and the answer comes back. The server handles asks in the
-- order sent, the answers come back in any order ('Handle', 'Deliver'):
-- the HTTP backend over one connection. These calls are never cached by
-- the HTTP backend, so there is no cache to model.
module Test.ApiKeys
  ( spec

    -- * Shared with the component's tests
  , Server (..)
  , newServer
  , handled
  , keysOf
  , laptop
  , at
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), addDays, addUTCTime, fromGregorian)
import Protolude
import Test.Hspec
import Test.QuickCheck (Gen, chooseInt, counterexample, elements, forAllShrinkShow, frequency, listOf, shrinkList, withNumTests)
import Test.QuickCheck qualified as QC
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.App.Account (SignInError (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Mem.State (MemState (..), apiKeysIn, makeApiKeyIn, maxLiveKeys, revokeApiKeyIn)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.View.ApiKeys.Machine
import Wiki.App.View.Latest (madeAfter)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..))

spec :: Spec
spec = describe "API keys" $ do
  memRules
  machine

--------------------------------------------------------------------------------
-- The in-memory backend's rules

-- | A moment of the runs: the demo's day, and seconds after it.
at :: Int -> UTCTime
at s = addUTCTime (fromIntegral s) (UTCTime (fromGregorian 2026 11 20) 0)

-- | The demo's wiki with someone signed in.
as :: Text -> MemState
as u = (demoState demoNow) {current = Just (UserId ("user-" <> u))}

-- | A key to make, by name and right, with no expiry.
laptop :: Text -> KeyRight -> NewApiKey
laptop name right = NewApiKey {name, right, expiresAt = Nothing, password = Nothing}

-- | A key to make that expires at a moment.
expiring :: UTCTime -> NewApiKey -> NewApiKey
expiring t new = NewApiKey {name = new.name, right = new.right, expiresAt = Just t, password = new.password}

-- | The result of a write that the test expects to be done.
sure :: (Show e) => Either e a -> a
sure = either (panic . show) identity

made :: Int -> ApiKeyId -> NewApiKey -> MemState -> Either WikiError (MemState, ApiKeyInfo)
made s i new = makeApiKeyIn (at s) i new

memRules :: Spec
memRules = describe "in the in-memory backend, by the server's rules" $ do
  it "makes a key, named by the moment when no name is given, and lists the user's keys the newest first" $ do
    let (st1, a) = sure (made 0 (ApiKeyId "a") (laptop "laptop" WriteRight) (as "editor"))
        (st2, b) = sure (made 60 (ApiKeyId "b") (laptop "  " ReadRight) st1)
    a.name `shouldBe` "laptop"
    b.name `shouldBe` "Made on 2026-11-20 at 00:01 UTC"
    map (\k -> k.id) <$> apiKeysIn st2 `shouldBe` Right [ApiKeyId "b", ApiKeyId "a"]
    -- Another user's list is their own.
    apiKeysIn st2 {current = Just (UserId "user-admin")} `shouldBe` Right []
    apiKeysIn st2 {current = Nothing} `shouldBe` Left Forbidden

  it "refuses as the server does, in its order: the right to administer, then the expiry, the name, the count" $ do
    let editor = as "editor"
        refused new st = leftToMaybe (made 0 (ApiKeyId "x") new st)
        refusedWith new st = leftToMaybe (made 0 (ApiKeyId "x") new st)
    refusedWith (expiring (at (-1)) (laptop "x" AdminRight)) editor `shouldBe` Just Forbidden
    isRight (made 0 (ApiKeyId "x") (laptop "x" AdminRight) (as "admin")) `shouldBe` True
    refusedWith (expiring (at 0) (laptop "x" ReadRight)) editor `shouldBe` Just (Invalid "A key's expiry has to be in the future: choose a later day, or none.")
    refusedWith (laptop (T.replicate 101 "n") ReadRight) editor `shouldBe` Just (Invalid "A key's name has at most 100 characters.")
    isRight (made 0 (ApiKeyId "x") (laptop (" " <> T.replicate 100 "n" <> " ") ReadRight) editor) `shouldBe` True
    let full = foldl' (\st k -> fst (sure (made 0 (ApiKeyId (show k)) (laptop "k" ReadRight) st))) editor [1 .. maxLiveKeys]
    refusedWith (laptop "one more" ReadRight) full `shouldBe` Just (Invalid "You have 20 keys that work already: revoke one first.")
    -- A revoked key and an expired one do not count.
    let (revoked, _) = sure (revokeApiKeyIn (at 0) (ApiKeyId "1") full)
    isRight (made 0 (ApiKeyId "x") (laptop "one more" ReadRight) revoked) `shouldBe` True
    let soon = foldl' (\st k -> fst (sure (made 0 (ApiKeyId (show k)) (expiring (at 10) (laptop "k" ReadRight)) st))) editor [1 .. maxLiveKeys]
    isJust (refused (laptop "x" ReadRight) soon) `shouldBe` True
    isRight (made 10 (ApiKeyId "x") (laptop "x" ReadRight) soon) `shouldBe` True
    -- Nobody signed in makes nothing.
    refusedWith (laptop "x" ReadRight) (as "editor") {current = Nothing} `shouldBe` Just Forbidden

  it "revokes a key, which stays listed with its first time, and finds no key of another user" $ do
    let (st1, _) = sure (made 0 (ApiKeyId "a") (laptop "laptop" WriteRight) (as "editor"))
        (st2, revokedAll) = sure (revokeApiKeyIn (at 5) (ApiKeyId "a") st1)
        (_, again) = sure (revokeApiKeyIn (at 9) (ApiKeyId "a") st2)
    map (\k -> (k.id, k.revokedAt)) revokedAll `shouldBe` [(ApiKeyId "a", Just (at 5))]
    map (\k -> k.revokedAt) again `shouldBe` [Just (at 5)]
    leftToMaybe (revokeApiKeyIn (at 5) (ApiKeyId "a") st2 {current = Just (UserId "user-admin")}) `shouldBe` Just NotFound

--------------------------------------------------------------------------------
-- The model of the server

-- | 'Server' is the server's keys and what it knows of the readers: the
-- in-memory state, the time, which reader is signed in, and each reader's
-- password and count of wrong ones.
data Server = Server
  { state :: MemState
  , clock :: Int
  -- ^ Seconds since the day of the runs.
  , minted :: Int
  -- ^ How many keys it has made: the next one's number.
  , wrong :: Map Text Int
  -- ^ Each reader's wrong passwords in a row.
  }
  deriving stock (Show)

-- | The demo's wiki with no key yet, the reader @editor@ signed in.
newServer :: Server
newServer = Server {state = as "editor", clock = 0, minted = 0, wrong = mempty}

-- | Each reader's password: the editor has one, the administrator none (a
-- provider's sign-in).
passwordOf :: Text -> Maybe Text
passwordOf = \case
  "editor" -> Just "correct horse"
  _ -> Nothing

-- | The wrong passwords in a row after which the server answers "too many
-- attempts".
limit :: Int
limit = 5

-- | The reader a session's number names: the two take turns.
readerOf :: Int -> Text
readerOf s = if even s then "editor" else "admin"

-- | A reader's keys on the server.
keysOf :: Text -> Server -> [ApiKeyInfo]
keysOf u s = Map.findWithDefault [] (UserId ("user-" <> u)) s.state.apiKeys

-- | 'Answer' is what the server answered an ask.
data Answer
  = Listed (Either WikiError [ApiKeyInfo])
  | Minted (Either SignInError MadeApiKey)
  deriving stock (Show)

-- | The server's handling of an ask sent with a reader's cookie: the
-- server after it, and the answer.
handled :: Text -> Effect -> Server -> (Server, Maybe Answer)
handled u e s0 = case e of
  Load _ -> (s, Just (Listed (apiKeysIn st)))
  Revoke _ i -> case revokeApiKeyIn (at s.clock) i st of
    Left err -> (s, Just (Listed (Left err)))
    Right (st', ks) -> (s {state = st'}, Just (Listed (Right ks)))
  Make _ d
    | count >= limit -> (s, Just (Minted (Left (TooManyAttempts "Too many attempts: wait a while, then try again."))))
    | Just p <- passwordOf u, fmap (\x -> x.text) d.password /= Just p -> (s {wrong = Map.insert u (count + 1) s.wrong}, Just (Minted (Left WrongCredentials)))
    | otherwise ->
        let i = ApiKeyId ("k" <> show (s.minted + 1))
            new = NewApiKey {name = d.name, right = d.right, expiresAt = expiryAt d.expiry, password = Nothing}
         in case makeApiKeyIn (at s.clock) i new st of
              Left err -> (s {wrong = Map.delete u s.wrong}, Just (Minted (Left (SignInFailed err))))
              Right (st', info) -> (s {state = st', minted = s.minted + 1, wrong = Map.delete u s.wrong}, Just (Minted (Right MadeApiKey {key = "wiki_" <> i.text <> "_secret", info})))
  _ -> (s, Nothing)
  where
    s = s0
    st = s.state {current = Just (UserId ("user-" <> u))}
    count = Map.findWithDefault 0 u s.wrong
    expiryAt = \case
      Never -> Nothing
      AfterDays n -> Just (at (s.clock + n * 86400))
      -- The day's end in UTC, as natively.
      EndOf day -> Just (UTCTime (succ day) 0)

--------------------------------------------------------------------------------
-- Runs

-- | 'Step' is one thing that happens in a run. A step that cannot happen in
-- the state it comes to is skipped, so that a run shrinks to any part.
data Step
  = TypeName Int
  | ChooseRight KeyRight
  | Choose Choice
  | TypeDay Int
  | -- | The reader types their password, or a wrong one.
    TypePassword Bool
  | -- | The reader submits the form, having typed their password, or a
    -- wrong one, first, or nothing.
    ClickMake (Maybe Bool)
  | ClickRevoke Int
  | -- | The reader clicks Revoke on the key at this place and confirms.
    RevokeNow Int
  | Confirm
  | Cancel
  | ClickCopy
  | CopyAnswered Bool
  | CloseKey
  | -- | The server handles the oldest ask it has not handled.
    Handle
  | -- | The handled answer at this place among those handled comes back.
    Deliver Int
  | -- | A program uses the key at this place on the server.
    Use Int
  | -- | The console makes this many keys for the reader signed in.
    MakeElsewhere Int
  | -- | Another tab revokes the reader's key at this place.
    RevokeElsewhere Int
  | -- | The reader's sign-in fails elsewhere until the limit is reached.
    FailElsewhere
  | -- | Time goes on, by this many days.
    Tick Int
  | -- | Another reader signs in, in another tab.
    SwitchUser
  | Finish
  deriving stock (Eq, Show)

-- | A run's steps. A revoke answered takes a make, its answer, the read
-- after it, and the revoke's ask and answer: runs are three times
-- QuickCheck's usual length, so that enough of them get there.
genRun :: Gen [Step]
genRun =
  QC.scale (* 3) . listOf . frequency $
    [ (1, TypeName <$> frequency [(4, chooseInt (0, 1)), (1, pure 2)])
    , (1, ChooseRight <$> elements [minBound .. maxBound])
    , (1, Choose <$> elements [minBound .. maxBound])
    , (1, TypeDay <$> chooseInt (-2, 40))
    , (2, TypePassword <$> frequency [(4, pure True), (1, pure False)])
    , (6, ClickMake <$> frequency [(6, pure (Just True)), (1, pure (Just False)), (1, pure Nothing)])
    , (2, ClickRevoke <$> place)
    , (5, RevokeNow <$> place)
    , (3, pure Confirm)
    , (1, pure Cancel)
    , (1, pure ClickCopy)
    , (1, CopyAnswered <$> elements [True, False])
    , (1, pure CloseKey)
    , (8, pure Handle)
    , -- Mostly the oldest answer: with one flight there is mostly one.
      (8, Deliver <$> frequency [(6, pure 0), (1, pure 1), (1, pure 2)])
    , (1, Use <$> chooseInt (0, 3))
    , (1, MakeElsewhere <$> elements [1, 19])
    , (1, RevokeElsewhere <$> chooseInt (0, 3))
    , (1, pure FailElsewhere)
    , (1, Tick <$> elements [1, 30, 100])
    , (1, pure SwitchUser)
    , (1, pure Finish)
    ]

-- | A place in a list, mostly the first: a run's lists are short.
place :: Gen Int
place = frequency [(4, pure 0), (1, chooseInt (1, 3))]

shrinkRun :: [Step] -> [[Step]]
shrinkRun = shrinkList $ \case
  Deliver i -> Deliver <$> [0 .. i - 1]
  ClickRevoke i -> ClickRevoke <$> [0 .. i - 1]
  RevokeNow i -> ClickRevoke i : (RevokeNow <$> [0 .. i - 1])
  ClickMake typed -> ClickMake <$> [Nothing | isJust typed]
  RevokeElsewhere i -> RevokeElsewhere <$> [0 .. i - 1]
  MakeElsewhere n -> MakeElsewhere <$> [1 | n > 1]
  _ -> []

-- | An ask the panel made: the effect, the reader whose cookie it went
-- with, and the answer once handled.
data Out = Out
  { effect :: Effect
  , reader :: Text
  , inSession :: Int
  , answer :: Maybe Answer
  }
  deriving stock (Show)

-- | A run under way.
data World = World
  { panel :: Model
  , backend :: Server
  , outs :: [Out]
  , turn :: Int
  -- ^ The number of the session signed in.
  , finished :: Bool
  , copies :: [ApiKeyId]
  -- ^ The copies asked for and not answered.
  , revokedHeard :: Map ApiKeyId Int
  -- ^ The keys whose revoke the panel has had an answer to, with the
  -- session it was in.
  , expectShown :: Bool
  -- ^ Whether the panel should hold a key's text: an answer that made a
  -- key came in this session, and its box was not closed since.
  , seen :: [Text]
  -- ^ What the run reached, for the coverage of the runs.
  , trace :: [Text]
  , faults :: [Text]
  }

start :: World
start = afterEvent "mounted" Started World {panel = initial 0 False, backend = newServer, outs = [], turn = 0, finished = False, copies = [], revokedHeard = mempty, expectShown = False, seen = [], trace = [], faults = []}

-- | The world after a step.
play :: World -> Step -> World
play w = \case
  TypeName k -> afterEvent "types a name" (NameTyped (["", "laptop", T.replicate 101 "n"] `at'` k)) w
  ChooseRight r -> afterEvent ("chooses " <> show r) (RightChosen r) w
  Choose c -> afterEvent ("chooses " <> show c) (ChoiceMade c) w
  TypeDay d -> afterEvent "types a day" (DayTyped (if d < 0 then "someday" else show (addDays (fromIntegral (w.backend.clock `div` 86400 + d)) (fromGregorian 2026 11 20)))) w
  TypePassword right -> afterEvent "types a password" (PasswordTyped (Password (if right then "correct horse" else "battery staple"))) w
  ClickMake typed
    | w.finished -> w
    | otherwise ->
        let w1 = maybe w (play w . TypePassword) typed
         in afterEvent "makes a key" MakeClicked w1
  ClickRevoke i -> case atMay [k | k <- ordered w.panel, keyState w.panel.now k == Works] i of
    Just k | not w.finished -> afterEvent ("revokes " <> k.id.text) (RevokeClicked k.id) w
    _ -> w
  RevokeNow i -> case play w (ClickRevoke i) of
    w1 | w1.panel.confirming /= w.panel.confirming -> play w1 Confirm
    _ -> w
  Confirm -> afterEvent "confirms" RevokeConfirmed w
  Cancel -> afterEvent "cancels" RevokeCancelled w
  ClickCopy -> afterEvent "copies" CopyClicked w
  CopyAnswered ok -> case w.copies of
    i : rest -> afterEvent "copy answered" (Copied i ok) w {copies = rest}
    [] -> w
  CloseKey -> afterEvent "closes the key" KeyClosed w {expectShown = False}
  Handle -> case break (isNothing . (\o -> o.answer)) w.outs of
    (earlier, o : later') ->
      let (backend, a) = handled o.reader o.effect w.backend
       in noted ("the server handles " <> show o.effect) w {backend, outs = earlier <> [o {answer = a}] <> later'}
    _ -> w
  Deliver i -> case atMay [(k, o, a) | (k, o) <- zip [0 :: Int ..] w.outs, Just a <- [o.answer]] i of
    Just (k, o, a) ->
      let w' = w {outs = [x | (j, x) <- zip [0 ..] w.outs, j /= k]}
          now = at w.backend.clock
       in case (o.effect, a) of
            (Load t, Listed r) -> afterEvent "a list comes back" (Loaded t now r) w'
            (Revoke t i', Listed r) ->
              afterEvent "a revoke comes back" (Revoked t i' now r) w' {revokedHeard = if isRight r && o.inSession == w.turn then Map.insert i' w.turn w.revokedHeard else w.revokedHeard, seen = ["a revoke answered" | isRight r, o.inSession == w.turn] <> w.seen}
            (Make t _, Minted r) ->
              let ours = o.inSession == w.turn
                  tag = case r of
                    Right _ -> "a key made"
                    Left WrongCredentials -> "refused: a wrong password"
                    Left (TooManyAttempts _) -> "refused: too many attempts"
                    Left (SignInFailed (Invalid why)) | "20 keys" `T.isInfixOf` why -> "refused: 20 keys"
                    Left _ -> "refused: another rule"
               in afterEvent "a make comes back" (Made t now r) w' {expectShown = w.expectShown || (ours && isRight r && not w.finished), seen = [tag | ours] <> w.seen}
            _ -> w'
    Nothing -> w
  Use i -> case atMay (keysOf (readerOf w.turn) w.backend) i of
    Just k ->
      let used = k {lastUsedAt = Just (at w.backend.clock)}
          st = w.backend.state
       in noted ("a program uses " <> k.id.text) w {backend = w.backend {state = st {apiKeys = Map.adjust (map (\x -> if x.id == k.id then used else x)) (UserId ("user-" <> readerOf w.turn)) st.apiKeys}}}
    Nothing -> w
  MakeElsewhere n ->
    let u = UserId ("user-" <> readerOf w.turn)
        makeOne srv k = case makeApiKeyIn (at srv.clock) (ApiKeyId ("c" <> show (srv.minted + k))) (laptop "console" ReadRight) srv.state {current = Just u} of
          Right (st', _) -> srv {state = st'}
          Left _ -> srv
        made' = foldl' makeOne w.backend [1 .. n]
     in noted ("the console makes " <> show n) w {backend = made' {minted = w.backend.minted + n}}
  RevokeElsewhere i -> case atMay (keysOf (readerOf w.turn) w.backend) i of
    Just k ->
      let u = UserId ("user-" <> readerOf w.turn)
       in case revokeApiKeyIn (at w.backend.clock) k.id w.backend.state {current = Just u} of
            Right (st', _) -> noted ("another tab revokes " <> k.id.text) w {backend = w.backend {state = st'}}
            Left _ -> w
    Nothing -> w
  FailElsewhere -> noted "sign-ins fail elsewhere" w {backend = w.backend {wrong = Map.insert (readerOf w.turn) limit w.backend.wrong}}
  Tick d -> noted ("time goes on " <> show d <> " days") w {backend = w.backend {clock = w.backend.clock + d * 86400}}
  SwitchUser
    | w.finished -> w
    | otherwise ->
        let next = w.turn + 1
         in afterEvent ("reader " <> readerOf next <> " signs in") (Given next (readerOf next == "admin")) w {turn = next, expectShown = False, seen = ["a switch with an ask out" | any (\o -> o.inSession == w.turn) w.outs] <> w.seen}
  Finish -> afterEvent "goes away" Finished w {finished = True, expectShown = False}
  where
    at' xs k = fromMaybe "" (atMay xs k)

noted :: Text -> World -> World
noted what w = w {trace = what : w.trace}

-- | The world after the panel took an event, its effects out, and the
-- rules checked.
afterEvent :: Text -> Event -> World -> World
afterEvent what event w =
  checked
    . noted (what <> if null effects then "" else " -> " <> show effects)
    $ w
      { panel
      , outs = outs'
      , copies = w.copies <> [i | Copy i <- effects]
      , faults =
          w.faults
            <> [what <> ": " <> show effects <> " after the panel went away" | w.finished, not (null effects)]
            <> [what <> ": two asks out at once: " <> show (map (\o -> o.effect) live) | length live > 1]
      }
  where
    (panel, effects) = step event w.panel
    outs' = w.outs <> [Out {effect = e, reader = readerOf w.turn, inSession = w.turn, answer = Nothing} | e <- effects, isJust (askOf e)]
    -- The asks of this session out: sent, and their answers not yet come.
    live = [o | o <- outs', Just t <- [askOf o.effect], t `madeAfter` panel.sessionFrom]

-- | The rules, checked after every step.
checked :: World -> World
checked w = w {faults = w.faults <> shownRule <> if w.finished then [] else concat rule}
  where
    shownRule =
      [ "holds a key's text: " <> show (isJust w.panel.shown) <> ", where it should: " <> show w.expectShown
      | isJust w.panel.shown /= w.expectShown
      ]
    rule =
      [ [ "shows " <> k.id.text <> ", which the server does not have for " <> me
        | k <- listed w.panel
        , k.id `notElem` map (\x -> x.id) (keysOf me w.backend)
        ]
      , [ "shows " <> k.id.text <> " working, whose revoke was answered"
        | k <- listed w.panel
        , Just s <- [Map.lookup k.id w.revokedHeard]
        , s == w.turn
        , keyState w.panel.now k == Works
        ]
      ]
    me = readerOf w.turn

-- | A run played from the start, with what broke a rule, if anything.
runOf :: [Step] -> World
runOf = foldl' play start

machine :: Spec
machine = describe "the panel's machine, on generated runs" $ do
  it "keeps every rule on any run" $
    QC.property . QC.checkCoverage . withNumTests 20000 $
      forAllShrinkShow genRun shrinkRun show $ \run ->
        let w = runOf run
            reached tag = tag `elem` w.seen
         in QC.cover 15 (reached "a key made") "a key made"
              . QC.cover 3 (reached "a revoke answered") "a revoke answered"
              . QC.cover 3 (reached "refused: a wrong password") "a wrong password"
              . QC.cover 0.5 (reached "refused: 20 keys") "20 keys"
              . QC.cover 8 (reached "refused: too many attempts") "too many attempts"
              . QC.cover 20 (reached "a switch with an ask out") "a switch with an ask out"
              $ counterexample (toS (T.unlines (reverse w.trace) <> "\n" <> T.unlines w.faults)) (null w.faults)

  it "holds the key's text only from the make's answer to the box's close" $ do
    let w = runOf [TypePassword True, Handle, Deliver 0, ClickMake Nothing, Handle, Deliver 0]
    isJust w.panel.shown `shouldBe` True
    let closed = play w CloseKey
    closed.panel.shown `shouldBe` Nothing
    (play w Finish).panel.shown `shouldBe` Nothing

  it "shows nobody's key after another reader signs in, and never the key the earlier one made" $ do
    let w = runOf [TypePassword True, Handle, Deliver 0, ClickMake Nothing, Handle, SwitchUser, Deliver 0, Deliver 0]
    w.panel.shown `shouldBe` Nothing
    listed w.panel `shouldBe` []

  it "sends one ask at a time: a revoke clicked while the list is read waits for its answer" $ do
    let w = runOf [Handle, Deliver 0, TypePassword True, ClickMake Nothing, ClickMake Nothing]
    map (\o -> o.effect) w.outs `shouldSatisfy` ((== 1) . length)
    let w2 = runOf [TypePassword True, Handle, Deliver 0, ClickMake Nothing, Handle, Deliver 0, ClickRevoke 0, Confirm]
    -- The read that follows the make is out: the revoke waits.
    length w2.outs `shouldBe` 1
    length w2.panel.queued `shouldBe` 1

  it "is shown able to fail: a panel that forgets what was revoked breaks the rule" $ do
    -- The rule that a revoked key is never shown working, checked against
    -- a list older than the revoke put over it by hand.
    let w = runOf [TypePassword True, Handle, Deliver 0, ClickMake Nothing, Handle, Deliver 0, Handle, Deliver 0, ClickRevoke 0, Confirm, Handle, Deliver 0]
        stale = w {panel = w.panel {known = mempty, keys = fmap (map (\k -> k {revokedAt = Nothing})) <$> w.panel.keys}}
    (checked stale).faults `shouldSatisfy` (not . null)

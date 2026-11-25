-- | The rules of the host's field for a plugin's secret
-- ("Wiki.App.View.SecretField.Machine"), over generated orders of the
-- reader's acts and of the answers to the field's asks: one ask out at a
-- time; each ask's owner of keys let go once, when it answers or when the
-- field closes; a value kept only after the plugin's check passed for it,
-- and as the check answered it, under the binding the field has when the
-- check answers; the value typed cleared once kept; the plugin told of a
-- keep, a forget and an unlock only when they were done; and the secret
-- read again after an unlock, and after a change heard from elsewhere (the
-- vault unlocked or locked, a secret kept or forgotten), at once or once
-- the ask out answers. The real component on the in-memory DOM is
-- @Test.SecretFieldDom@'s.
module Test.SecretField (spec) where

import Data.List (lookup)
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude hiding (asks)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Wiki.App.View.SecretField.Machine

spec :: Spec
spec = describe "Wiki.App.View.SecretField.Machine" $ do
  modifyMaxSuccess (const 400) . prop "keeps its rules over any order of acts and answers" $
    -- Most fields open, as a mounted field does, and act from there.
    forAll ((<>) <$> frequency [(4, pure [Act Opened]), (1, pure [])] <*> scale (* 2) (listOf genMove)) $ \moves ->
      let (checks, s) = foldl' play ([], start) moves
          (_, closing) = step Finished s.model
          outAtClose = [n | Just (n, _) <- [s.model.doing]]
       in checkCoverage . tabulate "jobs done" (map jobName s.done)
            $ cover 15 ("Put" `elem` map jobName s.done) "kept a value"
            $ cover 10 ("Unlock" `elem` map jobName s.done) "unlocked"
            $ cover 3 s.reboundRefused "a check refused for its binding"
            $ cover 20 (any heardChange moves) "heard a change"
            $ conjoin (reverse checks)
              .&&. counterexample "the ask out at the close is let go" ([n | Release n <- closing] === outAtClose)

  it "reads the secret when it opens, and again after an unlock" $ do
    let (m1, e1) = step Opened initial
    e1 `shouldBe` [Ask 0 Read]
    let (m2, _) = step (Answered 0 (ReadAs LockedHere)) m1
        (m3, _) = step (TypedPassword "pw") m2
        (m4, e4) = step PasswordPressed m3
    e4 `shouldBe` [Ask 1 (UnlockByPassword "pw")]
    let (m5, e5) = step (Answered 1 (Done (Right ()))) m4
    e5 `shouldBe` [Release 1, Tell UnlockedIt, Ask 2 Read]
    m5.password `shouldBe` ""

  -- The check answers the value to keep, as the plugin makes it from the
  -- text typed (the assistant trims a key): that value is kept, not the
  -- text typed, and the text typed is cleared.
  it "keeps a value in two asks, the check and the keep, the value as the check answered it, and clears it" $ do
    let (m1, _) = step (Answered 0 (ReadAs (Here False))) (fst (step Opened initial))
        (m2, _) = step (Typed " sk-1\n") m1
        (m3, e3) = step KeepPressed m2
    e3 `shouldBe` [Ask 1 (Check " sk-1\n" "")]
    let (m4, e4) = step (Answered 1 (Checked (Right "sk-1"))) m3
    e4 `shouldBe` [Release 1, Ask 2 (Put " sk-1\n" "sk-1")]
    let (m5, e5) = step (Answered 2 (Done (Right ()))) m4
    e5 `shouldBe` [Release 2, Tell KeptIt]
    (m5.shown, m5.typed) `shouldBe` (Here True, "")

  it "keeps nothing when what the check was for changed while it was out" $ do
    let (m1, _) = step (Answered 0 (ReadAs (Here False))) (fst (step Opened initial {binding = "anthropic https://gateway.example"}))
        (m2, e2) = step KeepPressed (fst (step (Typed "sk-1") m1))
    e2 `shouldBe` [Ask 1 (Check "sk-1" "anthropic https://gateway.example")]
    let (m3, _) = step (Rebound "anthropic ") m2
        (m4, e4) = step (Answered 1 (Checked (Right "sk-1"))) m3
    e4 `shouldBe` [Release 1]
    (m4.problem, m4.typed, m4.shown) `shouldBe` (Just reboundProblem, "sk-1", Here False)

  it "keeps nothing when the check fails, and says why" $ do
    let (m1, _) = step (Answered 0 (ReadAs (Here False))) (fst (step Opened initial))
        (m2, _) = step KeepPressed (fst (step (Typed "bad") m1))
        (m3, e3) = step (Answered 1 (Checked (Left "refused"))) m2
    e3 `shouldBe` [Release 1]
    (m3.problem, m3.typed, m3.shown) `shouldBe` (Just "refused", "bad", Here False)

-- | 'Move' is the reader's act, or the answer to the ask out.
data Move = Act Event | Answer AnswerKind
  deriving stock (Show)

-- | How the ask out answers: done, or failed; a read with what it found.
data AnswerKind = Succeeds | Fails | Finds Shown
  deriving stock (Show)

genMove :: Gen Move
genMove =
  frequency
    [ (1, pure (Act Opened))
    , (3, Act <$> elements [KeepPressed, KeepPressed, ForgetPressed, PasskeyPressed, PasswordPressed])
    , (2, Act . Typed <$> elements ["", "sk-1", "sk-1", " sk-2\n", "  "])
    , (1, Act . TypedPassword <$> elements ["", "pw", "pw"])
    , (1, Act . Rebound <$> elements ["", "a", "b"])
    , (1, pure (Act Changed))
    , (5, Answer <$> elements [Succeeds, Succeeds, Succeeds, Fails, Finds (Here False), Finds (Here True), Finds (Here False), Finds LockedHere, Finds LockedHere, Finds NoReader, Finds (Unreadable "no")])
    ]

-- | What the test knows as the moves are played.
data Sim = Sim
  { model :: Model
  , out :: [(Int, Job)]
  -- ^ The asks made and not answered.
  , released :: Set Int
  , passed :: [(Int, Text)]
  -- ^ The values the checks answered as passed, by the number of the keep
  -- that followed.
  , typedSince :: Bool
  -- ^ Whether the reader typed a value since the last check was asked.
  , done :: [Job]
  -- ^ The jobs that answered done.
  , reboundRefused :: Bool
  -- ^ Whether a check passed for a binding the field no longer had.
  }

start :: Sim
start = Sim {model = initial, out = [], released = mempty, passed = [], typedSince = False, done = [], reboundRefused = False}

-- | Play one move: the step, and what it must keep.
play :: ([Property], Sim) -> Move -> ([Property], Sim)
play (checks, s) = \case
  Act e ->
    let (m', effects) = step e s.model
        (checks', s') = record (checks, s {model = m', typedSince = s.typedSince || isTyped e}) Nothing effects
        -- A change heard: the secret is read now, or wanted after the ask
        -- out, which the rules of answers then check is asked.
        heard = case e of
          Changed
            | isJust s.model.doing -> [counterexample "a change heard while an ask is out: a read wanted" (m'.wanted && null effects)]
            | otherwise -> [counterexample "a change heard: a read asked" ([j | Ask _ j <- effects] == [Read])]
          _ -> []
     in (heard <> checks', s')
  Answer kind -> case s.out of
    [] -> (checks, s)
    (n, job) : _ ->
      let answer = case (job, kind) of
            (Read, Finds shown) -> ReadAs shown
            (Read, Fails) -> ReadAs (Unreadable "no")
            (Read, Succeeds) -> ReadAs (Here False)
            -- A plugin's check that trims what was typed, as the
            -- assistant's does.
            (Check _ _, Fails) -> Checked (Left "no")
            (Check v _, _) -> Checked (Right (T.strip v))
            (_, Fails) -> Done (Left "no")
            _ -> Done (Right ())
          passes = answer == Done (Right ()) || checkPassed answer
          (m', effects) = step (Answered n answer) s.model
          s' = s {model = m', out = filter ((/= n) . fst) s.out, done = [job | passes] <> s.done}
          -- A check passed under the binding the field has now: the keep
          -- it makes must be of the value it answered. Under another, no
          -- keep.
          s'' = case (job, answer) of
            (Check _ b, Checked (Right v))
              | b == s.model.binding -> s' {passed = (m'.next - 1, v) : s'.passed}
              | otherwise -> s' {reboundRefused = True}
            _ -> s'
       in record (checks, s'') (Just (n, job, answer, s.model)) effects
  where
    isTyped = \case
      Typed _ -> True
      _ -> False
    checkPassed = \case
      Checked (Right _) -> True
      _ -> False

-- | Check a step's effects, and note its asks and releases.
record :: ([Property], Sim) -> Maybe (Int, Job, Answer, Model) -> [Effect] -> ([Property], Sim)
record (checks, s) answered effects =
  let asks = [(n, j) | Ask n j <- effects]
      releases = [n | Release n <- effects]
      s' = s {out = s.out <> asks, released = s.released <> Set.fromList releases, typedSince = if any (isCheck . snd) asks then False else s.typedSince}
      told = [t | Tell t <- effects]
      rules =
        [ counterexample "at most one ask out" (length s'.out <= 1)
        , counterexample "each owner let go once" (all (`Set.notMember` s.released) releases)
        , counterexample "the ask answered is let go first" (maybe True (\(n, _, _, _) -> take 1 releases == [n]) answered)
        , counterexample "a keep only of the value its check answered, under the binding the field has" $
            and [lookup n s'.passed == Just v | (n, Put _ v) <- asks]
        , counterexample "a check asked under the binding the field has" $
            and [b == s'.model.binding | (_, Check _ b) <- asks]
        , counterexample "the model's busy is the ask out" (isJust s'.model.doing == not (null s'.out))
        , counterexample "the plugin told only of what was done" $ case answered of
            Just (_, job, Done (Right ()), _) -> told == expectedTold job
            _ -> null told
        , counterexample "the value kept is cleared, unless typed again" $ case answered of
            Just (_, Put _ _, Done (Right ()), _) | not s.typedSince -> s'.model.typed == ""
            _ -> True
        , counterexample "the secret read after an unlock" $ case answered of
            Just (_, job, Done (Right ()), _) | isUnlock job -> map snd asks == [Read]
            _ -> True
        ]
   in (counterexample (show effects) (conjoin rules) : checks, s')
  where
    isCheck = \case
      Check _ _ -> True
      _ -> False
    isUnlock = \case
      UnlockByPasskey -> True
      UnlockByPassword _ -> True
      _ -> False
    expectedTold = \case
      Put _ _ -> [KeptIt]
      Remove -> [RemovedIt]
      UnlockByPasskey -> [UnlockedIt]
      UnlockByPassword _ -> [UnlockedIt]
      _ -> []

-- | Whether a move is a change heard from elsewhere.
heardChange :: Move -> Bool
heardChange = \case
  Act Changed -> True
  _ -> False

-- | A job's kind, for the coverage.
jobName :: Job -> [Char]
jobName = \case
  Read -> "Read"
  Check _ _ -> "Check"
  Put _ _ -> "Put"
  Remove -> "Remove"
  UnlockByPasskey -> "Unlock"
  UnlockByPassword _ -> "Unlock"

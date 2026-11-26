-- | The rules of the editor's lookup of other spaces' pages after @[[@
-- ("Wiki.App.Editor.Elsewhere.Machine", #301), over generated orders of
-- questions, pauses ending, answers of the checks and the lookups, and
-- changes of the reader, the space, whether the page is sealed and the
-- spaces held, against a model of the server: which spaces each reader may
-- view, an encrypted space that is never searched, and the ranking and the
-- limit of "Wiki.Core.PageMatch". At most one ask out, a check or a
-- lookup; no lookup while the page is sealed; a lookup only on the yes of
-- its own question's check, for the question still waiting, asked under
-- the input as it is; every question replied to once, before any newer
-- question; a reply holding only the answer to its own lookup, under its
-- own reader; and, once the events stop, the last question replied to
-- with the server's pages. The real component on the in-memory DOM is
-- "Test.EditorDom"'s.
module Test.Elsewhere (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Wiki.App.Editor.Complete (HeldSpace (..), Request (..))
import Wiki.App.Editor.Elsewhere.Machine
import Wiki.Core.Id (PageId (..), SpaceId (..))
import Wiki.Core.PageMatch (TitleHit (..), TitleQuery (..), maxLetters, rankTitles)
import Wiki.Core.Slug (mkSlug, slugify)

spec :: Spec
spec = describe "Wiki.App.Editor.Elsewhere.Machine" $ do
  modifyMaxSuccess (const 500) . prop "keeps its rules over any order of questions, pauses, answers and changes of input" $
    forAll (scale (* 2) (listOf genMove)) $ \moves ->
      let played = foldl' play start moves
          (sim, checks) = drain played
       in checkCoverage
            . cover 20 (sim.stale > 0) "an answer dropped as another question's"
            . cover 10 (sim.queued > 0) "a question checked once the ask out answered"
            . cover 10 (sim.changedWithOut > 0) "the input changed while an ask was out"
            . cover 10 (sim.lateChecks > 0) "a check answered after its question was gone"
            . cover 5 (not (null sim.refused)) "a check answered no"
            . cover 10 (not (null sim.abandoned)) "a question given up"
            . cover 30 (any isJust (Map.elems sim.replies)) "a question replied to with pages"
            $ conjoin (reverse checks) .&&. lastAnswered sim

  it "checks the space once the pause ends, looks up the last letters on a yes, and replies with what the server found" $ do
    let (m1, e1) = step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0))
    e1 `shouldBe` [StopPause, Pause]
    let (m2, e2) = step Paused m1
    e2 `shouldBe` [Check (Ticket 0)]
    let (m3, e3) = step (Checked (Ticket 0) True) m2
    e3 `shouldBe` [Lookup (Ticket 0) (queryOf "onb" spaceA)]
    let found = serverHits 0 (queryOf "onb" spaceA)
        (m4, e4) = step (Found (Ticket 0) (Just found)) m3
    e4 `shouldBe` [Reply 1 (requestFor 1 "onb") (Just found)]
    (m4.waiting, m4.out) `shouldBe` (Nothing, Nothing)

  it "replies with nothing, and looks nothing up, when the check says the space is not plain" $ do
    let checking = fst (step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0)))))
        (m, e) = step (Checked (Ticket 0) False) checking
    e `shouldBe` [Reply 1 (requestFor 1 "onb") Nothing]
    (m.waiting, m.out) `shouldBe` (Nothing, Nothing)

  -- Round 4 of #389's review: a question given up, replaced, or dropped by
  -- a change of input while its check is out sends no lookup when the
  -- check says yes.
  it "sends no lookup when the check answers after its question was given up, replaced, or dropped by a change of input" $ do
    let checking = fst (step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0)))))
        afterYes event = step (Checked (Ticket 0) True) (fst (step event checking))
    snd (afterYes (Abandoned 1)) `shouldBe` []
    snd (afterYes (Asked 2 (requestFor 2 "onbo") (Just "onbo"))) `shouldBe` []
    snd (afterYes (Inputs (inputOf (1, spaceA, False, 0)))) `shouldBe` []
    -- The turn is free again: the newer question is checked once its
    -- pause ends, under a ticket of its own.
    snd (step Paused (fst (afterYes (Asked 2 (requestFor 2 "onbo") (Just "onbo"))))) `shouldBe` [Check (Ticket 1)]

  it "asks the server nothing from a sealed page, nor for one letter, nor where no link opens, and replies at once, with no pause" $ do
    snd (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf (0, spaceA, True, 0)))) `shouldBe` [StopPause, Reply 1 (requestFor 1 "onb") Nothing]
    snd (step (Asked 1 (requestFor 1 "o") (Just " o ")) (initial (inputOf given0))) `shouldBe` [StopPause, Reply 1 (requestFor 1 "o") Nothing]
    snd (step (Asked 1 (requestFor 1 "") Nothing) (initial (inputOf given0))) `shouldBe` [StopPause, Reply 1 (requestFor 1 "") Nothing]

  it "drops the question waiting when the reader changes, and the answer of its lookup with it" $ do
    let looking = fst (step (Checked (Ticket 0) True) (fst (step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0)))))))
        (m2, e2) = step (Inputs (inputOf (1, spaceA, False, 0))) looking
    e2 `shouldBe` [Reply 1 (requestFor 1 "onb") Nothing, StopPause, Close]
    snd (step (Found (Ticket 0) (Just [])) m2) `shouldBe` []
    -- The same input again changes nothing.
    step (Inputs (inputOf (1, spaceA, False, 0))) m2 `shouldBe` (m2, [])

  it "sends no lookup for a question given up before its pause ended, and replies to it with nothing" $ do
    let (m1, _) = step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0))
        (m2, e2) = step (Abandoned 1) m1
    e2 `shouldBe` [StopPause, Reply 1 (requestFor 1 "onb") Nothing]
    step Paused m2 `shouldBe` (m2, [])

  it "asks nothing for more letters than the server takes" $
    snd (step (Asked 1 (requestFor 1 "long") (Just (T.replicate (maxLetters + 1) "o"))) (initial (inputOf given0))) `shouldBe` [StopPause, Reply 1 (requestFor 1 "long") Nothing]

  -- Round 4 of #389's review: a refresh of the spaces can take a space
  -- away, move it, or mark it encrypted, with the reader and the space
  -- edited unchanged; a list open then was made against the spaces before.
  it "closes the list shown when the reader, the space, the sealing or the spaces held change after the pages were replied" $ do
    let looking = fst (step (Checked (Ticket 0) True) (fst (step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial (inputOf given0)))))))
        (replied, _) = step (Found (Ticket 0) (Just (serverHits 0 (queryOf "onb" spaceA)))) looking
    for_ [(1, spaceA, False, 0), (0, spaceB, False, 0), (0, spaceA, True, 0), (0, spaceA, False, 1), (0, spaceA, False, 2)] $ \changed ->
      (changed, snd (step (Inputs (inputOf changed)) replied)) `shouldBe` (changed, [StopPause, Close])

-- | 'Given' is the input as the test tells it: the reader's session, the
-- space edited, whether the page is sealed, and which of the test's maps
-- of spaces held ('spacesOf').
type Given = (Int, SpaceId, Bool, Int)

given0 :: Given
given0 = (0, spaceA, False, 0)

-- | The machine's input for a 'Given'.
inputOf :: Given -> Input
inputOf (session, space, sealed, held) = Input {session, space, sealed, spaces = spacesOf held}

-- | The spaces the editor holds: every space plain at its own path; the
-- second space encrypted since; or the third moved under the first.
spacesOf :: Int -> Map SpaceId HeldSpace
spacesOf held = Map.fromList [(s, HeldSpace {name = s.text, path = toList (mkSlug (pathOf s)), plain = not (held == 1 && s == spaceB)}) | s <- [spaceA, spaceB, spaceC]]
  where
    pathOf s
      | held == 2 && s == spaceC = "moved"
      | otherwise = s.text

-- | 'Move' is what happens to the machine next: a question with letters
-- (or none, where no list of pages opens), the end of the pause if one
-- runs, the answer of the ask out (a check's yes or no, a lookup's pages
-- or its failure), or the input as it is now.
data Move
  = Ask (Maybe Text)
  | Abandon
  | PauseEnds
  | Answer Bool
  | Change Given
  deriving stock (Show)

genMove :: Gen Move
genMove =
  frequency
    [ (4, Ask <$> elements [Just "on", Just "onb", Just "no", Just "o", Just "op", Just "zz", Just (T.replicate (maxLetters + 1) "o"), Nothing])
    , (1, pure Abandon)
    , (4, pure PauseEnds)
    , (6, Answer <$> frequency [(5, pure True), (1, pure False)])
    , (2, (,,,) <$> elements [0, 0, 1] <*> elements [spaceA, spaceA, spaceB] <*> frequency [(5, pure False), (1, pure True)] <*> elements [0, 0, 0, 1, 2] <&> Change)
    ]

-- | 'Out' is an ask out, on the test's side: a check, or a lookup with the
-- reader it was sent under.
data Out
  = OutCheck Ticket
  | OutLookup Ticket TitleQuery Int
  deriving stock (Show)

-- | What the test knows as the moves are played: the model; whether a
-- pause runs; the asks out; the questions asked, by number, with their
-- letters and the input when they were asked; each question's replies,
-- and the input when each was made; the question each check and each
-- lookup was sent for (the last one asked then), the questions whose
-- check said no, and those whose lookup failed; the input as it is now;
-- and counts for the coverage.
data Sim = Sim
  { model :: Model
  , pausing :: Bool
  , out :: [Out]
  , asked :: Map Int (Maybe Text, Given)
  , replies :: Map Int (Maybe [TitleHit])
  , replyCounts :: Map Int Int
  , repliedUnder :: Map Int Given
  , checkedFor :: Map Ticket Int
  , sentFor :: Map Ticket Int
  , refused :: [Int]
  , failed :: [Int]
  , abandoned :: [Int]
  -- ^ The questions CodeMirror gave up.
  , displayed :: Maybe Given
  -- ^ The pages of other spaces CodeMirror shows: the input of the
  -- question they were replied to, until a newer question is asked (the
  -- library asks again, and shows none of them meanwhile) or the list is
  -- closed.
  , inputs :: Given
  , stale :: Int
  , queued :: Int
  , changedWithOut :: Int
  , lateChecks :: Int
  }

start :: (Sim, [Property])
start =
  ( Sim
      { model = initial (inputOf given0)
      , pausing = False
      , out = []
      , asked = mempty
      , replies = mempty
      , replyCounts = mempty
      , repliedUnder = mempty
      , checkedFor = mempty
      , sentFor = mempty
      , refused = []
      , failed = []
      , displayed = Nothing
      , abandoned = []
      , inputs = given0
      , stale = 0
      , queued = 0
      , changedWithOut = 0
      , lateChecks = 0
      }
  , []
  )

-- | Play a move: step the machine with its event, if it has one now, and
-- check the step's effects.
play :: (Sim, [Property]) -> Move -> (Sim, [Property])
play (sim, checks) = \case
  Ask letters ->
    let n = Map.size sim.asked + 1
        sim' = sim {asked = Map.insert n (letters, sim.inputs) sim.asked, displayed = Nothing}
     in stepped sim' checks (Asked n (requestFor n (fromMaybe "" letters)) letters)
  -- CodeMirror gives up the last question asked, while it has no reply
  -- (its asker tells of none it has had a reply to).
  Abandon
    | n <- Map.size sim.asked
    , n > 0
    , Map.notMember n sim.replyCounts ->
        stepped sim {abandoned = n : sim.abandoned, displayed = Nothing} checks (Abandoned n)
    | otherwise -> (sim, checks)
  PauseEnds
    | sim.pausing -> stepped sim {pausing = False} checks Paused
    | otherwise -> (sim, checks)
  Answer ok -> case sim.out of
    asking : rest ->
      let ticket = case asking of
            OutCheck t -> t
            OutLookup t _ _ -> t
          waitingTicket = sim.model.waiting >>= (.ticket)
          counted =
            sim
              { out = rest
              , stale = sim.stale + (if waitingTicket /= Just ticket then 1 else 0)
              , queued = sim.queued + (if waitingTicket /= Just ticket && maybe False (\q -> q.paused && isNothing q.ticket) sim.model.waiting then 1 else 0)
              }
       in case asking of
            OutCheck _ ->
              stepped
                counted
                  { refused = [n | not ok, Just n <- [Map.lookup ticket sim.checkedFor]] <> sim.refused
                  , lateChecks = sim.lateChecks + (if waitingTicket /= Just ticket then 1 else 0)
                  }
                checks
                (Checked ticket ok)
            OutLookup _ query session ->
              let found = if ok then Just (serverHits session query) else Nothing
               in stepped counted {failed = [n | not ok, Just n <- [Map.lookup ticket sim.sentFor]] <> sim.failed} checks (Found ticket found)
    [] -> (sim, checks)
  Change given ->
    let changed = given /= sim.inputs
     in stepped sim {inputs = given, changedWithOut = sim.changedWithOut + (if changed && not (null sim.out) then 1 else 0)} checks (Inputs (inputOf given))

-- | Step the machine with an event, carry out its effects on the test's
-- side, and check them.
stepped :: Sim -> [Property] -> Event -> (Sim, [Property])
stepped sim checks event =
  let (model, effects) = step event sim.model
      sim' = foldl' carry sim {model} effects
      latest = Map.size sim'.asked
      (session, space, sealed, _) = sim.inputs
      lookups = [q | Lookup _ q <- effects]
      newChecks =
        [ counterexample ("at most one ask out, a check or a lookup, after " <> show event) (length sim'.out <= 1)
        , counterexample ("no lookup while sealed, after " <> show event) (not sealed || null lookups)
        , counterexample ("each lookup for the last question, asked under the input as it is, after " <> show event) $
            conjoin
              [ case Map.lookup latest sim.asked of
                  Just (Just letters, askedUnder) -> (q.letters, q.except, askedUnder) === (letters, Just space, sim.inputs)
                  other -> counterexample (show other) (property False)
              | q <- lookups
              ]
        , counterexample ("each lookup on the yes of the check sent for the same question, in that step, after " <> show event) $
            conjoin
              [ (event, Map.lookup ticket sim.checkedFor) === (Checked ticket True, Just latest)
              | Lookup ticket _ <- effects
              ]
        , counterexample ("each question replied to once, after " <> show event) (all (<= 1) sim'.replyCounts)
        , counterexample ("every question before the last replied to, after " <> show event) $
            and [Map.lookup n sim'.replyCounts == Just 1 | n <- [1 .. latest - 1]]
        , counterexample ("no lookup for a question given up, nor for more letters than the server takes, after " <> show event) $
            null lookups || (latest `notElem` sim.abandoned && all (\q -> T.length q.letters <= maxLetters) lookups)
        , counterexample ("no pages shown that were found under another input, after " <> show event) $
            maybe True (== sim'.inputs) sim'.displayed
        , counterexample ("a reply with pages only to its own lookup, under its own reader, after " <> show event) $
            conjoin
              [ case Map.lookup n sim.asked of
                  Just (Just letters, (askedSession, askedSpace, _, _)) ->
                    (askedSession, hits) === (session, serverHits session (queryOf letters askedSpace))
                  other -> counterexample (show other) (property False)
              | Reply n _ (Just hits) <- effects
              ]
        ]
   in (sim', newChecks <> checks)

-- | Carry out an effect on the test's side: a pause armed or stopped, a
-- check or a lookup sent under the reader of the moment, a reply counted.
carry :: Sim -> Effect -> Sim
carry sim = \case
  Pause -> sim {pausing = True}
  StopPause -> sim {pausing = False}
  Check ticket -> sim {out = sim.out <> [OutCheck ticket], checkedFor = Map.insert ticket (Map.size sim.asked) sim.checkedFor}
  Lookup ticket query -> let (session, _, _, _) = sim.inputs in sim {out = sim.out <> [OutLookup ticket query session], sentFor = Map.insert ticket (Map.size sim.asked) sim.sentFor}
  Reply n _ hits ->
    sim
      { replies = Map.insert n hits sim.replies
      , replyCounts = Map.insertWith (+) n 1 sim.replyCounts
      , repliedUnder = Map.insert n sim.inputs sim.repliedUnder
      , displayed = case hits of
          Just (_ : _) -> snd <$> Map.lookup n sim.asked
          _ -> sim.displayed
      }
  Close -> sim {displayed = Nothing}

-- | When the events stop: the pause ends, and every ask answers (a check
-- yes, a lookup with pages), until nothing is left to happen.
drain :: (Sim, [Property]) -> (Sim, [Property])
drain (sim, checks)
  | sim.pausing = drain (play (sim, checks) PauseEnds)
  | not (null sim.out) = drain (play (sim, checks) (Answer True))
  | otherwise = (sim, checks)

-- | Once the events stop, the last question has had its one reply: the
-- server's pages for its letters when it was asked with letters enough,
-- on a page not sealed, under the input still in force when it was
-- replied to, its check said yes and its lookup did not fail; nothing
-- otherwise.
lastAnswered :: Sim -> Property
lastAnswered sim = case Map.lookupMax sim.asked of
  Nothing -> property True
  Just (n, (letters, askedUnder@(session, space, sealed, _))) ->
    counterexample "the last question's reply" $
      (Map.lookup n sim.replyCounts, Map.lookup n sim.replies)
        === ( Just 1
            , Just $ case letters of
                Just l
                  | Map.lookup n sim.repliedUnder == Just askedUnder
                  , n `notElem` sim.failed
                  , n `notElem` sim.refused
                  , n `notElem` sim.abandoned
                  , T.length l <= maxLetters
                  , not sealed
                  , T.length (T.strip l) >= 2 ->
                      Just (serverHits session (queryOf l space))
                _ -> Nothing
            )

-- | The lookup of these letters from a page of this space.
queryOf :: Text -> SpaceId -> TitleQuery
queryOf letters space = TitleQuery {letters, except = Just space, limit = elsewhereShown}

----------------------------------------------------------------------
-- The model of the server

spaceA, spaceB, spaceC, spaceSealed :: SpaceId
spaceA = SpaceId "a"
spaceB = SpaceId "b"
spaceC = SpaceId "c"
spaceSealed = SpaceId "sealed"

-- | The spaces each reader may view: the first every one, the second only
-- the second space. The encrypted space is never searched, whoever asks.
readable :: Int -> [SpaceId]
readable = \case
  0 -> [spaceA, spaceB, spaceC, spaceSealed]
  _ -> [spaceB]

-- | The pages of the spaces, by title.
pages :: [(SpaceId, Text)]
pages =
  [ (s, title)
  | s <- [spaceA, spaceB, spaceC, spaceSealed]
  , title <- ["Onboarding", "On call", "Notes", "Ops", "Zone"]
  ]

-- | What the server answers a lookup under a reader: the pages of the
-- spaces the reader may view but the one left out and the encrypted one,
-- ranked and cut as "Wiki.Core.PageMatch" does.
serverHits :: Int -> TitleQuery -> [TitleHit]
serverHits session query =
  rankTitles
    query
    [ TitleHit {page = PageId (s.text <> "-" <> title), space = s, spacePath = toList (mkSlug s.text), pagePath = toList (slugify title), title}
    | (s, title) <- pages
    , s `elem` readable session
    , s /= spaceSealed
    , Just s /= query.except
    ]

-- | The place of the question with this number: its own line.
requestFor :: Int -> Text -> Request
requestFor n letters = Request {line = n, before = "[[" <> letters, after = "", nodes = []}

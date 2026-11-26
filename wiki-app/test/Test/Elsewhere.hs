-- | The rules of the editor's lookup of other spaces' pages after @[[@
-- ("Wiki.App.Editor.Elsewhere.Machine", #301), over generated orders of
-- questions, pauses ending, answers of the lookups, and changes of the
-- reader, the space and whether the page is sealed, against a model of
-- the server: which spaces each reader may view, an encrypted space that
-- is never searched, and the ranking and the limit of
-- "Wiki.Core.PageMatch". At most one lookup out; none while the page is
-- sealed; none for letters asked under another reader or space; every
-- question replied to once, before any newer question; a reply holding
-- only the answer to its own lookup, under its own reader; and, once the
-- events stop, the last question replied to with the server's pages. The
-- real component on the in-memory DOM is "Test.EditorDom"'s.
module Test.Elsewhere (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Wiki.App.Editor.Complete (Request (..))
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
            . cover 10 (sim.queued > 0) "a question asked once the lookup out answered"
            . cover 10 (sim.changedWithOut > 0) "the input changed while a lookup was out"
            . cover 10 (not (null sim.abandoned)) "a question given up"
            . cover 30 (any isJust (Map.elems sim.replies)) "a question replied to with pages"
            $ conjoin (reverse checks) .&&. lastAnswered sim

  it "looks up the last letters once the pause ends, and replies with what the server found" $ do
    let (m1, e1) = step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial 0 spaceA False)
    e1 `shouldBe` [StopPause, Pause]
    let (m2, e2) = step Paused m1
    e2 `shouldBe` [Lookup (Ticket 0) TitleQuery {letters = "onb", except = Just spaceA, limit = elsewhereShown, typedIn = Nothing}]
    let found = serverHits 0 (TitleQuery {letters = "onb", except = Just spaceA, limit = elsewhereShown, typedIn = Nothing})
        (m3, e3) = step (Found (Ticket 0) (Just found)) m2
    e3 `shouldBe` [Reply 1 (requestFor 1 "onb") (Just found)]
    (m3.waiting, m3.out) `shouldBe` (Nothing, Nothing)

  it "asks the server nothing from a sealed page, nor for one letter, and replies at once" $ do
    snd (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial 0 spaceA True)) `shouldBe` [StopPause, Reply 1 (requestFor 1 "onb") Nothing]
    snd (step (Asked 1 (requestFor 1 "o") (Just " o ")) (initial 0 spaceA False)) `shouldBe` [StopPause, Reply 1 (requestFor 1 "o") Nothing]

  it "drops the question waiting when the reader changes, and the answer of its lookup with it" $ do
    let (m1, _) = step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial 0 spaceA False)))
        (m2, e2) = step (Inputs 1 spaceA False) m1
    e2 `shouldBe` [Reply 1 (requestFor 1 "onb") Nothing, StopPause, Close]
    snd (step (Found (Ticket 0) (Just [])) m2) `shouldBe` []
    -- The same input again changes nothing.
    step (Inputs 1 spaceA False) m2 `shouldBe` (m2, [])

  it "sends no lookup for a question given up before its pause ended, and replies to it with nothing" $ do
    let (m1, _) = step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial 0 spaceA False)
        (m2, e2) = step (Abandoned 1) m1
    e2 `shouldBe` [StopPause, Reply 1 (requestFor 1 "onb") Nothing]
    step Paused m2 `shouldBe` (m2, [])

  it "asks nothing for more letters than the server takes" $
    snd (step (Asked 1 (requestFor 1 "long") (Just (T.replicate (maxLetters + 1) "o"))) (initial 0 spaceA False)) `shouldBe` [StopPause, Reply 1 (requestFor 1 "long") Nothing]

  it "closes the list shown when the reader changes after the pages were replied" $ do
    let asked = fst (step Paused (fst (step (Asked 1 (requestFor 1 "onb") (Just "onb")) (initial 0 spaceA False))))
        (replied, _) = step (Found (Ticket 0) (Just (serverHits 0 (TitleQuery {letters = "onb", except = Just spaceA, limit = elsewhereShown, typedIn = Nothing})))) asked
    snd (step (Inputs 1 spaceA False) replied) `shouldBe` [StopPause, Close]
    snd (step (Inputs 0 spaceB False) replied) `shouldBe` [StopPause, Close]
    snd (step (Inputs 0 spaceA True) replied) `shouldBe` [StopPause, Close]

-- | 'Move' is what happens to the machine next: a question with letters
-- (or none, where no list of pages opens), the end of the pause if one
-- runs, the answer of the lookup out (or its failure), or the input as it
-- is now.
data Move
  = Ask (Maybe Text)
  | Abandon
  | PauseEnds
  | Answer Bool
  | Input Int SpaceId Bool
  deriving stock (Show)

genMove :: Gen Move
genMove =
  frequency
    [ (4, Ask <$> elements [Just "on", Just "onb", Just "no", Just "o", Just "op", Just "zz", Just (T.replicate (maxLetters + 1) "o"), Nothing])
    , (1, pure Abandon)
    , (4, pure PauseEnds)
    , (4, Answer <$> frequency [(5, pure True), (1, pure False)])
    , (2, Input <$> elements [0, 0, 1] <*> elements [spaceA, spaceA, spaceB] <*> frequency [(5, pure False), (1, pure True)])
    ]

-- | What the test knows as the moves are played: the model; whether a
-- pause runs; the lookups out, each with the reader it was sent under;
-- the questions asked, by number, with their letters and the reader,
-- the space and whether the page was sealed when they were asked; each
-- question's replies, and the input when each was made; the question each
-- lookup was sent for (the last one asked then), and the questions whose
-- lookup failed; the input as it is now; and counts for the coverage.
data Sim = Sim
  { model :: Model
  , pausing :: Bool
  , sent :: [(Ticket, TitleQuery, Int)]
  , asked :: Map Int (Maybe Text, (Int, SpaceId, Bool))
  , replies :: Map Int (Maybe [TitleHit])
  , replyCounts :: Map Int Int
  , repliedUnder :: Map Int (Int, SpaceId, Bool)
  , sentFor :: Map Ticket Int
  , failed :: [Int]
  , abandoned :: [Int]
  -- ^ The questions CodeMirror gave up.
  , displayed :: Maybe (Int, SpaceId, Bool)
  -- ^ The pages of other spaces CodeMirror shows: the input of the
  -- question they were replied to, until a newer question is asked (the
  -- library asks again, and shows none of them meanwhile) or the list is
  -- closed.
  , inputs :: (Int, SpaceId, Bool)
  , stale :: Int
  , queued :: Int
  , changedWithOut :: Int
  }

start :: (Sim, [Property])
start =
  ( Sim
      { model = initial 0 spaceA False
      , pausing = False
      , sent = []
      , asked = mempty
      , replies = mempty
      , replyCounts = mempty
      , repliedUnder = mempty
      , sentFor = mempty
      , failed = []
      , displayed = Nothing
      , abandoned = []
      , inputs = (0, spaceA, False)
      , stale = 0
      , queued = 0
      , changedWithOut = 0
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
  Answer ok -> case sim.sent of
    (ticket, query, session) : rest ->
      let found = if ok then Just (serverHits session query) else Nothing
          waitingTicket = sim.model.waiting >>= (.ticket)
          counted =
            sim
              { sent = rest
              , failed = [n | not ok, Just n <- [Map.lookup ticket sim.sentFor]] <> sim.failed
              , stale = sim.stale + (if waitingTicket /= Just ticket then 1 else 0)
              , queued = sim.queued + (if waitingTicket /= Just ticket && maybe False (\q -> q.paused && isNothing q.ticket) sim.model.waiting then 1 else 0)
              }
       in stepped counted checks (Found ticket found)
    [] -> (sim, checks)
  Input session space sealed ->
    let changed = (session, space, sealed) /= sim.inputs
     in stepped sim {inputs = (session, space, sealed), changedWithOut = sim.changedWithOut + (if changed && not (null sim.sent) then 1 else 0)} checks (Inputs session space sealed)

-- | Step the machine with an event, carry out its effects on the test's
-- side, and check them.
stepped :: Sim -> [Property] -> Event -> (Sim, [Property])
stepped sim checks event =
  let (model, effects) = step event sim.model
      sim' = foldl' carry sim {model} effects
      latest = Map.size sim'.asked
      (session, space, sealed) = sim.inputs
      lookups = [q | Lookup _ q <- effects]
      newChecks =
        [ counterexample ("at most one lookup out, after " <> show event) (length sim'.sent <= 1)
        , counterexample ("no lookup while sealed, after " <> show event) (not sealed || null lookups)
        , counterexample ("each lookup for the last question, asked under the input as it is, after " <> show event) $
            conjoin
              [ case Map.lookup latest sim.asked of
                  Just (Just letters, askedUnder) -> (q.letters, q.except, askedUnder) === (letters, Just space, (session, space, sealed))
                  other -> counterexample (show other) (property False)
              | q <- lookups
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
                  Just (Just letters, (askedSession, askedSpace, _)) ->
                    (askedSession, hits) === (session, serverHits session (TitleQuery {letters, except = Just askedSpace, limit = elsewhereShown, typedIn = Nothing}))
                  other -> counterexample (show other) (property False)
              | Reply n _ (Just hits) <- effects
              ]
        ]
   in (sim', newChecks <> checks)

-- | Carry out an effect on the test's side: a pause armed or stopped, a
-- lookup sent under the reader of the moment, a reply counted.
carry :: Sim -> Effect -> Sim
carry sim = \case
  Pause -> sim {pausing = True}
  StopPause -> sim {pausing = False}
  Lookup ticket query -> let (session, _, _) = sim.inputs in sim {sent = sim.sent <> [(ticket, query, session)], sentFor = Map.insert ticket (Map.size sim.asked) sim.sentFor}
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

-- | When the events stop: the pause ends, and every lookup answers, until
-- nothing is left to happen.
drain :: (Sim, [Property]) -> (Sim, [Property])
drain (sim, checks)
  | sim.pausing = drain (play (sim, checks) PauseEnds)
  | not (null sim.sent) = drain (play (sim, checks) (Answer True))
  | otherwise = (sim, checks)

-- | Once the events stop, the last question has had its one reply: the
-- server's pages for its letters when it was asked with letters enough,
-- on a page not sealed, under the input still in force when it was
-- replied to, and its lookup did not fail; nothing otherwise.
lastAnswered :: Sim -> Property
lastAnswered sim = case Map.lookupMax sim.asked of
  Nothing -> property True
  Just (n, (letters, askedUnder@(session, space, sealed))) ->
    counterexample "the last question's reply" $
      (Map.lookup n sim.replyCounts, Map.lookup n sim.replies)
        === ( Just 1
            , Just $ case letters of
                Just l
                  | Map.lookup n sim.repliedUnder == Just askedUnder
                  , n `notElem` sim.failed
                  , n `notElem` sim.abandoned
                  , T.length l <= maxLetters
                  , not sealed
                  , T.length (T.strip l) >= 2 ->
                      Just (serverHits session (TitleQuery {letters = l, except = Just space, limit = elsewhereShown, typedIn = Nothing}))
                _ -> Nothing
            )

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

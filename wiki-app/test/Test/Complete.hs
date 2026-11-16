-- | The editor's answer to CodeMirror's question about the list of the
-- wiki's own Markdown ("Wiki.App.Editor.Complete"): against the text the
-- editor holds, from the sources it holds, in the shape
-- @web/src/editor.js@ reads; and the links to pages it writes
-- ("Wiki.App.Route"'s 'pageLinkText'), read back by the wiki's parser and
-- 'resolvePageLink' over generated trees of pages.
module Test.Complete (spec) where

import Data.Aeson (Value, object, (.=))
import Data.List (lookup, nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Protolude
import System.Mem (getAllocationCounter, setAllocationCounter)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, elements, forAll, property, (===))
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Editor.Complete
import Wiki.App.Route (PageLink (..), pageLinkText, pageRoute, resolvePageLink)
import Wiki.Core.Id (PageId (..))
import Wiki.Core.Markdown (Destination (..), classifyDestination)
import Wiki.Core.Markdown.Ast (Inline (..), allInlines, parseMarkdown, plainText)
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))
import Wiki.Core.Markdown.Complete
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.Slug (Slug, mkSlug, slugText, slugify)
import Wiki.Core.Space (Space (homePage, slug))
import Wiki.Core.Space qualified as S

-- | The answer, from no memo of the parser's readings.
answered :: [Source] -> Text -> Request -> Maybe Completion
answered found text request = fst (answer found noReadings text request)

spec :: Spec
spec = describe "Wiki.App.Editor.Complete" $ do
  let text = "# Title\n\n[[!div]]\n\n[[/"
      atEnd = Request {line = 5, before = "[[/", after = "", nodes = ["Paragraph", "Document"]}
      document = sources (held Document [])
  describe "answer" $ do
    it "answers from the text the editor holds, at the request's line" $
      (map (.label) . (.candidates) <$> answered document text atEnd) `shouldBe` Just ["[[/div]]"]
    it "answers nothing when the line is no longer as CodeMirror saw it" $ do
      -- The block is open still, but the line was typed over since.
      answered document "# Title\n\n[[!div]]\n\nText" atEnd `shouldBe` Nothing
      answered document "# Title\n\n[[!div]]\n\n[[" atEnd `shouldBe` Nothing
      answered document (text <> "x") atEnd `shouldBe` Nothing
    it "answers nothing for a line the text does not have" $ do
      answered document text Request {line = 9, before = "[[/", after = "", nodes = []} `shouldBe` Nothing
      answered document text Request {line = 0, before = "[[/", after = "", nodes = []} `shouldBe` Nothing
    it "answers nothing where the syntax tree says the text is its own" $
      answered document text Request {line = 5, before = "[[/", after = "", nodes = ["CodeText", "FencedCode", "Document"]} `shouldBe` Nothing

    it "answers from the memo of the parser's readings as from none, and forgets it when the lines above change" $ do
      let (first', memo) = answer document noReadings text atEnd
          (again, _) = answer document memo text atEnd
          (other, _) = answer document memo "# Title\n\n[[!notes]]\n\n[[/" atEnd
      (map (.label) . (.candidates) <$> first') `shouldBe` Just ["[[/div]]"]
      again `shouldBe` first'
      (map (.label) . (.candidates) <$> other) `shouldBe` Just ["[[/notes]]"]

  describe "sources" $ do
    it "offers a document's markers, and a presentation's" $ do
      let offered kind = map (.label) . (.candidates) <$> answered (sources (held kind [])) "[[!" (lineRequest "[[!" "")
      offered Document `shouldBe` Just ["[[!spoiler]]", "[[!div]]", "[[!toc]]"]
      length <$> offered Presentation `shouldBe` Just 7
    it "offers the kinds of box the administration defined" $ do
      let defined = [BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}]
      (map (.label) . (.candidates) <$> answered (sources (held Document defined)) "> [!ex" (lineRequest "> [!ex" ""))
        `shouldBe` Just ["[!EXERCISE]"]

  describe "the pages offered after [[" $ do
    let pages =
          [ linkable "home" "Team" []
          , linkable "handbook" "Handbook" ["handbook"]
          , linkable "editing" "Editing" ["handbook", "editing"]
          , linkable "guide" "Guide" ["handbook", "guide"]
          , linkable "old" "Old notes" ["handbook", "guide-old"]
          , linkable "deep" "Guide to deploys" ["handbook", "ops", "guide-to-deploys"]
          , linkable "far" "Guide" ["projects", "guide"]
          , linkable "inside" "The user guide" ["projects", "the-user-guide"]
          , linkable "other" "Projects" ["projects"]
          ]
        onHandbook = Held {kind = Document, boxes = [], editing = PageId "editing", pages = linkablesFrom handbookEditing pages}
        offered typed = map (\c -> (c.label, c.place)) . (.candidates) <$> answered (sources onHandbook) typed (lineRequest typed "")
    it "offers every page but the one edited, the nearer first: siblings, then its branch, then the others" $
      (map fst <$> offered "[[")
        `shouldBe` Just ["Guide", "Old notes", "Handbook", "Guide to deploys", "Team", "Projects", "Guide", "The user guide"]
    it "puts a title's start before a later word or a path's segment, and those before a match inside" $
      (map fst <$> offered "See [[gui")
        `shouldBe` Just ["Guide", "Guide to deploys", "Guide", "Old notes", "The user guide"]
    it "shows where each page is, so that two of one title are told apart" $
      (filter ((== "Guide") . fst) <$> offered "[[Guide")
        `shouldBe` Just [("Guide", Just "handbook/guide"), ("Guide", Just "projects/guide")]
    it "shows the home page as such" $
      (lookup "Team" <$> offered "[[Te") `shouldBe` Just (Just (Just "the home page"))
    it "opens no list when no page has the letters" $
      offered "[[zebra" `shouldBe` Nothing
    it "writes the link with its end, the cursor after it" $ do
      written onHandbook "[[Gui" "" `shouldBe` Just "[[Guide]]${0}"
      written onHandbook "[[Old" "" `shouldBe` Just "[[guide-old|Old notes]]${0}"
      written onHandbook "[[Projects" "" `shouldBe` Just "[[../projects|Projects]]${0}"
    it "replaces the end typed already, and keeps a label typed already" $ do
      -- The cursor after "Gui" of "[[Gui|For me]]": the entry replaces
      -- "[[Gui" and "|For me]]", and writes the label back as it was.
      let reach typed afterwards = (\c -> (c.from, c.through)) <$> answered (sources onHandbook) (typed <> afterwards) (lineRequest typed afterwards)
      reach "[[Gui" "de]] now" `shouldBe` Just (0, 4)
      written onHandbook "[[Gui" "de]] now" `shouldBe` Just "[[Guide]]${0}"
      reach "[[Old" "|Mine]]" `shouldBe` Just (0, 7)
      written onHandbook "[[Old" "|Mine]]" `shouldBe` Just "[[guide-old|Mine]]${0}"
    modifyMaxSuccess (* 10) . prop "replaces the address of the link the parser reads at the cursor, keeps its label as read, and leaves the rest of the line" $
      forAll genLinkLine (choosesPage onHandbook)

  describe "the pages offered in a space of 10,000 pages" $
    it "are matched and ranked within a bound of allocation, their paths and links worked out before, and again no more" $ do
      -- The work is counted, not timed ('allocated'). Each answer only
      -- matches and ranks the pages kept, and allocates under 1 MB
      -- natively when the letters match pages, 18 MB when they match none
      -- and every page is looked at. An answer that worked out each
      -- page's path (each page above it searched for in the list) and its
      -- link, as the editor did before #294's review, allocated 11 GB.
      let kept = linkables space largeSpace (slugs ["section-0", "page-77"])
          h = Held {kind = Document, boxes = [], editing = PageId "p77", pages = kept}
      _ <- evaluate (kept == kept)
      let asked typed = answered (sources h) typed (lineRequest typed "")
      for_ ["[[", "[[pag", "[[zz"] $ \typed -> do
        first' <- evaluate typed >>= \t -> allocated (let a = asked t in a == a)
        again <- evaluate (T.copy typed) >>= \t -> allocated (let a = asked t in a == a)
        (typed, first' < answerBound, again <= first') `shouldBe` (typed, True, True)

  describe "pageLinkText, over generated trees of pages" $ do
    it "names a sibling by its title when the title reads back as its slug" $
      pageLinkText (slugs ["a", "b"]) (slugs ["a", "on-call"]) "On-call" `shouldBe` ("On-call", Nothing)
    it "names the parent with ., and pages above with .." $ do
      pageLinkText (slugs ["a", "b"]) (slugs ["a"]) "A" `shouldBe` (".", Just "A")
      pageLinkText (slugs ["a", "b", "c"]) (slugs ["a"]) "A" `shouldBe` ("..", Just "A")
      pageLinkText (slugs ["a", "b"]) [] "Home" `shouldBe` ("..", Just "Home")
    it "writes a bracket and an ampersand of the label as character references" $
      pageLinkText (slugs ["x"]) (slugs ["q-a"]) "Q&A [draft]" `shouldBe` ("q-a", Just "Q&amp;A [draft&#93;")
    modifyMaxSuccess (* 3) . prop "writes, for every page and every other, a link the parser and resolvePageLink read as that page and its title, the shortest of the forms considered" $
      forAll genTree linksResolve

  describe "completionJson" $ do
    it "shows a candidate's place and its origin as the detail, the wiki's own marker neither" $ do
      let made = Completion {from = 2, through = 0, candidates = [own, page, fromPlugin], enter = EnterChooses, escape = EscapeCloses}
          own = Candidate {label = "[[!div]]", skeleton = [Literal "[[!div]]", Field 0 ""], place = Nothing, origin = Nothing}
          page = Candidate {label = "Guide", skeleton = [Literal "[[Guide]]", Field 0 ""], place = Just "handbook/guide", origin = Nothing}
          fromPlugin = Candidate {label = "[[!day]]", skeleton = [Literal "[[!day]]{", Field 1 "date", Literal "}"], place = Just "today", origin = Just (FromPlugin "calendar")}
      completionJson made
        `shouldBe` object
          [ "from" .= (2 :: Int)
          , "through" .= (0 :: Int)
          , "options"
              .= [ object ["label" .= ("[[!div]]" :: Text), "template" .= ("[[!div]]${0}" :: Text)]
                 , object ["label" .= ("Guide" :: Text), "template" .= ("[[Guide]]${0}" :: Text), "detail" .= ("handbook/guide" :: Text)]
                 , object ["label" .= ("[[!day]]" :: Text), "template" .= ("[[!day]]\\{${1:date}\\}" :: Text), "detail" .= ("today · from plugin calendar" :: Text)]
                 ]
          , "enter" .= ("chooses" :: Text)
          , "escape" .= ("closes" :: Text)
          ]
    it "says how many characters Enter waits for" $
      completionJson Completion {from = 0, through = 0, candidates = [], enter = EnterChoosesAfter 2, escape = EscapeCloses}
        `shouldBe` object ["from" .= (0 :: Int), "through" .= (0 :: Int), "options" .= ([] :: [Value]), "enter" .= object ["after" .= (2 :: Int)], "escape" .= ("closes" :: Text)]

-- | 'LinkLine' is a line with a wiki link being typed in it: the text
-- before the link, what of the address is before the cursor and after
-- it, the label written (with its @|@, escaped or not), whether the link
-- is closed, and the text after it.
data LinkLine = LinkLine {pre :: Text, typed :: Text, rest :: Text, label :: Text, closed :: Bool, after :: Text}
  deriving stock (Show)

genLinkLine :: Gen LinkLine
genLinkLine =
  LinkLine
    <$> elements ["", "See ", "- ", "> ", "[[x]] and ", "`a` "]
    <*> elements ["", "Gu", "Ol", "Pro", "gu "]
    <*> elements ["", "ide", "-a", " x", "a&amp;b", "a\\", "a[b"]
    <*> elements ["", "|Mine", "|[draft&#93;", "|a [b", "\\|escaped", "|Q&amp;A", "|", "|x|y", "| spaced "]
    <*> elements [True, False]
    <*> elements ["", " now", " and [[b]]", " [[c|C]] end", " `x`"]

-- | Choosing the first page offered on a 'LinkLine', read by the wiki's
-- parser. The line as written is read with the link the generator meant
-- (or none at the cursor, when it is not closed). After the choice, the
-- text before the link is as it was, and so is the text after the link:
-- the link's end and what follows it when it was closed, everything
-- after the cursor when it was not. The line is read with the links it
-- had, the one at the cursor now to the page chosen, with the label that
-- was read there, or the page's own label when it had none.
choosesPage :: Held -> LinkLine -> Property
choosesPage h l = case answered (sources h) line (lineRequest typedUpTo afterCursor) of
  Nothing -> counterexample "no list" (property False)
  Just found -> case head found.candidates of
    Nothing -> counterexample "no page" (property False)
    Just c ->
      let (text, _) = skeletonText c.skeleton
          result = T.take found.from typedUpTo <> text <> T.drop found.through afterCursor
          page = fromMaybe (panic "no page") (find (\p -> p.title == c.label) h.pages)
          (address, own) = pageLinkText handbookEditing page.path page.title
          title
            | l.closed && not (T.null l.label) = maybe "" snd (head (drop (length (linksOf l.pre)) (linksOf line)))
            | otherwise = maybe address (const page.title) own
          kept = if l.closed then l.after else afterCursor
       in counterexample (toS (line <> "  =>  " <> result)) $
            conjoin
              [ T.take (T.length l.pre) result === l.pre
              , T.takeEnd (T.length kept) result === kept
              , linksOf result === linksOf l.pre <> [(address, title)] <> linksOf kept
              , if l.closed then length (linksOf line) === length (linksOf l.pre) + 1 + length (linksOf l.after) else property True
              ]
  where
    typedUpTo = l.pre <> "[[" <> l.typed
    afterCursor = l.rest <> l.label <> (if l.closed then "]]" else "") <> l.after
    line = typedUpTo <> afterCursor
    linksOf t = case parseMarkdown t of
      Right blocks -> [(address, plainText text) | Link attributes address _ text <- allInlines blocks, ("class", "wikilink") `elem` attributes]
      Left _ -> []

-- | The bytes allocated in forcing a value: what its work costs, counted
-- rather than timed, so that a slow or a busy machine changes nothing.
allocated :: Bool -> IO Int64
allocated value = do
  setAllocationCounter 0
  _ <- evaluate value
  negate <$> getAllocationCounter

-- | The allocation an answer over 'largeSpace' stays within.
answerBound :: Int64
answerBound = 40_000_000

-- | A space of 10,000 pages under its home page: a hundred sections of a
-- hundred pages each, titled "Page n" (the sections "Section n").
largeSpace :: [PageSummary]
largeSpace =
  summary "p0" Nothing "Home" (slugOf "home") 0
    : concat
      [ summary ("s" <> show s) (Just (PageId "p0")) ("Section " <> show s) (slugOf ("section-" <> show s)) s
          : [summary ("p" <> show n) (Just (PageId ("s" <> show s))) ("Page " <> show n) (slugOf ("page-" <> show n)) n | k <- [1 .. 99 :: Int], let n = s * 100 + k]
      | s <- [0 .. 99 :: Int]
      ]
  where
    slugOf = either (panic . show) identity . mkSlug

-- | What the editor holds of a page of a kind, with no page to link to.
held :: PageKind -> [BoxKind] -> Held
held kind boxes = Held {kind, boxes, editing = PageId "none", pages = []}

-- | The path of the page edited in the tests of the pages offered.
handbookEditing :: [Slug]
handbookEditing = slugs ["handbook", "editing"]

-- | A question about the first line of a text: before the cursor and after.
lineRequest :: Text -> Text -> Request
lineRequest typed rest = Request {line = 1, before = typed, after = rest, nodes = ["Paragraph", "Document"]}

-- | What the first entry writes, as its snippet, for a line typed so far.
written :: Held -> Text -> Text -> Maybe Text
written h typed afterwards = do
  found <- answered (sources h) (typed <> afterwards) (lineRequest typed afterwards)
  first' <- head found.candidates
  Just (snippetTemplate first'.skeleton)

linkable :: Text -> Text -> [Text] -> (PageId, Text, [Slug])
linkable i title path = (PageId i, title, slugs path)

slugs :: [Text] -> [Slug]
slugs = map (either (panic . show) identity . mkSlug)

----------------------------------------------------------------------
-- Generated trees of pages

-- | Titles that slugify alike, that a link's address cannot hold, or its
-- label, and plain ones.
titles :: [Text]
titles = ["On-call", "On call", "on-call", "Guide", "guide", "Plans | Q3", "A]b", "Ops/Dev", "Q&A", "Who is on call?", "Café", "Notes: 2026", "x", "Deep", "Ops", "Release 2.0", "[draft]"]

-- | A tree of pages under a home page, each title from 'titles', each slug
-- its title's, made unique among its siblings as the wiki does (@-2@).
genTree :: Gen [PageSummary]
genTree = do
  n <- choose (1, 12 :: Int)
  foldM add [summary "p0" Nothing "Home" (slug' "home") 0] [1 .. n]
  where
    add pages i = do
      parent <- elements (map (\p -> p.id) pages)
      title <- elements titles
      let base = fromMaybe "page" (slugText <$> slugify title)
          taken = [slugText p.slug | p <- pages, p.parent == Just parent]
          free = fromMaybe base (head [s | s <- base : [base <> "-" <> show k | k <- [2 :: Int ..]], s `notElem` taken])
      pure (pages <> [summary ("p" <> show i) (Just parent) title (slug' free) i])
    slug' = either (panic . show) identity . mkSlug

summary :: Text -> Maybe PageId -> Text -> Slug -> Int -> PageSummary
summary i parent title slug position =
  PageSummary {id = PageId i, space = space.id, parent, title, slug, createdAt = time, updatedAt = time, position, template = False, kind = Document}
  where
    time = UTCTime (fromGregorian 2026 11 16) 0

-- | The space the trees are in: the demo's Docs, its home page the tree's.
space :: Space
space = (fromMaybe (panic "no docs") (find (\s -> slugText s.slug == "docs") (Map.elems (demoState demoNow).spaces))) {homePage = Just (PageId "p0")}

-- | For every page edited and every other page, the link written is read
-- by the wiki's parser as a wiki link whose text is the page's title, its
-- address is a path ('classifyDestination'), and 'resolvePageLink' leads it
-- to that page; and no shorter one among the forms considered (any number
-- of levels up to a page above both, then slugs down, the last one the
-- title where 'pageLinkText' allows a title) is also read so.
linksResolve :: [PageSummary] -> Property
linksResolve pages =
  conjoin
    [ counterexample (toS (show (pathOf e) <> " -> " <> show (pathOf p) <> ": " <> writtenLink)) $
        (readsAs (pathOf e) writtenLink, [f | f <- forms (pathOf e) p, T.length f < T.length writtenLink, readsAs (pathOf e) f == Just (target p)])
          === (Just (target p), [])
    | e <- pages
    , p <- pages
    , p.id /= e.id
    , let (address, label) = pageLinkText (pathOf e) (pathOf p) p.title
          writtenLink = "[[" <> address <> maybe "" ("|" <>) label <> "]]"
    ]
  where
    spaces = space : filter (\s -> s.id /= space.id) (Map.elems (demoState demoNow).spaces)
    spacePath = [space.slug]
    pathOf p = fromMaybe (panic "no path") (S.pagePath space (\i -> find (\q -> q.id == i) pages) p.id)
    target p = (pageRoute spacePath (pathOf p), p.title)
    -- What a written link is read as: where it leads, and its text.
    readsAs linking written' = case parseMarkdown written' of
      Right blocks
        | [Link _ destination _ description] <- [i | i@Link {} <- allInlines blocks]
        , ToPath _ <- classifyDestination destination
        , Just (PageLink route "") <- resolvePageLink spaces spacePath (Just (PageId "p0")) pages linking destination ->
            Just (route, plainText description)
      _ -> Nothing
    -- The forms considered: any number of levels up to a page above both,
    -- then the slugs down, the last one the title where 'pageLinkText'
    -- allows a title; each written with the title as its label and
    -- without one.
    forms linking p =
      nub
        [ "[[" <> address <> labelPart <> "]]"
        | let parent = take (length linking - 1) linking
        , level <- [0 .. length parent]
        , take level parent `isPrefixOf` pathOf p
        , let ups = replicate (length parent - level) ".."
              down = map slugText (drop level (pathOf p))
        , address <- joined (ups <> down) : [joined (ups <> initDef [] down <> [p.title]) | not (null down), titleAllowed p.title]
        , labelPart <- ["", "|" <> labelOf p.title]
        ]
    joined = \case
      [] -> "."
      segments -> T.intercalate "/" segments
    labelOf = T.replace "]" "&#93;" . T.replace "&" "&amp;"
    -- 'pageLinkText''s rule for a title as the last segment.
    titleAllowed t = T.all (\c -> isAlphaNum c || c == ' ' || c == '-') t && T.strip t == t && not (T.null t)

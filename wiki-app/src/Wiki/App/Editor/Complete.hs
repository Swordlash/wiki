-- | 'Complete' is the editor's side of the completion of the wiki's own
-- Markdown ("Wiki.Core.Markdown.Complete", WP5.124 (b) and (c) of
-- docs/PLAN.md): the sources the editor's list is made from, out of what
-- the editor holds (the pages of the space among them, 'pageSource'), and
-- the answer to CodeMirror's question, in the shape @web/src/editor.js@
-- reads.
--
-- CodeMirror asks with a @wiki-complete@ event ("Wiki.App.Editor.Browser"'s
-- 'Wiki.App.Editor.Browser.completeAsked'): the number of the cursor's line
-- in the whole text, that line before the cursor and after it, and the
-- names of the syntax tree's nodes at the cursor. The component answers at
-- once, in the step that handles the event, from the text it holds
-- ('answer'): the question asks the backend nothing.
module Wiki.App.Editor.Complete
  ( Request (..)
  , Held (..)
  , Linkable (..)
  , linkables
  , linkablesFrom
  , sources
  , pageSource
  , Readings
  , noReadings
  , answer
  , completionJson
  )
where

import Data.Aeson (Value (String), object, (.=))
import Data.Map.Lazy qualified as Map
import Data.Map.Strict qualified as Strict
import Data.Text qualified as T
import Protolude
import Wiki.App.Route (pageLinkText)
import Wiki.Core.Id (PageId)
import Wiki.Core.Markdown.Box (BoxKind)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (PageKind)
import Wiki.Core.Slug (Slug, slugText, slugify)
import Wiki.Core.Space (Space, pagePath)

-- | 'Request' is CodeMirror's question: the cursor's line, from 1, in the
-- whole text (the master's, for a row of a presentation edited slide by
-- slide), that line before the cursor and after it, and the names of the
-- syntax tree's nodes at the cursor, innermost first.
data Request = Request
  { line :: Int
  , before :: Text
  , after :: Text
  , nodes :: [Text]
  }
  deriving stock (Eq, Show)

-- | 'Held' is what the editor holds that the sources read: the page's
-- kind, which says the markers it reads; the kinds of box the
-- administration defined; the page being edited, by its id; and the
-- pages of its space a link may name, as the editor keeps them
-- ('linkables').
data Held = Held
  { kind :: PageKind
  , boxes :: [BoxKind]
  , editing :: PageId
  , pages :: [Linkable]
  }

-- | 'Linkable' is a page of the space a link may name, with what the list
-- shows of it and what choosing it writes, worked out once for the page
-- being edited ('linkable'): its id, its title, and its path under the
-- space's home page ('Wiki.Core.Space.pagePath'; the home page's is
-- empty); the link's address and label ('pageLinkText'); how near it is
-- to the page edited (0 a sibling, 1 a page of its branch, 2 another);
-- and its title in lower case, the words of that, its path's segments,
-- and those joined with @/@, which the letters typed are matched against
-- and the list shows.
--
-- A completion's answer only matches and ranks these: the paths and the
-- links are the editor's to work out when the pages, or the page's own
-- path, change, not at each letter typed (a space of 10,000 pages took a
-- second an answer when each answer worked them out).
data Linkable = Linkable
  { id :: PageId
  , title :: Text
  , path :: [Slug]
  , address :: Text
  , label :: Maybe Text
  , nearness :: Int
  , lowerTitle :: Text
  , titleWords :: [Text]
  , segments :: [Text]
  , joined :: Text
  }
  deriving stock (Eq, Show)

-- | A page a link from the page at the first path may name: its id, its
-- title and its path.
linkable :: [Slug] -> PageId -> Text -> [Slug] -> Linkable
linkable editingPath i title path =
  Linkable
    { id = i
    , title
    , path
    , address
    , label
    , nearness
    , lowerTitle
    , titleWords = drop 1 (T.split (not . isAlphaNum) lowerTitle)
    , segments = map slugText path
    , joined = T.intercalate "/" (map slugText path)
    }
  where
    (address, label) = pageLinkText editingPath path title
    lowerTitle = T.toLower title
    parentOf p = take (length p - 1) p
    nearness
      | not (null path) && parentOf path == parentOf editingPath = 0
      | (_ : _, _ : _) <- (path, editingPath), take 1 path == take 1 editingPath = 1
      | otherwise = 2

-- | The pages of a space a link from the page at the path may name, as the
-- editor keeps them: every one but a template, each with its path (the
-- pages looked up by id in a map, not searched for in the list), in the
-- order the list offers them in before anything is typed ('pageSource').
linkables :: Space -> [PageSummary] -> [Slug] -> [Linkable]
linkables space summaries editingPath =
  linkablesFrom
    editingPath
    [ (p.id, p.title, path)
    | p <- summaries
    , not p.template
    , Just path <- [pagePath space (`Strict.lookup` byId) p.id]
    ]
  where
    byId = Strict.fromList [(p.id, p) | p <- summaries]

-- | The pages a link from the page at the path may name, each given by its
-- id, its title and its path, as the editor keeps them ('linkables').
linkablesFrom :: [Slug] -> [(PageId, Text, [Slug])] -> [Linkable]
linkablesFrom editingPath found = sortOn baseOrder [linkable editingPath i title path | (i, title, path) <- found]

-- | The order of the pages within one group of matches ('pageSource'):
-- the nearer first, then the shorter link, then the title in
-- alphabetical order, then the path.
baseOrder :: Linkable -> (Int, Int, Text, [Text])
baseOrder p = (p.nearness, T.length p.address + maybe 0 (\l -> 1 + T.length l) p.label, p.lowerTitle, p.segments)

-- | The sources of the editor's list, in the order their candidates are
-- listed: the wiki's own markers and kinds of box, and the space's pages.
-- A plugin's source (#186) goes after them.
sources :: Held -> [Source]
sources held = [wikiSource held.kind held.boxes, pageSource held]

-- | The pages of the space, after @[[@: every one but the page being
-- edited, at most 'shownPages', each shown by its title with its path in
-- grey beside it (a candidate's place, so that two pages of one title are
-- told apart; a plugin's candidate shows its origin after a place, see
-- 'candidateDetail'). Choosing one writes the link 'pageLinkText' gives,
-- with its @]]@, the cursor after it.
--
-- The letters typed after @[[@ choose the pages, whatever their case:
--
-- 1. the pages whose title starts with them (all of them, with no
--    letters);
-- 2. then those with a later word of the title that starts with them, or
--    a segment of the path that starts with them made a slug (@on c@
--    finds @on-call@);
-- 3. then those whose title or path holds them anywhere else.
--
-- A page that holds them nowhere is not offered. Within each group, the
-- pages nearer the page being edited come first: its siblings (the same
-- parent), then the pages of its branch (under the same top-level page),
-- then the others; then the shorter link, then the title in alphabetical
-- order, then the path.
pageSource :: Held -> Source
pageSource held wanted
  | wanted.typed.trigger /= LinkToPage = []
  | otherwise = map candidate (take shownPages (concat [[p | (r, p) <- matched, r == rank] | rank <- [1, 2, 3 :: Int]]))
  where
    typed = wanted.typed
    -- The pages are kept in the order of each group ('linkables'), so
    -- the list takes the first of each group in turn, and goes through
    -- the pages no further than it needs: each page is matched once.
    matched = [(r, p) | p <- held.pages, p.id /= held.editing, Just r <- [matchOf p]]
    query = T.toLower (T.strip typed.letters)
    querySlug = maybe query slugText (slugify query)
    matchOf p
      | T.null query || query `T.isPrefixOf` p.lowerTitle = Just (1 :: Int)
      | any (query `T.isPrefixOf`) p.titleWords = Just 2
      | any (querySlug `T.isPrefixOf`) p.segments = Just 2
      | query `T.isInfixOf` p.lowerTitle || querySlug `T.isInfixOf` p.joined = Just 3
      | otherwise = Nothing
    candidate p =
      Candidate
        { label = p.title
        , skeleton = [Literal written, Field 0 ""]
        , place = Just (if null p.path then "the home page" else p.joined)
        , writing = if "\n" `T.isInfixOf` written then AsText else AsSnippet
        , origin = Nothing
        }
      where
        -- The reader's own label, typed already, stays; otherwise the
        -- link's own, as a label is written at the cursor (in a table's
        -- cell, its bars escaped; where none reads as one, none), and its
        -- end.
        written
          | T.null typed.kept = "[[" <> p.address <> labelPart <> "]]"
          | otherwise = "[[" <> p.address <> typed.kept
        labelPart = case (p.label, typed.labels) of
          (Just label, PlainLabel) -> "|" <> label
          (Just label, EscapedLabel) -> "\\|" <> T.replace "|" "\\|" label
          _ -> ""

-- | How many pages the list offers at most: enough to scroll through, few
-- enough for a space of thousands of pages to answer at once.
shownPages :: Int
shownPages = 50

-- | 'Readings' is the editor's memo of what the wiki's parser reads on a
-- line put after the lines above a cursor ('readLine'): the lines above,
-- and each line asked about with its reading, kept while the lines above
-- do not change. Typing the letters after a trigger changes only the
-- cursor's line, and the line asked about is the same each time (the
-- letters replaced by a whole name), so the parser reads the text above
-- once for each trigger typed, not once for each letter. A reading is
-- worked out only when it is wanted.
data Readings = Readings
  { above :: [Text]
  , byLine :: Map.Map Probe LineReading
  }

-- | A memo of nothing.
noReadings :: Readings
noReadings = Readings {above = [], byLine = Map.empty}

-- | The list for a request, against the text the editor holds, or
-- 'Nothing': when nothing opens there, and when the cursor's line in that
-- text is not the request's (the text has changed since CodeMirror asked,
-- and CodeMirror asks again as more is typed); and the memo of the
-- parser's readings, to answer the next request with.
answer :: [Source] -> Readings -> Text -> Request -> (Maybe Completion, Readings)
answer found memo text request = case T.splitOn "\n" text of
  ls
    | request.line >= 1
    , Just current <- ls `atMay` (request.line - 1)
    , current == request.before <> request.after ->
        let above = take (request.line - 1) ls
            at = At {place = placeOf request.nodes, above, before = request.before, after = request.after, below = drop request.line ls}
            kept = if memo.above == above then memo.byLine else Map.empty
            -- Each probe the list may ask about, read only when it is
            -- asked: the first ones, then those written from the block
            -- the first ones find the cursor's line in.
            add m probe = Map.insertWith (\_ old -> old) probe (readLine above probe) m
            readFrom m probe = fromMaybe (readLine above probe) (Map.lookup probe m)
            first' = foldl' add kept (firstProbes at)
            byLine = foldl' add first' (probesOf (readFrom first') at)
         in (complete found (readFrom byLine) at, Readings {above, byLine})
  _ -> (Nothing, memo)

-- | An answer as @web/src/editor.js@ reads it: where the text the entry
-- chosen replaces starts in the line, and how far after the cursor it
-- goes, in characters; the options, each with its label, its snippet
-- ('snippetTemplate') and what is shown grey beside the label
-- ('candidateDetail': where the page is, where a plugin's candidate comes
-- from); and what Enter and Escape do.
completionJson :: Completion -> Value
completionJson c =
  object
    [ "from" .= c.from
    , "through" .= c.through
    , "options" .= map option c.candidates
    , "enter" .= case c.enter of
        EnterChooses -> String "chooses"
        EnterChoosesAfter n -> object ["after" .= n]
    , "escape" .= case c.escape of
        EscapeCloses -> String "closes"
    ]
  where
    option candidate =
      object
        ( ["label" .= candidate.label]
            <> ( case candidate.writing of
                   AsSnippet -> ["template" .= snippetTemplate candidate.skeleton]
                   AsText -> ["insert" .= fst (skeletonText candidate.skeleton)]
               )
            <> ["detail" .= detail | Just detail <- [candidateDetail candidate]]
        )

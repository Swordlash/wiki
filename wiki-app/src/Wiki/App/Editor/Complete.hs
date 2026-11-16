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
import Data.Text qualified as T
import Protolude
import Wiki.App.Route (pageLinkText)
import Wiki.Core.Id (PageId)
import Wiki.Core.Markdown.Box (BoxKind)
import Wiki.Core.Markdown.Complete
import Wiki.Core.PageKind (PageKind)
import Wiki.Core.Slug (Slug, slugText, slugify)

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
-- administration defined; the page being edited, by its id and its path
-- (what a link from it is read against: a new page has its path from the
-- moment it is made, before its first save); and the pages of its space a
-- link may name.
data Held = Held
  { kind :: PageKind
  , boxes :: [BoxKind]
  , editing :: PageId
  , editingPath :: [Slug]
  , pages :: [Linkable]
  }

-- | 'Linkable' is a page of the space a link may name: its id, its title,
-- and its path under the space's home page ('Wiki.Core.Space.pagePath';
-- the home page's is empty).
data Linkable = Linkable
  { id :: PageId
  , title :: Text
  , path :: [Slug]
  }
  deriving stock (Eq, Show)

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
  | otherwise =
      [ candidate p
      | (_, p) <-
          take shownPages . sortOn fst $
            [ ((rank, nearness p, T.length (linkText p), T.toLower p.title, map slugText p.path), p)
            | p <- held.pages
            , p.id /= held.editing
            , Just rank <- [matchOf p]
            ]
      ]
  where
    typed = wanted.typed
    query = T.toLower (T.strip typed.letters)
    querySlug = maybe query slugText (slugify query)
    matchOf p
      | T.null query || query `T.isPrefixOf` titleLower = Just (1 :: Int)
      | any (query `T.isPrefixOf`) (drop 1 (T.split (not . isAlphaNum) titleLower)) = Just 2
      | any ((querySlug `T.isPrefixOf`) . slugText) p.path = Just 2
      | query `T.isInfixOf` titleLower || querySlug `T.isInfixOf` T.intercalate "/" (map slugText p.path) = Just 3
      | otherwise = Nothing
      where
        titleLower = T.toLower p.title
    parentOf path = take (length path - 1) path
    nearness p
      | not (null p.path) && parentOf p.path == parentOf held.editingPath = 0 :: Int
      | (_ : _, _ : _) <- (p.path, held.editingPath), take 1 p.path == take 1 held.editingPath = 1
      | otherwise = 2
    linkText p = let (address, label) = pageLinkText held.editingPath p.path p.title in maybe address (\l -> address <> "|" <> l) label
    candidate p =
      Candidate
        { label = p.title
        , skeleton = [Literal written, Field 0 ""]
        , place = Just (if null p.path then "the home page" else T.intercalate "/" (map slugText p.path))
        , origin = Nothing
        }
      where
        (address, label) = pageLinkText held.editingPath p.path p.title
        -- The reader's own label, typed already, stays; otherwise the
        -- link's own, and its end.
        written
          | T.null typed.kept = "[[" <> address <> maybe "" ("|" <>) label <> "]]"
          | otherwise = "[[" <> address <> typed.kept

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
            at = At {place = placeOf request.nodes, above, before = request.before, after = request.after}
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
        ( ["label" .= candidate.label, "template" .= snippetTemplate candidate.skeleton]
            <> ["detail" .= detail | Just detail <- [candidateDetail candidate]]
        )

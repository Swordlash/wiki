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
  , MathCommand (..)
  , macroCommands
  , pageMacroCommands
  , pageSource
  , HeldSpace (..)
  , heldSpaces
  , elsewhereSource
  , Readings
  , noReadings
  , Earlier (..)
  , earlierFrom
  , noEarlier
  , answer
  , linkLetters
  , completionJson
  )
where

import Data.Aeson (Value (String), object, (.=))
import Data.List (lookup)
import Data.Map.Lazy qualified as Map
import Data.Map.Strict qualified as Strict
import Data.Text qualified as T
import Protolude
import Wiki.App.Route (ownPageLinkText, pageLinkText)
import Wiki.Core.Id (PageId, SpaceId)
import Wiki.Core.Markdown.Box (BoxKind)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Markdown.Edit (pageMacros)
import Wiki.Core.Markdown.Starts (Earlier (..), earlierFrom, linesBelow, linesFromStart, noEarlier)
import Wiki.Core.Math (definedMacros)
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (PageKind)
import Wiki.Core.PageMatch (Matchable (..), TitleHit (..), matchRank, matchable, typedLetters)
import Wiki.Core.Path (ancestry)
import Wiki.Core.Slug (Slug, slugText)
import Wiki.Core.Space (Space (conversion, encryption, id, name, slug), pagePath)

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
-- administration defined; the page being edited, by its id; the pages
-- of its space a link may name, as the editor keeps them ('linkables');
-- the entries of the @/@ menu, what the toolbar writes
-- ("Wiki.App.Editor.Tools"' 'Wiki.App.Editor.Tools.slashEntries'); and
-- the macros math may use, the nearest first (the page's, each space's up
-- its chain, the wiki's), each with where it comes from.
data Held = Held
  { kind :: PageKind
  , boxes :: [BoxKind]
  , editing :: PageId
  , pages :: [Linkable]
  , entries :: [SlashEntry]
  , mathMacros :: [MathCommand]
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
    , lowerTitle = matched.lowerTitle
    , titleWords = matched.titleWords
    , segments = matched.segments
    , joined = matched.joined
    }
  where
    (address, label) = pageLinkText editingPath path title
    matched = matchable title path
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
-- listed: the wiki's own markers and kinds of box, the space's pages, what
-- the toolbar writes, and the commands of math. A plugin's source (#186)
-- goes after them.
sources :: Held -> [Source]
sources held = [wikiSource held.kind held.boxes, pageSource held, slashSource held.entries, mathSource (mathCommands held.mathMacros)]

-- | The macros a block of TeX defines ("Wiki.Core.Math"' 'definedMacros'),
-- as commands math offers, each with where it comes from: a space by its
-- name, the wiki.
macroCommands :: Text -> Text -> [MathCommand]
macroCommands origin tex = [MathCommand {name, origin} | name <- definedMacros tex]

-- | The macros of a page's block of macros, the @math@ fence under its
-- title that the editor's "Macros" panel edits ("Wiki.Core.Markdown.Edit"'
-- 'pageMacros'), as commands math offers. A definition elsewhere in the
-- page (in a later formula, which KaTeX reads for the formulas after it)
-- is not offered: finding it means reading every formula of the page.
pageMacroCommands :: Text -> [MathCommand]
pageMacroCommands body = macroCommands "this page" (fromMaybe "" (pageMacros body))

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
    query = typedLetters typed.letters
    -- The rule shared with the lookup of pages by title
    -- ("Wiki.Core.PageMatch").
    matchOf p = matchRank query Matchable {lowerTitle = p.lowerTitle, titleWords = p.titleWords, segments = p.segments, joined = p.joined}
    candidate p =
      Candidate
        { label = p.title
        , skeleton = [Literal written, Field 0 ""]
        , -- As text, which the editor writes as the smallest change: the
          -- end of a link kept is not written again, and what is on it (an
          -- assistant's undecided change) stays.
          writing = AsText
        , action = Nothing
        , note = Just (if null p.path then "the home page" else p.joined)
        , origin = Nothing
        }
      where
        written = linkWritten typed p.address p.label

-- | The wiki link a page's entry writes, given its address and its label
-- if it needs one ('pageLinkText', 'ownPageLinkText'). The reader's own
-- label, typed already, stays, and so does the end the reader typed
-- ('Typed''s 'kept'). Otherwise the link's own label is written as a label
-- is written at the cursor: after a bar; in a table's cell, after an
-- escaped bar, its own bars escaped; where none reads as one, none. Then
-- the link's end.
linkWritten :: Typed -> Text -> Maybe Text -> Text
linkWritten typed address label
  | T.null typed.kept = "[[" <> address <> labelPart <> "]]"
  | otherwise = "[[" <> address <> typed.kept
  where
    labelPart = case (label, typed.labels) of
      (Just l, PlainLabel) -> "|" <> l
      (Just l, EscapedLabel) -> "\\|" <> T.replace "|" "\\|" l
      _ -> ""

-- | 'HeldSpace' is a space as the editor holds it, for the pages of other
-- spaces after @[[@ (#301): its name, its path from the top level, and
-- whether it is plain (neither encrypted nor being encrypted or
-- decrypted). The lookup's machine ("Wiki.App.Editor.Elsewhere.Machine")
-- closes the list when one of these changes, and 'elsewhereSource' offers
-- a page only of a space held plain at the same path.
data HeldSpace = HeldSpace
  { name :: Text
  , path :: [Slug]
  , plain :: Bool
  }
  deriving stock (Eq, Show)

-- | The spaces given, as the editor holds them, by id: each space's path
-- is the slugs of its chain of spaces from the top level, as
-- 'Wiki.Core.Permission.spaceChain' reads it. The chain is followed in a
-- map of the spaces by id, made once, rather than by a scan of the list
-- for each space of each chain: the editor works this out at every change
-- of its input.
heldSpaces :: [Space] -> Map.Map SpaceId HeldSpace
heldSpaces spaces = Map.map held byId
  where
    byId = Map.fromList [(s.id, s) | s <- spaces]
    held s =
      HeldSpace
        { name = s.name
        , path = map (\c -> c.slug) (fromMaybe [] (ancestry (`Map.lookup` byId) s.id))
        , plain = isNothing s.encryption && isNothing s.conversion
        }

-- | The pages of other spaces after @[[@ (#301): the pages the lookup of
-- pages by title found ("Wiki.Core.PageMatch"'s 'TitleHit'), in the order
-- it gave them. The lookup leaves out the space of the page edited, whose
-- pages 'pageSource' offers. Each entry shows the page's title, and in
-- grey the name of its space (from the spaces given, by the space's id)
-- and where it is: the space's path and the page's path, or the space's
-- path for its home page.
--
-- A hit is offered only when its space, by its id, is one the editor
-- holds plain at the same path ('HeldSpace'): the preview resolves the
-- link written against the spaces the editor holds, and a space created
-- or moved since the editor heard of the spaces is offered once it has.
-- A space the editor holds as encrypted, or being encrypted or decrypted,
-- is not offered: the answer may have been made before the space's
-- encryption began.
--
-- Choosing one writes a link to the page's own address with its title as
-- the label ('ownPageLinkText'): @[[\/s\/eng\/p\/onboarding\/|Onboarding]]@.
-- A space's home page is the space's address, @[[\/s\/eng\/|Engineering]]@,
-- labelled with the space's name. The label is written as 'linkWritten'
-- writes it: escaped in a table's cell, and the reader's own label kept.
elsewhereSource :: Map.Map SpaceId HeldSpace -> [TitleHit] -> Source
elsewhereSource held hits wanted
  | wanted.typed.trigger /= LinkToPage = []
  | otherwise = [candidate hit s.name | hit <- hits, Just s <- [Map.lookup hit.space held], s.plain, s.path == hit.spacePath]
  where
    candidate hit spaceName =
      Candidate
        { label = hit.title
        , skeleton = [Literal (linkWritten wanted.typed address (Just label)), Field 0 ""]
        , writing = AsText
        , action = Nothing
        , note = Just (spaceName <> " · " <> place)
        , origin = Nothing
        }
      where
        home = null hit.pagePath
        (address, label) = ownPageLinkText hit.spacePath hit.pagePath (if home then spaceName else hit.title)
        spacePlace = T.intercalate "/" (map slugText hit.spacePath)
        place
          | home = spacePlace <> ", the home page"
          | otherwise = spacePlace <> "/" <> T.intercalate "/" (map slugText hit.pagePath)

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
--
-- The parser reads the text above the cursor's line from the last start
-- of a block the editor's last check found there that the text has kept
-- ('Earlier', "Wiki.Core.Markdown.Starts"' 'linesFromStart'), not from
-- the top: the first trigger in the middle of a long page read the whole
-- text above, seconds on WebAssembly. The answer is the same. Where no
-- start serves within the limit, a marker or a link to a page reads the
-- whole text above, and anything else opens no list
-- ('readsWholeAbove').
answer :: [Source] -> Earlier -> Readings -> Text -> Request -> (Maybe Completion, Readings)
answer found earlier memo text request = case readAround earlier memo text request of
  Just (readAt, at, memo') -> (complete found readAt at, memo')
  Nothing -> (Nothing, memo)

-- | The letters typed after @[[@ where a link to a page opens a list (the
-- trigger 'LinkToPage'), against the text the editor holds, as 'answer'
-- reads the request; 'Nothing' when no such list opens there, or the
-- cursor's line in that text is not the request's. And the memo of the
-- parser's readings, as 'answer' leaves it. The lookup of other spaces'
-- pages (#301) asks the server for these letters.
linkLetters :: Earlier -> Readings -> Text -> Request -> (Maybe Text, Readings)
linkLetters earlier memo text request = case readAround earlier memo text request of
  Just (readAt, at, memo') -> ((\typed -> typed.letters) <$> mfilter (\typed -> typed.trigger == LinkToPage) (triggerAt readAt at), memo')
  Nothing -> (Nothing, memo)

-- | The cursor's place for a request, against the text the editor holds,
-- and the parser's reading of a line there, as 'answer' reads them; and
-- the memo of those readings. 'Nothing' when the cursor's line in that
-- text is not the request's, or no start of a block serves and nothing
-- reads the whole text above.
readAround :: Earlier -> Readings -> Text -> Request -> Maybe (LineReader, At, Readings)
readAround earlier memo text request = case T.splitOn "\n" text of
  ls
    | request.line >= 1
    , Just current <- ls `atMay` (request.line - 1)
    , current == request.before <> request.after
    , let whole = take (request.line - 1) ls
          allBelow = drop request.line ls
          keptBelow = linesBelow afterBound earlier whole request.before request.after allBelow
          placed above = At {place = placeOf request.nodes, above, before = request.before, after = request.after, below = maybe allBelow (`take` allBelow) keptBelow, cutBelow = isJust keptBelow && pastBound request.after allBelow}
    , Just above <- linesFromStart earlier whole <|> (whole <$ guard (readsWholeAbove (placed whole))) ->
        let at = placed above
            kept = if memo.above == above then memo.byLine else Map.empty
            -- Each probe the list may ask about, read only when it is
            -- asked: the first ones, then those written from the block
            -- the first ones find the cursor's line in.
            add m probe = Map.insertWith (\_ old -> old) probe (readLine above probe) m
            readFrom m probe = fromMaybe (readLine above probe) (Map.lookup probe m)
            first' = foldl' add kept (firstProbes at)
            byLine = foldl' add first' (probesOf (readFrom first') at)
         in Just (readFrom byLine, at, Readings {above, byLine})
  _ -> Nothing

-- | An answer as @web/src/editor.js@ reads it: where the text the entry
-- chosen replaces starts in the line, and how far after the cursor it
-- goes, in characters; the options, each with its label, what it writes,
-- what is shown grey beside the label ('candidateDetail': where the page
-- is, what an entry of the @/@ menu writes, where a plugin's candidate
-- comes from), and what the editor does once it is written (@action@);
-- and what Enter and Escape do.
--
-- What an option writes ('Writes') is its snippet (@template@,
-- 'snippetTemplate') in place of the answer's range, or there the text
-- @insert@ as it is, the cursor after it; or, with @line@, the text
-- @insert@ written as it is in place of the cursor's whole line, which
-- must still read as @line@ (the editor writes nothing otherwise),
-- @select@ selected in it (from and to, in characters), the @/@ and its
-- letters, at @query@ in @line@ (from and to, in characters), taken away
-- as a change of their own; and for a command that leaves more than one
-- place to fill, @places@, each from and to in @insert@ in the order Tab
-- goes through them (@select@ the first), and @after@, where the cursor
-- goes after the last.
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
        EscapeDismisses -> String "dismisses"
    ]
  where
    option candidate =
      object
        ( ["label" .= candidate.label]
            <> written candidate
            <> ["detail" .= detail | Just detail <- [candidateDetail candidate]]
            <> ["action" .= action | Just action <- [candidate.action]]
        )
    written candidate = case candidate.writing of
      AsSnippet -> ["template" .= snippetTemplate candidate.skeleton]
      AsText -> ["insert" .= fst (skeletonText candidate.skeleton)]
      LineAsText line (queryFrom, queryTo) ->
        let (text, places) = skeletonText candidate.skeleton
            fill = [range | (n, range) <- places, n /= 0]
            (selectFrom, selectTo) = fromMaybe (T.length text, T.length text) (head fill)
            -- A command with more than one place: all of them, and where
            -- the writing goes on after them.
            walk = case (fill, lookup 0 places) of
              (_ : _ : _, Just (after, _)) -> ["places" .= [[a, b] | (a, b) <- fill], "after" .= after]
              _ -> []
         in ["insert" .= text, "select" .= [selectFrom, selectTo], "line" .= line, "query" .= [queryFrom, queryTo]] <> walk

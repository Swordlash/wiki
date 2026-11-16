-- | 'Complete' is the editor's completion of the wiki's own Markdown
-- (WP5.124 (b) of docs/PLAN.md, #187): what a list opens after @[[!@,
-- @[[/@ and @[!@, what it offers, and what choosing an entry writes.
--
-- The editor's CodeMirror (@web/src/editor.js@) shows the list, moves in
-- it and writes the entry chosen. It decides nothing of this: it says
-- where the cursor is (the names of the syntax tree's nodes there, and
-- the cursor's line up to the cursor), and the component answers with
-- 'complete'. So every rule here is a pure function, tested natively.
--
-- * __Where a list opens__ ('triggerAt'): @[[!@ (a start marker) and
--   @[[/@ (an end marker) at a line's start, after its indentation and
--   any @>@ of a quotation, as the parser reads a marker
--   ("Wiki.Core.Markdown.Ast"); @[!@ (a kind of box) on the first line of
--   a quotation, after its @>@. Never in code, HTML, a comment, math or a
--   link's address ('placeOf').
-- * __What it offers__: candidates, from a list of 'Source's. The wiki's
--   own is 'wikiSource': the markers the page's kind reads, the blocks
--   open at the cursor ('openBlocks', innermost first), the kinds of box.
--   A plugin will give a source of its own (#186), whose candidates say
--   where they come from ('Origin'), shown grey beside each one.
-- * __What choosing writes__: a 'Skeleton', literal text and numbered
--   places to fill, written as a CodeMirror snippet ('snippetTemplate'):
--   the cursor goes to the first place, and Tab to the next.
-- * __What Enter and Escape do__ in the list ('EnterPolicy',
--   'EscapePolicy'), carried by the answer.
module Wiki.Core.Markdown.Complete
  ( -- * Where a list opens
    Place (..)
  , placeOf
  , At (..)
  , Trigger (..)
  , Typed (..)
  , triggerAt

    -- * Blocks open at the cursor
  , openBlocks

    -- * Candidates
  , Origin (..)
  , originText
  , Candidate (..)
  , Wanted (..)
  , Source
  , wikiSource
  , markersRead

    -- * The answer
  , EnterPolicy (..)
  , EscapePolicy (..)
  , Completion (..)
  , complete

    -- * Skeletons
  , Piece (..)
  , Skeleton
  , snippetTemplate
  , skeletonText
  )
where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (nub)
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (MarkerName (..), MarkerStart (..), closingMarker, holdsBlocks, markerAt, markerNameText, openingMarker)
import Wiki.Core.Markdown.Box (BoxKind, boxChoices)
import Wiki.Core.PageKind (PageKind (..))

----------------------------------------------------------------------
-- Where a list opens

-- | 'Place' is what the syntax tree says of the cursor's place: in text,
-- where the wiki's markup counts, or in something whose text is its own
-- (code, HTML, a comment, math, a link's address), where no list opens.
data Place = InText | InOwnText
  deriving stock (Eq, Show)

-- | The place the cursor is in, from the names of the syntax tree's nodes
-- around it, innermost first, as CodeMirror's Markdown parser names them
-- (and the editor's @TeXMath@, in @web/src/editor.js@). A link's text is
-- text: CodeMirror reads @[[!notes]]@ as brackets around a link, which
-- the wiki's parser does not.
placeOf :: [Text] -> Place
placeOf nodes = if any (`elem` ownText) nodes then InOwnText else InText
  where
    ownText =
      [ "FencedCode"
      , "CodeBlock"
      , "InlineCode"
      , "HTMLBlock"
      , "HTMLTag"
      , "CommentBlock"
      , "Comment"
      , "ProcessingInstructionBlock"
      , "ProcessingInstruction"
      , "TeXMath"
      , "URL"
      , "LinkReference"
      , "Autolink"
      ]

-- | 'At' is where a completion is asked for: the place, the lines of the
-- text above the cursor's line (in order), and the cursor's line up to
-- the cursor.
data At = At
  { place :: Place
  , above :: [Text]
  , before :: Text
  }
  deriving stock (Eq, Show)

-- | 'Trigger' is what opens a list.
data Trigger
  = -- | @[[!@ at a line's start: a block of the wiki's own.
    StartMarker
  | -- | @[[/@ at a line's start: the end of a block open there.
    EndMarker
  | -- | @[!@ on the first line of a quotation: a box's kind.
    BoxMarker
  deriving stock (Eq, Show)

-- | 'Typed' is what was typed: the trigger, the letters after it, where
-- the trigger starts in the cursor's line (in characters), which the
-- entry chosen replaces up to the cursor, and what each further line of
-- a skeleton starts with ('lineStart').
data Typed = Typed
  { trigger :: Trigger
  , letters :: Text
  , from :: Int
  , lineStart :: Text
  }
  deriving stock (Eq, Show)

-- | What opens a list at a place, if anything.
--
-- A line's start is after its indentation and the @>@ of the quotations
-- it is in, each with one space or none after it. 'lineStart' is those
-- @>@ without the indentation: CodeMirror's snippet puts the indentation
-- of the cursor's line before each further line of a skeleton itself,
-- and the skeleton adds the @>@, so that a block written in a quotation
-- or in a list's item stays in it.
--
-- A kind of box opens on the first line of a quotation: the line before
-- is in fewer quotations (or none), since a box's marker is its
-- quotation's first line.
triggerAt :: At -> Maybe Typed
triggerAt at = do
  guard (at.place == InText)
  let (indentation, afterIndentation) = T.span isBlank at.before
      (marks, rest) = quotationMarks afterIndentation
      start = T.length indentation + T.length marks
      typed trigger prefix allowed = do
        letters <- T.stripPrefix prefix rest
        guard (T.all allowed letters)
        Just Typed {trigger, letters, from = start, lineStart = marks}
  asum
    [ typed StartMarker "[[!" isAsciiLower
    , typed EndMarker "[[/" isAsciiLower
    , do
        guard (depth marks > maybe 0 (depth . fst . quotationMarks . T.dropWhile isBlank) (lastMay at.above))
        typed BoxMarker "[!" (\c -> isAsciiLower c || isAsciiUpper c || c == '-')
    ]
  where
    depth = T.count ">"

-- | The @>@ a line starts with (each with a space or a tab after it, or
-- none), and the rest of the line.
quotationMarks :: Text -> (Text, Text)
quotationMarks t = case T.stripPrefix ">" t of
  Just rest ->
    let (space, after) = T.splitAt (if T.take 1 rest `elem` [" ", "\t"] then 1 else 0) rest
        (more, final) = quotationMarks after
     in (">" <> space <> more, final)
  Nothing -> ("", t)

isBlank :: Char -> Bool
isBlank c = c == ' ' || c == '\t'

----------------------------------------------------------------------
-- Blocks open at the cursor

-- | The blocks of the wiki's own open after these lines, the innermost
-- first: those a start marker opened ('holdsBlocks') and no end marker
-- closed. The lines are read one by one, not parsed: a line is a marker
-- by 'markerAt', after its indentation and its quotations' @>@; the lines
-- of a fenced code block are code, and are skipped. An end marker closes
-- the innermost open block when it has its name, and otherwise nothing,
-- as the parser reads it.
--
-- Not read: an HTML block or a comment holding a marker line, and a
-- quotation or a list's item that ends before its block's end marker.
-- Either reads as open here a block the parser has closed or never
-- opened; the list then offers its end marker first, which writes text
-- the page shows as a problem, as typing it would.
openBlocks :: [Text] -> [MarkerName]
openBlocks = go Nothing []
  where
    go :: Maybe (Char, Int) -> [MarkerName] -> [Text] -> [MarkerName]
    go _ open [] = open
    go fence open (line : rest) =
      let content = snd (quotationMarks (T.dropWhile isBlank line))
       in case fence of
            Just (c, n)
              | closesFence c n content -> go Nothing open rest
              | otherwise -> go fence open rest
            Nothing
              | Just opened <- fenceOf content -> go (Just opened) open rest
              | otherwise -> case markerAt content of
                  Just (Opening name) | holdsBlocks name -> go Nothing (name : open) rest
                  Just (Closing name) | take 1 open == [name] -> go Nothing (drop 1 open) rest
                  _ -> go Nothing open rest
    fenceOf content = do
      c <- head (T.unpack (T.take 1 content))
      guard (c == '`' || c == '~')
      let run = T.takeWhile (== c) content
      guard (T.length run >= 3)
      -- A backtick fence's info string holds no backtick.
      guard (c /= '`' || not (T.any (== '`') (T.drop (T.length run) content)))
      Just (c, T.length run)
    closesFence c n content =
      let run = T.takeWhile (== c) content
       in T.length run >= n && T.all isBlank (T.drop (T.length run) content)

----------------------------------------------------------------------
-- Candidates

-- | 'Origin' is where a candidate comes from, when not from the wiki
-- itself: a plugin, by its name (#186). The list shows it beside the
-- candidate, in grey ('originText').
newtype Origin = FromPlugin Text
  deriving stock (Eq, Show)

-- | An origin as the list shows it.
originText :: Origin -> Text
originText (FromPlugin name) = "from plugin " <> name

-- | 'Candidate' is an entry of the list: what it shows, what choosing it
-- writes in place of what was typed, and where it comes from ('Nothing'
-- for the wiki's own).
data Candidate = Candidate
  { label :: Text
  , skeleton :: Skeleton
  , origin :: Maybe Origin
  }
  deriving stock (Eq, Show)

-- | 'Wanted' is what a source is asked for: what was typed, and the
-- blocks open at the cursor, innermost first.
data Wanted = Wanted
  { typed :: Typed
  , open :: [MarkerName]
  }
  deriving stock (Eq, Show)

-- | 'Source' gives the candidates for what is wanted, in its order, or
-- none. The list holds those of every source, in the sources' order.
type Source = Wanted -> [Candidate]

-- | The wiki's own candidates, for a page of a kind, with the kinds of
-- box the administration defined:
--
-- * after @[[!@, the markers the page's kind reads ('markersRead'), each
--   written with its end marker where it has one;
-- * after @[[/@, the blocks open at the cursor, the innermost first;
-- * after @[!@, GitHub's five kinds of box and the defined ones
--   ('boxChoices').
--
-- Each one whose name starts with the letters typed, whatever their case.
wikiSource :: PageKind -> [BoxKind] -> Source
wikiSource kind defined wanted = case t.trigger of
  StartMarker -> [startMarker t.lineStart name | name <- markersRead kind, matches (markerNameText name)]
  EndMarker -> [endMarker name | name <- nub wanted.open, matches (markerNameText name)]
  BoxMarker -> [box t.lineStart marker | (marker, _) <- boxChoices defined, matches marker]
  where
    t = wanted.typed
    matches name = T.toLower t.letters `T.isPrefixOf` T.toLower name

-- | The markers a page of a kind reads: in a presentation every one; in a
-- document, or a page of a kind this program does not know, none that
-- only a presentation reads (its notes, its incremental lists, a pause,
-- its look), which "Wiki.Core.Markdown.Problems" warns about there.
markersRead :: PageKind -> [MarkerName]
markersRead = \case
  Presentation -> [minBound .. maxBound]
  _ -> [SpoilerName, DivName, TocName]

-- | A start marker's candidate: the marker, and for a block that holds
-- others its end marker two lines below, the cursor on the line between.
-- A spoiler's name and description are places to fill first; a
-- presentation's look has its theme to fill.
startMarker :: Text -> MarkerName -> Candidate
startMarker lineStart name = Candidate {label = openingMarker name, skeleton, origin = Nothing}
  where
    skeleton = case name of
      SpoilerName ->
        [Literal (openingMarker name <> "{"), Field 1 "name", Literal "}(", Field 2 "Description", Literal (")\n" <> lineStart), Field 3 ""]
          <> closing
      StyleName -> [Literal (openingMarker name <> "{theme="), Field 1 "dark", Literal "}", Field 0 ""]
      _
        | holdsBlocks name -> [Literal (openingMarker name <> "\n" <> lineStart), Field 1 ""] <> closing
        | otherwise -> [Literal (openingMarker name), Field 0 ""]
    closing = [Literal ("\n" <> lineStart <> closingMarker name), Field 0 ""]

-- | An end marker's candidate.
endMarker :: MarkerName -> Candidate
endMarker name = Candidate {label = closingMarker name, skeleton = [Literal (closingMarker name), Field 0 ""], origin = Nothing}

-- | A kind of box's candidate: its marker (the kind's name in capitals,
-- as 'boxChoices' gives it), and the box's first line of text under it,
-- the cursor there.
box :: Text -> Text -> Candidate
box lineStart marker = Candidate {label = written, skeleton = [Literal (written <> "\n" <> lineStart), Field 1 ""], origin = Nothing}
  where
    written = "[!" <> marker <> "]"

----------------------------------------------------------------------
-- The answer

-- | 'EnterPolicy' is what Enter does while the list is shown: choose the
-- entry selected at once, or only once the text the entry replaces (the
-- trigger and what was typed after it) is so many characters long, or the
-- reader moved in the list; before that, Enter is the editor's own, a new
-- line. The emoji's list waits so, for two letters after its colon
-- (@web/src/editor.js@); the wiki's own Markdown chooses at once.
data EnterPolicy = EnterChooses | EnterChoosesAfter Int
  deriving stock (Eq, Show)

-- | 'EscapePolicy' is what Escape does while the list is shown: it closes
-- the list, and leaves the text as it is. (The emoji's list also writes a
-- backslash before its colon, in @web/src/editor.js@.)
data EscapePolicy = EscapeCloses
  deriving stock (Eq, Show)

-- | 'Completion' is the answer: where the text the entry chosen replaces
-- starts in the cursor's line (in characters; it ends at the cursor), the
-- candidates, and what Enter and Escape do.
data Completion = Completion
  { from :: Int
  , candidates :: [Candidate]
  , enter :: EnterPolicy
  , escape :: EscapePolicy
  }
  deriving stock (Eq, Show)

-- | The list for a place, from the sources, or 'Nothing' when nothing
-- opens there or no source has a candidate.
complete :: [Source] -> At -> Maybe Completion
complete sources at = do
  typed <- triggerAt at
  let wanted = Wanted {typed, open = openBlocks at.above}
      found = concatMap ($ wanted) sources
  guard (not (null found))
  Just Completion {from = typed.from, candidates = found, enter = EnterChooses, escape = EscapeCloses}

----------------------------------------------------------------------
-- Skeletons

-- | 'Piece' is a part of a skeleton: text written as it is, or a place to
-- fill, by its number and what it holds until it is typed over. The
-- cursor goes to place 1 first, and Tab to 2, 3 and so on; place 0 is
-- where it goes after the last, which ends the skeleton. What a place
-- holds has no brace, which a snippet's place cannot hold.
data Piece = Literal Text | Field Int Text
  deriving stock (Eq, Show)

-- | 'Skeleton' is what choosing a candidate writes.
type Skeleton = [Piece]

-- | A skeleton as CodeMirror's snippet ('snippet' of
-- @\@codemirror\/autocomplete@) reads it: a place is @${n:text}@ or
-- @${n}@, and a brace of the text is written with a backslash before it,
-- which the snippet takes away (@\\{@ for @{@), so that neither @${@ in
-- the text nor a brace of TeX starts a place. A backslash is written as
-- it is: the snippet takes away only one before a brace. Each further
-- line gets the indentation of the cursor's line from the snippet.
snippetTemplate :: Skeleton -> Text
snippetTemplate = foldMap $ \case
  Literal t -> T.concatMap escape t
  Field n t
    | T.null t -> "${" <> show n <> "}"
    | otherwise -> "${" <> show n <> ":" <> t <> "}"
  where
    escape c
      | c == '{' || c == '}' = T.pack ['\\', c]
      | otherwise = T.singleton c

-- | What a skeleton writes, and where each place is in it, in the order
-- of the places' numbers (place 0 last): the text and the ranges the
-- snippet shows, but for the indentation it adds to further lines.
skeletonText :: Skeleton -> (Text, [(Int, (Int, Int))])
skeletonText pieces = (foldMap pieceText pieces, sortOn (order . fst) (go 0 pieces))
  where
    pieceText = \case
      Literal t -> t
      Field _ t -> t
    go _ [] = []
    go at (piece : rest) = case piece of
      Literal t -> go (at + T.length t) rest
      Field n t -> (n, (at, at + T.length t)) : go (at + T.length t) rest
    order n = if n == 0 then maxBound else n

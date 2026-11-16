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
-- * __Where a list opens__ ('triggerAt'): @[[!@ (a start marker) where
--   the wiki's parser ("Wiki.Core.Markdown.Ast") would read a marker on
--   the cursor's line; @[!@ (a kind of box) where it would read a box's
--   marker; @[[/@ (an end marker) where an end marker would end a block.
--   Never in code, HTML, a comment, math or a link's address
--   ('placeOf'). Which line is in which quotation, list's item or code
--   block is CommonMark's to say, and the parser says it: the line is
--   asked about with what would be written on it ('readLine'), rather
--   than its containers read again from its characters.
-- * __What it offers__: candidates, from a list of 'Source's. The wiki's
--   own is 'wikiSource': the markers the page's kind reads, the blocks
--   an end marker at the cursor would end, the kinds of box.
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

    -- * The parser's reading of the cursor's line
  , LineReading (..)
  , LineReader
  , readLine
  , probeLines

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
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (BoxMarker (markerLine), MarkerLine (..), MarkerName (..), MarkerRole (..), closingMarker, holdsBlocks, markerNameText, openingMarker, parseWithAllMarkers)
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

-- | What opens a list at a place, if anything, the parser asked through
-- the given reading of a line put after 'above' ('LineReader').
--
-- The trigger and the letters after it are the last ones the cursor's
-- line ends with ('shapeAt'). A start marker's list opens where the parser
-- reads @[[!toc]]@ written in their place as a marker, and a box's where
-- it reads @[!NOTE]@ as a box's marker: on a line of its own or a
-- quotation's first line, after any container's marks and spaces the
-- parser takes, and not in code, a lazy line of a paragraph, or the middle
-- of a line. An end marker's list is asked for after @[[/@ anywhere; its
-- candidates are the blocks an end marker there ends ('complete').
triggerAt :: LineReader -> At -> Maybe Typed
triggerAt readAt at = do
  guard (at.place == InText)
  shape <- shapeAt at
  case shape.trigger of
    StartMarker -> guard (PlacesContents `elem` (readAt (shape.prefix <> "[[!toc]]")).roles)
    BoxMarker -> guard (readAt (shape.prefix <> "[!NOTE]")).boxed
    EndMarker -> pass
  Just
    Typed
      { trigger = shape.trigger
      , letters = shape.letters
      , from = T.length shape.prefix
      , lineStart = continuation shape.prefix
      }

-- | 'Shape' is a trigger the cursor's line ends with, before the parser is
-- asked about it: the trigger, the letters typed after it, and what of the
-- line comes before it.
data Shape = Shape {trigger :: Trigger, letters :: Text, prefix :: Text}

-- | The trigger the line ends with, followed by letters a name may have:
-- @[[!@ or @[[/@ and lower-case letters, or @[!@ (not the end of @[[!@)
-- and letters or dashes.
shapeAt :: At -> Maybe Shape
shapeAt at =
  asum
    [ shaped StartMarker "[[!" isAsciiLower
    , shaped EndMarker "[[/" isAsciiLower
    , do
        shape <- shaped BoxMarker "[!" (\c -> isAsciiLower c || isAsciiUpper c || c == '-')
        guard (not ("[" `T.isSuffixOf` shape.prefix))
        Just shape
    ]
  where
    shaped trigger written allowed = do
      let (upTo, letters) = T.breakOnEnd written at.before
      guard (not (T.null upTo) && T.all allowed letters)
      Just Shape {trigger, letters, prefix = T.dropEnd (T.length written) upTo}

-- | What each further line of a skeleton starts with, after the
-- indentation of the cursor's line (which CodeMirror's snippet adds): the
-- line's text before the trigger, its own indentation taken away, its
-- quotations' marks and spaces kept, and anything else (a list's item's
-- marker) made spaces, so that the further lines stay in the containers
-- the parser read the cursor's line in. A @>@ with no space after it gets
-- one: the space after a quotation's mark is the mark's own when it is
-- there, so without it the columns after the mark would not line up with
-- the cursor's line. The properties check this against the parser
-- ("Test.Markdown.CompleteOracle").
continuation :: Text -> Text
continuation = go . T.dropWhile isBlank
  where
    go t = case T.uncons t of
      Nothing -> ""
      Just ('>', rest)
        | Just (c, _) <- T.uncons rest, isBlank c -> ">" <> go rest
        | otherwise -> "> " <> go rest
      Just (c, rest)
        | isBlank c -> T.cons c (go rest)
        | otherwise -> " " <> go rest

isBlank :: Char -> Bool
isBlank c = c == ' ' || c == '\t'

----------------------------------------------------------------------
-- The parser's reading of the cursor's line

-- | 'LineReading' is what the wiki's parser reads on a line put after the
-- lines above the cursor: the roles of the marker lines it reads there
-- ('MarkerRole'), and whether a box's marker is there.
data LineReading = LineReading
  { roles :: [MarkerRole]
  , boxed :: Bool
  }
  deriving stock (Eq, Show)

-- | 'LineReader' gives the parser's reading of a line put after the lines
-- above the cursor ('At''s 'above'): 'readLine', or the editor's memo of
-- it, which keeps a reading while the lines above do not change, so that
-- the letters typed after a trigger ask the parser nothing again.
type LineReader = Text -> LineReading

-- | What the parser reads on a line put after these lines: the text
-- above it is parsed whole, since a container or a block opened anywhere
-- above may hold the line. The text below is not, and does not change
-- what the line is.
readLine :: [Text] -> Text -> LineReading
readLine above line = case parseWithAllMarkers (T.intercalate "\n" (above <> [line])) of
  Right (_, boxes, lines', _) -> LineReading {roles = [r | MarkerLine l _ r <- lines', l == n], boxed = any (\b -> b.markerLine == n) boxes}
  Left _ -> LineReading {roles = [], boxed = False}
  where
    n = length above + 1

-- | The lines a list at this place may ask the parser about, for the
-- editor's memo: @[[!toc]]@ or @[!NOTE]@ after the line's text before the
-- trigger, or the end marker of each kind of block whose start marker the
-- lines above hold.
probeLines :: At -> [Text]
probeLines at = case shapeAt at of
  Nothing -> []
  Just shape -> case shape.trigger of
    StartMarker -> [shape.prefix <> "[[!toc]]"]
    BoxMarker -> [shape.prefix <> "[!NOTE]"]
    EndMarker -> [shape.prefix <> closingMarker name | name <- startedAbove at.above]

-- | The kinds of block whose start marker the lines above hold somewhere:
-- the only ones an end marker may end.
startedAbove :: [Text] -> [MarkerName]
startedAbove above = [name | name <- filter holdsBlocks [minBound .. maxBound], any (T.isInfixOf ("[[!" <> markerNameText name)) above]

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

-- | 'Wanted' is what a source is asked for: what was typed, and the kinds
-- of block an end marker at the cursor would end, for the parser (the
-- innermost block open there, if one is: an end marker of another name
-- ends nothing).
data Wanted = Wanted
  { typed :: Typed
  , closable :: [MarkerName]
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
-- * after @[[/@, the blocks an end marker at the cursor would end;
-- * after @[!@, GitHub's five kinds of box and the defined ones
--   ('boxChoices').
--
-- Each one whose name starts with the letters typed, whatever their case.
wikiSource :: PageKind -> [BoxKind] -> Source
wikiSource kind defined wanted = case t.trigger of
  StartMarker -> [startMarker t.lineStart name | name <- markersRead kind, matches (markerNameText name)]
  EndMarker -> [endMarker name | name <- wanted.closable, matches (markerNameText name)]
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
complete :: [Source] -> LineReader -> At -> Maybe Completion
complete sources readAt at = do
  typed <- triggerAt readAt at
  let closable = case typed.trigger of
        EndMarker ->
          let prefix = T.take typed.from at.before
           in [name | name <- startedAbove at.above, any isEnd (readAt (prefix <> closingMarker name)).roles]
        _ -> []
      wanted = Wanted {typed, closable}
      found = concatMap ($ wanted) sources
  guard (not (null found))
  Just Completion {from = typed.from, candidates = found, enter = EnterChooses, escape = EscapeCloses}
  where
    isEnd = \case
      Ends _ -> True
      _ -> False

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

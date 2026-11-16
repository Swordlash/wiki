-- | 'Complete' is the editor's completion of the wiki's own Markdown
-- (WP5.124 (b), (c) and (d) of docs/PLAN.md, #187): what a list opens
-- after @[[!@, @[[/@, @[!@, @[[@ and @/@, what it offers, and what
-- choosing an entry writes.
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
--   marker; @[[/@ (an end marker) where an end marker would end a block;
--   @[[@ (a link to a page) anywhere a wiki link is read; @/@ (what the
--   toolbar writes) where the word it starts is read as text, a @/@ in a
--   link's address being the address's. Never in code,
--   HTML, a comment, math or a link's address ('placeOf'). Which line is
--   in which quotation, list's item or code block is CommonMark's to say,
--   and the parser says it: the line is asked about with what would be
--   written on it ('readLine'), rather than its containers read again
--   from its characters.
-- * __What it offers__: candidates, from a list of 'Source's. The wiki's
--   own is 'wikiSource': the markers the page's kind reads, the blocks
--   an end marker at the cursor would end, the kinds of box; the pages
--   are the editor's own source, and so are the toolbar's entries
--   ('slashSource').
--   A plugin will give a source of its own (#186), whose candidates say
--   where they come from ('Origin'), shown grey beside each one.
-- * __What choosing writes__: a 'Skeleton', literal text and numbered
--   places to fill, written as a CodeMirror snippet ('snippetTemplate'):
--   the cursor goes to the first place, and Tab to the next ('Writes'
--   says in place of what, and when the text is written as it is).
-- * __What Enter and Escape do__ in the list ('EnterPolicy',
--   'EscapePolicy'), carried by the answer.
module Wiki.Core.Markdown.Complete
  ( -- * Where a list opens
    Place (..)
  , placeOf
  , At (..)
  , Trigger (..)
  , Typed (..)
  , LabelForm (..)
  , triggerAt
  , besidesLink

    -- * The parser's reading of the cursor's line
  , Probe (..)
  , LineReading (..)
  , firstRoles
  , LineReader
  , readLine
  , firstProbes
  , probesOf

    -- * Candidates
  , Origin (..)
  , originText
  , Candidate (..)
  , Writes (..)
  , candidateDetail
  , Wanted (..)
  , Source
  , wikiSource
  , markersRead

    -- * The / menu
  , SlashEntry (..)
  , SlashDoes (..)
  , slashSource
  , furtherPlaces

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
import Wiki.Core.Markdown.Ast (Block (..), BoxHead (..), BoxMarker (markerLine), Footnote (..), Inline (..), ListItem (..), MarkerLine (..), MarkerName (..), MarkerRole (..), allInlines, closingMarker, holdsBlocks, markerNameText, openingMarker, parseWithStartsAndMarkers, plainText)
import Wiki.Core.Markdown.Ast qualified as Ast
import Wiki.Core.Markdown.Box (BoxKind, boxChoices)
import Wiki.Core.Markdown.Edit (Command, Selection (..), apply, cursor)
import Wiki.Core.Markdown.Edit qualified as Edit
import Wiki.Core.Markdown.Problems (everyBlock)
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
-- text above the cursor's line (in order), the cursor's line up to the
-- cursor and after it, and the lines below it (in order; read only as
-- far as a link to a page is looked for, 'afterBound').
data At = At
  { place :: Place
  , above :: [Text]
  , before :: Text
  , after :: Text
  , below :: [Text]
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
  | -- | @[[@ anywhere a wiki link is read, not followed by @!@ or @/@: a
    -- link to a page.
    LinkToPage
  | -- | @/@ at a line's start or after a space, where the parser reads the
    -- word it starts as text: what the toolbar writes.
    SlashMenu
  deriving stock (Eq, Show)

-- | 'Typed' is what was typed: the trigger, the letters after it, where
-- the trigger starts in the cursor's line (in characters), which the
-- entry chosen replaces up to the cursor, and what each further line of
-- a skeleton starts with ('lineStart'). After the cursor, the entry
-- replaces 'through' characters too, and writes 'kept' back after its own
-- text: for a link to a page that the parser reads closed after the
-- cursor already, the rest of its address and its @]]@, or its
-- @|label]]@, which stays the reader's ('linkAt'). Both are none for the
-- other triggers.
data Typed = Typed
  { trigger :: Trigger
  , letters :: Text
  , from :: Int
  , lineStart :: Text
  , through :: Int
  , kept :: Text
  , labels :: LabelForm
  }
  deriving stock (Eq, Show)

-- | 'LabelForm' is how a link to a page written at the cursor may hold a
-- label of its own, as the parser reads the shapes there ('linkAt'): after
-- a bar; after a bar escaped with a backslash, its own bars so too (in a
-- table's cell, whose bars the table takes otherwise); or not at all,
-- the link written with its address alone.
data LabelForm = PlainLabel | EscapedLabel | NoLabel
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
  case shapeAt at of
    Just shape -> do
      case shape.trigger of
        StartMarker -> guard (PlacesContents `elem` firstRoles (readAt (Probe 1 [shape.prefix <> "[[!toc]]"])))
        BoxMarker -> guard (readAt (Probe 1 [shape.prefix <> "[!NOTE]"])).boxed
        _ -> pass
      Just
        Typed
          { trigger = shape.trigger
          , letters = shape.letters
          , from = T.length shape.prefix
          , lineStart = continuation shape.prefix
          , through = 0
          , kept = ""
          , labels = PlainLabel
          }
    Nothing -> pageLink readAt at <|> slash readAt at

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

-- | The last @[[@ before the cursor, and what was typed after it, where
-- the wiki's parser reads a wiki link, and reads the cursor in its
-- address. The line's text before the @[[@, then @[[@, a word and @]]@,
-- is read with a wiki link to that word ('pageLinkProbe'): so a @[[@
-- escaped by a backslash, or in code or math the parser reads on the
-- line, opens nothing. The same with what was typed between the @[[@ and
-- the word is read with a link whose address ends with the word: so a
-- @|@ typed (the cursor in a label) or a @]@ (the link ended) opens
-- nothing. @[[!@ and @[[/@ are markers, or an address from the top of
-- the wiki, which no list completes. What of a link the entry replaces
-- after the cursor is the parser's to say too ('linkAt'), and where it
-- cannot say, no list opens.
pageLink :: LineReader -> At -> Maybe Typed
pageLink readAt at = do
  (start, letters) <- pageLinkShape at
  let first' = readAt (pageLinkProbe at start)
      pre = T.take start at.before
  guard (linkWord `elem` map fst first'.wikiLinks)
  guard (any (T.isSuffixOf linkWord . fst) (readAt (Probe first'.enclosing (T.splitOn "\n" (pre <> "[[" <> letters <> linkWord <> "]]" <> fst (stretchOf at))))).wikiLinks)
  (through, kept, labels) <- fst (linkAt readAt at start first'.enclosing)
  Just Typed {trigger = LinkToPage, letters, from = start, lineStart = "", through, kept, labels}

-- | Where the last @[[@ before the cursor starts, and what was typed after
-- it, if it is no marker's start and no address from the top.
pageLinkShape :: At -> Maybe (Int, Text)
pageLinkShape at = do
  let (upTo, letters) = T.breakOnEnd "[[" at.before
      start = T.length upTo - 2
  guard (start >= 0)
  guard (not (T.isPrefixOf "!" letters || T.isPrefixOf "/" letters))
  Just (start, letters)

-- | The cursor's line with a wiki link to 'linkWord' in place of the
-- @[[@ and what follows it.
pageLinkProbe :: At -> Int -> Probe
pageLinkProbe at start = Probe 1 (T.splitOn "\n" (T.take start at.before <> "[[" <> linkWord <> "]]" <> fst (stretchOf at)))

-- | The word 'pageLinkProbe' links to.
linkWord :: Text
linkWord = "Zqxjvlink"

-- | How many characters after the cursor the end of a link to a page is
-- looked for in: the rest of the cursor's line and the lines below, to
-- the text's end at most, so that an answer parses a bounded text. Where
-- the block that holds the cursor is still open at the bound, no list
-- opens ('linkAt'); where it ends before, the text after it is not
-- needed.
afterBound :: Int
afterBound = 2000

-- | The text after the cursor that 'linkAt' reads: the rest of the
-- cursor's line and the lines below, at most 'afterBound' characters,
-- and whether the text goes on after that.
stretchOf :: At -> (Text, Bool)
stretchOf at = go (T.length at.after) [at.after] at.below
  where
    go n taken rest
      | n > afterBound = (T.take afterBound (T.intercalate "\n" (reverse taken)), True)
      | otherwise = case rest of
          [] -> (T.intercalate "\n" (reverse taken), False)
          l : ls -> go (n + 1 + T.length l) (l : taken) ls

-- | What of the link at the cursor the reader had written after the
-- cursor already, as (how many characters an entry replaces after the
-- cursor, line breaks counted, and what it writes back of them), and the
-- probes asked for it. The parser is asked about the block that holds
-- the cursor's line, the text after the cursor to 'afterBound' with it,
-- with 'linkWord' in place of the letters typed: the link at the cursor
-- is the wiki link it reads starting at the @[[@, and the parser says
-- where it ends ('WikiLinkTo'), whatever lines it runs over.
--
-- * A link with no label (its text is its address, the word in both) is
--   replaced to its end, and the entry writes its own end: (n, \"\").
-- * A link with a label of its own keeps it as the reader wrote it: the
--   end written back starts at one of the first bars of the link after
--   the cursor (or at the backslash before one, as a table's cell
--   escapes it), the first with which the parser reads the same text.
-- * When the parser reads no link there, the link has no end yet: nothing
--   after the cursor is replaced, (0, \"\"). Nor when the text without
--   the @[[@ is read with a link that starts inside the link at the
--   cursor: the @[[@ typed took that link's end as its own (@[[On‸ and
--   [[b]]@ is one link for the parser).
--
-- Whatever is to be written, the parser reads it first, in each shape it
-- may take, with stand-in words of that shape (a word as the address, and
-- for a label a word with a bar and character references): a link at the
-- @[[@ with the address and the text written, and every other inline of
-- the block where it was ('besides'), or, for a link with no end yet, one
-- more wiki link than the block without the @[[@ has. A label kept is
-- read so as it is. An end of the entry's own is read with the address
-- alone first, which must be read so, and with a label after a bar and
-- after an escaped bar ('LabelForm'): the first read so is how a label is
-- written, and with neither, an entry writes its address alone, which
-- leads to the same page. Otherwise 'Nothing', and no list: an entry never
-- leaves a text the parser reads otherwise. Nor when the text after the
-- cursor was cut at 'afterBound' and the block that holds the cursor is
-- still open there (the parser reads no block after it, at any depth):
-- what follows could change how it is read. A block
-- that ends before the cut is read as the whole text reads it, however
-- long the text. The parser reads the block two times,
-- then at most 8 times for a label kept, or 3 for an end of the entry's
-- own.
linkAt :: LineReader -> At -> Int -> Int -> (Maybe (Int, Text, LabelForm), [Probe])
linkAt readAt at start from'
  | cut && not settled = (Nothing, [opened])
  | otherwise = case linkFrom opened of
      Just (through, _, text)
        | not (takenOver through) ->
            if linkWord `T.isInfixOf` text
              then ownEnd through
              else firstRead [(through, T.drop i written) | let { written = T.take through stretch }, i <- labelStarts written] text
      _ -> ownEnd 0
  where
    pre = T.take start at.before
    (stretch, cut) = stretchOf at
    -- Whether the block that holds the link ends before the cut: the
    -- parser reads a block after it, at any depth ('followed'), and what
    -- follows the cut cannot change how it is read. (A block of a long
    -- list or quotation is followed by the next, inside the same
    -- top-level block.)
    settled = followed holdsLink (readAt opened).blocksRead
    probeOn t = Probe from' (T.splitOn "\n" (pre <> t))
    opened = probeOn ("[[" <> linkWord <> stretch)
    unopened = probeOn (linkWord <> stretch)
    column = columnAfter pre
    -- Where the cursor is in the text of 'opened', and of 'unopened'.
    cursorAt = start + 2 + T.length linkWord
    unopenedAt = start + T.length linkWord
    -- The wiki link the parser reads at the @[[@ of a probe: how far
    -- after the cursor it ends (in characters), its address and its
    -- text.
    linkFrom probe = do
      let reading = readAt probe
      (address, text) <- find (T.isPrefixOf linkWord . fst) reading.wikiLinks
      ends <- head [e | ((0, c), e) <- reading.linkSpans, c == column]
      endAt <- offsetIn probe.written ends
      guard (endAt >= cursorAt)
      Just (endAt - cursorAt, address, text)
    -- Whether the text without the @[[@ has a link that starts after the
    -- cursor and before the end of the link at the cursor.
    takenOver through =
      any
        (\o -> o >= unopenedAt && o < unopenedAt + through)
        (mapMaybe (offsetIn unopened.written . fst) (readAt unopened).linkSpans)
    -- Where a label's end may start in what the link holds after the
    -- cursor: at each of its first bars, and before that at the
    -- backslash escaping it, if one does.
    labelStarts written =
      concat
        [ [j - 1 | j > 0, T.index written (j - 1) == '\\'] <> [j]
        | j <- take 4 [k | (k, c) <- zip [0 ..] (T.unpack written), c == '|']
        ]
    -- The first of these ends kept that the parser reads as it should,
    -- with the text given, and the probes asked.
    firstRead tries text = go tries []
      where
        go [] asked = (Nothing, [opened, unopened] <> reverse asked)
        go ((through, kept) : more) asked =
          let probe = probeOn ("[[" <> linkWord <> kept <> T.drop through stretch)
           in if readsAs probe through text then (Just (through, kept, PlainLabel), [opened, unopened] <> reverse (probe : asked)) else go more (probe : asked)
    -- An end of the entry's own, in place of what of the link follows the
    -- cursor: the address alone, then with a label after a bar, then
    -- after an escaped bar.
    ownEnd through =
      let rest = T.drop through stretch
          bare = probeOn ("[[" <> linkWord <> "]]" <> rest)
          plain = probeOn ("[[" <> linkWord <> "|" <> labelWord <> "]]" <> rest)
          escaped = probeOn ("[[" <> linkWord <> "\\|" <> T.replace "|" "\\|" labelWord <> "]]" <> rest)
          form
            | readsAs plain through labelRead = PlainLabel
            | readsAs escaped through labelRead = EscapedLabel
            | otherwise = NoLabel
       in ( if readsAs bare through linkWord then Just (through, "", form) else Nothing
          , [opened, unopened, bare, plain, escaped]
          )
    -- What a probe written so must read: a link to the word at the @[[@,
    -- with this text; and every other inline of the block where it was,
    -- or, for a link with no end yet, one more wiki link than without the
    -- @[[@.
    readsAs probe through text =
      fmap (\(_, a, t) -> (a, t)) (linkFrom probe) == Just (linkWord, text)
        && if through == 0
          then length (readAt probe).wikiLinks == length (readAt unopened).wikiLinks + 1
          else besides (readAt probe).blocksRead == besides (readAt opened).blocksRead

-- | The label 'linkAt' writes to read how a label is written at the
-- cursor: a word with a bar and the character references a label of
-- 'Wiki.App.Route.pageLinkText' holds; and its text as the parser reads
-- it.
labelWord, labelRead :: Text
labelWord = "Zqxjv label | &amp;&#93;"
labelRead = "Zqxjv label | &]"

-- | The column (from 1) just after a text on its line, as the parser
-- counts it: a tab goes on to the next column of four.
columnAfter :: Text -> Int
columnAfter = T.foldl' (\c ch -> if ch == '\t' then c + 4 - (c - 1) `mod` 4 else c + 1) 1

-- | The character a line and a column of a probe's lines are at (the
-- first written line is line 0), counted from the start of those lines,
-- if the column is one the parser counts on that line ('columnAfter').
offsetIn :: [Text] -> (Int, Int) -> Maybe Int
offsetIn written (line, column) = do
  guard (line >= 0)
  text <- atMay written line
  k <- go 0 1 (T.unpack text)
  Just (sum (map ((+ 1) . T.length) (take line written)) + k)
  where
    go k c chars
      | c == column = Just k
      | c > column = Nothing
      | otherwise = case chars of
          [] -> Nothing
          ch : more -> go (k + 1 :: Int) (if ch == '\t' then c + 4 - (c - 1) `mod` 4 else c + 1) more

-- | The blocks read, the wiki link to 'linkWord' (and what it holds) a
-- mark where it stands ('besidesLink').
besides :: [Block] -> [Block]
besides = besidesLink (T.isPrefixOf linkWord)

-- | Blocks with each wiki link whose address is one of these (and what
-- it holds) a mark where it stands, inside whatever holds it: emphasis, a
-- heading, a table's cell, a list's item, a quotation, a box, a spoiler,
-- a footnote; and a heading's identifier, which is made from its text,
-- and a spoiler's start marker as written, left out. Two texts whose blocks are alike so differ in those links alone.
besidesLink :: (Text -> Bool) -> [Block] -> [Block]
besidesLink isIt = map onBlock
  where
    onBlock = \case
      Para xs -> Para (onInlines xs)
      Plain xs -> Plain (onInlines xs)
      -- A heading's identifier is made from its text, the link's with it.
      Heading n attributes xs -> Heading n (filter ((/= "id") . fst) attributes) (onInlines xs)
      BlockQuote bs -> BlockQuote (onBlocks bs)
      List kind items -> List kind [ListItem task (onBlocks bs) | ListItem task bs <- items]
      Table aligns header rows -> Table aligns (map onInlines header) (map (map onInlines) rows)
      Div attributes bs -> Div attributes (onBlocks bs)
      Alert kind boxHead bs -> Alert kind boxHead {title = onInlines <$> boxHead.title} (onBlocks bs)
      -- A spoiler's start marker as written holds the link's text.
      Spoiler spoilerHead bs -> Spoiler spoilerHead {Ast.description = onInlines spoilerHead.description, Ast.marker = ""} (onBlocks bs)
      Footnotes notes -> Footnotes [Footnote n label (onBlocks bs) | Footnote n label bs <- notes]
      other -> other
    onBlocks = besidesLink isIt
    onInlines = map $ \case
      Link attributes address _ _
        | ("class", "wikilink") `elem` attributes
        , isIt address ->
            Str "\0"
      Emph xs -> Emph (onInlines xs)
      Strong xs -> Strong (onInlines xs)
      Strikeout xs -> Strikeout (onInlines xs)
      Hidden xs -> Hidden (onInlines xs)
      Link attributes address title xs -> Link attributes address title (onInlines xs)
      Image attributes address title xs -> Image attributes address title (onInlines xs)
      Marked attributes xs -> Marked attributes (onInlines xs)
      other -> other

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

-- | 'Probe' is text the parser is asked about: lines put after the lines
-- above the cursor from one of them on (from 1, all of them; or from the
-- first line of the block that holds the cursor's line, 'enclosing'),
-- the first of them in the cursor's line's place.
data Probe = Probe
  { from :: Int
  , written :: [Text]
  }
  deriving stock (Eq, Ord, Show)

-- | 'LineReading' is what the wiki's parser reads on a probe's lines: the
-- roles of the marker lines it reads on each of them ('MarkerRole');
-- whether a box's marker is on the first; the text of the paragraphs
-- inside the boxes it reads; the wiki links it reads, each by its
-- address and its text, and where each starts and ends, as (line,
-- column) pairs, the probe's first line counted as line 0 ('WikiLinkTo');
-- every inline it reads ('allInlines'); and the first line of the
-- top-level block that holds the first (counted in the lines above, from
-- 1), from which on the lines read alike whatever comes before.
data LineReading = LineReading
  { roles :: [[MarkerRole]]
  , boxed :: Bool
  , inBoxes :: [Text]
  , wikiLinks :: [(Text, Text)]
  , linkSpans :: [((Int, Int), (Int, Int))]
  , inlinesRead :: [Inline]
  , blocksRead :: [Block]
  , enclosing :: Int
  }
  deriving stock (Eq, Show)

-- | A reading of nothing, the block that holds the line starting at this
-- line.
noReading :: Int -> LineReading
noReading enclosing = LineReading {roles = [], boxed = False, inBoxes = [], wikiLinks = [], linkSpans = [], inlinesRead = [], blocksRead = [], enclosing}

-- | The roles read on a probe's first line.
firstRoles :: LineReading -> [MarkerRole]
firstRoles r = fromMaybe [] (head r.roles)

-- | 'LineReader' gives the parser's reading of a probe ('readLine'), or
-- the editor's memo of it, which keeps a reading while the lines above do
-- not change, so that the letters typed after a trigger ask the parser
-- nothing again.
type LineReader = Probe -> LineReading

-- | What the parser reads on a probe's lines, put after these lines from
-- the probe's line on. From the first line, the text above is parsed
-- whole, since a container or a block opened anywhere above may hold the
-- cursor's line; from the block that holds it, only that block, which
-- reads alike whatever comes before it. The text below is not parsed,
-- and does not change what the lines are.
readLine :: [Text] -> Probe -> LineReading
readLine above probe = case parseWithStartsAndMarkers (T.intercalate "\n" (kept <> probe.written)) of
  Right (blocks, boxes, lines') ->
    LineReading
      { roles = [[r | MarkerLine l _ r <- lines', l == n + k] | k <- [0 .. length probe.written - 1]]
      , boxed = any (\b -> b.markerLine == n) boxes
      , inBoxes = [plainText inlines | Alert _ _ bs <- concatMap (everyBlock . snd) blocks, b <- concatMap everyBlock bs, Just inlines <- [paragraphOf b]]
      , wikiLinks = [(address, plainText text) | Link attributes address _ text <- inlines', ("class", "wikilink") `elem` attributes]
      , linkSpans = [((l - n, c), (l' - n, c')) | MarkerLine l c (WikiLinkTo l' c') <- lines']
      , inlinesRead = inlines'
      , blocksRead = map snd blocks
      , enclosing = inAbove (fromMaybe n (lastMay [l | (Just l, _) <- blocks, l <= n]))
      }
    where
      inlines' = allInlines (map snd blocks)
  Left _ -> noReading (inAbove n)
  where
    kept = drop (probe.from - 1) above
    n = length kept + 1
    inAbove l = l + probe.from - 1
    paragraphOf = \case
      Para inlines -> Just inlines
      Plain inlines -> Just inlines
      _ -> Nothing

-- | The probes a list at this place may ask the parser about, for the
-- editor's memo: @[[!toc]]@ or @[!NOTE]@ after the line's text before the
-- trigger, the end marker of each kind of block whose start marker the
-- lines above hold, and, from the block that holds the cursor's line (as
-- the first probe reads it), each skeleton a marker or a box writes
-- ('writtenProbe').
probesOf :: LineReader -> At -> [Probe]
probesOf readAt at = firstProbes at <> written
  where
    written = case (shapeAt at, firstProbes at) of
      (Just shape, first' : _) -> case shape.trigger of
        StartMarker -> [writtenProbe at shape (enclosingOf first') (startMarker (continuation shape.prefix) name).skeleton | name <- filter holdsBlocks [minBound .. maxBound]]
        BoxMarker -> [writtenProbe at shape (enclosingOf first') (boxed' (box (continuation shape.prefix) "NOTE"))]
        _ -> []
      (Nothing, first' : _)
        | Just (start, letters) <- pageLinkShape at ->
            -- The line with the letters typed, then what 'linkAt' asks,
            -- which does not change as letters are typed.
            Probe (enclosingOf first') (T.splitOn "\n" (T.take start at.before <> "[[" <> letters <> linkWord <> "]]" <> fst (stretchOf at))) : snd (linkAt readAt at start (enclosingOf first'))
      _ -> []
    enclosingOf = (.enclosing) . readAt

-- | The probes a list at this place asks about first, which no other
-- reading decides: @[[!toc]]@ or @[!NOTE]@ after the line's text before
-- the trigger, or the end marker of each kind of block whose start marker
-- the lines above hold, each after all the lines above.
firstProbes :: At -> [Probe]
firstProbes at = case shapeAt at of
  Nothing -> [pageLinkProbe at start | Just (start, _) <- [pageLinkShape at]] <> [slashProbe at prefix | Just (prefix, _) <- [slashShape at]]
  Just shape -> case shape.trigger of
    EndMarker -> [Probe 1 [shape.prefix <> closingMarker name] | name <- startedAbove at.above]
    trigger -> [Probe 1 [shape.prefix <> probeOf trigger]]

-- | A skeleton as written at the cursor, as a probe from a line: its first
-- line in place of the trigger and the letters, and each further line
-- after the cursor line's indentation, as CodeMirror's snippet writes it.
writtenProbe :: At -> Shape -> Int -> Skeleton -> Probe
writtenProbe at shape from' skeleton = Probe from' lines'
  where
    indentation = T.takeWhile isBlank at.before
    lines' = case T.splitOn "\n" (fst (skeletonText skeleton)) of
      first' : rest -> (shape.prefix <> first') : map (indentation <>) rest
      [] -> []

-- | A box's skeleton with a word on its line of text, which the parser,
-- reading the box, finds inside it ('inBoxes').
boxed' :: Candidate -> Skeleton
boxed' c = c.skeleton <> [Literal boxWord]

-- | The word 'boxed'' writes.
boxWord :: Text
boxWord = "Zqxjvbox"

-- | What the first probe of a trigger writes in its place.
probeOf :: Trigger -> Text
probeOf = \case
  StartMarker -> "[[!toc]]"
  BoxMarker -> "[!NOTE]"
  EndMarker -> "[[/div]]"
  -- A shape is never a link to a page ('pageLink') nor a @/@ ('slash').
  LinkToPage -> ""
  SlashMenu -> ""

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

-- | What the list shows in grey beside a candidate's label: its note
-- (where what it names is, or what it writes), and where it comes from,
-- as many as it has, parted by a middle dot (@team\/on-call · from
-- plugin calendar@).
candidateDetail :: Candidate -> Maybe Text
candidateDetail c = case catMaybes [c.note, originText <$> c.origin] of
  [] -> Nothing
  parts -> Just (T.intercalate " · " parts)

-- | 'Candidate' is an entry of the list: what it shows; what choosing it
-- writes, and in place of what ('Writes'); what the editor does then, if
-- anything ('action': @link-page@ opens the dialog of a link to a page,
-- @attach@ the choice of a file to upload); a note, which says where what
-- it names is, if that tells two entries of one label apart (a page's
-- path), or what it writes, in a few characters (@**bold**@); and where it
-- comes from ('Nothing' for the wiki's own). The list shows the last two
-- in grey beside the label, the note first ('candidateDetail').
data Candidate = Candidate
  { label :: Text
  , skeleton :: Skeleton
  , writing :: Writes
  , action :: Maybe Text
  , note :: Maybe Text
  , origin :: Maybe Origin
  }
  deriving stock (Eq, Show)

-- | 'Writes' is how a candidate's skeleton is written: as CodeMirror's
-- snippet in place of what was typed ('Completion''s range), the cursor at
-- its first place; or as it is there, the cursor after it, for one with no
-- place to fill whose further lines must not get the cursor line's
-- indentation, which a snippet puts before them (a link to a page whose
-- label, kept, runs over lines); as a snippet in place of the cursor's
-- whole line, which read as the text given when the list was made (an
-- entry of the @/@ menu, whose command may change the line's start and
-- write lines after it); or so, but written as it is, its first place
-- selected and no other, where a snippet would not write it as it is (its
-- places hold a line break or a brace, or its further lines would get the
-- cursor line's indentation).
data Writes = AsSnippet | AsText | LineAsSnippet Text | LineAsText Text
  deriving stock (Eq, Show)

-- | 'Wanted' is what a source is asked for: where (the cursor's place and
-- lines); what was typed; the kinds of block an end marker at the cursor
-- would end, for the parser (the innermost block open there, if one is:
-- an end marker of another name ends nothing); and what the parser reads
-- of a skeleton written at the cursor, so that a source offers what is
-- read as it says.
data Wanted = Wanted
  { at :: At
  , typed :: Typed
  , closable :: [MarkerName]
  , writes :: Skeleton -> LineReading
  }

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
  StartMarker -> [checkedStart name | name <- markersRead kind, matches (markerNameText name)]
  EndMarker -> [endMarker name | name <- wanted.closable, matches (markerNameText name)]
  BoxMarker -> [if bodyRead then box t.lineStart marker else boxAlone marker | (marker, _) <- boxChoices defined, matches marker]
  -- The pages are the editor's to know ("Wiki.App.Editor.Complete"), and
  -- what the toolbar writes is its own ('slashSource').
  LinkToPage -> []
  SlashMenu -> []
  where
    t = wanted.typed
    matches name = T.toLower t.letters `T.isPrefixOf` T.toLower name
    -- A block's skeleton is offered where the parser reads it, written,
    -- as one block: its start marker opening it and its end marker
    -- ending it. Elsewhere (a tab after a list's mark, a footnote's
    -- indentation, anything the lines after the first do not keep the
    -- containers of) its start marker is written alone, on its line,
    -- where the parser reads a marker; its end is the author's.
    checkedStart name
      | holdsBlocks name =
          let full = startMarker t.lineStart name
              reading = wanted.writes full.skeleton
              ends = any isEnd (fromMaybe [] (lastMay reading.roles))
           in if any isStart (firstRoles reading) && ends then full else startAlone name
      | otherwise = startMarker t.lineStart name
    -- A box's skeleton with its line of text is offered where the parser
    -- reads that line inside the box; elsewhere its marker alone.
    bodyRead =
      let reading = wanted.writes (boxed' (box t.lineStart "NOTE"))
       in reading.boxed && any (T.isInfixOf boxWord) reading.inBoxes
    isStart = \case
      Opens _ -> True
      Starts _ _ -> True
      _ -> False
    isEnd = \case
      Ends _ -> True
      _ -> False

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
startMarker lineStart name = Candidate {label = openingMarker name, skeleton, note = Nothing, writing = AsSnippet, action = Nothing, origin = Nothing}
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

-- | A start marker's candidate written alone on its line, with no end
-- marker: a spoiler with its name and description to fill.
startAlone :: MarkerName -> Candidate
startAlone name = Candidate {label = openingMarker name, skeleton, note = Nothing, writing = AsSnippet, action = Nothing, origin = Nothing}
  where
    skeleton = case name of
      SpoilerName -> [Literal (openingMarker name <> "{"), Field 1 "name", Literal "}(", Field 2 "Description", Literal ")", Field 0 ""]
      _ -> [Literal (openingMarker name), Field 0 ""]

-- | A kind of box's candidate written alone, its marker only.
boxAlone :: Text -> Candidate
boxAlone marker = Candidate {label = written, skeleton = [Literal written, Field 0 ""], note = Nothing, writing = AsSnippet, action = Nothing, origin = Nothing}
  where
    written = "[!" <> marker <> "]"

-- | An end marker's candidate.
endMarker :: MarkerName -> Candidate
endMarker name = Candidate {label = closingMarker name, skeleton = [Literal (closingMarker name), Field 0 ""], note = Nothing, writing = AsSnippet, action = Nothing, origin = Nothing}

-- | A kind of box's candidate: its marker (the kind's name in capitals,
-- as 'boxChoices' gives it), and the box's first line of text under it,
-- the cursor there.
box :: Text -> Text -> Candidate
box lineStart marker = Candidate {label = written, skeleton = [Literal (written <> "\n" <> lineStart), Field 1 ""], note = Nothing, writing = AsSnippet, action = Nothing, origin = Nothing}
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
-- the list, and leaves the text as it is; and the list of a @/@ also
-- leaves that @/@ alone, so that the letters typed after it do not open
-- it again (as the emoji's list writes a backslash before its colon, in
-- @web/src/editor.js@), until the @/@ is typed anew.
data EscapePolicy = EscapeCloses | EscapeDismisses
  deriving stock (Eq, Show)

-- | 'Completion' is the answer: where the text the entry chosen replaces
-- starts in the cursor's line (in characters), how many characters after
-- the cursor it replaces too ('Typed''s 'through'), the candidates, and
-- what Enter and Escape do.
data Completion = Completion
  { from :: Int
  , through :: Int
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
           in [name | name <- startedAbove at.above, any isEnd (firstRoles (readAt (Probe 1 [prefix <> closingMarker name])))]
        _ -> []
      writes skeleton = case shapeAt at of
        Just shape -> readAt (writtenProbe at shape (readAt (Probe 1 [shape.prefix <> probeOf shape.trigger])).enclosing skeleton)
        Nothing -> noReading 1
      wanted = Wanted {at, typed, closable, writes}
      found = concatMap ($ wanted) sources
  guard (not (null found))
  Just Completion {from = typed.from, through = typed.through, candidates = found, enter = EnterChooses, escape = if typed.trigger == SlashMenu then EscapeDismisses else EscapeCloses}
  where
    isEnd = \case
      Ends _ -> True
      _ -> False

----------------------------------------------------------------------
-- The / menu

-- | The @/@ before the cursor and the letters typed after it, where the
-- @/@ starts a line or follows a space or a tab and nothing but spaces
-- follows the cursor on the line, as the line's text before the @/@ and
-- the letters. Whether it opens a list there is the parser's to say
-- ('slash').
slashShape :: At -> Maybe (Text, Text)
slashShape at = do
  let (upTo, letters) = T.breakOnEnd "/" at.before
  prefix <- T.stripSuffix "/" upTo
  guard (T.all (\c -> isAsciiLower c || isAsciiUpper c) letters)
  guard (maybe True (isBlank . snd) (T.unsnoc prefix))
  guard (T.all isBlank at.after)
  Just (prefix, letters)

-- | The @/@ menu opens only in the plain place where text is written: a
-- word of its own at the end of a line of a paragraph, the paragraph at
-- the top of the page, in a list's item, a quotation, a box's text, or a
-- block of the wiki's own (a @div@, a spoiler's body); an empty line is
-- such a paragraph. The parser says so: a word written in place of the
-- @/@ and the letters is read as text of such a paragraph, and of no
-- inline in it ('plainWords').
--
-- No list, then, anywhere else, without exception:
--
-- * with anything but spaces after the cursor on its line;
-- * inside a word (@and\/or@) or an address (@https:\/\/…@);
-- * in code (inline or a block, fenced or indented), math or HTML;
-- * in a link's text or its address, an image's, emphasis, strong or
--   struck text, an inline spoiler, or any other inline;
-- * in a heading, a table's cell, a box's title, a spoiler's description,
--   a marker line or a footnote's definition;
-- * where the block that holds the cursor is still open where the text
--   read after the cursor is cut ('afterBound'), since what follows could
--   make the line something else.
slash :: LineReader -> At -> Maybe Typed
slash readAt at = do
  (prefix, letters) <- slashShape at
  let reading = readAt (slashProbe at prefix)
  guard (slashWord `elem` plainWords reading.blocksRead)
  guard (not (snd (stretchOf at)) || followed holdsSlash reading.blocksRead)
  Just Typed {trigger = SlashMenu, letters, from = T.length prefix, lineStart = "", through = 0, kept = "", labels = PlainLabel}

-- | The cursor's line with 'slashWord' in place of the @/@ and the letters,
-- and the lines after it that 'linkAt' reads, after the lines above.
slashProbe :: At -> Text -> Probe
slashProbe at prefix = Probe 1 (T.splitOn "\n" (prefix <> slashWord <> fst (stretchOf at)))

-- | The word 'slashProbe' writes.
slashWord :: Text
slashWord = "Zqxjvslash"

-- | Whether the parser reads a block after the block that holds a word
-- (its own text, not a block inside it), at any depth, in the order of
-- the text (the footnotes, which it reads last, aside): that block then
-- ends within the text read, and what follows the cut cannot change it.
-- The word is 'slashWord' in a paragraph ('slash'), or 'linkWord' as a
-- wiki link's address in a paragraph, a heading, a table, a box's title
-- or a spoiler's description ('linkAt').
followed :: (Block -> Bool) -> [Block] -> Bool
followed holdsWord blocks = case break holdsWord (concatMap inOrder [b | b <- blocks, not (isFootnotes b)]) of
  (_, _ : _ : _) -> True
  _ -> False
  where
    inOrder b =
      b : case b of
        BlockQuote bs -> concatMap inOrder bs
        List _ items -> concat [concatMap inOrder bs | ListItem _ bs <- items]
        Alert _ _ bs -> concatMap inOrder bs
        Div _ bs -> concatMap inOrder bs
        Spoiler _ bs -> concatMap inOrder bs
        _ -> []
    isFootnotes = \case
      Footnotes _ -> True
      _ -> False

-- | Whether a paragraph's own text holds 'slashWord'.
holdsSlash :: Block -> Bool
holdsSlash = \case
  Para xs -> slashWord `elem` concat [T.words t | Str t <- xs]
  Plain xs -> slashWord `elem` concat [T.words t | Str t <- xs]
  _ -> False

-- | Whether a block's own text holds the wiki link to 'linkWord', or the
-- word as text (a link with no end yet): a paragraph's, a heading's, a
-- table's, a box's title, a spoiler's description.
holdsLink :: Block -> Bool
holdsLink = \case
  Alert _ boxHead _ -> ours (fromMaybe [] boxHead.title)
  Spoiler spoilerHead _ -> ours spoilerHead.description
  BlockQuote _ -> False
  List _ _ -> False
  Div _ _ -> False
  Footnotes _ -> False
  b -> any isOurs (allInlines [b])
  where
    ours xs = any isOurs (allInlines [Plain xs])
    isOurs = \case
      Link attributes address _ _ -> ("class", "wikilink") `elem` attributes && linkWord `T.isPrefixOf` address
      -- A link with no end yet is text.
      Str t -> linkWord `T.isInfixOf` t
      _ -> False

-- | The words of the paragraphs where the @/@ menu opens ('slash'), each
-- paragraph's own text, not that of an inline in it: those at the top of
-- the page, in a list's item, a quotation, a box's text, a @div@ or a
-- spoiler's body, however nested.
plainWords :: [Block] -> [Text]
plainWords = concatMap $ \case
  Para xs -> own xs
  Plain xs -> own xs
  BlockQuote bs -> plainWords bs
  List _ items -> concat [plainWords bs | ListItem _ bs <- items]
  Alert _ _ bs -> plainWords bs
  Div _ bs -> plainWords bs
  Spoiler _ bs -> plainWords bs
  _ -> []
  where
    own xs = concat [T.words t | Str t <- xs]

-- | 'SlashEntry' is an entry of the @/@ menu: its label, as the toolbar
-- names it; words it is found by too; what it writes, in a few
-- characters, shown grey beside it; and what choosing it does.
data SlashEntry = SlashEntry
  { label :: Text
  , keywords :: [Text]
  , sample :: Text
  , does :: SlashDoes
  }
  deriving stock (Eq, Show)

-- | 'SlashDoes' is what an entry of the @/@ menu does: write what the
-- toolbar's command writes at the cursor, or open one of the toolbar's
-- dialogs (@link-page@, @attach@; 'Candidate''s 'action').
data SlashDoes = WritesCommand Command | OpensDialog Text
  deriving stock (Eq, Show)

-- | The @/@ menu: the entries whose label, or a word of it, or a keyword,
-- starts with the letters typed after the @/@, whatever their case. With
-- no letters, every entry, in the toolbar's order. With letters, those
-- whose label starts with them first, then those with a later word that
-- does, then those with a keyword that does; and within each group the
-- shorter label first (the letters are more of it: @/ta@ is the table
-- before the task list), then the toolbar's order.
--
-- Choosing an entry that writes takes the @/@ and the letters away and
-- writes what the toolbar's command writes at that place, selection and
-- all ('slashWritten'): as a snippet, its selection the first place to
-- fill and, where the command's result has others ('furtherPlaces'), the
-- next ones Tab goes to. An entry that opens a dialog takes the @/@ and
-- the letters away, and the editor opens it.
slashSource :: [SlashEntry] -> Source
slashSource entries wanted
  | wanted.typed.trigger /= SlashMenu = []
  | otherwise = mapMaybe candidate (map snd (sortOn fst [(key, e) | (i, e) <- zip [0 :: Int ..] entries, Just rank <- [rankOf e], let key = (rank, if T.null query then 0 else T.length e.label, i)]))
  where
    query = T.toLower wanted.typed.letters
    prefix = T.take wanted.typed.from wanted.at.before
    window = windowOf wanted.at prefix
    rankOf e
      | T.null query || query `T.isPrefixOf` T.toLower e.label = Just (1 :: Int)
      | any (query `T.isPrefixOf`) (drop 1 (T.words (T.toLower (T.map (\c -> if isAlphaNum c then c else ' ') e.label)))) = Just 2
      | any ((query `T.isPrefixOf`) . T.toLower) e.keywords = Just 3
      | otherwise = Nothing
    candidate e = case e.does of
      OpensDialog action -> Just Candidate {label = e.label, skeleton = [Field 0 ""], writing = AsSnippet, action = Just action, note = Just e.sample, origin = Nothing}
      WritesCommand command -> do
        written <- slashWritten window command
        Just (lineCandidate wanted.at command written) {label = e.label, note = Just e.sample}

-- | 'Window' is the text a command of the @/@ menu is applied to: the
-- lines above the cursor's line from the nearest one with text in it (or
-- the text's first), the cursor's line without the @/@ and the letters (up
-- to the cursor, and after it), and the lines below it up to the first
-- with text in it (or the text's last). The toolbar's commands read no
-- more of the text around an empty selection ("Wiki.Core.Markdown.Edit"'s
-- 'apply'): an inline command writes at the cursor, a command of lines
-- changes the cursor's line, and a block goes after the cursor's line
-- with a blank line on either side but where one is there already, or
-- the text ends, or has nothing but white space up to there. A block of
-- the page around the cursor is not read at all. The properties check
-- that 'apply' on the window writes what it writes on the whole text.
data Window = Window {above :: [Text], prefix :: Text, after :: Text, below :: [Text]}

-- | The window around the cursor's line, given the line's text before the
-- @/@.
windowOf :: At -> Text -> Window
windowOf at prefix = Window {above = reverse (upToText (reverse at.above)), prefix, after = at.after, below = upToText at.below}
  where
    upToText ls = let (blank, rest) = break (not . T.all isSpace) ls in blank <> take 1 rest

-- | What the toolbar's command writes in place of the cursor's line, with
-- the cursor where the @/@ was: the line's new text (with the lines the
-- command writes after it), and what is selected in it afterwards, as
-- (start, end) in characters. 'Nothing' when the command changes the
-- text elsewhere, which none does.
slashWritten :: Window -> Command -> Maybe (Text, (Int, Int))
slashWritten window command = do
  let before' = T.concat [l <> "\n" | l <- window.above]
      after' = T.concat ["\n" <> l | l <- window.below]
      (new, Selection s e) = apply command (before' <> window.prefix <> window.after <> after') (cursor (T.length before' + T.length window.prefix))
  written <- T.stripPrefix before' new >>= T.stripSuffix after'
  let start = s - T.length before'
      end = e - T.length before'
  guard (0 <= start && start <= end && end <= T.length written)
  Just (written, (start, end))

-- | An entry of the @/@ menu that writes, as a candidate: the cursor's
-- line replaced by what the command writes there ('slashWritten'), the
-- selection the first place to fill and the command's further places the
-- next ('furtherPlaces'). Written as a snippet where CodeMirror's snippet
-- writes it as it is: its places hold no line break and no brace, and it
-- is one line, or the cursor's line has no indentation (which the snippet
-- would put before each further line) and no further line starts with a
-- tab (which the snippet makes an indentation). Otherwise it is written
-- as it is, with the selection and no further place ('LineAsText').
lineCandidate :: At -> Command -> (Text, (Int, Int)) -> Candidate
lineCandidate at command (written, selected@(_, end)) =
  Candidate {label = "", skeleton, writing, action = Nothing, note = Nothing, origin = Nothing}
  where
    line = at.before <> at.after
    indentation = T.takeWhile isBlank at.before
    further = [(p, p + T.length t) | (offset, t) <- furtherPlaces command, let p = end + offset, T.take (T.length t) (T.drop p written) == t, p + T.length t <= T.length written]
    places = selected : if length further == length (furtherPlaces command) then further else []
    slice (a, b) = T.take (b - a) (T.drop a written)
    asSnippet =
      (T.null indentation || not ("\n" `T.isInfixOf` written))
        && not (any ("\t" `T.isPrefixOf`) (drop 1 (T.splitOn "\n" written)))
        && all (not . T.any (`elem` ("{}\n" :: [Char])) . slice) places
    (skeleton, writing)
      | asSnippet = (piecesOf places, LineAsSnippet line)
      | otherwise = (piecesOf [selected], LineAsText line)
    piecesOf ps = go 0 (zip [1 ..] ps)
      where
        go at' = \case
          [] -> [Literal (T.drop at' written) | at' < T.length written]
          (n, range@(a, b)) : rest -> [Literal (slice (at', a)) | a > at'] <> [Field n (slice range)] <> go b rest

-- | The places after the one a command selects that a skeleton of the @/@
-- menu gives too, Tab going to each in turn: where each starts after the
-- end of the selection, in characters, and what it holds as the command
-- writes it ("Wiki.Core.Markdown.Edit"'s templates): a table's other
-- header cells, and nothing else, the command's own selection being the
-- one place of any other. A place is given only where the text written
-- holds what it should there ('lineCandidate').
furtherPlaces :: Command -> [(Int, Text)]
furtherPlaces = \case
  Edit.Table -> [(3, "Column 2"), (14, "Column 3")]
  _ -> []

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

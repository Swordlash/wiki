-- | 'Starts' are where a page's text may be cut so that a parse of the
-- part after the cut reads it as the parse of the whole page does: the
-- lines at which a top-level block starts with nothing open above it.
-- The editor's Enter ("Wiki.Core.Markdown.Enter") parses only the part
-- of the page around the cursor, and must read it as the page reads it.
--
-- CodeMirror's syntax tree cannot say where those lines are: it knows no
-- footnote's definition, none of the wiki's own blocks (@[[!notes]]@ …
-- @[[/notes]]@), and reads HTML in a page otherwise than the wiki's
-- parser, so a block it finds may start inside one of them. So the starts
-- come from the wiki's own parse, the one the editor makes of the whole
-- text after a pause in the typing ("Wiki.Core.Markdown.Problems"'
-- 'Wiki.Core.Markdown.Problems.checkText'), and are kept with the text
-- they were found in. As the text changes, the editor maps them through
-- the changes and keeps where the first change since that parse is: the
-- text before it is the text parsed, so a start whose line ends at or
-- before it is still one ('textRange'). Its own line counts: an indent
-- typed on it can put it in the item above.
--
-- A label of a footnote defined twice is the one exception: the parser
-- drops a second definition of a label with its blocks, whatever lies
-- between, so the part after a cut reads such a definition as the first.
module Wiki.Core.Markdown.Starts
  ( blockStarts
  , Kept (..)
  , textRange
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Shape (..), ShapeKind (..), shapeLines)
import Wiki.Core.Markdown.Enter (textLimit)

-- | The lines, from 1, at which a top-level block of a text starts with
-- nothing open above it, in order, the first line always among them: the
-- first line of each block of the page's own (not one in a footnote's
-- definition, which the parser places at the page's end) that comes after
-- a blank line, and the first line of each item after the first of a list
-- of the page's own (not nested, not in a quotation, a footnote or a
-- block of the wiki's own). Cut at such an item, the rest reads as a list
-- of the same kind beginning with it, which is all Enter needs: the mark
-- and its number are on the item's line, and whether the list is loose
-- does not change what Enter writes. So in a list of any length Enter
-- reads only the items around the cursor's. A block of HTML that reads on, a fence left open, one of
-- the wiki's own blocks not closed is one block to its end, with no start
-- inside it; so is a footnote's definition. A block right after another
-- is read as it is because of the one before: an item may start a list
-- after a paragraph's line only if it has text, and is no task then.
blockStarts :: Text -> [Shape] -> [Int]
blockStarts source shapes =
  ordNub (1 : sort ([a | s <- shapes, s.kind /= NoteShape, let { a = fst (shapeLines s) }, a > 1, afterBlank a, not (inNote a)] <> items))
  where
    -- The items after the first of a list at the page's own level.
    items = [fst (shapeLines i) | s <- shapes, s.kind == ListShape, i <- drop 1 s.inner, not (inNote (fst (shapeLines i)))]
    -- The footnotes' definitions, each by its first line, with its last:
    -- they do not overlap, so the one that may hold a line is the last
    -- that starts at or before it.
    notes = Map.fromList [shapeLines s | s <- shapes, s.kind == NoteShape]
    inNote l = maybe False (\(_, b) -> l <= b) (Map.lookupLE l notes)
    -- The blank lines, found in one pass over the text, so that each
    -- block's start is a lookup.
    blanks = IntSet.fromList [l | (l, line) <- zip [1 ..] (T.splitOn "\n" source), T.null (T.strip line)]
    afterBlank l = IntSet.member (l - 1) blanks

-- | 'Kept' is a start the last parse found, as the editor holds it after
-- the changes since: mapped through them, where it is now, and where the
-- line it is on now starts and ends (offsets in the text now). A change
-- can map a start into the middle of a line, or several starts onto one
-- place (a deletion across them).
data Kept = Kept
  { at :: Int
  , lineFrom :: Int
  , lineTo :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | Where the text handed to Enter starts and ends, for a cursor, as
-- offsets in the text now: given the starts kept ('Kept'), the offset of
-- the first change since they were found, the start and the end of the
-- cursor's line, and the text's length. Nothing when the text would be
-- longer than 'textLimit': Enter is then a line break alone.
--
-- Only a start still at a line's start counts, and once however many
-- starts the changes put there (several collapsed onto one would count a
-- line twice and cut the block below short).
--
-- The text starts at the last start before the start of the cursor's
-- line whose line ends at or before the first change, or at 0. Before the
-- cursor's line, not at it: on a line that starts a block, the block
-- above comes too, since Enter may empty that line, and a block above
-- that goes on over blank lines (a fence left open in an item) would take
-- the emptied line in. Typing in the cursor's block below its first line
-- leaves its start; an edit above it, or on its first line, until the
-- next parse, gives an earlier start and a longer text.
--
-- It ends just before the second start after the end of the cursor's
-- line, so that the block below comes whole, or at the end of the text. A
-- block cut short reads otherwise (a table's header without its delimiter
-- row is a paragraph), and Enter checks that a new line leaves the lines
-- below it read as they were; the blocks after one read as it was are
-- read as they were. A start the changes have put in a block that reads
-- on (an end marker typed above it) Enter sees only on the lines it is
-- given.
textRange :: [Kept] -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
textRange kept firstChange lineStart lineEnd len = do
  let starts = ordNub (sort [k | k <- kept, k.at == k.lineFrom])
      positions = ordNub (map (.at) starts)
      begins = fromMaybe 0 (lastMay [k.at | k <- starts, k.at < lineStart, k.lineTo <= firstChange])
      ends = case dropWhile (<= lineEnd) positions of
        _ : end : _ -> end - 1
        _ -> len
  guard (ends - begins <= textLimit)
  pure (begins, ends)

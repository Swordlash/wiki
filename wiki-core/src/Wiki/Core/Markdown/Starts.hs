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
-- before it is still one ('startFor'). Its own line counts: an indent
-- typed on it can put it in the item above.
--
-- A label of a footnote defined twice is the one exception: the parser
-- drops a second definition of a label with its blocks, whatever lies
-- between, so the part after a cut reads such a definition as the first.
module Wiki.Core.Markdown.Starts
  ( blockStarts
  , startFor
  , endFor
  )
where

import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Shape (..), ShapeKind (..), shapeLines)

-- | The lines, from 1, at which a top-level block of a text starts with
-- nothing open above it, in order, the first line always among them: the
-- first line of each block of the page's own (not one in a footnote's
-- definition, which the parser places at the page's end) that comes after
-- a blank line. A block of HTML that reads on, a fence left open, one of
-- the wiki's own blocks not closed is one block to its end, with no start
-- inside it; so is a footnote's definition. A block right after another
-- is read as it is because of the one before: an item may start a list
-- after a paragraph's line only if it has text, and is no task then.
blockStarts :: Text -> [Shape] -> [Int]
blockStarts source shapes =
  ordNub (1 : sort [a | s <- shapes, s.kind /= NoteShape, let a = fst (shapeLines s), a > 1, afterBlank a, not (any (holds a) notes)])
  where
    notes = [shapeLines s | s <- shapes, s.kind == NoteShape]
    holds l (a, b) = a <= l && l <= b
    sourceLines = T.splitOn "\n" source
    afterBlank l = maybe False (T.null . T.strip) (atMay sourceLines (l - 2))

-- | Where the text handed to a parse starts, for a cursor: the last start
-- before the start of the cursor's line whose line ends at or before the
-- first change since the starts were found, or 0. Before it, not at it:
-- on a line that starts a block, the block above comes too, since Enter
-- may empty that line, and a block above that goes on over blank lines
-- (a fence left open in an item) would take the emptied line in. The starts
-- are offsets in the text as it is now (mapped through the changes; those
-- before the first change are where they were), in order, each with the
-- end of its line; the positions are offsets too. Typing in the cursor's
-- block below its first line leaves its start; an edit above it, or on
-- its first line, until the next parse, gives an earlier start and a
-- longer text.
startFor :: [(Int, Int)] -> Int -> Int -> Int
startFor starts firstChange lineStart = fromMaybe 0 (lastMay [s | (s, end) <- takeWhile ((< lineStart) . fst) starts, end <= firstChange])

-- | Where the text handed to a parse ends, for a cursor: just before the
-- second start after the end of the cursor's line, so that the block
-- below the cursor's comes whole, or at the end of the text when there is
-- no such start. A block cut short reads otherwise (a table's header
-- without its delimiter row is a paragraph), and Enter checks that a new
-- line leaves the lines below it read as they were; the blocks after one
-- read as it was are read as they were. What is cut off cannot change how
-- the lines before it are read while the cut is at a start of the text as
-- it is now. A start after a change since the parse may no longer be one:
-- the caller passes only those still at a line's start after a blank line
-- (a change across lines maps a start into the middle of one, or takes
-- away the blank line before it); one the change has put in a block that
-- reads on (an end marker typed above it) Enter sees only on the lines it
-- is given.
endFor :: [Int] -> Int -> Maybe Int
endFor starts lineEnd = case dropWhile (<= lineEnd) starts of
  _ : end : _ -> Just end
  _ -> Nothing

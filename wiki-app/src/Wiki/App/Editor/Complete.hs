-- | 'Complete' is the editor's side of the completion of the wiki's own
-- Markdown ("Wiki.Core.Markdown.Complete", WP5.124 (b) of docs/PLAN.md):
-- the sources the editor's list is made from, out of what the editor
-- holds, and the answer to CodeMirror's question, in the shape
-- @web/src/editor.js@ reads.
--
-- CodeMirror asks with a @wiki-complete@ event ("Wiki.App.Editor.Browser"'s
-- 'Wiki.App.Editor.Browser.completeAsked'): the number of the cursor's line
-- in the whole text, that line up to the cursor, and the names of the
-- syntax tree's nodes at the cursor. The component answers at once, in the
-- step that handles the event, from the text it holds ('answer'): the
-- question asks the backend nothing.
module Wiki.App.Editor.Complete
  ( Request (..)
  , Held (..)
  , sources
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
import Wiki.Core.Markdown.Box (BoxKind)
import Wiki.Core.Markdown.Complete
import Wiki.Core.PageKind (PageKind)

-- | 'Request' is CodeMirror's question: the cursor's line, from 1, in the
-- whole text (the master's, for a row of a presentation edited slide by
-- slide), that line up to the cursor, and the names of the syntax tree's
-- nodes at the cursor, innermost first.
data Request = Request
  { line :: Int
  , before :: Text
  , nodes :: [Text]
  }
  deriving stock (Eq, Show)

-- | 'Held' is what the editor holds that the sources read: the kinds of
-- box the administration defined, and the page's kind, which says the
-- markers it reads.
data Held = Held
  { kind :: PageKind
  , boxes :: [BoxKind]
  }

-- | The sources of the editor's list, in the order their candidates are
-- listed: the wiki's own. A plugin's source (#186) goes after it.
sources :: Held -> [Source]
sources held = [wikiSource held.kind held.boxes]

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
  , byLine :: Map.Map Text LineReading
  }

-- | A memo of nothing.
noReadings :: Readings
noReadings = Readings {above = [], byLine = Map.empty}

-- | The list for a request, against the text the editor holds, or
-- 'Nothing': when nothing opens there, and when the cursor's line in that
-- text does not start with the request's (the text has changed since
-- CodeMirror asked, and CodeMirror asks again as more is typed); and the
-- memo of the parser's readings, to answer the next request with.
answer :: [Source] -> Readings -> Text -> Request -> (Maybe Completion, Readings)
answer found memo text request = case T.splitOn "\n" text of
  ls
    | request.line >= 1
    , Just current <- ls `atMay` (request.line - 1)
    , request.before `T.isPrefixOf` current ->
        let above = take (request.line - 1) ls
            at = At {place = placeOf request.nodes, above, before = request.before}
            kept = if memo.above == above then memo.byLine else Map.empty
            byLine = foldl' (\m l -> Map.insertWith (\_ old -> old) l (readLine above l) m) kept (probeLines at)
            readAt l = fromMaybe (readLine above l) (Map.lookup l byLine)
         in (complete found readAt at, Readings {above, byLine})
  _ -> (Nothing, memo)

-- | An answer as @web/src/editor.js@ reads it: where the text the entry
-- chosen replaces starts in the line, in characters; the options, each
-- with its label, its snippet ('snippetTemplate') and, for one from
-- elsewhere than the wiki, its origin as CodeMirror's @detail@, shown
-- grey beside the label; and what Enter and Escape do.
completionJson :: Completion -> Value
completionJson c =
  object
    [ "from" .= c.from
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
            <> ["detail" .= originText o | Just o <- [candidate.origin]]
        )

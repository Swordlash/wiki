-- | The errors of formulas and diagrams: a built-in plugin
-- ("Wiki.App.Plugin") whose 'Diagnostics' are the failures of the
-- libraries that draw the preview's islands. A formula KaTeX cannot parse,
-- a diagram Mermaid cannot parse and a picture TeX cannot draw each become
-- a problem on the lines where the island is written, with the library's
-- message.
--
-- The editor collects the failures; this plugin only turns them into
-- problems. With the plugin off, the editor neither marks nor counts them.
-- The island still shows its error in place, in the preview and on the
-- page.
module Wiki.App.Plugin.Errors
  ( errors
  , islandFailures
  )
where

import Data.Map.Strict qualified as Map
import Protolude
import Wiki.App.Plugin (Diagnostics (..), Drawn (..), Plugin (..), builtin)
import Wiki.Core.Markdown.Problems (Problem, islandProblem)

-- | The plugin.
errors :: Plugin m
errors =
  Plugin
    { manifest = builtin "errors" "Errors of formulas and diagrams" "Marks in the editor's text the formulas, diagrams and TikZ pictures that cannot be drawn, with the reason."
    , diagnostics = Just (Diagnostics islandFailures)
    , agent = Nothing
    , options = Nothing
    }

-- | A problem for each island that failed and is in the text checked.
islandFailures :: Drawn -> [Problem]
islandFailures drawn =
  [islandProblem place message | (key, message) <- Map.toList drawn.failures, Just place <- [Map.lookup key drawn.islands]]

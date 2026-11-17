-- | 'Tools' is what the editor's toolbar offers, in its order: the
-- commands of "Wiki.Core.Markdown.Edit" with their buttons, and the menus
-- and buttons that do more than run one command (the colours, the emoji,
-- a link to a page, an attachment, the boxes, the math macros, HTML).
--
-- The toolbar is rendered from these definitions ("Wiki.App.View.Editor"),
-- and so is any other list of what the toolbar writes, so that a tool
-- added here appears in each.
module Wiki.App.Editor.Tools
  ( Tool (..)
  , toolName
  , toolTitle
  , textTools
  , listTools
  , codeTools
  , blockTools
  , tableTool
  , htmlTags
  , Control (..)
  , toolbarGroups
  , controlName
  , slashEntries
  , notOffered
  )
where

import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Complete (SlashDoes (..), SlashEntry (..))
import Wiki.Core.Markdown.Edit (Command (..), HtmlTag, htmlTagLabel, htmlTagName)

-- | 'Tool' is a toolbar button: the command it runs, its name, what more
-- its title says of it (the heading's levels), the shortcut that also
-- runs it, words the @/@ menu finds it by besides its name, what it
-- writes in a few characters, which the menu shows beside it, and its
-- icon (an SVG path in a 16 by 16 box, drawn as a line in the text's
-- colour by the stylesheet).
data Tool = Tool
  { command :: Command
  , label :: Text
  , hint :: Maybe Text
  , shortcut :: Maybe Text
  , keywords :: [Text]
  , sample :: Text
  , icon :: Text
  }

-- | A button's name, as a screen reader says it: its label, and what more
-- its title says.
toolName :: Tool -> Text
toolName tool = tool.label <> maybe "" (\h -> " (" <> h <> ")") tool.hint

-- | A button's title: its name, and its shortcut.
toolTitle :: Tool -> Text
toolTitle tool = toolName tool <> maybe "" (\k -> " (" <> k <> ")") tool.shortcut

textTools, listTools, codeTools, blockTools :: [Tool]
textTools =
  [ Tool Bold "Bold" Nothing (Just "Ctrl+B") ["strong"] "**bold**" "M4.5 2.5h4a2.75 2.75 0 0 1 0 5.5h-4zM4.5 8h4.75a2.75 2.75 0 0 1 0 5.5H4.5z"
  , Tool Italic "Italic" Nothing (Just "Ctrl+I") ["emphasis"] "_italic_" "M7 2.5h5M4 13.5h5M10 2.5l-4 11"
  , Tool Strikethrough "Strikethrough" Nothing Nothing ["strike", "delete"] "~~struck~~" "M2 8h12M11.5 4.5c-.5-1.3-1.8-2-3.5-2-2 0-3.5 1-3.5 2.5 0 1.2 1 2 2.5 2.5M5 11c.4 1.5 1.7 2.5 3.3 2.5 2 0 3.3-1 3.3-2.5"
  , -- An eye struck through: text hidden until it is clicked.
    Tool Spoiler "Inline spoiler" Nothing Nothing ["hide"] "||hidden||" "M1 8s2.5-5 7-5 7 5 7 5-2.5 5-7 5-7-5-7-5zM8 6.5a1.5 1.5 0 1 0 0 3 1.5 1.5 0 1 0 0-3zM2 2l12 12"
  , Tool Heading "Heading" (Just "again for a smaller one, then none") Nothing ["title"] "# Heading" "M3 2.5v11M10 2.5v11M3 8h7M12.5 7.5l1.5-1v7"
  ]
listTools =
  [ Tool BulletList "Bulleted list" Nothing (Just "Ctrl+Shift+8") ["unordered"] "- item" "M6 4h8M6 8h8M6 12h8M2.5 4h.01M2.5 8h.01M2.5 12h.01"
  , Tool NumberedList "Numbered list" Nothing (Just "Ctrl+Shift+7") ["ordered"] "1. item" "M6 4h8M6 8h8M6 12h8M2 2.5l1-.5v3.5M2 9.5c.3-.6 1.7-.7 1.7.2 0 .7-1.7 1.3-1.7 2.3h1.8"
  , Tool TaskList "Task list" Nothing Nothing ["todo", "checkbox"] "- [ ] task" "M2 2.5h3.5V6H2zM2 10.5l1.2 1.2 2.3-2.7M8 4.25h6M8 11h6"
  , Tool Quote "Quote" Nothing Nothing ["blockquote"] "> quote" "M2.5 3v10M6 4.5h7.5M6 8h7.5M6 11.5h5"
  ]
codeTools =
  [ Tool InlineCode "Inline code" Nothing (Just "Ctrl+E") ["monospace"] "`code`" "M5.5 4 1.5 8l4 4M10.5 4l4 4-4 4"
  , Tool CodeBlock "Code block" Nothing Nothing ["fence"] "```" "M1.5 2.5h13v11h-13zM6 6 4 8l2 2M10 6l2 2-2 2"
  , Tool Link "Link" Nothing (Just "Ctrl+K") ["url", "address"] "[text](https://)" "M6.8 9.2a2.8 2.8 0 0 0 4 0l2.2-2.2a2.8 2.8 0 0 0-4-4l-.9.9M9.2 6.8a2.8 2.8 0 0 0-4 0L3 9a2.8 2.8 0 0 0 4 4l.9-.9"
  ]
blockTools =
  [ Tool InlineMath "Inline math" Nothing Nothing ["latex", "formula"] "$x^2$" "M12.5 3h-8l4.5 5-4.5 5h8"
  , Tool DisplayMath "Display math" Nothing Nothing ["latex", "equation"] "$$" "M1.5 2.5h13v11h-13zM11 5H5.5L8.5 8l-3 3H11"
  , Tool Mermaid "Mermaid diagram" Nothing Nothing ["chart", "graph"] "```mermaid" "M1.5 2.5h5v4h-5zM9.5 9.5h5v4h-5zM4 6.5v5h5.5"
  , Tool Tikz "TikZ picture" Nothing Nothing ["drawing", "latex"] "```tikz" "M2.5 1.5v12h12M4.5 11c2-1 3-7 6-7 1.5 0 2.5 1 3.5 2"
  , -- A frame around angle brackets: HTML in a frame of its own.
    Tool HtmlBlock "HTML block" Nothing Nothing ["frame"] "```html" "M1.5 2.5h13v11h-13zM5.5 6 3.5 8l2 2M10.5 6l2 2-2 2M9 5.5 7 10.5"
  , Tool HorizontalRule "Horizontal rule" Nothing Nothing ["divider", "line"] "---" "M1.5 8h13M4 4.5h8M4 11.5h8"
  , -- A box with a disclosure triangle, as a closed spoiler shows.
    Tool SpoilerBlock "Block spoiler" Nothing Nothing ["hide", "collapse"] "[[!spoiler]]" "M1.5 2.5h13v11h-13zM4 5l2.5 1.5L4 8zM8.5 6.5h4M4 10.5h8.5"
  ]

tableTool :: Tool
tableTool = Tool Table "Table" Nothing Nothing ["grid"] "| a | b |" "M1.5 2.5h13v11h-13zM1.5 6h13M6 2.5v11M10.5 2.5v11"

-- | The HTML elements the toolbar's menu offers.
htmlTags :: [HtmlTag]
htmlTags = [minBound .. maxBound]

-- | 'Control' is one of the toolbar's controls: a command's button, or a
-- control that does more than run one command. The colours' palette and
-- the emoji's menu, "Link to a page" (a dialog), "Image or attachment"
-- (a file to upload), the boxes' menu (a kind of box, or a new kind),
-- the math macros' panel and the HTML menu.
--
-- A control added here is drawn by the toolbar ("Wiki.App.View.Editor")
-- and offered by the @/@ menu ('slashEntries'), or named among those the
-- menu does not offer ('notOffered'): each of the two has a case for
-- every control.
data Control
  = ToolControl Tool
  | ColourControl
  | EmojiControl
  | LinkPageControl
  | ImageControl
  | BoxControl
  | MacrosControl
  | HtmlControl

-- | The toolbar's controls, in its groups and its order: the text's look,
-- the lists, code and links, and the blocks.
toolbarGroups :: [[Control]]
toolbarGroups =
  [ map ToolControl textTools <> [ColourControl, EmojiControl]
  , map ToolControl listTools
  , map ToolControl codeTools <> [LinkPageControl, ImageControl, ToolControl tableTool]
  , map ToolControl blockTools <> [BoxControl, MacrosControl, HtmlControl]
  ]

-- | A control's name, as its button's title says it.
controlName :: Control -> Text
controlName = \case
  ToolControl tool -> tool.label
  ColourControl -> "Colour"
  EmojiControl -> "Emoji"
  LinkPageControl -> "Link to a page"
  ImageControl -> "Image or attachment"
  BoxControl -> "Box"
  MacrosControl -> "Math macros"
  HtmlControl -> "HTML"

-- | The controls the @/@ menu does not offer, by name: the colours, which
-- colour a selection; the emoji, which have their own list after a colon;
-- and the math macros' panel, which writes nothing at the cursor. (The
-- box menu's "New kind…" defines a kind of box, and is not offered
-- either.)
notOffered :: [Text]
notOffered = ["Colour", "Emoji", "Math macros"]

-- | The entries of the @/@ menu ("Wiki.Core.Markdown.Complete"'s
-- 'Wiki.Core.Markdown.Complete.slashSource'), made from the toolbar's
-- controls in its order, with the kinds of box the box menu offers (each
-- marker with its label, 'Wiki.Core.Markdown.Box.boxChoices'): each
-- command under its button's name; "Link to a page" and "Image or
-- attachment", which open their dialogs; a kind of box and an HTML
-- element under the names their menus give them.
slashEntries :: [(Text, Text)] -> [SlashEntry]
slashEntries boxes = concatMap entriesOf (concat toolbarGroups)
  where
    entriesOf = \case
      ToolControl tool -> [SlashEntry {label = tool.label, keywords = tool.keywords, sample = tool.sample, does = WritesCommand tool.command}]
      ColourControl -> []
      EmojiControl -> []
      MacrosControl -> []
      LinkPageControl -> [SlashEntry {label = "Link to a page", keywords = ["wiki", "page"], sample = "[Title](page)", does = OpensDialog "link-page"}]
      ImageControl -> [SlashEntry {label = "Image or attachment", keywords = ["file", "upload", "picture"], sample = "![](file)", does = OpensDialog "attach"}]
      BoxControl ->
        [ SlashEntry {label, keywords = ["box", "callout", T.toLower marker], sample = "> [!" <> marker <> "]", does = WritesCommand (Box marker)}
        | (marker, label) <- boxes
        ]
      HtmlControl ->
        [ SlashEntry {label = htmlTagLabel tag, keywords = ["html", htmlTagName tag], sample = "<" <> htmlTagName tag <> ">", does = WritesCommand (Html tag)}
        | tag <- htmlTags
        ]

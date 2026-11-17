{-# LANGUAGE QuasiQuotes #-}

-- | The parts of a page as its reader sees it that two views draw: the
-- page view ("Wiki.App.View.Page"), and the preview in a window of its own
-- ("Wiki.App.View.LivePreview"), which shows the text being typed laid out
-- as the page will show it. Both draw these parts with the same markup and
-- classes, so the stylesheet and @jsbits/toc.js@ treat them the same: the
-- width chosen, the title's row, a template's note, a blog post's date and
-- neighbours, the column of the text with the "Contents" panel beside it,
-- and the list of attachments.
--
-- Each part is polymorphic in the action, so that each view gives its own
-- for a click (a choice of width, a link followed).
module Wiki.App.View.Reading
  ( -- * The width of the page
    widthClass
  , widthSwitch

    -- * The page's parts
  , titleRow
  , templateNote
  , postInfo
  , mainClasses
  , contentsPanel
  , textEnd
  , attachmentList
  )
where

import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow)
import Halogen.Router qualified as Router
import Halogen.TH (hh, hhs)
import Protolude
import Wiki.App.Browser (ArticleLink)
import Wiki.App.Route (Route (Page))
import Wiki.App.View.Body qualified as Body
import Wiki.App.View.Common
import Wiki.Core.Attachment (Attachment (..), FileName (..), MimeType (..))
import Wiki.Core.Blog (Neighbours (..), neighbours)
import Wiki.Core.Id (PageId)
import Wiki.Core.Markdown qualified as Md
import Wiki.Core.Page (PageSummary (slug, title))
import Wiki.Core.Space (Space (blog, id))
import Wiki.Core.Template (placeholderNames)

-- | The class of the page's frame (@page-view@) for how wide it lays the
-- page out.
widthClass :: PageWidth -> Text
widthClass = \case
  Compact -> "page-width-compact"
  FullWidth -> "page-width-full"

-- | The switch between a compact page and a page as wide as the window, at
-- the end of the page's bar of actions (WP5.55 of docs/PLAN.md): two radio
-- buttons, each with a small picture of what it does, each giving
-- @choose@ its width. It changes the layout on a wide screen only; on a
-- narrower one both are the same, and the stylesheet hides the switch.
widthSwitch :: (PageWidth -> action) -> PageWidth -> HH.HTML w action
widthSwitch choose width =
  [hh|
    <div class="page-width-switch" role="radiogroup" aria-label="Page width">
      <button type="button" role="radio" class="page-width-option page-width-option-compact" aria-checked={{ ariaBool (width == Compact) }} title="The text at a reading width, in the middle" onClick={{ const (choose Compact) }}>
        <svg class="icon" viewBox="0 0 16 16" aria-hidden="true"><path d="M1.5 2.5h13v11h-13z M5.5 4.5v7 M10.5 4.5v7"></path></svg>
        <span>Compact</span>
      </button>
      <button type="button" role="radio" class="page-width-option page-width-option-full" aria-checked={{ ariaBool (width == FullWidth) }} title="The text as wide as the window" onClick={{ const (choose FullWidth) }}>
        <svg class="icon" viewBox="0 0 16 16" aria-hidden="true"><path d="M1.5 2.5h13v11h-13z M3.5 4.5h9 M3.5 8h9 M3.5 11.5h9"></path></svg>
        <span>Full width</span>
      </button>
    </div>
  |]

-- | The row of the page's title, above its text, with what the view puts
-- at its right (the page view: who else reads the page). Only for a page
-- whose Markdown does not start with its own title ('Body.startsWithHeading'):
-- that heading is the title then.
titleRow :: Text -> Text -> [HH.HTML w i] -> HH.HTML w i
titleRow title body beside =
  [hh|<div class="page-title-row">{{ Body.heading title body }}{{ beside }}</div>|]

-- | A note on a template saying what it is, since it is not in the tree.
templateNote :: Bool -> Maybe (HH.HTML w i)
templateNote template =
  guard template
    $> HH.p
      [HP.class_ "template-note", attr "role" "note"]
      ( [HH.text "This page is a template: \"New page\" offers it, with "]
          <> intercalate [HH.text ", "] [[HH.code_ [HH.text ("{{" <> name <> "}}")]] | name <- placeholderNames]
          <> [HH.text " filled in."]
      )

-- | A post of a blog ("Wiki.Core.Blog"), by its id and when it was
-- created: when it was posted, and the posts before and after it, each a
-- link followed through @followed@. 'Nothing' for a page of a space that
-- is not a blog.
postInfo :: (Follow Route -> action) -> Context -> Space -> [PageSummary] -> PageId -> UTCTime -> Maybe (HH.HTML w action)
postInfo followed context space pages post createdAt = do
  guard space.blog
  Neighbours {older, newer} <- neighbours space pages post
  let spacePath = spacePathOf context.spaces space.id
      linkTo p = Router.linkProps context.router followed (Page spacePath [p.slug])
  pure
    [hh|
      <div class="post-info">
        <p class="post-date">Posted <time {{ [attr "datetime" (toS (iso8601Show createdAt))] }}>{{ dateOf createdAt }}</time></p>
        {% if isJust older || isJust newer %}
          <nav class="post-nav" aria-label="Posts">
            {% for o in maybeToList older %}<a class="post-previous" rel="prev" {{ linkTo o }}><span class="post-nav-label">Previous</span><span class="post-nav-title">{{ o.title }}</span></a>{% end %}
            {% for n in maybeToList newer %}<a class="post-next" rel="next" {{ linkTo n }}><span class="post-nav-label">Next</span><span class="post-nav-title">{{ n.title }}</span></a>{% end %}
          </nav>
        {% end %}
      </div>
    |]

-- | The classes of the element that holds the page's text (@page-main@),
-- with the column beside it (@page-side@) when it has one: for the
-- "Contents" panel, and for the panel of comments when that is open. The
-- stylesheet lays the text and the column out by these classes.
mainClasses :: Bool -> Bool -> Text
mainClasses contents comments =
  "page-main"
    <> (if contents || comments then " with-side" else "")
    <> (if contents then " with-contents" else "")
    <> (if comments then " with-comments" else "")

-- | The "Contents" panel (WP4.15 of docs/PLAN.md): the page's headings, in
-- the column beside its text. It is a @\<details>@, so the reader can fold
-- it; @jsbits/toc.js@ remembers that choice and marks the section being
-- read. A click on an entry goes to @onLink@ (for
-- 'Wiki.App.Browser.followArticleLink'), which scrolls to the heading.
contentsPanel :: (ArticleLink -> action) -> Body.Place -> [Md.Entry] -> HH.HTML w action
contentsPanel onLink place entries =
  [hh|
    <details class="page-contents" open>
      <summary class="page-contents-title">Contents</summary>
      <nav aria-label="Contents" {{ Body.followingLinks onLink place }}>{{ Md.contentsList entries }}</nav>
    </details>
  |]

-- | The end of the page's text: an empty element, the last in the column
-- of the text (@page-text@), after the article. @jsbits/toc.js@ watches it
-- come into the window: once it is there, the page has been read to its
-- end, and the last heading in the window is the section being read,
-- though the last sections, too short, never reached the line a quarter of
-- the way down the window.
textEnd :: HH.HTML w i
textEnd = [hh|<div class="page-text-end" aria-hidden="true"></div>|]

-- | The page's attachments, under its text, each a link that downloads
-- it, from the addresses the view fetched for them. Nothing for a page
-- with none.
attachmentList :: [Attachment] -> Body.Addresses -> [HH.HTML w i]
attachmentList files addresses =
  [hhs|
    {% if not (null files) %}
      <section class="attachments">
        <h2>Attachments</h2>
        <ul>
          {% for a in files %}
            <li>
              <a href={{ Body.linkUrlOf addresses a.id }} download={{ a.name.text }}>{{ a.name.text }}</a>
              <span class="attachment-meta">{{ a.mediaType.text }}, {{ fileSize a.size }}</span>
            </li>
          {% end %}
        </ul>
      </section>
    {% end %}
  |]

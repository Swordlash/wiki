{-# LANGUAGE QuasiQuotes #-}

-- | The form for a new page: its title, its kind, and what it starts from,
-- under the page the route names or at the space's top level. Creating it creates
-- the page at once, with its body, and opens the editor on it, so that the
-- page has an id from the start: files can be attached to it before
-- anything is written, and the editor only ever saves a page that exists.
-- A page created and then left without a word written stays, empty; it is
-- deleted with the page's Delete, as any other page is.
--
-- A page is a document or a presentation ("Wiki.Core.PageKind"), or of a
-- kind a plugin that is on brings (a calendar: 'Wiki.App.Plugin.KindView'),
-- chosen first: the kinds are the form's first choice (the owner, issue
-- #191). Then it starts blank, or from a template of that kind
-- ("Wiki.Core.Template"): one of the space's own (its pages marked
-- 'Wiki.Core.Page.template'), or one built into the wiki (meeting notes, a
-- decision record and a how-to guide for a document, a starter deck for a
-- presentation). Choosing another kind starts it blank again. A
-- template's body is copied into the new page with @{{title}}@, @{{date}}@
-- and @{{author}}@ filled in: the title typed, today's date (UTC, in ISO
-- 8601 format) and the display name of the user creating it.
--
-- Only someone the rules let edit the space sees the form; the backend
-- checks again, and a refusal is shown in place. The title's input is
-- uncontrolled (the component never sets its text), so no render between
-- a key press and its input event can undo a typed character.
module Wiki.App.View.NewPage
  ( Input (..)
  , component
  )
where

import Data.Row (Empty)
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow)
import Halogen.Router qualified as Router
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.App.Backend (MonadWiki (..), Session (..), WikiError (..))
import Wiki.App.Plugin (KindView (..))
import Wiki.App.Plugin.Registry (Contributed (..), contributed, plugins, pluginsOn)
import Wiki.App.Route (Route (..), pageRoute)
import Wiki.App.View.Common
import Wiki.Core.Id (PageId)
import Wiki.Core.Page (Page (body), PageSummary (kind), templatePages)
import Wiki.Core.Page qualified as P
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (id, name), pageAt)
import Wiki.Core.Template (Placeholders (Placeholders), builtinTemplates, fillTemplate)
import Wiki.Core.Template qualified as Template
import Wiki.Core.User (User (displayName))

-- | The form's input: the space the shell has loaded, with its pages, and
-- where in it the page goes.
data Input = Input
  { context :: Context
  , space :: Space
  , pages :: [PageSummary]
  , parent :: Maybe [Slug]
  -- ^ The path of the page to put it under, or 'Nothing' for the space's
  -- top level.
  }

data State = State
  { input :: Input
  , title :: Text
  -- ^ The title typed so far.
  , kind :: PageKind
  -- ^ The kind of page to make.
  , start :: Start
  -- ^ What the page starts from, among the templates of that kind.
  , submitted :: Maybe Submitted
  -- ^ The submission being carried out, if any. Changed by 'Submit', by
  -- the answer to its create, and by 'unsubmitted'.
  , submissions :: Int
  -- ^ How many submissions were made: the next one's ticket.
  , problem :: Maybe WikiError
  }

-- | 'Submitted' is where a submission is. A submission whose template is
-- being fetched ('Fetching', with its ticket) is dropped by anything that
-- changes what it would make: another kind, start, title, parent or space,
-- or a template no longer offered. The continuation after the fetch
-- creates the page only while its own ticket is the state's. A create
-- sent ('Sending') cannot be taken back, and is not dropped.
data Submitted = Fetching Int | Sending
  deriving stock (Eq, Show)

-- | What a new page starts from.
data Start
  = Blank
  | -- | One of the space's templates.
    SpaceTemplate PageId
  | -- | One of the wiki's built-in templates, by its key.
    BuiltIn Text
  deriving stock (Eq, Show)

data Action = Receive Input | Followed (Follow Route) | Typed Text | ChoseKind PageKind | Chose Start | Submit

component :: forall query m. (MonadWiki m, MonadIO m) => H.Component query Input ViewOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, title = "", kind = Document, start = Blank, submitted = Nothing, submissions = 0, problem = Nothing}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.receive = Just . Receive}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      [hh|
        <section class="new-page-view">
          <h1>{{ headingOf input }}</h1>
          {{ body }}
          {{ errorView <$> st.problem }}
        </section>
      |]
      where
        input = st.input
        ctx = input.context
        body = case under input of
          _ | isNothing ctx.session.user -> [hh|<p class="error sign-in-hint" role="alert">You are not signed in. To write pages, choose who you are at the top right.</p>|]
          _ | not (mayEdit ctx input.space.id) -> errorView Forbidden
          Left e -> errorView e
          Right _ ->
            [hh|
              <form class="inline-form new-page-form" {{ HE.preventingDefault (HE.onSubmit (const Submit)) }}>
                <label class="field">
                  <span>Title</span>
                  <input type="text" class="new-page-title" onValueInput={{ Typed }} required autofocus>
                </label>
                <fieldset class="new-page-kind">
                  <legend>Kind</legend>
                  {% for (kind, label, hint) in kindsOffered ctx %}
                    {{ kindChoice kind label hint }}
                  {% end %}
                </fieldset>
                <fieldset class="new-page-start">
                  <legend>Start from</legend>
                  {{ choice Blank "Blank" (if st.kind == Presentation then "An empty deck." else "An empty page.") }}
                  {% for t in [t | t <- templatePages input.pages, t.kind == st.kind] %}
                    {{ choice (SpaceTemplate t.id) t.title ("A template of " <> input.space.name <> ".") }}
                  {% end %}
                  {% for t in [t | t <- builtinTemplates, t.kind == st.kind] %}
                    {{ choice (BuiltIn t.key) t.title "Built in." }}
                  {% end %}
                </fieldset>
                <button type="submit" class="primary" disabled={{ isJust st.submitted }}>Create and edit</button>
                {{ Router.link ctx.router Followed (leaving input) [HP.class_ "button"] [HH.text "Cancel"] }}
              </form>
            |]

        kindChoice :: PageKind -> Text -> Text -> H.ComponentHTML Action Empty m
        kindChoice kind label hint =
          [hh|
            <label class="check new-page-kind-choice">
              <input type="radio" name="new-page-kind" checked={{ kind == st.kind }} onChange={{ const (ChoseKind kind) }}>
              <span>{{ label }} <small class="check-help">{{ hint }}</small></span>
            </label>
          |]

        choice :: Start -> Text -> Text -> H.ComponentHTML Action Empty m
        choice start label hint =
          [hh|
            <label class="check new-page-choice">
              <input type="radio" name="new-page-start" checked={{ start == st.start }} onChange={{ const (Chose start) }}>
              <span>{{ label }} <small class="check-help">{{ hint }}</small></span>
            </label>
          |]

    handleAction :: Action -> H.HalogenM State Action Empty ViewOutput m ()
    handleAction = \case
      Receive input -> modify (received input)
      Followed f -> gets (\st -> st.input.context.router) >>= (`Router.follow` f)
      Typed title -> modify (\st -> unsubmitted st {title})
      -- Another kind has other templates: the page starts blank again.
      ChoseKind kind -> modify (\st -> unsubmitted st {kind, start = if kind == st.kind then st.start else Blank})
      Chose start -> modify (\st -> unsubmitted st {start})
      Submit -> do
        st <- get
        case under st.input of
          Right parentPage | isNothing st.submitted -> do
            let ticket = st.submissions + 1
            modify (\s -> s {submitted = Just (Fetching ticket), submissions = ticket, problem = Nothing})
            body <- startingBody st
            -- The fetch of a template waits, and the form may change
            -- meanwhile (the kind's plugin turned off, then on again):
            -- the create is decided here, in the step that sends it, by
            -- the submission's ticket.
            still <- state (\s -> if s.submitted == Just (Fetching ticket) then (True, s {submitted = Just Sending}) else (False, s))
            when still $ do
              result <- createPageOf st.kind st.input.space.id ((\p -> p.id) <$> parentPage) st.title body
              modify (\s -> s {submitted = Nothing})
              case result of
                Left e -> modify (\s -> s {problem = Just e})
                -- The editor, on the page just created: the shell reloads the
                -- space's pages, which now include it, and navigates there.
                Right page ->
                  H.raise (Changed (Just (EditPage (spacePathOf st.input.context.spaces st.input.space.id) (fromMaybe [] st.input.parent <> [page.slug]))))
          _ -> pass

-- | The form given a new input. A kind no longer offered (its plugin
-- turned off) is a document again, which starts blank; a space's template
-- no longer among its pages, or another space, starts blank. Each of
-- these, and another parent, drops a submission whose template is being
-- fetched ('unsubmitted').
received :: Input -> State -> State
received input st
  | st.kind `notElem` map (\(k, _, _) -> k) (kindsOffered input.context) = unsubmitted st {input, kind = Document, start = Blank}
  | input.space.id /= st.input.space.id = unsubmitted st {input, start = Blank}
  | SpaceTemplate pid <- st.start, pid `notElem` [t.id | t <- templatePages input.pages] = unsubmitted st {input, start = Blank}
  | input.parent /= st.input.parent = unsubmitted st {input}
  | otherwise = st {input}

-- | The form with a submission whose template is being fetched dropped:
-- what it would make has changed. A create already sent stays.
unsubmitted :: State -> State
unsubmitted st = case st.submitted of
  Just (Fetching _) -> st {submitted = Nothing}
  _ -> st

-- | The new page's body: empty, or the chosen template with its
-- placeholders filled in. A space template is fetched from the backend
-- here, because the list of the space's pages has no bodies. A template
-- that cannot be fetched any more (deleted since the form was shown) gives
-- a blank page. It runs in the view's program: only the fetch waits (in
-- 'H.async'); the clock is read in place ('H.sync').
startingBody :: (MonadWiki m, MonadIO m) => State -> H.HalogenM st act slots output m Text
startingBody st = case st.start of
  Blank -> pure ""
  SpaceTemplate pid
    | Just _ <- find (\p -> p.id == pid) st.input.pages ->
        getPage st.input.space.id (fromMaybe [] (pagePathOf st.input.space st.input.pages pid)) >>= filled . either (const "") (\p -> p.body)
    | otherwise -> filled ""
  BuiltIn key -> filled (maybe "" (\t -> t.body) (find (\t -> t.key == key) builtinTemplates))
  where
    filled body = do
      today <- utctDay <$> H.sync (liftIO getCurrentTime)
      let author = maybe "" (\u -> u.displayName) st.input.context.session.user
      pure (fillTemplate Placeholders {Template.title = T.strip st.title, Template.date = today, Template.author = author} body)

-- | The kinds the form offers, each with its label and hint: a document,
-- a presentation, then the kinds of the plugins that are on for the
-- reader, in the registry's order. Only their labels are read here, so
-- the plugins are taken in 'IO', as the editor takes their diagnostics.
--
-- A plugin's kind is offered only once this session's read of the plugins
-- has answered ('Context''s @pluginsRead@): before it, or when it failed,
-- the context holds the defaults, which would offer a kind the reader or
-- the wiki turned off.
kindsOffered :: Context -> [(PageKind, Text, Text)]
kindsOffered ctx =
  [ (Document, "Document", "A page, read as a document.")
  , (Presentation, "Presentation", "A deck of slides, presented one at a time.")
  ]
    <> [(k.kind, k.label, k.hint) | ctx.pluginsRead, (_, k) <- (contributed (pluginsOn ctx) (plugins @IO)).pageKinds]

-- | The page the new one goes under, if the route names one; 'Nothing'
-- for the space's top level, which the backend reads as under the space's
-- home page ('Wiki.Core.Space.parentOrRoot'); 'NotFound' for a path that
-- names no page.
under :: Input -> Either WikiError (Maybe PageSummary)
under input = case input.parent of
  Nothing -> Right Nothing
  Just path -> maybe (Left NotFound) (Right . Just) (pageAt input.space input.pages path)

headingOf :: Input -> Text
headingOf input = case input.parent of
  Nothing -> "New page in " <> input.space.name
  Just parent -> "New page under " <> maybe "a page" (\p -> p.title) (pageAt input.space input.pages parent)

-- | Where Cancel navigates to: where the page would have been created.
leaving :: Input -> Route
leaving input = case input.parent of
  Just parent -> pageRoute spacePath parent
  Nothing -> Space spacePath
  where
    spacePath = spacePathOf input.context.spaces input.space.id

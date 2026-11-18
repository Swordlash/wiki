{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'DeckViewer' is a viewer of a presentation in a page (WP5.97 of
-- docs/PLAN.md): one slide at a time, as a card of the deck's shape and
-- look, with buttons to the first, the previous, the next and the last
-- step, the slide's number out of the deck's, and "Present", which opens
-- the slides view there.
--
-- A page embeds one with @![Caption](page){.presentation}@
-- ('Wiki.Core.Markdown.Render.deckEmbed'). The viewer's first render is
-- the link the embed is without the class, the same HTML, which is what
-- the prerendered page shows. Only in a browser, after the page has been
-- adopted, does it fetch the page, through the reader's own backend; a
-- presentation is then shown in the link's place. The server's render and
-- the static prerender, which run natively, fetch nothing: the link is
-- what they write, and the page they carry for the program holds no
-- embedded page. Anything
-- else (a document, a page the reader may not read, a space they cannot
-- see, a backend that cannot be reached) leaves the link, with no banner
-- and no prompt: the reader sees what they would without the class. The
-- page view of a presentation shows one above the page, fed with the page
-- it has loaded.
--
-- The keys (Left, Right, Home, End) are the viewer's own element's, heard
-- only while the focus is in it; the window's keys are the page's. Its
-- slides are rendered as one document, as the slides view renders them,
-- with no viewer inside: an embed in a viewer is its link, so nesting
-- ends there. Printed, it is the slide shown, whole, without its
-- controls.
module Wiki.App.View.DeckViewer
  ( Input (..)
  , Target (..)
  , Watcher (..)
  , browserWatcher
  , Slots
  , component
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Map.Strict qualified as Map
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Callback, Safety (..), browserJS, freeCallback, mkSyncCallback)
import Halogen.Machine qualified as Machine
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (First, Last, State)
-- With its constructor: the FFI passes the value inside the newtype.
import Web.DOM.Internal.Types (Element (..))
import Web.Event.Event (EventType (..))
import Web.Event.Event qualified as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), UrlOwner)
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Browser (ArticleLink, followArticleLink)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (PageLink (..), Route (..))
import Wiki.App.View.Body (IslandSlots, Place (..))
import Wiki.App.View.Body qualified as Body
import Wiki.App.View.Common (Context (..), documentRoute, spacePathOf)
import Wiki.App.View.DeckViewer.Machine qualified as M
import Wiki.App.View.Slides (deckClasses, slideCard)
import Wiki.Core.Markdown (parseMarkdown)
import Wiki.Core.Notification (PageLocation (..))
import Wiki.Core.Page (Page (attachments, body, kind, title))
import Wiki.Core.PageKind (isPresentation)
import Wiki.Core.Slides (Deck (..), Fit (..), Slide (..), deckOf, showingStep)
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (id))

-- | The viewer's input: the reader's context, the presentation shown, and
-- the caption the embed's link shows.
data Input = Input
  { context :: Context
  , target :: Target
  , caption :: Text
  , ids :: Text
  -- ^ What every id the slides write starts with, and every link to a
  -- place in them: the viewer's own, apart from the page's and from any
  -- other viewer's on it.
  , watcher :: Watcher
  -- ^ How the viewer learns that its link came near the window: in a
  -- page, the browser's ('browserWatcher').
  }

-- | 'Watcher' is how the viewer learns that its link came near the
-- window. 'start' watches an element and runs the action once when it
-- comes near, and gives the watch's number and what lets go of what the
-- watch holds when the viewer closes; 'stop' stops a watch by its number.
-- Neither waits: both run in the step that keeps the watch. A test gives
-- one of its own and says when the link is near.
data Watcher = Watcher
  { start :: Element -> IO () -> IO (Int, IO ())
  , stop :: Int -> IO ()
  }

-- | What the viewer shows.
data Target
  = -- | An embedded page, by its link, and the place of the page that
    -- embeds it: fetched, and shown if it is a presentation the reader
    -- may read. What that page has already (its space, its space's list of
    -- pages) is not asked for again.
    Linked PageLink Place
  | -- | The page view's own presentation, already loaded, in its place.
    Shown Place Page

type Slots = IslandSlots

-- | The viewer's machine ("Wiki.App.View.DeckViewer.Machine"), for this
-- view: a reading's session is the context's number of it (the same user
-- out and in again is another), its page the embed's link; a
-- read needs the place of the page that embeds it; what is shown is a
-- page in its place.
type Model = M.Model Int PageLink Place (Place, Page)

type Event = M.Event Int PageLink Place (Place, Page)

data State = State
  { input :: Input
  , machine :: Machine.Machine () Event Model
  -- ^ What the viewer knows ("Wiki.App.View.DeckViewer.Machine"), run by
  -- "Halogen.Machine": every change to it is a step.
  , root :: H.RefLabel
  , nearIds :: Map Int Int
  -- ^ The browser's watch of each watch the model runs, by its number.
  , callbacks :: [IO ()]
  -- ^ The callbacks the watches tell through, each freed once, when the
  -- viewer closes: a watch stopped, or one that told, never calls its
  -- callback again, and freeing one while it runs would break it.
  , owners :: Map Int UrlOwner
  -- ^ The owner of each read's addresses ("Wiki.App.Backend"'s
  -- 'UrlOwner'), by the read's number, until the model lets it go.
  }

data Action = Initialize | Finalize | Receive Input | Happen Event | FollowLink ArticleLink

$( browserJS
     ["jsbits/deck-viewer.js"]
     [ ("js_when_near", "wiki_deck_when_near", Safe, [t|Element -> Callback -> IO Int|])
     , ("js_stop", "wiki_deck_stop", Safe, [t|Int -> IO ()|])
     ]
 )

component :: forall query m. (MonadLoad m, MonadNotifications m, MonadIslands m, MonadUUID m, MonadIO m) => H.Component query Input Void m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          root <- H.newRefLabel "deck-viewer"
          pure State {input, machine = Machine.new (M.initial (targetOf input)), root, nearIds = mempty, callbacks = [], owners = mempty}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.receive = Just . Receive
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              }
      }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render st = case (Machine.current st.machine).loaded of
      M.Link -> linkOf st.root st.input
      M.Ready (place, page) deck ->
        let count = length deck.slides
            current = (Machine.current st.machine).current
            shownStep = (Machine.current st.machine).shownStep
            shown = [showingStep (if n == current then shownStep else s.steps) s | (n, s) <- zip [1 ..] deck.slides]
            rendered = Body.slidesWithIds st.input.ids place deck {slides = shown}
            present = Router.href st.input.context.router (documentRoute st.input.context place.space.id (PageSlides (spacePathOfPlace place) place.path current shownStep))
            atStart = current == 1 && shownStep == 1
            atEnd = current >= count && shownStep >= M.stepsAt deck current
         in [hh|
              <div
                ref={{ st.root }}
                class="deck-embed {{ deckClasses deck }}"
                tabindex="0"
                role="region"
                {{ [HP.attr (HH.AttrName "aria-label") (page.title <> ", as slides")] }}
                {{ HE.preventingDefault (HE.handler' (EventType "keydown") keyMove) }}
                {{ Body.followingLinks FollowLink place }}
              >
                {% for ((n, s), html) in zip (zip [1 ..] deck.slides) rendered %}
                  {{ slideCard page.title count n (n == current) Unmeasured s html }}
                {% end %}
                <div class="deck-embed-controls">
                  <button type="button" class="deck-embed-first" title="The first slide" disabled={{ atStart }} onClick={{ const (Happen (M.Go M.First)) }}>⏮</button>
                  <button type="button" class="deck-embed-back" title="Back a step" disabled={{ atStart }} onClick={{ const (Happen (M.Go M.Back)) }}>◀</button>
                  <span class="deck-embed-position">{{ show current <> " / " <> show count :: Text }}</span>
                  <button type="button" class="deck-embed-forward" title="On a step" disabled={{ atEnd }} onClick={{ const (Happen (M.Go M.Forward)) }}>▶</button>
                  <button type="button" class="deck-embed-last" title="The last slide" disabled={{ atEnd }} onClick={{ const (Happen (M.Go M.Last)) }}>⏭</button>
                  <a class="button deck-embed-present" href={{ present }}>Present</a>
                </div>
              </div>
            |]

    handleAction :: Action -> H.HalogenM State Action Slots Void m ()
    handleAction = \case
      -- An embedded page is read only in a browser, once the page has been
      -- adopted (a child is initialized after the tree's first render),
      -- and only when the link comes near the window: the server's render
      -- and the static prerender run natively, where the viewer is the
      -- link and asks the backend for nothing, so the page carries no
      -- embedded page and adoption repairs nothing.
      Initialize -> happen M.Started
      Finalize -> do
        Machine.finish machineSpec M.Finished
        gets (\st -> st.callbacks) >>= traverse_ (H.liftEffect . liftIO)
        modify (\st -> st {callbacks = []})
      Receive new -> do
        modify (\st -> st {input = new})
        happen (M.Received (targetOf new))
      Happen event -> happen event
      FollowLink target -> do
        router <- gets (\st -> st.input.context.router)
        H.liftEffect (followArticleLink router target)

    happen :: Event -> H.HalogenM State Action Slots Void m ()
    happen = Machine.happen machineSpec

    -- The viewer's machine, run by "Halogen.Machine": one step in one
    -- step of the program, then its effects, with no other event of the
    -- viewer in between. Each effect is done in place ('Machine.Now', which
    -- cannot wait: a watch started or stopped, an owner let go, a channel
    -- to tell 'M.Rendered' after this action), or is the read of the page
    -- ('Machine.AskWith'), whose owner of addresses is made and kept in
    -- the state in place before the read starts. So a viewer closed while
    -- a watch starts finds it kept, a change of reader finds the watch it
    -- stops, and a viewer closed while the read is out releases its owner
    -- (#275's faults, which came from waits here).
    machineSpec :: Machine.Spec () State Action Slots Void m Event (M.Effect PageLink Place) Model
    machineSpec = Machine.Spec {Machine.step = M.step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    carry :: M.Effect PageLink Place -> Machine.Carry () State Action Slots Void m Event
    carry = \case
      -- The link watched until it comes near the window, in a browser.
      M.Watch w -> Machine.Now $ do
        st <- get
        Sync.getRef st.root
          >>= traverse_
            ( \element -> do
                channel <- Sync.sync (liftIO HS.create)
                void (Sync.subscribe channel.emitter)
                (i, freeIt) <- Sync.sync (liftIO (st.input.watcher.start element (HS.notify channel.listener (Happen (M.Near w)))))
                modify (\s -> s {nearIds = Map.insert w i s.nearIds, callbacks = freeIt : s.callbacks})
            )
      M.Unwatch w -> Machine.Now $ do
        st <- get
        traverse_ (Sync.sync . liftIO . st.input.watcher.stop) (Map.lookup w st.nearIds)
        modify (\s -> s {nearIds = Map.delete w s.nearIds})
      -- The read's owner of addresses is made and kept in place, before
      -- the read starts with it: 'Finalize' releases it if the viewer
      -- closes while the read is out. What either part throws is the
      -- read's answer, the link.
      M.Fetch f named host ->
        Machine.AskWith
          ( do
              owner <- Sync.sync newUrlOwner
              modify (\s -> s {owners = Map.insert f owner s.owners})
              context <- gets (\st -> st.input.context)
              pure (owner, context)
          )
          (\(owner, context) -> Machine.run (attempt (fetchDeck context owner host named)))
          (M.Fetched f . either (const Nothing) (either (const Nothing) identity))
      M.Release f -> Machine.Now $ do
        gets (\st -> Map.lookup f st.owners) >>= traverse_ (Sync.sync . releaseUrls)
        modify (\s -> s {owners = Map.delete f s.owners})
      -- The element the ref holds until this action ends is the viewer's,
      -- being removed: the link is watched in an action of its own.
      M.WhenRendered -> Machine.Now $ do
        channel <- Sync.sync (liftIO HS.create)
        void (Sync.subscribe channel.emitter)
        Sync.sync (liftIO (HS.notify channel.listener (Happen M.Rendered)))

    -- Which of the viewer's keys a keydown is: the arrows, Home and End,
    -- without a modifier. Any other key is left to the browser.
    keyMove :: Web.Event -> Maybe Action
    keyMove event
      | pressed.ctrlKey || pressed.metaKey || pressed.altKey || pressed.shiftKey = Nothing
      | otherwise =
          Happen . M.Go <$> case pressed.key of
            "ArrowLeft" -> Just M.Back
            "ArrowRight" -> Just M.Forward
            "Home" -> Just M.First
            "End" -> Just M.Last
            _ -> Nothing
      where
        pressed = KeyboardEvent event

-- | What the input asks the viewer to show: an embedded page, for this
-- reading; or the page view's own page, a presentation shown as one.
targetOf :: Input -> M.Target Int PageLink Place (Place, Page)
targetOf input = case input.target of
  Linked named host -> M.Linked M.Reading {session = input.context.sessionNumber, revision = input.context.revision, link = named} host
  Shown place page
    | isPresentation page.kind -> M.Shown (M.Ready (place, page) (deckOfPage page))
    | otherwise -> M.Shown M.Link

-- | The link the embed is without the class: what the viewer shows until
-- it has a presentation to show, and for anything else.
linkOf :: H.RefLabel -> Input -> H.ComponentHTML Action Slots m
linkOf root input = case input.target of
  Linked (PageLink route fragment) _ -> HH.a [HP.ref root, HP.href (Router.href input.context.router route <> fragment)] [HH.text input.caption]
  Shown place page -> HH.a [HP.href (Router.href input.context.router (Page (spacePathOfPlace place) place.path))] [HH.text page.title]

deckOfPage :: Page -> Deck
deckOfPage page = either (const (deckOf [])) deckOf (parseMarkdown page.body)

-- | The path of the place's space, as the reader's spaces give it.
spacePathOfPlace :: Place -> [Slug]
spacePathOfPlace place = spacePathOf place.context.spaces place.space.id

-- | Fetch an embedded page, as the reader may read it: a presentation is
-- shown, anything else is the link. Its space is the one the reader's
-- spaces hold, and a page of the embedding page's own space takes that
-- page's list of pages: neither is asked for again.
fetchDeck :: (MonadWiki n, MonadNotifications n) => Context -> UrlOwner -> Place -> PageLink -> n (Maybe ((Place, Page), Deck))
fetchDeck context owner host (PageLink route _) = case route of
  Page sp pp -> fetchAt sp pp
  -- A version's permanent address (as "Copy permanent link" gives it)
  -- names that version, which the viewer does not show: it is the link.
  PermanentLink pid Nothing -> pageLocation pid >>= either (const (pure Nothing)) (\at -> fetchAt at.space at.page)
  _ -> pure Nothing
  where
    known sp = head [s | s <- context.spaces, spacePathOf context.spaces s.id == sp]
    fetchAt sp pp =
      maybe (getSpace sp) (pure . Right) (known sp) >>= \case
        Left _ -> pure Nothing
        Right space ->
          getPage space.id pp >>= \case
            Right page | isPresentation page.kind -> do
              pages <- if space.id == host.space.id then pure host.pages else either (const []) identity <$> listPages space.id
              addresses <- Body.attachmentAddresses owner page.attachments
              people <- Body.askPeople space (Body.unasked Body.noPeople [page.body])
              let place = Place {context, space, pages, path = pp, attachments = page.attachments, urls = addresses.urls, downloads = addresses.downloads, frames = addresses.frames, people}
              pure (Just ((place, page), deckOfPage page))
            _ -> pure Nothing

-- | The browser's watch: an @IntersectionObserver@ ("jsbits/deck-viewer.js"),
-- telling through a callback freed when the viewer closes: a watch
-- stopped, or one that told, never calls its callback again, and freeing
-- one while it runs would break it. Natively it watches nothing.
browserWatcher :: Watcher
browserWatcher =
  Watcher
    { start = \element tell -> do
        callback <- mkSyncCallback (const tell)
        i <- js_when_near element callback
        pure (i, freeCallback callback)
    , stop = js_stop
    }

{-# LANGUAGE QuasiQuotes #-}

-- | The preview of a page being edited, in a window of its own (WP5.36 of
-- docs/PLAN.md): "Detach preview" in the editor opens it at the page's
-- @live-preview@ address, so that the editor can fill one screen and the
-- preview another.
--
-- The window shows the page as saved until the editor of the page speaks.
-- The editor's CodeMirror sends what is typed, and the line being written,
-- on the browser's channel between the wiki's windows ('listenLive'), with
-- no server between them; this window renders the text with the editor's
-- own preview ("Wiki.App.View.Preview"), and scrolls to the block of that
-- line ('followLine'). When the editor goes (saved and closed, cancelled,
-- its window closed), the window says so and keeps the last text, but in
-- an encrypted space: there it shows nothing more, since the editor that
-- fed it may have gone because its reader left the space, signed out, or
-- closed a space that asks each time it is opened, and this window is not
-- to keep the page's text open on the screen after it. A window
-- opened, or loaded again, while the editor is open asks it for what it
-- has, and follows it from there. When a file is uploaded in the editor,
-- the window fetches the page's attachments again. In the demo, whose wiki
-- each tab keeps in memory from what was stored when it loaded, the window
-- does not see a file uploaded after it opened until it is reloaded.
--
-- = Laid out as the page
--
-- The window is there for the writer to see the page as its readers will,
-- so it lays the text out with the page view's own markup and classes
-- ("Wiki.App.View.Reading"): the page's frame, at the width the reader
-- chose, with the same switch, Compact and Full width, in the window's
-- line at the top; the title's row; a template's note and a blog post's
-- date and neighbours, from the page as saved; the "Contents" panel beside
-- the text when the text shown has enough headings and places no
-- @[[!toc]]@; and the attachments under it. The panel's entries are worked
-- out from the text shown, each time that text changes, so they follow
-- what is typed. The page's actions, its comments, who reads it and who
-- wrote it are left out: they are about the page as saved, and need asks
-- the window does not make.
module Wiki.App.View.LivePreview
  ( Input (..)
  , Listener (..)
  , browserListener
  , component
  , liveKey
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Row (type (.==))
import Data.Text qualified as T
import Data.Time (UTCTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Router (Follow)
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), UrlOwner, WikiError, noUrlOwner)
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Browser (ArticleLink, followArticleLink)
import Wiki.App.Editor.Browser (followLine, listenLive, stopLive)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route)
import Wiki.App.View.Body qualified as Body
import Wiki.App.View.Common
import Wiki.App.View.Preview qualified as Preview
import Wiki.App.View.Reading qualified as Reading
import Wiki.Core.Attachment (Attachment (..))
import Wiki.Core.Id (PageId, SpaceId (..))
import Wiki.Core.Markdown qualified as Md
import Wiki.Core.Page (Page (..), PageSummary)
import Wiki.Core.Slug (Slug, slugText)
import Wiki.Core.Space (Space (encryption, id))

-- | The window's input: the page, where it is, and how the window hears
-- the page's editor.
data Input = Input
  { context :: Context
  , space :: Space
  , pages :: [PageSummary]
  , path :: [Slug]
  , listener :: Listener
  }

-- | 'Listener' is how the window hears the editor of its page. 'listen'
-- starts listening for the page a key names ('liveKey'), gives each
-- message, as JSON, to the handler, and returns what stops it. It does not
-- wait, so that it runs in the step that keeps what stops it. The shell
-- gives 'browserListener'; a native test, which has no browser, gives one
-- of its own and speaks through it as the editor would.
newtype Listener = Listener
  { listen :: Text -> (Text -> IO ()) -> IO (IO ())
  }

-- | The browser's channel between the wiki's windows, which the editor's
-- CodeMirror speaks on ("Wiki.App.Editor.Browser"'s 'listenLive').
browserListener :: Listener
browserListener = Listener {listen = \key heard -> stopLive <$> listenLive key heard}

-- | Where the text shown comes from.
data Source
  = -- | The page as saved: no editor of it has spoken.
    Saved
  | -- | The editor, as typed.
    Editor
  | -- | The editor, which has gone; the text as last typed.
    Gone
  deriving stock (Eq)

data Shown
  = Loading
  | Failed WikiError
  | -- | A title and a text, with the entries of the "Contents" panel the
    -- text shows, if it shows one: worked out when the text is taken
    -- ('showing'), not at each render.
    Shows Source Text Text (Maybe [Md.Entry])
  | -- | The editor of a page of an encrypted space has gone: nothing of
    -- the page is shown.
    Closed

-- | A text to show, from where it came, with its "Contents" panel's
-- entries ('Md.contentsPanel'), as the page view works them out for the
-- page it loads.
showing :: Source -> Text -> Text -> Shown
showing source title body = Shows source title body (either (const Nothing) Md.contentsPanel (Md.parseMarkdown body))

-- | 'Facts' is what the window shows of the page as saved besides its
-- text: its id and when it was created (a blog post's date and its
-- neighbours) and whether it is a template (the note above it). Read with
-- the page; the editor sends none of them.
data Facts = Facts
  { page :: PageId
  , createdAt :: UTCTime
  , template :: Bool
  }

data State = State
  { input :: Input
  , shown :: Shown
  , facts :: Maybe Facts
  , feed :: Maybe (IO (), H.SubscriptionId)
  -- ^ What stops the listening for the editor, and the subscription its
  -- messages come through.
  , article :: H.RefLabel
  , attachments :: [Attachment]
  -- ^ The page's attachments, and their addresses: what its links to them
  -- are resolved with. Fetched again each time the editor uploads a file.
  , addresses :: Body.Addresses
  , owners :: Body.Owners
  -- ^ Who holds those addresses ("Wiki.App.View.Body"'s 'Body.Owners'):
  -- one owner for each page the window shows, registered before anything
  -- is asked under it, and let go when the window shows another page or
  -- closes.
  , page :: Int
  -- ^ How many pages this window has shown: an answer to a read made for
  -- an earlier one is dropped.
  , refreshes :: Int
  -- ^ How many times the attachments were fetched again: only the answer
  -- to the last is kept, whichever arrives first.
  , heardPage :: Maybe PageId
  -- ^ The id of the page whose text the editor sent last ('Heard'), which
  -- the text shown from the editor is of ('textPage').
  , fetched :: Bool
  -- ^ Whether the saved page has been fetched: at once in the initial
  -- state when the backend can, so that the first render matches the
  -- server's prerender, and otherwise in 'Initialize'.
  }

-- | A message of the editor: the text and the line written, that a file
-- was uploaded to the page, or that the editor has gone.
data Heard = Heard
  { page :: Maybe PageId
  -- ^ The id of the page whose text it is, which the editor sends with
  -- the text (web/src/editor.js); 'Nothing' from an editor that sends none.
  , text :: Maybe Text
  , title :: Maybe Text
  , line :: Maybe Int
  , uploaded :: Bool
  , closed :: Bool
  }

instance FromJSON Heard where
  parseJSON = withObject "message" $ \o ->
    Heard <$> o .:? "page" <*> o .:? "text" <*> o .:? "title" <*> o .:? "line" <*> o .:? "attachments" .!= False <*> o .:? "closed" .!= False

-- | The page as saved: what the window shows until the editor speaks, its
-- facts, and the attachments its links resolve to, with their addresses.
data Fetched = Fetched Shown (Maybe Facts) [Attachment] Body.Addresses

fetchSaved :: (MonadWiki m) => UrlOwner -> Input -> m Fetched
fetchSaved owner input =
  getPage input.space.id input.path >>= \case
    Left e -> pure (Fetched (Failed e) Nothing [] Body.noAddresses)
    Right page -> do
      addresses <- Body.attachmentAddresses owner page.attachments
      pure (Fetched (showing Saved page.title page.body) (Just Facts {page = page.id, createdAt = page.createdAt, template = page.template}) page.attachments addresses)

data Action
  = Initialize
  | Finalize
  | Receive Input
  | Listened Text
  | ChooseWidth PageWidth
  | FollowLink ArticleLink
  | Followed (Follow Route)

type Slots = ("preview" .== H.Slot (Const Void) Preview.Output ())

-- | The name of a page on the channel between the wiki's windows: its
-- space's id and its path. The editor's CodeMirror is given the same
-- ("Wiki.App.View.Editor").
liveKey :: SpaceId -> [Slug] -> Text
liveKey (SpaceId sid) path = sid <> "/" <> T.intercalate "/" (map slugText path)

-- | 'textPage' is the id of the page whose text the window shows, which
-- the preview asks about before it sends the logins the text mentions
-- (the preview's 'Wiki.App.View.Preview.page'). The page as saved is the
-- page the window read ('Facts'). A text from the editor is the page the
-- editor named with it ('heardPage'), never the page read at the same
-- address: the edited page may have moved since, and another page taken
-- its address. A text the editor sent without an id is of no page, and
-- nobody it mentions is looked up in a plain space.
textPage :: Shown -> Maybe Facts -> Maybe PageId -> Maybe PageId
textPage shown facts heard = case shown of
  Shows Saved _ _ _ -> (\f -> f.page) <$> facts
  Shows _ _ _ _ -> heard
  _ -> Nothing

component :: forall query m. (MonadLoad m, MonadNotifications m, MonadIslands m, MonadUUID m, MonadIO m) => H.Component query Input ViewOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          article <- H.newRefLabel "live-preview"
          found <- atOnce (fetchSaved noUrlOwner input)
          pure $ case found of
            Just (Fetched shown facts attachments addresses) -> State {input, shown, facts, feed = Nothing, article, attachments, addresses, owners = Body.ownersOf noUrlOwner, page = 0, refreshes = 0, heardPage = Nothing, fetched = True}
            Nothing -> State {input, shown = Loading, facts = Nothing, feed = Nothing, article, attachments = [], addresses = Body.noAddresses, owners = Body.ownersOf noUrlOwner, page = 0, refreshes = 0, heardPage = Nothing, fetched = False}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.receive = Just . Receive, H.initialize = Just Initialize, H.finalize = Just Finalize}
      }
  where
    -- The page view's frame, with the window's line in the place of the
    -- page's bar of actions: what the window follows, and the width.
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      [hh|
        <div class="live-preview page-view {{ Reading.widthClass width }}">
          <div class="actions page-actions live-preview-bar"><p class="live-preview-state" role="status">{{ stateText st.shown }}</p>{{ Reading.widthSwitch ChooseWidth width }}</div>
          {{ bodyOf st }}
        </div>
      |]
      where
        width = st.input.context.pageWidth

    -- What the page view shows of a page as it reads it, in its order,
    -- each part the page view's own ("Wiki.App.View.Reading"); the text in
    -- the editor's preview, which marks each block's line to follow. Every
    -- part is the text's: none is left once the text is gone ('Closed').
    -- The text's element is @live-preview-body@ in every state.
    bodyOf :: State -> [H.ComponentHTML Action Slots m]
    bodyOf st = case st.shown of
      Shows _ title text contents ->
        toList (Reading.templateNote . (\f -> f.template) =<< st.facts)
          <> [Reading.titleRow title text [] | not (Body.startsWithHeading text)]
          <> toList (st.facts >>= \f -> Reading.postInfo Followed st.input.context st.input.space st.input.pages f.page f.createdAt)
          <> [ [hh|
                <div class={{ Reading.mainClasses (isJust contents) False }}>
                  <div class="page-text live-preview-body" ref={{ st.article }}>{{ previewOf st title text }}{{ Reading.textEnd }}</div>
                  {% for entries in maybeToList contents %}
                    <div class="page-side">{{ Reading.contentsPanel FollowLink (place st) entries }}</div>
                  {% end %}
                </div>
              |]
             ]
          <> Reading.attachmentList st.attachments st.addresses
      Failed e -> [[hh|<div class="live-preview-body">{{ errorView e }}</div>|]]
      Loading -> [[hh|<div class="live-preview-body"><p class="loading">Loading…</p></div>|]]
      Closed -> [[hh|<div class="live-preview-body"></div>|]]

    -- The text in the editor's preview, without its title: the title's
    -- row is above the column of the text, as on the page.
    previewOf :: State -> Text -> Text -> H.ComponentHTML Action Slots m
    previewOf st title text = HH.slot_ "preview" () Preview.component Preview.Input {place = place st, page = textPage st.shown st.facts st.heardPage, title, body = text, titled = False, slides = Nothing}

    stateText :: Shown -> Text
    stateText = \case
      Shows Saved _ _ _ -> "The page as saved. Once its editor is open in another window, this follows what is typed there."
      Shows Editor _ _ _ -> "Following the editor."
      Shows Gone _ _ _ -> "The editor was closed: this is the text as it was last typed."
      Closed -> "The editor was closed. This space is encrypted, so nothing of the page is kept here: open the page again to read it."
      _ -> ""

    handleAction :: Action -> H.HalogenM State Action Slots ViewOutput m ()
    handleAction = \case
      Initialize -> do
        -- The channel first, so that an open editor is asked at once, and
        -- what it says is shown whether or not the page has loaded.
        input <- gets (\st -> st.input)
        -- On the loop, without a wait, so that the window cannot be
        -- finalized between listening and keeping what stops it.
        channel <- H.liftEffect (liftIO HS.create)
        subscription <- H.subscribe (Listened <$> channel.emitter)
        stop <- H.liftEffect (liftIO (input.listener.listen (liveKey input.space.id input.path) (HS.notify channel.listener)))
        modify (\st -> st {feed = Just (stop, subscription)})
        fetched <- gets (\st -> st.fetched)
        unless fetched $ do
          shownPage <- gets (\st -> st.page)
          owner <- Body.ensureOwned (\st -> st.owners) (\owners st -> st {owners})
          Fetched saved facts attachments addresses <- H.async (fetchSaved owner input)
          modifyFor shownPage $ \st ->
            st
              { attachments
              , addresses
              , facts
              , fetched = True
              , shown = case (st.shown, saved) of
                  -- The editor spoke while the page was loading: its text,
                  -- under its title, or the page's if it sent none.
                  (Shows source typed body contents, Shows _ title _ _) -> Shows source (if T.null typed then title else typed) body contents
                  (Shows {}, _) -> st.shown
                  (Closed, _) -> Closed
                  _ -> saved
              }
        H.raise Loaded
      Finalize -> do
        Body.releaseOwned (\st -> st.owners) (\owners st -> st {owners})
        stopListening
      Receive input -> do
        old <- gets (\st -> st.input)
        modify (\st -> st {input})
        -- Another page in the same window: its own text and attachments,
        -- and its own editor to listen for.
        when (liveKey old.space.id old.path /= liveKey input.space.id input.path) $ do
          stopListening
          -- The new page's fetch registers its own owner.
          Body.releaseOwned (\st -> st.owners) (\owners st -> st {owners})
          modify (\st -> st {shown = Loading, facts = Nothing, attachments = [], addresses = Body.noAddresses, page = st.page + 1, heardPage = Nothing, fetched = False})
          handleAction Initialize
      Listened raw -> case eitherDecodeStrict @Heard (encodeUtf8 raw) of
        Left _ -> pass
        Right heard
          | heard.closed -> modify $ \st -> case st.shown of
              _ | isJust st.input.space.encryption -> st {shown = Closed}
              Shows _ title body contents -> st {shown = Shows Gone title body contents}
              _ -> st
          | heard.uploaded -> do
              -- The page's attachments as the server has them now. The
              -- page as the server prerendered it is cached until the
              -- first route; this window has no other route, so the cache
              -- is told to fetch from now on.
              H.async endPreload
              modify (\st -> st {refreshes = st.refreshes + 1})
              owner <- Body.ensureOwned (\st -> st.owners) (\owners st -> st {owners})
              State {input, page = shownPage, refreshes = refresh} <- get
              H.async (fetchSaved owner input) >>= \case
                Fetched (Shows {}) _ attachments addresses ->
                  modifyFor shownPage (\st -> if st.refreshes == refresh then st {attachments, addresses} else st)
                -- Not fetched: the attachments as they were.
                _ -> pass
          | otherwise -> do
              for_ heard.text $ \body -> modify $ \st ->
                let kept = case st.shown of
                      Shows _ t _ _ -> t
                      _ -> ""
                    title = fromMaybe kept (mfilter (not . T.null) heard.title)
                 in st
                      { heardPage = heard.page
                      , shown = case st.shown of
                          -- The same text under another title: its entries
                          -- as they were.
                          Shows _ _ same contents | same == body -> Shows Editor title body contents
                          _ -> showing Editor title body
                      }
              for_ heard.line $ \n ->
                gets (\st -> st.article) >>= H.getRef >>= traverse_ (H.liftEffect . (`followLine` n))
      -- The same choice as on a page: the shell keeps it, and gives it back
      -- in the context.
      ChooseWidth width -> H.raise (ChoseWidth width)
      -- An entry of the "Contents" panel is followed as a link in the
      -- text is: scrolled to, with the address's fragment changed in place,
      -- so the route, and the editor followed, stay the same.
      FollowLink target -> do
        router <- gets (\st -> st.input.context.router)
        H.liftEffect (followArticleLink router target)
      Followed f -> gets (\st -> st.input.context.router) >>= (`Router.follow` f)

    -- Change the state with what was read for a page, if the window still
    -- shows that page.
    modifyFor :: Int -> (State -> State) -> H.HalogenM State Action Slots ViewOutput m ()
    modifyFor shownPage f = modify (\st -> if st.page == shownPage then f st else st)

    stopListening :: H.HalogenM State Action Slots ViewOutput m ()
    -- On the loop, without a wait, so that a second change of page cannot
    -- stop the same feed again.
    stopListening =
      gets (\st -> st.feed)
        >>= traverse_
          ( \(stop, subscription) -> do
              modify (\st -> st {feed = Nothing})
              H.unsubscribe subscription
              H.liftEffect (liftIO stop)
          )

    -- Links are resolved from where the page is, as in its editor.
    place :: State -> Body.Place
    place st =
      Body.Place
        { Body.context = st.input.context
        , Body.space = st.input.space
        , Body.pages = st.input.pages
        , Body.path = st.input.path
        , Body.attachments = st.attachments
        , Body.urls = st.addresses.urls
        , Body.downloads = st.addresses.downloads
        , Body.frames = st.addresses.frames
        , Body.people = mempty
        }

-- | The harness every view's native spec uses: haskell-halogen's
-- "Halogen.Test.Native", re-exported, with what is the wiki's own. A view
-- is the real component, mounted on the in-memory DOM in a page of its
-- own, clicked and typed into as a reader does, read with CSS selectors,
-- and waited for through what its program is doing, never by a clock.
--
-- > withView run component input $ \view -> do
-- >   click view ".notifications-toggle"
-- >   settle view
-- >   rows <- findAll view ".notification"
--
-- "Halogen.Test.Native"'s documentation says what each of its names does:
-- 'withView' (with the parent that gives the input, 'setInput', and keeps
-- the outputs, 'outputs'), 'click' (a reader's whole click, with what the
-- browser does by default), 'typeInto', 'press', 'clickOutside', 'find',
-- 'textOf', 'settle' and 'settleHolding' (which give up after ten seconds,
-- saying what the view is doing), a view's own activity ('activity',
-- 'waitActivity'), the 'Gate' a test's backend holds its asks in, and the
-- library's held waits and test clock ("Halogen.Activity.Control").
--
-- What this module adds is what a view of the wiki is given: 'viewContext',
-- and a live channel the test tells through ('TestFeed').
--
-- Each view has a page of its own, with its own document: an event in one
-- view's page never reaches another's listeners, so views run side by side.
-- The address is still the process's one, and the prerender's router
-- ("Wiki.App.Static") writes it beside them: a spec reads where its view
-- went from its own router's 'Halogen.Router.changes', not from the address.
module Test.Harness.MemDom
  ( -- * The library's harness
    module Halogen.Test.Native

    -- * What the shell gives a view
  , viewContext
  , viewContextAs
  , TestFeed (..)
  , newTestFeed
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Halogen.Router (Router)
import Halogen.Subscription qualified as HS
import Halogen.Test.Native
import Protolude hiding (find)
import Wiki.Api.Plugins (noPlugins)
import Wiki.Api.Plugins.External (noExternalPlugins)
import Wiki.App.Backend (Session, anonymousIn)
import Wiki.App.Live (Feed (..), Heard)
import Wiki.App.Route (Route)
import Wiki.App.View.Common (Context (..), PageWidth (..))
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (PageId)
import Wiki.Core.Space (Space)

-- | What the shell gives a view ('Context'), as a native spec gives it:
-- this router, nobody signed in on an open wiki, no spaces, at a
-- revision. Every native spec builds its context here, so that a field
-- the context gains is given in one place.
viewContext :: Router Route -> Int -> Context
viewContext router = viewContextAs router (anonymousIn Open) []

-- | 'viewContext' for someone signed in ('session', as the backend
-- answers it), with the wiki's spaces.
viewContextAs :: Router Route -> Session -> [Space] -> Int -> Context
viewContextAs router session spaces revision = Context {router, session, sessionNumber = 0, spaces, icons = mempty, revision, pageWidth = Compact, plugins = noPlugins, external = noExternalPlugins, optionsRefused = mempty, pluginsRead = True}

-- | 'TestFeed' is a live channel the test tells through ('Wiki.App.Live'):
-- the feed a view is given, how to deliver a message to it, and what the
-- view asked of it: how many listen ('listen' less 'unlisten') and the
-- pages watched ('watch' less 'unwatch').
data TestFeed = TestFeed
  { feed :: Feed
  , tell :: Heard -> IO ()
  , listening :: IORef Int
  , watching :: IORef [PageId]
  }

newTestFeed :: IO TestFeed
newTestFeed = do
  channel <- HS.create
  listening <- newIORef (0 :: Int)
  watching <- newIORef []
  let count d = atomicModifyIORef' listening (\n -> (n + d, ()))
      feed =
        Feed
          { heard = channel.emitter
          , watch = \p -> atomicModifyIORef' watching (\ps -> (p : ps, ()))
          , unwatch = \p -> atomicModifyIORef' watching (\ps -> (deleteFirst p ps, ()))
          , restart = pass
          , startEditing = \_ -> pass
          , stopEditing = \_ -> pass
          , editorsOf = \_ -> pure Nothing
          , readersOf = \_ -> pure Nothing
          , listen = count 1
          , unlisten = count (-1)
          }
  pure TestFeed {feed, tell = HS.notify channel.listener, listening, watching}
  where
    deleteFirst p ps = let (before, after) = break (== p) ps in before <> drop 1 after

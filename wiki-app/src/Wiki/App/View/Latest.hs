-- | The one rule for answers that come late (WP5.95 of docs/PLAN.md): an
-- answer of a kind is taken only if no newer answer of that kind was
-- taken already.
--
-- The rule's tickets are haskell-halogen's "Halogen.Latest", which this
-- module re-exports. Its documentation says how a ticket is taken and
-- compared, each in one step of the view's program, why a failure is an
-- answer like any other, and how 'madeAfter' and 'stillAnswerable' keep
-- what a view learnt another way (the live channel) against a snapshot of
-- the asks. This module adds what is the wiki's own.
--
-- What "newer" means depends on the answer:
--
-- * A page has an order of its own, its version ('takesPage'). A later
--   ask can return an older version: a fetch asked for before a save and
--   answered after it. So the page's version decides, whatever the order
--   of the asks.
-- * An answer with no order of its own (a page's threads, the pages that
--   link to a page, a space's pages, who is signed in and the spaces) is
--   ordered by its ask, with a ticket of "Halogen.Latest": the answer to a
--   later ask is newer than the answer to an earlier one.
module Wiki.App.View.Latest
  ( Asks
  , Ask
  , noAsks
  , newAsk
  , takes
  , madeAfter
  , stillAnswerable
  , supersede
  , asking
  , answered
  , superseded
  , takesPage
  )
where

import Halogen.Latest (Ask, Asks, answered, asking, madeAfter, newAsk, noAsks, stillAnswerable, supersede, takes)
import Protolude
import Wiki.Core.Page (Page (id, updatedAt))

-- | The state with something newer than every answer of the kind put in
-- its place, as 'supersede' says, and the change that puts it there, in
-- one step.
superseded :: (st -> Asks) -> (Asks -> st -> st) -> (st -> st) -> st -> st
superseded get' set' change st = change (set' (supersede (get' st)) st)

-- | Whether a view takes a page it was given (a fetch's answer, a save's),
-- with the page it shows: not when the two are the same page (the same
-- 'Page.id') and the one given is an older version.
--
-- A version is the page's @updatedAt@, which the backend sets on every
-- save of the title or the text, later than the one before. Restoring an
-- old version saves it again, as a newer version, which is taken. A page
-- deleted and made again at the same address has another id, and is
-- taken. The same version is taken: it has the same title and text.
-- What a save does not version (the page's place in the tree, the
-- template mark, the attachments) comes as the answer has it, which a
-- late answer may have older.
--
-- So the order the answers come in cannot put an older version back: a
-- fetch asked for before a save and answered after it is dropped.
takesPage :: Maybe Page -> Page -> Bool
takesPage shown given = case shown of
  Just page | page.id == given.id -> given.updatedAt >= page.updatedAt
  _ -> True

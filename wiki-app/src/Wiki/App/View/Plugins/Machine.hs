-- | 'Model' is everything the application knows of which plugins are on,
-- and 'step' is the one place it changes (decision 82 of docs/PLAN.md):
-- the one value of what is on ('known': the plugins off for the wiki and
-- the user's own states), the external plugins installed and who may add
-- them ('installed', "Wiki.Api.Plugins.External"), what the reader asked
-- to change and is not done yet, and why a change was refused. The shell keeps it ("Wiki.App.Shell")
-- and carries out its effects, so that an ask goes on after the view that
-- made it is gone. Every view of the plugins draws it: the editor, through
-- the context ('Wiki.App.Plugin.Registry.pluginsOn'), the settings'
-- section "Plugins" ("Wiki.App.View.Plugins") and the administration's
-- switch ("Wiki.App.View.WikiPlugins"), whose acts are its events. A
-- plugin's own surfaces (its agent in the editor, its settings) change
-- the plugin's options through it too ('OptionsChosen').
--
-- = One ask at a time
--
-- Every ask about the plugins is one flight: the read the shell wants with
-- each load of the session and the spaces ('Refresh'), the read a view
-- wants when it opens on nothing read ('Opened'), the user's writes of
-- their states, and the administrator's saves of the plugins off for the
-- wiki, and the user's writes of a plugin's options. While one of them is
-- out, no other is sent ('flight'), so the
-- server answers them in the order sent and a later answer is never an
-- older one. A write chosen meanwhile waits its turn, in the order chosen
-- ('queued'); a later choice for the same plugin, later options for the
-- same plugin, or a later set of the plugins off for the wiki, takes the
-- place of the one that waits. A read
-- wanted meanwhile is sent when the ask out answers, failed or not, after
-- what waits ('wanted').
--
-- = What is known
--
-- A read is two requests, one after the other in the same flight: what is
-- on ('Load'), then, when its answer comes, the external plugins installed
-- ('LoadInstalled'), before any write that waits. The server answers each
-- in a transaction of its own, so a change made elsewhere between the two
-- is seen by the second only; the next read sees it in both. What is on is
-- read in the session ('readIn') once both have answered in it.
--
-- A read answers with the whole value. A write, of a state or of options,
-- answers with the user's choice for the one plugin written, and a save
-- with the plugins off for
-- the wiki: each is put in by what it answers, and a read follows, since
-- neither answers with the whole value. A write of a state refused is
-- shown on its plugin's row ('refused'), a write of options refused on the
-- plugin's own surfaces ('optionsRefused'), a save refused beside the boxes
-- ('problem'), until a later one succeeds or another choice is made; a
-- read follows them too, since a write whose answer was lost may have been
-- done. A read made after a write is sent after the write's answer came:
-- the HTTP backend's cache, which a write empties when it answers, never
-- answers it with what the write changed.
--
-- = A plugin's options
--
-- While a write of a plugin's options is out or waits, the plugin is given
-- the options last chosen ('writingOptions', 'held'), as the section's
-- switch shows the state last chosen ('writing', 'shownState'): a plugin
-- that set its options sees them at once, and does not see the server's
-- older ones come back with a read in between. Once the last write
-- answers, the plugin is given what the server keeps. A write refused is
-- told to the plugin's surfaces with their options ('optionsRefused'): the
-- surface where the reader made the change says why it was not kept, and
-- shows the server's options again. The refusal is set in the step its
-- answer arrives, and cleared by the next write of the plugin's options
-- that succeeds, or the next options chosen, even the ones shown.
--
-- Options found by the move of a plugin's legacy settings
-- ("Wiki.App.Assistant.Migrate") are adopted only when the server keeps
-- none for the plugin and none were chosen since ('OptionsAdopted'): other
-- options were moved by another browser, or chosen by the user. Before
-- what is on is read in the session, they wait for the read
-- ('adopting'). The write is the server's to decide, too ('AdoptWrite',
-- @If-None-Match: *@): two browsers that both read "none" cannot both
-- write, and a refusal (the server has options) is no failure; the read
-- that follows gives the plugin the server's.
--
-- = External plugins
--
-- The administrator sets who may add external plugins ('PolicyChosen'),
-- and installs, replaces and removes the wiki's; a user, while the policy
-- lets users add theirs, does the same with their own. Each is a write of
-- the one flight, and a read follows its answer, as for the others.
--
-- An upload starts with the files the reader picked. The view reads them
-- and checks them with 'chooseFiles' (the manifest by the server's own
-- 'Wiki.Api.Plugins.External.parseManifest', the bundle by its
-- 'Wiki.Api.Plugins.External.checkBundle'), and gives the result to the
-- model ('FilesPicked'), which numbers the pick ('picked') and keeps it
-- until the reader confirms it after the warning, or drops it. A
-- confirmation ('Confirmed') carries what the reader saw: the pick's
-- number, whose the plugin will be, its id, its bundle's hash, its hosts,
-- and the hash of the bundle it replaces. The upload goes only when that
-- is still the pick the model holds, in the step that takes the
-- confirmation; a pick replaced since, or of an earlier session, uploads
-- nothing. A replacement sends the hash of the bundle its row showed when
-- the reader chose to replace it ('replaces'): when the plugin was
-- replaced or removed since, the server refuses it, and the refusal is
-- shown on the plugin's row ('externalRefused'), not sent again. A removal
-- goes only for a plugin the model holds installed in the step it is
-- chosen, with the hash its row showed.
--
-- A plugin is told apart by its owner and its id ('Owner'), and a version
-- of it by its bundle's hash. An install's answer is the plugin as
-- installed, a removal's nothing, and a policy's the whole list as the
-- administrator then reads it: each is put into 'installed' as it says,
-- so that the editor's frames follow in that step, and a read follows.
--
-- = Who is signed in
--
-- The model is the signed-in user's, by the session's number
-- ("Wiki.App.View.Common"'s @sessionNumber@), which the shell gives it
-- each time it changes ('Given'). The step that takes a new one forgets
-- what is known, what waits its turn and what was refused, and puts the
-- asks out behind it: the shell's load of the new session then reads. It
-- forgets the external plugins installed, the files picked, and the
-- uploads, removals and policy that wait, too. An
-- answer to an ask of the earlier session changes nothing ('ofSession'),
-- and a write chosen in it never goes out. The ask of the earlier session
-- still out stays the flight all the same: the new session's read is sent
-- after its answer, so that the server has handled it, and the read sees
-- what it did. Until the new session's read answers, what is on is the
-- defaults with the plugins off for the wiki as the earlier session knew
-- them ('offBefore', 'held'): the wiki's set is the same for every reader.
module Wiki.App.View.Plugins.Machine
  ( Model (..)
  , Event (..)
  , Effect (..)
  , Write (..)
  , PluginFiles (..)
  , Picked (..)
  , Confirmation (..)
  , Upload (..)
  , Change (..)
  , initial
  , initialWith
  , initialRead
  , step
  , knownNow
  , held
  , readIn
  , settingsReadIn
  , installedNow
  , heldInstalled
  , stateOf
  , shownState
  , shownOptions
  , shownOff
  , shownPolicy
  , chooseFiles
  , askOf
  )
where

import Data.Aeson (Value)
import Data.ByteString.Lazy qualified as BSL
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), WikiPlugins (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy, Owner (..), checkBundle, noExternalPlugins, ownerOf, parseManifest)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.View.Latest (Ask, Asks, madeAfter, newAsk, noAsks, supersede, takes)

-- | 'Model' is the plugins' state.
data Model = Model
  { known :: Maybe (Either WikiError Plugins)
  -- ^ What the server keeps, as the last answers said it, or why it could
  -- not be read; 'Nothing' until the first answer in the session.
  , asks :: Asks
  -- ^ The asks sent, and which one's answer was taken last.
  , flight :: Maybe Ask
  -- ^ The last ask sent, until its answer comes, of this session or an
  -- earlier one. While it is out, no other is sent.
  , wanted :: Bool
  -- ^ Whether a read was wanted while that ask was out.
  , queued :: [(Int, Write)]
  -- ^ The writes chosen while an ask was out, in the order chosen, each
  -- with the session it was chosen in.
  , writing :: Map PluginId PluginState
  -- ^ The state last chosen for each plugin whose write is out or waits.
  , writingOptions :: Map PluginId (Maybe Value)
  -- ^ The options last chosen for each plugin whose write of options is
  -- out or waits: a value, or 'Nothing' to clear them.
  , wantedOff :: Maybe (Set PluginId)
  -- ^ The plugins off for the wiki last ticked, while their save is out or
  -- waits.
  , refused :: Map PluginId WikiError
  -- ^ Why the last write of a plugin's state failed: what its row in the
  -- settings shows.
  , optionsRefused :: Map PluginId WikiError
  -- ^ Why the last write of a plugin's options failed, chosen or adopted:
  -- what the plugin's surfaces are told with their options
  -- ('Wiki.App.View.Common.Context''s @optionsRefused@).
  , problem :: Maybe WikiError
  -- ^ Why the last save of the plugins off for the wiki failed.
  , session :: Int
  -- ^ The number of the session the model was last given.
  , sessionFrom :: Asks
  -- ^ The asks as they were when that session began.
  , offBefore :: Set PluginId
  -- ^ The plugins off for the wiki as the earlier session last knew them,
  -- for what is on until this session has read them ('held').
  , adopting :: Map PluginId Value
  -- ^ The options to adopt for each plugin once what is on is read in
  -- this session ('OptionsAdopted').
  , installed :: Maybe (Either WikiError ExternalPlugins)
  -- ^ The external plugins the reader may run and who may add them, as
  -- the last answers said it, or why they could not be read; 'Nothing'
  -- until the first answer in the session. Nothing of it is held across a
  -- change of reader: a user's own plugins are theirs alone.
  , wantedPolicy :: Maybe ExternalPolicy
  -- ^ The policy last chosen, while its write is out or waits.
  , policyProblem :: Maybe WikiError
  -- ^ Why the last write of the policy failed.
  , picked :: Maybe Picked
  -- ^ The files picked for an upload, until it is confirmed or dropped.
  , picks :: Int
  -- ^ The number the next pick gets. It is never reset, so that no two
  -- picks of the tab have the same.
  , changing :: Map (Owner, PluginId) Change
  -- ^ The plugins whose upload or removal is out or waits.
  , externalRefused :: Map (Owner, PluginId) WikiError
  -- ^ Why the last upload or removal of a plugin failed.
  }
  deriving stock (Eq, Show)

-- | 'PluginFiles' is the two files of an external plugin as 'chooseFiles'
-- checked them: the manifest's text and what it says, and the bundle's
-- bytes and their SHA-256, in lowercase hex, as the server names it.
data PluginFiles = PluginFiles
  { manifest :: ExternalManifest
  , manifestText :: Text
  , bundle :: ByteString
  , hash :: Text
  }
  deriving stock (Eq, Show)

-- | 'Picked' is the files the reader picked for an upload: the pick's
-- number, whose the plugin will be, the plugin it replaces with the hash
-- of the bundle its row showed (or 'Nothing' for a plugin added), and the
-- files as checked, or why they cannot be uploaded, in the words of the
-- rule they break.
data Picked = Picked
  { number :: Int
  , owner :: Owner
  , replacing :: Maybe (PluginId, Text)
  , files :: Either Text PluginFiles
  }
  deriving stock (Eq, Show)

-- | 'Confirmation' is what the reader confirmed after the warning: the
-- pick's number, whose the plugin will be, its id, its bundle's hash, the
-- hosts the warning named, and the hash of the bundle it replaces.
data Confirmation = Confirmation
  { pick :: Int
  , owner :: Owner
  , id :: PluginId
  , hash :: Text
  , hosts :: [Text]
  , replaces :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | 'Upload' is an external plugin to install: whose, the manifest's text,
-- its id, the bundle's bytes, and the hash of the bundle it replaces.
data Upload = Upload
  { owner :: Owner
  , id :: PluginId
  , manifestText :: Text
  , bundle :: ByteString
  , replaces :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | 'Change' is what is out or waits for an external plugin.
data Change = Installing | Removing
  deriving stock (Eq, Show)

-- | 'Write' is a change sent to the server, or waiting its turn.
data Write
  = -- | The user's state for a plugin.
    StateWrite PluginId PluginState
  | -- | The plugins off for the wiki, the whole set.
    OffWrite (Set PluginId)
  | -- | The user's options for a plugin: a value, or 'Nothing' to clear
    -- them.
    OptionsWrite PluginId (Maybe Value)
  | -- | Options found by a move of the plugin's legacy settings, written
    -- only if the server keeps none for the plugin ('OptionsAdopted').
    AdoptWrite PluginId Value
  | -- | Who may add external plugins.
    PolicyWrite ExternalPolicy
  | -- | An external plugin to install.
    InstallWrite Upload
  | -- | An external plugin to remove.
    RemoveWrite Owner PluginId
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the plugins' state.
data Event
  = -- | The shell loads the session: read what is on, in turn.
    Refresh
  | -- | A view of the plugins was opened, or is open in a new session: a
    -- read, unless what is on was read in this session already.
    Opened
  | -- | The shell was given the number of the session signed in.
    Given Int
  | -- | The answer to the read made under this ticket.
    Loaded Ask (Either WikiError Plugins)
  | -- | The answer to the read of the external plugins installed made
    -- under this ticket, the second request of a read.
    LoadedInstalled Ask (Either WikiError ExternalPlugins)
  | -- | The user chose a state for a plugin: on, off, or its default.
    Chosen PluginId PluginState
  | -- | The answer to the write for this plugin sent under this ticket:
    -- the user's choice for it after the write.
    Written Ask PluginId (Either WikiError PluginChoice)
  | -- | A plugin's surface set the plugin's options: a value, or 'Nothing'
    -- to clear them.
    OptionsChosen PluginId (Maybe Value)
  | -- | The answer to the write of a plugin's options sent under this
    -- ticket: the user's choice for it after the write.
    OptionsWritten Ask PluginId (Either WikiError PluginChoice)
  | -- | The answer to the write of adopted options sent under this ticket:
    -- the user's choice after it, or 'Conflict' when the server keeps
    -- options for the plugin already, and wrote nothing.
    AdoptWritten Ask PluginId (Either WikiError PluginChoice)
  | -- | The move of a plugin's legacy settings, started in the session of
    -- this number, found these options: kept if the server keeps none for
    -- the plugin and none were chosen since.
    OptionsAdopted Int PluginId Value
  | -- | The administrator ticked a plugin's box "off for everyone"
    -- ('True'), or cleared it.
    Ticked PluginId Bool
  | -- | The answer to the save sent under this ticket.
    Saved Ask (Either WikiError WikiPlugins)
  | -- | The administrator chose who may add external plugins.
    PolicyChosen ExternalPolicy
  | -- | The answer to the write of the policy sent under this ticket: the
    -- plugins as the administrator then reads them.
    PolicySaved Ask (Either WikiError ExternalPlugins)
  | -- | The reader picked, in the session of this number, the files of a
    -- plugin for this owner, to add it or to replace the plugin of this id
    -- whose row showed this hash, and the view read and checked them
    -- ('chooseFiles'). Files read for an earlier session are dropped.
    FilesPicked Int Owner (Maybe (PluginId, Text)) (Either Text PluginFiles)
  | -- | The reader dropped the files picked.
    PickDropped
  | -- | The reader confirmed the upload after the warning.
    Confirmed Confirmation
  | -- | The answer to the upload of this owner's plugin of this id sent
    -- under this ticket.
    Uploaded Ask Owner PluginId (Either WikiError ExternalPlugin)
  | -- | The reader chose to remove this owner's plugin of this id, whose
    -- row showed this hash.
    RemoveChosen Owner PluginId Text
  | -- | The answer to the removal sent under this ticket.
    Removed Ask Owner PluginId (Either WikiError ())
  deriving stock (Show)

-- | 'Effect' is what a step asks the shell to do.
data Effect
  = -- | Read what is on; the answer comes back as 'Loaded'.
    Load Ask
  | -- | Read the external plugins installed; the answer comes back as
    -- 'LoadedInstalled'.
    LoadInstalled Ask
  | -- | Set the user's state for a plugin; the answer comes back as
    -- 'Written'.
    WriteState Ask PluginId PluginState
  | -- | Set the user's options for a plugin, or clear them ('Nothing');
    -- the answer comes back as 'OptionsWritten'.
    WriteOptions Ask PluginId (Maybe Value)
  | -- | Set the user's options for a plugin only if the server keeps none;
    -- the answer comes back as 'AdoptWritten'.
    WriteOptionsIfNone Ask PluginId Value
  | -- | Set the plugins off for the wiki; the answer comes back as 'Saved'.
    SaveOff Ask (Set PluginId)
  | -- | Set who may add external plugins; the answer comes back as
    -- 'PolicySaved'.
    SetPolicy Ask ExternalPolicy
  | -- | Install an external plugin; the answer comes back as 'Uploaded'.
    Install Ask Upload
  | -- | Remove an owner's external plugin; the answer comes back as
    -- 'Removed'.
    Remove Ask Owner PluginId
  deriving stock (Eq, Show)

-- | The plugins' state for a session, before anything is read.
initial :: Int -> Model
initial = initialWith Nothing

-- | The plugins' state for a session, with what the first request of a
-- read made before the program started answered, if it did, and the
-- external plugins installed not read.
initialWith :: Maybe Plugins -> Int -> Model
initialWith read' = initialRead read' Nothing

-- | The plugins' state for a session, with what the two requests of a read
-- made before the program started answered (the shell's first load), if
-- they did.
initialRead :: Maybe Plugins -> Maybe ExternalPlugins -> Int -> Model
initialRead read' readInstalled session =
  Model
    { known = Right <$> read'
    , asks = noAsks
    , flight = Nothing
    , wanted = False
    , queued = []
    , writing = mempty
    , writingOptions = mempty
    , wantedOff = Nothing
    , refused = mempty
    , optionsRefused = mempty
    , problem = Nothing
    , session
    , sessionFrom = noAsks
    , offBefore = mempty
    , adopting = mempty
    , installed = Right <$> readInstalled
    , wantedPolicy = Nothing
    , policyProblem = Nothing
    , picked = Nothing
    , picks = 0
    , changing = mempty
    , externalRefused = mempty
    }

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Refresh -> reading m
  Opened
    | isNothing (knownNow m) || isNothing (installedNow m) -> reading m
    | otherwise -> (m, [])
  Given session
    | session /= m.session ->
        let behind = supersede m.asks
         in ((initial session) {asks = behind, sessionFrom = behind, flight = m.flight, offBefore = (held m).offForWiki, picks = m.picks}, [])
    | otherwise -> (m, [])
  Loaded asked answer
    | ofSession asked m ->
        let (m1, effects) = secondRequest asked (taking asked answer m)
         in second (effects <>) (adoptWaiting m1)
    | otherwise -> landed asked m
  LoadedInstalled asked answer
    | ofSession asked m -> landed asked (takingInstalled asked answer m)
    | otherwise -> landed asked m
  Chosen p wanted'
    | shownState m p == wanted' -> (m, [])
    | otherwise -> writingNow (StateWrite p wanted') m {writing = Map.insert p wanted' m.writing, refused = Map.delete p m.refused}
  Written asked _ _
    | not (ofSession asked m) -> landed asked m
  Written asked p answer ->
    let waits = any (\(_, w) -> w `writes` p) m.queued
     in wrote asked p answer m {writing = if waits then m.writing else Map.delete p m.writing}
  OptionsChosen p wanted'
    -- The options shown already: nothing to write, but the choice is the
    -- plugin's last, and a refusal of earlier options is told no more.
    | shownOptions m p == wanted' -> (m {optionsRefused = Map.delete p m.optionsRefused}, [])
    | otherwise -> writingNow (OptionsWrite p wanted') m {writingOptions = Map.insert p wanted' m.writingOptions, optionsRefused = Map.delete p m.optionsRefused}
  OptionsWritten asked _ _
    | not (ofSession asked m) -> landed asked m
  OptionsWritten asked p answer ->
    let waits = any (\(_, w) -> w `writesOptions` p) m.queued
     in wroteOptions asked p answer m {writingOptions = if waits then m.writingOptions else Map.delete p m.writingOptions}
  AdoptWritten asked _ _
    | not (ofSession asked m) -> landed asked m
  AdoptWritten asked p answer ->
    let waits = any (\(_, w) -> w `writesOptions` p) m.queued
        m1 = m {writingOptions = if waits then m.writingOptions else Map.delete p m.writingOptions}
     in case answer of
          -- The server keeps options already: nothing to say, and the read
          -- that follows gives them.
          Left (Conflict _) -> landed asked m1 {wanted = True}
          _ -> wroteOptions asked p answer m1
  OptionsAdopted session p v
    | session /= m.session -> (m, [])
    | isNothing (knownNow m) -> (m {adopting = Map.insert p v m.adopting}, [])
    | otherwise -> adopt p v m
  Ticked p isOff -> case shownOff m of
    Just now'
      | Set.member p now' /= isOff ->
          let next = (if isOff then Set.insert else Set.delete) p now'
           in writingNow (OffWrite next) m {wantedOff = Just next, problem = Nothing}
    _ -> (m, [])
  Saved asked _
    | not (ofSession asked m) -> landed asked m
  Saved asked answer ->
    let waits = any (\(_, w) -> isOffWrite w) m.queued
        m1 = if waits then m else m {wantedOff = Nothing}
        m2 = case (answer, takes asked m1.asks) of
          (Right w, Just after) -> m1 {asks = after, known = fmap (\k -> k {offForWiki = w.off}) <$> m1.known, problem = Nothing}
          (Right _, Nothing) -> m1
          (Left e, _) -> m1 {problem = Just e}
     in -- The answer is the set alone: a read follows, after what waits.
        landed asked m2 {wanted = True}
  PolicyChosen p -> case shownPolicy m of
    Just now' | now' /= p -> writingNow (PolicyWrite p) m {wantedPolicy = Just p, policyProblem = Nothing}
    _ -> (m, [])
  PolicySaved asked _
    | not (ofSession asked m) -> landed asked m
  PolicySaved asked answer ->
    let waits = any (\(_, w) -> isPolicyWrite w) m.queued
        m1 = if waits then m else m {wantedPolicy = Nothing}
        m2 = case (answer, takes asked m1.asks) of
          -- The answer is the whole list, as the administrator reads it.
          (Right listed, Just after) -> m1 {asks = after, installed = Just (Right listed), policyProblem = Nothing}
          (Right _, Nothing) -> m1
          (Left e, _) -> m1 {policyProblem = Just e}
     in landed asked m2 {wanted = True}
  FilesPicked session owner replacing read'
    | session /= m.session -> (m, [])
    | otherwise ->
        ( m
            { picked = Just Picked {number = m.picks, owner, replacing, files = read' >>= fitting owner replacing}
            , picks = m.picks + 1
            }
        , []
        )
  PickDropped -> (m {picked = Nothing}, [])
  Confirmed c -> case m.picked of
    Just p
      | Right ch <- p.files
      , p.number == c.pick
      , p.owner == c.owner
      , ch.manifest.id == c.id
      , ch.hash == c.hash
      , ch.manifest.hosts == c.hosts
      , (snd <$> p.replacing) == c.replaces ->
          let key = (p.owner, ch.manifest.id)
           in writingNow
                (InstallWrite Upload {owner = p.owner, id = ch.manifest.id, manifestText = ch.manifestText, bundle = ch.bundle, replaces = snd <$> p.replacing})
                m {picked = Nothing, changing = Map.insert key Installing m.changing, externalRefused = Map.delete key m.externalRefused}
    _ -> (m, [])
  Uploaded asked _ _ _
    | not (ofSession asked m) -> landed asked m
  Uploaded asked owner p answer ->
    let m1 = doneChanging (owner, p) m
        m2 = case (answer, takes asked m1.asks) of
          (Right plugin, Just after) -> m1 {asks = after, installed = fmap (withInstalled plugin) <$> m1.installed, externalRefused = Map.delete (owner, p) m1.externalRefused}
          (Right _, Nothing) -> m1
          (Left e, _) -> m1 {externalRefused = Map.insert (owner, p) e m1.externalRefused}
     in landed asked m2 {wanted = True}
  RemoveChosen owner p shown
    | isInstalled ->
        writingNow (RemoveWrite owner p) m {changing = Map.insert (owner, p) Removing m.changing, externalRefused = Map.delete (owner, p) m.externalRefused}
    | otherwise -> (m, [])
    where
      isInstalled = any (\q -> ownerOf q == owner && q.manifest.id == p && q.bundle == shown) (maybe [] (\i -> i.external) (installedNow m))
  Removed asked _ _ _
    | not (ofSession asked m) -> landed asked m
  Removed asked owner p answer ->
    let m1 = doneChanging (owner, p) m
        m2 = case (answer, takes asked m1.asks) of
          (Right (), Just after) -> m1 {asks = after, installed = fmap (withoutInstalled owner p) <$> m1.installed, externalRefused = Map.delete (owner, p) m1.externalRefused}
          (Right (), Nothing) -> m1
          (Left e, _) -> m1 {externalRefused = Map.insert (owner, p) e m1.externalRefused}
     in landed asked m2 {wanted = True}
  where
    -- What a pick checked must also fit where it goes: a replacement keeps
    -- the plugin's id, and a plugin added takes an id its owner has none
    -- of, as the model holds it now.
    fitting owner replacing ch = case replacing of
      Just (p, _)
        | ch.manifest.id /= p -> Left ("The manifest's id is " <> ch.manifest.id.text <> ": a plugin replaced keeps its id, " <> p.text <> ".")
      Nothing
        | any (\q -> ownerOf q == owner && q.manifest.id == ch.manifest.id) (maybe [] (\i -> i.external) (installedNow m)) ->
            Left ("A plugin with the id " <> ch.manifest.id.text <> " is installed already: replace it from its row.")
      _ -> Right ch

-- | The model once the answer about an external plugin's upload or
-- removal came: it is no longer changing, unless another for it waits.
doneChanging :: (Owner, PluginId) -> Model -> Model
doneChanging key m
  | any (\(_, w) -> w `touches` key) m.queued = m
  | otherwise = m {changing = Map.delete key m.changing}

-- | The plugins installed with this one put in, in the place of the
-- owner's plugin of its id, the wiki's first, then the reader's own, each
-- in the order of their ids, as the server lists them.
withInstalled :: ExternalPlugin -> ExternalPlugins -> ExternalPlugins
withInstalled plugin ps = ps {external = sortOn (\q -> (q.mine, q.manifest.id)) (plugin : [q | q <- ps.external, (ownerOf q, q.manifest.id) /= (ownerOf plugin, plugin.manifest.id)])}

-- | The plugins installed without the owner's plugin of this id.
withoutInstalled :: Owner -> PluginId -> ExternalPlugins -> ExternalPlugins
withoutInstalled owner p ps = ps {external = [q | q <- ps.external, (ownerOf q, q.manifest.id) /= (owner, p)]}

-- | The two files of an external plugin, as the reader picked them,
-- checked by the server's own rules: the manifest's bytes by
-- 'parseManifest', the bundle's by 'checkBundle', each refusal in the
-- rule's words. The bundle's hash is the SHA-256 of its bytes, as the
-- server names a bundle. Hashing two megabytes takes a while: the view
-- calls this off its loop.
chooseFiles :: ByteString -> ByteString -> Either Text PluginFiles
chooseFiles manifestBytes bundle = do
  manifest <- parseManifest manifestBytes
  manifestText <- first (const "A plugin's manifest is JSON in UTF-8.") (decodeUtf8' manifestBytes)
  _ <- checkBundle bundle
  pure PluginFiles {manifest, manifestText, bundle, hash = toS (showDigest (sha256 (BSL.fromStrict bundle)))}

-- | The policy the administrator's choice shows: the one last chosen while
-- its write is out or waits, and otherwise the server's; 'Nothing' before
-- it was read.
shownPolicy :: Model -> Maybe ExternalPolicy
shownPolicy m = m.wantedPolicy <|> ((\i -> i.policy) <$> installedNow m)

-- | Options found by a move adopted for a plugin: written, unless the
-- plugin has options, the server's or chosen since ('shownOptions'), or a
-- choice of its options is out or waits (a clear chosen is a choice too).
-- The server writes them only if it keeps none ('AdoptWrite').
adopt :: PluginId -> Value -> Model -> (Model, [Effect])
adopt p v m
  | isJust (shownOptions m p) || Map.member p m.writingOptions = (m, [])
  | otherwise = writingNow (AdoptWrite p v) m {writingOptions = Map.insert p (Just v) m.writingOptions, optionsRefused = Map.delete p m.optionsRefused}

-- | The options waiting for what is on to be read, adopted once it is.
adoptWaiting :: Model -> (Model, [Effect])
adoptWaiting m
  | isNothing (knownNow m) || Map.null m.adopting = (m, [])
  | otherwise = Map.foldrWithKey (\p v (x, effects) -> second (effects <>) (adopt p v x)) (m {adopting = mempty}, []) m.adopting

-- | The answer to a write of a plugin's state, of this session, taken
-- into the model: a refusal is the plugin's row's ('refused').
wrote :: Ask -> PluginId -> Either WikiError PluginChoice -> Model -> (Model, [Effect])
wrote = answered (\m -> m.refused) (\r m -> m {refused = r})

-- | The answer to a write of a plugin's options, chosen or adopted, of
-- this session, taken into the model: a refusal is told to the plugin's
-- surfaces ('optionsRefused').
wroteOptions :: Ask -> PluginId -> Either WikiError PluginChoice -> Model -> (Model, [Effect])
wroteOptions = answered (\m -> m.optionsRefused) (\r m -> m {optionsRefused = r})

-- | The answer to a write for a plugin, with the refusals of its kind of
-- write, read and set.
answered :: (Model -> Map PluginId WikiError) -> (Map PluginId WikiError -> Model -> Model) -> Ask -> PluginId -> Either WikiError PluginChoice -> Model -> (Model, [Effect])
answered refusals setRefusals asked p answer m1 =
  let m2 = case (answer, takes asked m1.asks) of
        -- A success clears the plugin's refusal of its kind: the last
        -- answer about it is not one.
        (Right choice, Just after) -> setRefusals (Map.delete p (refusals m1)) m1 {asks = after, known = withChoice p choice <$> m1.known}
        (Right _, Nothing) -> m1
        (Left e, _) -> setRefusals (Map.insert p e (refusals m1)) m1
   in -- The answer is the one plugin's: a read follows, after what waits.
      landed asked m2 {wanted = True}

-- | What the server keeps, as the model holds it, once read.
knownNow :: Model -> Maybe Plugins
knownNow m = m.known >>= rightToMaybe

-- | The external plugins installed, as the model holds them, once read.
installedNow :: Model -> Maybe ExternalPlugins
installedNow m = m.installed >>= rightToMaybe

-- | Whether what is on and the external plugins installed are this
-- session's read ('knownNow', 'installedNow'), for the session of this
-- number: not the defaults 'held' gives before that read answers, after it
-- failed, or for a session the model has not been given yet.
readIn :: Int -> Model -> Bool
readIn session m = settingsReadIn session m && isJust (installedNow m)

-- | Whether what is on is this session's read ('knownNow'), for the
-- session of this number, whatever became of the read of the external
-- plugins installed: what a built-in plugin's contribution needs, which
-- the list of external plugins does not change.
settingsReadIn :: Int -> Model -> Bool
settingsReadIn session m = m.session == session && isJust (knownNow m)

-- | The external plugins as the rest of the application sees them (the
-- context's): what is installed, once read in this session, and before
-- that, or when it could not be read, none, under the policy that runs
-- none.
heldInstalled :: Model -> ExternalPlugins
heldInstalled m = fromMaybe noExternalPlugins (installedNow m)

-- | What is on as the rest of the application sees it (the context's
-- plugins): what is known, or, before a read of this session answered or
-- when none could, the defaults with the plugins off for the wiki that an
-- earlier session knew. Each plugin whose write of options is out or
-- waits has the options last chosen in place of the server's, which is
-- what its surfaces are given ('Wiki.App.Plugin.Registry.optionsFor').
held :: Model -> Plugins
held m = withChosen (fromMaybe Plugins {offForWiki = m.offBefore, mine = mempty} (knownNow m))
  where
    withChosen k = Plugins {offForWiki = k.offForWiki, mine = Map.foldrWithKey chosenFor k.mine m.writingOptions}
    chosenFor p o mine = putChoice p PluginChoice {state = maybe PluginDefault (\c -> c.state) (Map.lookup p mine), options = o} mine

-- | The user's state for a plugin, as the server keeps it: the default
-- when they made no choice.
stateOf :: Plugins -> PluginId -> PluginState
stateOf known p = maybe PluginDefault (\c -> c.state) (Map.lookup p known.mine)

-- | The state a plugin's switch shows: the one chosen while its write is
-- out or waits, and otherwise the server's.
shownState :: Model -> PluginId -> PluginState
shownState m p = fromMaybe (maybe PluginDefault (`stateOf` p) (knownNow m)) (Map.lookup p m.writing)

-- | A plugin's options as its surfaces are given them: the ones last
-- chosen while their write is out or waits, and otherwise the server's.
shownOptions :: Model -> PluginId -> Maybe Value
shownOptions m p = (\c -> c.options) =<< Map.lookup p (held m).mine

-- | The plugins off for the wiki the administrator's boxes show: the set
-- last ticked while its save is out or waits, and otherwise the server's;
-- 'Nothing' before it was read.
shownOff :: Model -> Maybe (Set PluginId)
shownOff m = m.wantedOff <|> ((\k -> k.offForWiki) <$> knownNow m)

-- | What is known with the user's choice for a plugin put in, by its id.
-- A choice of the default with no options is none, as the server keeps it:
-- the plugin is taken out. A read that failed holds nothing to put it in.
withChoice :: PluginId -> PluginChoice -> Either WikiError Plugins -> Either WikiError Plugins
withChoice p choice = fmap (\k -> Plugins {offForWiki = k.offForWiki, mine = putChoice p choice k.mine})

-- | The user's choices with the choice for a plugin put in, or taken out
-- when it is the default with no options, as the server keeps them.
putChoice :: PluginId -> PluginChoice -> Map PluginId PluginChoice -> Map PluginId PluginChoice
putChoice p choice
  | choice.state /= PluginDefault || isJust choice.options = Map.insert p choice
  | otherwise = Map.delete p

-- | Whether a write is one of this plugin's state.
writes :: Write -> PluginId -> Bool
writes w p = case w of
  StateWrite q _ -> q == p
  OffWrite _ -> False
  OptionsWrite _ _ -> False
  AdoptWrite _ _ -> False
  PolicyWrite _ -> False
  InstallWrite _ -> False
  RemoveWrite {} -> False

-- | Whether a write is one of this plugin's options.
writesOptions :: Write -> PluginId -> Bool
writesOptions w p = case w of
  OptionsWrite q _ -> q == p
  AdoptWrite q _ -> q == p
  StateWrite {} -> False
  OffWrite _ -> False
  PolicyWrite _ -> False
  InstallWrite _ -> False
  RemoveWrite {} -> False

isOffWrite :: Write -> Bool
isOffWrite = \case
  OffWrite _ -> True
  StateWrite {} -> False
  OptionsWrite {} -> False
  AdoptWrite {} -> False
  PolicyWrite _ -> False
  InstallWrite _ -> False
  RemoveWrite {} -> False

isPolicyWrite :: Write -> Bool
isPolicyWrite = \case
  PolicyWrite _ -> True
  OffWrite _ -> False
  StateWrite {} -> False
  OptionsWrite {} -> False
  AdoptWrite {} -> False
  InstallWrite _ -> False
  RemoveWrite {} -> False

-- | Whether a write is an upload or a removal of this owner's plugin of
-- this id.
touches :: Write -> (Owner, PluginId) -> Bool
touches w key = case w of
  InstallWrite u -> (u.owner, u.id) == key
  RemoveWrite o p -> (o, p) == key
  PolicyWrite _ -> False
  OffWrite _ -> False
  StateWrite {} -> False
  OptionsWrite {} -> False
  AdoptWrite {} -> False

-- | The answer to a read taken, if no answer to a later ask was. A failure
-- is shown only where nothing is held: what is held is kept.
taking :: Ask -> Either WikiError Plugins -> Model -> Model
taking asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case answer of
    Right _ -> m {asks = after, known = Just answer}
    Left _ -> case m.known of
      Just (Right _) -> m {asks = after}
      _ -> m {asks = after, known = Just answer}

-- | The answer to the read of the external plugins installed taken, as
-- 'taking' takes what is on.
takingInstalled :: Ask -> Either WikiError ExternalPlugins -> Model -> Model
takingInstalled asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case answer of
    Right _ -> m {asks = after, installed = Just answer}
    Left _ -> case m.installed of
      Just (Right _) -> m {asks = after}
      _ -> m {asks = after, installed = Just answer}

-- | After the answer to the first request of a read of this session came:
-- when it was the ask out, the second request is sent at once, before any
-- write that waits, whatever the first answered.
secondRequest :: Ask -> Model -> (Model, [Effect])
secondRequest asked m
  | m.flight /= Just asked = (m, [])
  | otherwise = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket}, [LoadInstalled ticket])

-- | The state after the answer to an ask came, taken or not, failed or
-- not: when it was the ask out, the next is sent, the first write waiting
-- of this session, or else the read wanted.
landed :: Ask -> Model -> (Model, [Effect])
landed asked m
  | m.flight /= Just asked = (m, [])
  | otherwise = case [w | (n, w) <- m.queued, n == m.session] of
      w : rest -> send w m {flight = Nothing, queued = [(m.session, x) | x <- rest]}
      []
        | m.wanted -> sendRead m {flight = Nothing, queued = []}
        | otherwise -> (m {flight = Nothing, queued = []}, [])

-- | Whether an ask was made in the session the model is in.
ofSession :: Ask -> Model -> Bool
ofSession asked m = asked `madeAfter` m.sessionFrom

-- | Whether an ask is out, of this session or an earlier one.
inFlight :: Model -> Bool
inFlight m = isJust m.flight

-- | A read begun, or wanted while an ask is out.
reading :: Model -> (Model, [Effect])
reading m
  | inFlight m = (m {wanted = True}, [])
  | otherwise = sendRead m

-- | A write begun, or put in its turn while an ask is out, in the place of
-- one that waits for the same thing.
writingNow :: Write -> Model -> (Model, [Effect])
writingNow w m
  | inFlight m = (m {queued = [x | x@(_, q) <- m.queued, not (same q)] <> [(m.session, w)]}, [])
  | otherwise = send w m
  where
    same q = case (q, w) of
      (StateWrite a _, StateWrite b _) -> a == b
      (OffWrite _, OffWrite _) -> True
      -- The options last chosen for one plugin take the turn of options
      -- chosen or adopted before. Adopted options never take a choice's
      -- turn: they are not adopted while one is out or waits ('adopt').
      (OptionsWrite a _, OptionsWrite b _) -> a == b
      (AdoptWrite a _, OptionsWrite b _) -> a == b
      (AdoptWrite a _, AdoptWrite b _) -> a == b
      (PolicyWrite _, PolicyWrite _) -> True
      -- A later removal of the same plugin takes the place of one that
      -- waits; an upload never takes another's place, each being a
      -- version the reader confirmed.
      (RemoveWrite a x, RemoveWrite b y) -> (a, x) == (b, y)
      _ -> False

sendRead :: Model -> (Model, [Effect])
sendRead m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket, wanted = False}, [Load ticket])

send :: Write -> Model -> (Model, [Effect])
send w m =
  let (ticket, after) = newAsk m.asks
   in ( m {asks = after, flight = Just ticket}
      ,
        [ case w of
            StateWrite p s -> WriteState ticket p s
            OffWrite off -> SaveOff ticket off
            OptionsWrite p o -> WriteOptions ticket p o
            AdoptWrite p v -> WriteOptionsIfNone ticket p v
            PolicyWrite p -> SetPolicy ticket p
            InstallWrite u -> Install ticket u
            RemoveWrite o p -> Remove ticket o p
        ]
      )

-- | The ticket an effect was sent under.
askOf :: Effect -> Maybe Ask
askOf = \case
  Load a -> Just a
  LoadInstalled a -> Just a
  WriteState a _ _ -> Just a
  WriteOptions a _ _ -> Just a
  WriteOptionsIfNone a _ _ -> Just a
  SaveOff a _ -> Just a
  SetPolicy a _ -> Just a
  Install a _ -> Just a
  Remove a _ _ -> Just a

-- | 'Model' is the writing assistant's settings as one of its surfaces
-- holds them (its agent in the editor, its settings' section), and 'step'
-- the one place they change (decision 82 of docs/PLAN.md). The settings
-- are in three places ("Wiki.Core.Assistant.Split"): the options, which
-- the host gives the surface with its input; the keys, the plugin's
-- secrets, read with 'Wiki.App.Plugin.get'; and what was learnt from the
-- providers, the plugin's cache in this browser. 'settings' puts them
-- together.
--
-- = One ask at a time
--
-- The keys and the cache are each read by one ask at a time ('keysOut',
-- 'cacheOut'). A read wanted while one is out is asked when it answers
-- ('keysWanted'), so that a later answer is never an older one. The keys
-- are read again each time the host says the secrets changed
-- ('SecretsChanged'). The cache is written by one ask at a time
-- ('saving'); a value to write meanwhile waits, and a later one takes its
-- place ('toSave'): the cache is one value, written whole.
--
-- = A change
--
-- A change of the settings ('Changed') is made on the settings as they
-- are, and split again: the options, if they changed, are given to the
-- host to keep ('SetOptions'), and held as the surface's options until
-- the host gives them back; what was learnt, if it changed, is written to
-- the cache. A change never touches a key: the host's field of a secret
-- keeps and forgets them.
--
-- = The providers' lists of models
--
-- A surface that wants it (the agent) has each provider that can be asked
-- asked for its list of models once the settings are first read for a
-- reader ('wantsLists'), and a provider asked again when it says so
-- ('Check'). One ask per provider is out at a time ('checking'). The list,
-- or whether the provider could be reached, is kept only if the
-- provider's key and address are still those it was asked with
-- ('Listed').
--
-- = The reader
--
-- The reader (the session's number and the user) comes with the input,
-- and changes in the step that takes it ('Given'): what was read is
-- forgotten, and read again for the new reader. An answer to an ask made
-- for another reader is dropped, and a write of the cache that waits is
-- dropped with its reader.
module Wiki.App.Plugin.Assistant.Settings
  ( Model (..)
  , Reader
  , Event (..)
  , Effect (..)
  , initial
  , step
  , settings
  , lockedKeys
  , listedAnswer
  )
where

import Data.Aeson (Value)
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Protolude hiding (Reader, reader)
import Text.Show (Show (showsPrec))
import Wiki.App.Plugin (SecretName, Unavailable)
import Wiki.Core.Assistant (AssistantError (..), ErrorKind (Failed, Unreachable), Listed, Provider, Ready (..), Settings, providers, readyFor, settingsOf, usable, withModels, withReached)
import Wiki.Core.Assistant.Split (fromParts, learntValue, optionsValue)
import Wiki.Core.Id (UserId)

-- | 'Reader' is who the settings are read for: the session's number and
-- the user signed in, if anyone is.
type Reader = (Int, Maybe UserId)

-- | 'Model' is what a surface holds of the assistant's settings.
data Model = Model
  { reader :: Reader
  , options :: Maybe Value
  -- ^ The options as the host last gave them, or as last set here.
  , keys :: Maybe (Either Unavailable (Map SecretName Text))
  -- ^ The keys as last read, or why they could not be; 'Nothing' until
  -- the first read for the reader answers.
  , learnt :: Maybe (Maybe Value)
  -- ^ The cache as last read or written; 'Nothing' until it is read.
  , keysOut :: Maybe (Int, Reader)
  -- ^ The read of the keys out, with the reader it was asked for.
  , keysWanted :: Bool
  , cacheOut :: Maybe (Int, Reader)
  , cacheWanted :: Bool
  , saving :: Maybe Int
  -- ^ The write of the cache out.
  , toSave :: Maybe Value
  -- ^ The value to write once that write answers.
  , next :: Int
  -- ^ The next ask's number.
  , problem :: Maybe Text
  -- ^ Why the last write of the cache failed, until one does not.
  , checking :: Set Provider
  -- ^ The providers asked for their list of models, until they answer.
  , wantsLists :: Maybe Bool
  -- ^ Whether the surface wants the providers asked for their lists once
  -- the settings are read, with whether the scripted provider is
  -- installed.
  , listedFor :: Maybe Reader
  -- ^ The reader for whom they were asked.
  }
  deriving stock (Eq)

-- | Shown without the keys.
instance Show Model where
  showsPrec _ m rest = "Model {reader = " <> show m.reader <> ", keysRead = " <> show (isJust m.keys) <> ", keysOut = " <> show m.keysOut <> ", cacheOut = " <> show m.cacheOut <> ", saving = " <> show m.saving <> ", checking = " <> show (Set.toList m.checking) <> "}" <> rest

-- | 'Event' is what happens to the settings.
data Event
  = -- | The surface was mounted, for this reader, with these options;
    -- with, when it wants each provider asked for its list of models once
    -- the settings are read, whether the scripted provider is installed.
    Opened Reader (Maybe Value) (Maybe Bool)
  | -- | The host gave the surface its input again.
    Given Reader (Maybe Value)
  | -- | The host says the secrets may read differently.
    SecretsChanged
  | -- | The answer to the read of the keys asked under this number.
    KeysRead Int (Either Unavailable (Map SecretName Text))
  | -- | The answer to the read of the cache asked under this number.
    CacheRead Int (Maybe Value)
  | -- | The answer to the write of the cache asked under this number.
    CacheSaved Int (Either Text ())
  | -- | The settings changed: the provider, a model, what was learnt.
    Changed (Settings -> Settings)
  | -- | Ask a provider for its list of models.
    Check Provider
  | -- | The answer of a provider asked for its list of models, with the
    -- request it was asked with and the time of the answer.
    Listed Provider Ready UTCTime (Either AssistantError [Listed])

-- | 'Effect' is what a step asks the surface to do.
data Effect
  = -- | Read the keys; the answer comes back as 'KeysRead'.
    ReadKeys Int
  | -- | Read the cache; the answer comes back as 'CacheRead'.
    ReadCache Int
  | -- | Write the cache; the answer comes back as 'CacheSaved'.
    SaveCache Int Value
  | -- | Give the host the options to keep.
    SetOptions (Maybe Value)
  | -- | Ask a provider for its list of models; the answer comes back as
    -- 'Listed'.
    ListModels Provider Ready
  | -- | The settings shown changed.
    Shown
  deriving stock (Eq, Show)

-- | The settings for a reader, before anything is read.
initial :: Reader -> Maybe Value -> Model
initial reader options =
  Model
    { reader
    , options
    , keys = Nothing
    , learnt = Nothing
    , keysOut = Nothing
    , keysWanted = False
    , cacheOut = Nothing
    , cacheWanted = False
    , saving = Nothing
    , toSave = Nothing
    , next = 0
    , problem = Nothing
    , checking = mempty
    , wantsLists = Nothing
    , listedFor = Nothing
    }

-- | The settings made whole, once the keys and the cache are read. While
-- the keys cannot be read (the vault is locked here), the settings hold
-- none.
settings :: Model -> Maybe Settings
settings m = case (m.keys, m.learnt) of
  (Just keys, Just learnt) -> Just (fromParts m.options (fromRight mempty keys) learnt)
  _ -> Nothing

-- | The answer of a provider asked for its list of models, with the
-- time of the answer, as an event: what the work threw is a failure.
listedAnswer :: Provider -> Ready -> Either SomeException (UTCTime, Either AssistantError [Listed]) -> Event
listedAnswer p r = \case
  Right (at, answer) -> Listed p r at answer
  Left e -> Listed p r (posixSecondsToUTCTime 0) (Left (AssistantError Failed Nothing (toS (displayException e))))

-- | Why the keys cannot be read, when they cannot.
lockedKeys :: Model -> Maybe Unavailable
lockedKeys m = m.keys >>= leftToMaybe

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = listingOnRead . shownAfter m $ case event of
  Opened reader options wants -> reading (initial reader options) {next = m.next, wantsLists = wants}
  Given reader options
    | reader /= m.reader ->
        -- The asks out answer into nothing: the reads wanted for the new
        -- reader wait for them.
        reading (initial reader options) {next = m.next, keysOut = m.keysOut, cacheOut = m.cacheOut, saving = m.saving, wantsLists = m.wantsLists}
    | otherwise -> (m {options}, [])
  SecretsChanged -> readingKeys m
  KeysRead n answer -> case m.keysOut of
    Just (out, asked)
      | out == n ->
          let m1 = (if asked == m.reader then m {keys = Just answer} else m) {keysOut = Nothing}
           in if m1.keysWanted then readingKeys m1 {keysWanted = False} else (m1, [])
    _ -> (m, [])
  CacheRead n answer -> case m.cacheOut of
    Just (out, asked)
      | out == n ->
          -- A cache written since the read was asked is newer.
          let m1 = (if asked == m.reader && isNothing m.learnt then m {learnt = Just answer} else m) {cacheOut = Nothing}
           in if m1.cacheWanted then readingCache m1 {cacheWanted = False} else (m1, [])
    _ -> (m, [])
  CacheSaved n answer
    | m.saving == Just n ->
        let m1 = m {saving = Nothing, problem = either Just (const Nothing) answer}
         in maybe (m1, []) (\v -> saving' v m1 {toSave = Nothing}) m1.toSave
    | otherwise -> (m, [])
  Changed f -> changing f m
  Check p -> listing p m
  Listed p asked at answer ->
    changing
      ( \s' ->
          -- Only for the key and the address it was asked with.
          if either (const False) (\r -> r.key == asked.key && r.baseUrl == asked.baseUrl) (readyFor p s')
            then either (\e -> withReached p (e.kind /= Unreachable)) (\l -> withModels p l at) answer s'
            else s'
      )
      m {checking = Set.delete p m.checking}

-- | A change made on the settings as they are, split again.
changing :: (Settings -> Settings) -> Model -> (Model, [Effect])
changing f m = case settings m of
  Nothing -> (m, [])
  Just s ->
    let s' = f s
        options' = optionsValue s'
        learnt' = learntValue s'
        (m1, setting) = if options' /= optionsValue s then (m {options = Just options'}, [SetOptions (Just options')]) else (m, [])
     in if learnt' /= learntValue s
          then second (setting <>) (saving' learnt' m1 {learnt = Just (Just learnt')})
          else (m1, setting)

-- | A provider asked for its list of models, unless it is asked already
-- or cannot be asked.
listing :: Provider -> Model -> (Model, [Effect])
listing p m = case settings m of
  Just s
    | Right r <- readyFor p s
    , not (Set.member p m.checking) ->
        (m {checking = Set.insert p m.checking}, [ListModels p r])
  _ -> (m, [])

-- | Once the settings are read for a reader, each provider that can be
-- asked is asked for its list, by a surface that wants it.
listingOnRead :: (Model, [Effect]) -> (Model, [Effect])
listingOnRead (m, effects) = case (m.wantsLists, settings m) of
  (Just scripted, Just s)
    | m.listedFor /= Just m.reader ->
        foldl' (\(x, fx) p -> second (fx <>) (listing p x)) (m {listedFor = Just m.reader}, effects) [p | p <- providers scripted, usable p (settingsOf p s)]
  _ -> (m, effects)

-- | 'Shown' added when the settings shown, or whether the keys can be
-- read, changed.
shownAfter :: Model -> (Model, [Effect]) -> (Model, [Effect])
shownAfter before (after, effects)
  | (settings before, lockedKeys before, before.checking) /= (settings after, lockedKeys after, after.checking) = (after, effects <> [Shown])
  | otherwise = (after, effects)

-- | Both read for the reader.
reading :: Model -> (Model, [Effect])
reading m = let (m1, a) = readingKeys m; (m2, b) = readingCache m1 in (m2, a <> b)

readingKeys :: Model -> (Model, [Effect])
readingKeys m
  | isJust m.keysOut = (m {keysWanted = True}, [])
  | otherwise = (m {keysOut = Just (m.next, m.reader), next = m.next + 1}, [ReadKeys m.next])

readingCache :: Model -> (Model, [Effect])
readingCache m
  | isJust m.cacheOut = (m {cacheWanted = True}, [])
  | otherwise = (m {cacheOut = Just (m.next, m.reader), next = m.next + 1}, [ReadCache m.next])

-- | A write of the cache asked, or put to wait for the one out.
saving' :: Value -> Model -> (Model, [Effect])
saving' v m
  | isJust m.saving = (m {toSave = Just v}, [])
  | otherwise = (m {saving = Just m.next, next = m.next + 1}, [SaveCache m.next v])

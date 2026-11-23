{-# LANGUAGE MultiWayIf #-}

-- | The API keys (WP5.138 of docs/PLAN.md, issue #338): the second way a
-- program signs in to the API for programs ("Wiki.Server.MarkdownApi",
-- "Wiki.Server.ListApi"), beside the user's login and password. A user
-- makes a key in a signed-in session ("Wiki.Api.ApiKeys"), and a program
-- sends it in each request, as @Authorization: Bearer \<key>@ or
-- @X-API-Key: \<key>@.
--
-- = The key
--
-- A key is @wiki_\<id>_\<secret>@, 74 characters: the prefix @wiki_@, an
-- id of 16 characters (80 random bits), an underscore, and a secret of 52
-- characters (256 random bits), both in lowercase base32 (@a@ to @z@ and
-- @2@ to @7@). The prefix tells a reader, and a scanner of leaked secrets,
-- what the string is. The alphabet needs no quoting in a header, a shell
-- or a URL, and has no character that a double click stops at, so the
-- whole key is selected at once. The id is public: it names the key in
-- the user's list and in the server's log, and the server finds the key by
-- it. The secret is what signs in.
--
-- The whole key is shown once, in the answer that makes it. The server
-- stores the id, the SHA-256 of the secret in hex ('secretHash'), the
-- user, the key's name and right, and when it was made, expires, was last
-- used and was revoked (@api_keys@). A plain SHA-256 is right here: a
-- slow hash such as Argon2 protects a password a person chose, which can
-- be guessed, and a secret of 256 random bits cannot be; a slow hash would
-- only cost its time on every request. The stored hash is compared with
-- the hash of the secret sent in constant time ('constEq').
--
-- = Signing in
--
-- 'signInByKey' finds the key by its id, compares the secret, and lets
-- the request in when the key is not revoked, has not expired, and its
-- user is enabled. A malformed key, an unknown id, a wrong secret, an
-- expired or revoked key and a disabled user are refused alike: the
-- caller is not told which ('Refusal' is for the server's log). A removed
-- user's keys are gone with the user, so their keys are unknown ids. An
-- unknown id is compared against a stored hash all the same
-- ('unknownHash'), so that it takes the work a wrong secret takes.
--
-- When the key's time of last use has to be written ('lastUsedEvery'),
-- the write waits for the database's writer, and the key could be revoked
-- meanwhile. So the key is read again and decided again in the writing
-- transaction, and that second decision is the answer.
--
-- = The limit on failed keys
--
-- A refused key counts as a failure of its address, in a limiter of its
-- own (the 'Wiki.Server.Api.Env''s @keyFailures@), apart from the sign-in
-- form's: a script looping on a revoked key does not lock its address out
-- of the form, nor out of Basic sign-in. Once an address has failed 30
-- times in 15 minutes, its further failed keys are answered 429 instead
-- of 401 ('Wiki.Server.Limiter.countFailureAt').
--
-- The key is checked before the limit: a right key is let in whatever the
-- count of its address. The limit cannot protect the secret, which nobody
-- guesses in 256 bits; it bounds what a guesser costs the server and
-- writes in the log. Refusing right keys from a limited address would let
-- anyone who shares the address, or who knows a key's public id, lock its
-- owner out. Failures are counted by address and not by the key's id for
-- the same reason: the id is public.
module Wiki.Server.ApiKeys
  ( -- * The key
    keyPrefix
  , parseKey
  , secretHash
  , carriesKey

    -- * Managing keys
  , maxLiveKeys
  , maxKeptKeys
  , maxKeyName
  , apiKeysOf
  , makeApiKey
  , revokeApiKey
  , isAdministrator
  , allows

    -- * Signing in with a key
  , lastUsedEvery
  , KeyHolder (..)
  , Refusal (..)
  , refusalName
  , KeyOutcome (..)
  , signInByKey
  )
where

import Crypto.Random (getRandomBytes)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Char (isAsciiLower)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Database.Beam (all_, asc_, delete, desc_, filter_, insert, insertValues, join_, orderBy_, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, update, val_, (&&.), (<-.), (==.))
import Network.HTTP.Types (hAuthorization, methodGet, methodHead)
import Network.Wai (Request (..))
import Protolude
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..), rightName, rightOf)
import Wiki.App.Backend (MonadWiki (session), Session (..), WikiError (..), sessionPrincipal)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Permission (GlobalPermission (Administer), hasGlobalPermission)
import Wiki.Server.Limiter (Limiter, Verdict (..), countFailureAt)
import Wiki.Server.Sha256 (sha256)
import Wiki.Server.Storage (Storage, Who (..), reading, runStoredWikiFrom, writing, writingEither)
import Wiki.Server.Storage.Backend (Backend (..))
import Wiki.Server.Storage.Rows (micro)
import Wiki.Server.Storage.Schema (ApiKeys (..), Users (..), WikiDb (..), wikiDb)

--------------------------------------------------------------------------------
-- The key

-- | What every key starts with.
keyPrefix :: Text
keyPrefix = "wiki_"

-- | The length of a key's id and of its secret, in characters of base32:
-- 10 and 32 random bytes.
idLength, secretLength :: Int
idLength = 16
secretLength = 52

-- | A new key's id and secret, from the system's random source.
freshKey :: IO (ApiKeyId, Text)
freshKey = do
  i <- base32 <$> getRandomBytes 10
  secret <- base32 <$> getRandomBytes 32
  pure (ApiKeyId i, secret)
  where
    base32 :: ByteString -> Text
    base32 = T.toLower . T.dropWhileEnd (== '=') . decodeUtf8 . convertToBase Base32

-- | A whole key, as its user is given it.
keyText :: ApiKeyId -> Text -> Text
keyText i secret = keyPrefix <> i.text <> "_" <> secret

-- | A key's id and secret, if the text is a key of the form this module
-- makes: the prefix, an id and a secret of their lengths in lowercase
-- base32, and nothing else.
parseKey :: Text -> Maybe (ApiKeyId, Text)
parseKey t = do
  guard (T.length t == T.length keyPrefix + idLength + 1 + secretLength)
  rest <- T.stripPrefix keyPrefix t
  let (i, afterId) = T.splitAt idLength rest
  secret <- T.stripPrefix "_" afterId
  guard (T.all base32 i && T.all base32 secret)
  pure (ApiKeyId i, secret)
  where
    base32 c = isAsciiLower c || (c >= '2' && c <= '7')

-- | The SHA-256 of a key's secret, in hex: what the database keeps of it.
-- It must never change, or every key would stop working
-- ("Wiki.Server.Sha256").
secretHash :: Text -> Text
secretHash = decodeUtf8 . convertToBase Base16 . sha256 . encodeUtf8

-- | The hash an unknown id's secret is compared against: no secret's, as
-- a stored hash is a SHA-256 in hex and this is not hex.
unknownHash :: Text
unknownHash = T.replicate 64 "x"

-- | Whether a request carries something meant as an API key: an
-- @X-API-Key@ header, or an @Authorization: Bearer@ that holds a key of
-- this wiki's form (its prefix). The application's API (@api\/v1@) refuses
-- such a request rather than answer it as nobody's: a key signs in only
-- to the API for programs. A bearer token without the prefix is left
-- alone there, as a proxy in front of the wiki may send its own.
carriesKey :: Request -> Bool
carriesKey request = isJust (lookup "X-API-Key" hs) || any bearerKey [v | (n, v) <- hs, n == hAuthorization]
  where
    hs = requestHeaders request
    bearerKey v = case T.words (decodeUtf8With lenientDecode v) of
      [scheme, token] -> T.toCaseFold scheme == "bearer" && keyPrefix `T.isPrefixOf` token
      _ -> False

--------------------------------------------------------------------------------
-- Managing keys

-- | The most keys a user may have that work (neither revoked nor
-- expired): one for each program and machine, and a bound on what one
-- account stores.
maxLiveKeys :: Int
maxLiveKeys = 20

-- | The most keys a user's list keeps, revoked and expired ones included.
-- Making a key past it deletes the user's oldest revoked or expired keys,
-- so that making and revoking keys over and over does not grow the
-- table.
maxKeptKeys :: Int
maxKeptKeys = 100

-- | The longest name of a key, in characters.
maxKeyName :: Int
maxKeyName = 100

-- | A user's keys, the newest first: what their list shows.
apiKeysOf :: Storage -> UserId -> IO [ApiKeyInfo]
apiKeysOf st u = reading st (keysIn u)

keysIn :: (Backend be) => UserId -> Db be [ApiKeyInfo]
keysIn u = mapMaybe infoOf <$> rowsOf u

rowsOf :: (Backend be) => UserId -> Db be [ApiKeys Identity]
rowsOf u = runSelectReturningList (select (orderBy_ (\k -> (desc_ k.created_at, asc_ k.id)) (filter_ (\k -> k.user_id ==. val_ u.text) (all_ wikiDb.api_keys))))

infoOf :: ApiKeys Identity -> Maybe ApiKeyInfo
infoOf k =
  rightOf k.access <&> \right ->
    ApiKeyInfo {id = ApiKeyId k.id, name = k.name, right, createdAt = k.created_at, expiresAt = k.expires_at, lastUsedAt = k.last_used_at, revokedAt = k.revoked_at}

-- | Whether a key works at a time: not revoked, and not expired.
live :: UTCTime -> ApiKeys Identity -> Bool
live now k = isNothing k.revoked_at && maybe True (> now) k.expires_at

-- | Make a key for a user, who is an administrator or not, at a time. The
-- result holds the whole key, which nothing shows again. A key with the
-- right @admin@ for a user who is not an administrator is 'Forbidden'. An
-- expiry that is not after this time, a name longer than 'maxKeyName', or
-- a user with 'maxLiveKeys' keys that work already is 'Invalid'. An empty
-- name is replaced by the day the key is made.
makeApiKey :: Storage -> UTCTime -> UserId -> Bool -> NewApiKey -> IO (Either WikiError MadeApiKey)
makeApiKey st now u administrator new
  | new.right == AdminRight && not administrator = pure (Left Forbidden)
  | Just e <- new.expiresAt, e <= now = pure (Left (Invalid "A key's expiry has to be in the future: choose a later day, or none."))
  | T.length (T.strip new.name) > maxKeyName = pure (Left (Invalid ("A key's name has at most " <> show maxKeyName <> " characters.")))
  | otherwise = do
      (i, secret) <- freshKey
      writingEither st $ do
        mine <- rowsOf u
        taken <- isJust <$> runSelectReturningOne (select (filter_ (\k -> k.id ==. val_ i.text) (all_ wikiDb.api_keys)))
        if
          | length (filter (live now) mine) >= maxLiveKeys ->
              pure (Left (Invalid ("You have " <> show maxLiveKeys <> " keys that work already: revoke one first.")))
          -- 80 random bits: this does not happen, but a key is never
          -- given an id another key has.
          | taken -> pure (Left (Conflict "The key could not be made: try again."))
          | otherwise -> do
              -- The oldest revoked or expired keys make room past
              -- 'maxKeptKeys'.
              let dead = sortOn (\k -> (fromMaybe k.created_at (k.revoked_at <|> k.expires_at), k.id)) (filter (not . live now) mine)
              for_ (take (length mine + 1 - maxKeptKeys) dead) $ \k ->
                runDelete (delete wikiDb.api_keys (\r -> r.id ==. val_ k.id))
              let row =
                    ApiKeyRow
                      { id = i.text
                      , user_id = u.text
                      , secret_hash = secretHash secret
                      , name = case T.strip new.name of
                          named | not (T.null named) -> named
                          _ -> "Made on " <> toS (formatTime defaultTimeLocale "%Y-%m-%d at %H:%M UTC" now)
                      , access = rightName new.right
                      , created_at = micro now
                      , expires_at = micro <$> new.expiresAt
                      , last_used_at = Nothing
                      , revoked_at = Nothing
                      }
              runInsert (insert wikiDb.api_keys (insertValues [row]))
              pure (maybe (Left NotFound) (\info -> Right MadeApiKey {key = keyText i secret, info}) (infoOf row))

-- | Revoke one of a user's keys at a time: it never signs a request in
-- again, and stays in the list. A key revoked already keeps its time. The
-- result is the user's keys after it; a key that is not the user's is
-- 'NotFound'.
revokeApiKey :: Storage -> UTCTime -> UserId -> ApiKeyId -> IO (Either WikiError [ApiKeyInfo])
revokeApiKey st now u i = writingEither st $ do
  found <- runSelectReturningOne (select (filter_ (\k -> k.id ==. val_ i.text &&. k.user_id ==. val_ u.text) (all_ wikiDb.api_keys)))
  case found of
    Nothing -> pure (Left NotFound)
    Just k -> do
      when (isNothing k.revoked_at) $
        runUpdate (update wikiDb.api_keys (\r -> r.revoked_at <-. val_ (Just (micro now))) (\r -> r.id ==. val_ k.id))
      Right <$> keysIn u

-- | Whether a user is an administrator: holds the global administer,
-- given to them or to a group of theirs, as the administration's own
-- endpoints ask ('Wiki.Server.Storage.requireAdministrator'). A disabled
-- user is nobody, and is not. Read for a request from this address.
isAdministrator :: Storage -> Text -> UserId -> IO Bool
isAdministrator st address u = do
  s <- runStoredWikiFrom st (AsUser u) address session
  pure (hasGlobalPermission (sessionPrincipal s) s.grants Administer)

-- | Whether a key's right allows a request's method: @GET@ and @HEAD@ only
-- read, and any other method needs @write@ (which @admin@ includes). No
-- address of the API for programs changes anything on a @GET@.
allows :: KeyRight -> ByteString -> Bool
allows right method = method `elem` [methodGet, methodHead] || right >= WriteRight

--------------------------------------------------------------------------------
-- Signing in with a key

-- | How often a key's time of last use is written: at most once in ten
-- minutes, so that a program sending many requests does not make each
-- of them write to the database. The list shows the time to within that.
lastUsedEvery :: NominalDiffTime
lastUsedEvery = 10 * 60

-- | 'KeyHolder' is who a key signed in: its user, its id and its right.
data KeyHolder = KeyHolder
  { user :: UserId
  , key :: ApiKeyId
  , right :: KeyRight
  }
  deriving stock (Eq, Show)

-- | 'Refusal' is why a key did not sign in, for the server's log only: the
-- caller gets the same answer for each.
data Refusal
  = -- | Not a key of this wiki's form.
    Malformed
  | -- | No key has its id.
    UnknownKey
  | WrongSecret
  | Expired
  | Revoked
  | -- | Its user's login is disabled.
    UserDisabled
  deriving stock (Eq, Show)

-- | A refusal's name in the log.
refusalName :: Refusal -> Text
refusalName = \case
  Malformed -> "malformed"
  UnknownKey -> "unknown"
  WrongSecret -> "wrong-secret"
  Expired -> "expired"
  Revoked -> "revoked"
  UserDisabled -> "user-disabled"

-- | 'KeyOutcome' is what signing in with a key gave. A refusal carries
-- the key's id when the key was well formed (for the log; an id that is
-- not a key's may be a pasted secret, and is not logged), and, once the
-- address has failed too often, how many seconds it waits.
data KeyOutcome
  = KeySignedIn KeyHolder
  | KeyRefused (Maybe ApiKeyId) Refusal
  | KeyLimited (Maybe ApiKeyId) Refusal Int
  deriving stock (Eq, Show)

-- | Sign in with a key a request carries, at a time, from an address: see
-- the module's haddock for the steps and the limit.
signInByKey :: Storage -> Limiter -> UTCTime -> Text -> Text -> IO KeyOutcome
signInByKey st limiter now address presented = do
  let parsed = parseKey (T.strip presented)
      given = encodeUtf8 (secretHash (maybe "" snd parsed))
  found <- maybe (pure Nothing) (\(i, _) -> reading st (keyAndUser i)) parsed
  decided <- decide given parsed found
  outcome <- case decided of
    Right (k, _)
      | maybe True (\t -> diffUTCTime now t >= lastUsedEvery) k.last_used_at ->
          -- The write waits for the writer: the key is read and decided
          -- again in its transaction, and the time written only if it
          -- still signs in.
          writing st $ do
            again <- maybe (pure Nothing) (\(i, _) -> keyAndUser i) parsed
            let redecided = decidePure (constEq given (storedOf again)) parsed again
            for_ redecided $ \(k', _) -> runUpdate (update wikiDb.api_keys (\r -> r.last_used_at <-. val_ (Just (micro now))) (\r -> r.id ==. val_ k'.id))
            pure redecided
    other -> pure other
  case outcome of
    Right (_, holder) -> pure (KeySignedIn holder)
    Left why ->
      countFailureAt limiter now address <&> \case
        Allowed -> KeyRefused (fst <$> parsed) why
        Refused seconds -> KeyLimited (fst <$> parsed) why seconds
  where
    -- The comparison is made whether or not a key has the id, and forced
    -- before the decision, so that an unknown id costs what a wrong
    -- secret does.
    decide given parsed found = do
      matches <- evaluate (constEq given (storedOf found))
      pure (decidePure matches parsed found)
    storedOf found = encodeUtf8 (maybe unknownHash (\(k, _) -> k.secret_hash) found)
    decidePure :: Bool -> Maybe (ApiKeyId, Text) -> Maybe (ApiKeys Identity, Bool) -> Either Refusal (ApiKeys Identity, KeyHolder)
    decidePure matches parsed found = case (parsed, found) of
      (Nothing, _) -> Left Malformed
      (Just _, Nothing) -> Left UnknownKey
      (Just _, Just (k, disabled))
        | not matches -> Left WrongSecret
        | isJust k.revoked_at -> Left Revoked
        | maybe False (<= now) k.expires_at -> Left Expired
        | disabled -> Left UserDisabled
        | Just right <- rightOf k.access -> Right (k, KeyHolder {user = UserId k.user_id, key = ApiKeyId k.id, right})
        | otherwise -> Left Malformed

-- | A key by its id, with whether its user's login is disabled: one query,
-- whether or not a key has the id, so that the time of the answer does
-- not tell. A key's user always exists, since the key goes with them.
keyAndUser :: (Backend be) => ApiKeyId -> Db be (Maybe (ApiKeys Identity, Bool))
keyAndUser i = runSelectReturningOne . select $ do
  k <- filter_ (\k -> k.id ==. val_ i.text) (all_ wikiDb.api_keys)
  u <- join_ wikiDb.users (\u -> u.id ==. k.user_id)
  pure (k, u.disabled)

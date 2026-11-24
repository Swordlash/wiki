-- | The panel of API keys ("Wiki.App.View.ApiKeys"), the real component,
-- mounted on the in-memory DOM ("Test.Harness.MemDom"), its asks held in a
-- gate until the test answers each ('Gated'): what it shows in each state,
-- and one walk through making a key, seeing it once, and revoking it. Its
-- rules over any order of events are its machine's ("Test.ApiKeys").
module Test.ApiKeysDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Char (isAsciiLower)
import Data.IORef (IORef)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom (Gate, Held (..), PropScalar (..), View, answer, click, clickNode, find, findAll, held, hold, holds, newGate, property, settle, settleHolding, textOf, typeInto, withView)
import Test.Hspec
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Security (SecondFactorState (..))
import Wiki.App.Account (SignInError (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Mem (MemState, MemWiki, newMemWiki, runMemWiki, switchUser)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (Unknown), route)
import Wiki.App.Security (MonadSecurity (..))
import Wiki.App.View.ApiKeys (Input (..), component, programAddress)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..))

-- | What the panel asks of the backend.
data Asked
  = AskList
  | AskProof
  | AskMake NewApiKey
  | AskRevoke ApiKeyId
  deriving stock (Show)

-- | The test's answer to an ask.
data Answered
  = Keys (Either WikiError [ApiKeyInfo])
  | Proof (Either WikiError SecondFactorState)
  | Minted (Either SignInError MadeApiKey)

-- | 'Gated' is the in-memory backend with the panel's four asks held in a
-- gate until the test answers each.
newtype Gated m a = Gated (ReaderT (Gate Asked Answered) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Gated where
  lift = Gated . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Gated m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Gated m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Gated m)

instance (MonadUUID m) => MonadUUID (Gated m) where
  generateV4 = lift generateV4

instance (MonadSecurity m, MonadIO m) => MonadSecurity (Gated m) where
  ownApiKeys =
    held' AskList >>= \case
      Keys r -> pure r
      _ -> panic "not keys"
  secondFactorState =
    held' AskProof >>= \case
      Proof r -> pure r
      _ -> panic "not a proof"
  makeApiKey new =
    held' (AskMake new) >>= \case
      Minted r -> pure r
      _ -> panic "not a key"
  revokeApiKey i =
    held' (AskRevoke i) >>= \case
      Keys r -> pure r
      _ -> panic "not keys"

held' :: (MonadIO m) => Asked -> Gated m Answered
held' asked = Gated (ask >>= \gate -> liftIO (hold gate asked))

runGated :: Gate Asked Answered -> IORef MemState -> Gated (MemWiki n) a -> n a
runGated gate wiki (Gated p) = runMemWiki wiki (runReaderT p gate)

-- | The panel mounted with an input, under the gate; the body gets the
-- view and a way to wait until it waits only for the asks held.
withPanel :: Input -> (Gate Asked Answered -> View (Const Void) Input Void -> IO () -> IO a) -> IO a
withPanel input body = do
  wiki <- newMemWiki (demoState demoNow)
  gate <- newGate
  let run :: Gated (MemWiki MemDOM) a -> MemDOM a
      run = runGated gate wiki
  withView run component input $ \view -> do
    let quiet = settleHolding view (holds gate)
    quiet
    body gate view quiet

-- | Answer every held ask that the function answers, oldest first.
answerAll :: Gate Asked Answered -> (Asked -> Maybe Answered) -> IO ()
answerAll gate f = held gate >>= traverse_ (\h -> traverse_ (answer gate h) (f h.asked))

editor :: Input
editor = Input {session = 0, administers = False, address = "https://wiki.example/wiki", demo = False}

day :: Integer -> Int -> Int -> UTCTime
day y m d = UTCTime (fromGregorian y m d) 0

-- | A key, by name, with its right, made on a day, expiring at a moment
-- or never.
keyNamed :: Text -> KeyRight -> UTCTime -> Maybe UTCTime -> ApiKeyInfo
keyNamed name right createdAt expiresAt = ApiKeyInfo {id = ApiKeyId name, name, right, createdAt, expiresAt, lastUsedAt = Nothing, revokedAt = Nothing}

-- | The answer to a proof: whether the user has a password.
proof :: Bool -> Answered
proof has = Proof (Right SecondFactorState {enabled = False, recoveryCodesLeft = 0, asksPassword = has})

-- | The rows of the list: each key's id and state class.
rows :: View q i o -> IO [(Text, Text)]
rows view = findAll view ".api-key" >>= traverse (\r -> (,) <$> (fromMaybe "" <$> N.attribute "data-key" r) <*> (fromMaybe "" <$> N.attribute "class" r))

-- | The values of the rights the form offers.
rightsOffered :: View q i o -> IO [Text]
rightsOffered view = findAll view "input[name=api-key-right]" >>= traverse (fmap scalarText . property "value")
  where
    scalarText = \case
      Just (ScalarText t) -> t
      _ -> ""

valueOf :: View q i o -> Text -> IO Text
valueOf view selector =
  find view selector >>= \case
    Just n -> property "value" n <&> \case Just (ScalarText t) -> t; _ -> ""
    Nothing -> pure ""

spec :: Spec
spec = describe "Wiki.App.View.ApiKeys, mounted on the in-memory DOM" $ do
  it "writes the wiki's address as a program reaches it, under the path the wiki is served from" $ do
    let at' base = runMemDOM (Router.newRouter base Unknown route) >>= \r -> programAddress "https://wiki.example/" r <$ Router.stop r
    at' "/" `shouldReturn` "https://wiki.example"
    at' "/wiki/" `shouldReturn` "https://wiki.example/wiki"
    at' "/wiki" `shouldReturn` "https://wiki.example/wiki"

  it "says loading, then that there is no key yet" $
    withPanel editor $ \gate view quiet -> do
      textOf view ".loading" `shouldReturn` "Loading…"
      answerAll gate (\case AskList -> Just (Keys (Right [])); AskProof -> Just (proof True); _ -> Nothing)
      quiet
      textOf view ".api-keys-empty" `shouldReturn` "You have no keys yet."
      -- No address of the API's description, and a line on what the keys
      -- are not.
      textOf view ".api-keys-about" >>= (`shouldSatisfy` T.isInfixOf "not the keys of an AI provider")
      isJust <$> find view ".api-keys-demo" `shouldReturn` False

  it "lists the keys that work first, then the expired and revoked ones dimmed, each the newest first, with Revoke on those that work" $
    withPanel editor $ \gate view quiet -> do
      let fresh = (keyNamed "fresh" ReadRight (day 2026 11 20) Nothing) {lastUsedAt = Just (day 2026 11 21)}
          gone = (keyNamed "gone" ReadRight (day 2026 11 19) Nothing) {revokedAt = Just (day 2026 11 19)}
          old = keyNamed "old" ReadRight (day 2020 1 1) (Just (day 2020 2 1))
          older = keyNamed "older" WriteRight (day 2019 1 1) Nothing
      answerAll gate (\case AskList -> Just (Keys (Right [fresh, gone, old, older])); AskProof -> Just (proof True); _ -> Nothing)
      quiet
      rows view
        `shouldReturn` [ ("fresh", "api-key api-key-works")
                       , ("older", "api-key api-key-works")
                       , ("gone", "api-key api-key-revoked")
                       , ("old", "api-key api-key-expired")
                       ]
      (findAll view ".api-key-state" >>= traverse N.textContent)
        `shouldReturn` ["Works", "Works", "Revoked 2026-11-19 00:00 UTC", "Expired"]
      length <$> findAll view ".api-key-revoke" `shouldReturn` 2
      textOf view "[data-key=fresh] .api-key-times" `shouldReturn` "Made 2026-11-20 00:00 UTC. Expires never. Last used around 2026-11-21 00:00 UTC."
      textOf view "[data-key=older] .api-key-right" `shouldReturn` "Read and write"
      textOf view "[data-key=old] .api-key-times" >>= (`shouldSatisfy` T.isInfixOf "Expired 2020-02-01")

  it "offers the right to administer only to an administrator, and asks no password of a user who has none" $ do
    withPanel editor $ \gate view quiet -> do
      rightsOffered view `shouldReturn` ["read", "write"]
      -- While whether the user has a password is not known, the field shows.
      isJust <$> find view "input[name=api-key-password]" `shouldReturn` True
      answerAll gate (\case AskProof -> Just (proof False); _ -> Nothing)
      quiet
      isJust <$> find view "input[name=api-key-password]" `shouldReturn` False
    withPanel editor {administers = True, demo = True} $ \_ view _ -> do
      rightsOffered view `shouldReturn` ["read", "write", "admin"]
      textOf view ".api-keys-demo" >>= (`shouldSatisfy` T.isInfixOf "sign in nowhere")

  it "shows the server's refusal beside the form in its words, and sends the password with one attempt only" $
    withPanel editor $ \gate view quiet -> do
      answerAll gate (\case AskList -> Just (Keys (Right [])); AskProof -> Just (proof True); _ -> Nothing)
      quiet
      typeInto view "input[name=api-key-password]" "battery staple"
      quiet
      click view "form.api-key-make button[type=submit]"
      quiet
      valueOf view "input[name=api-key-password]" `shouldReturn` ""
      [h] <- held gate
      case h.asked of
        AskMake new -> fmap (\p -> p.text) new.password `shouldBe` Just "battery staple"
        other -> expectationFailure ("not a make: " <> show other)
      answer gate h (Minted (Left WrongCredentials))
      quiet
      textOf view ".api-key-refusal" `shouldReturn` "Your current password is not right."
      -- The field is required, as for a passkey: empty, nothing is sent.
      click view "form.api-key-make button[type=submit]"
      quiet
      length <$> held gate `shouldReturn` 0
      typeInto view "input[name=api-key-password]" "correct horse"
      click view "form.api-key-make button[type=submit]"
      quiet
      answerAll gate (\case AskMake _ -> Just (Minted (Left (SignInFailed (Invalid "You have 20 keys that work already: revoke one first.")))); _ -> Nothing)
      quiet
      textOf view ".api-key-refusal" `shouldReturn` "You have 20 keys that work already: revoke one first."
      length <$> held gate `shouldReturn` 0

  it "makes a key, shows it once with its examples, lists it, and revokes it after asking once more" $
    withPanel editor $ \gate view quiet -> do
      answerAll gate (\case AskList -> Just (Keys (Right [])); AskProof -> Just (proof True); _ -> Nothing)
      quiet
      typeInto view "input[name=api-key-name]" "laptop"
      typeInto view "input[name=api-key-password]" "correct horse"
      findAll view "input[name=api-key-right]" >>= traverse_ clickNode . (`atMay` 1)
      quiet
      click view "form.api-key-make button[type=submit]"
      quiet
      let info = keyNamed "laptop" WriteRight (day 2026 11 23) Nothing
          secret = "wiki_laptop_" <> T.replicate 52 "s"
      [h] <- held gate
      case h.asked of
        AskMake new -> (new.name, new.right, isJust new.expiresAt) `shouldBe` ("laptop", WriteRight, True)
        other -> expectationFailure ("not a make: " <> show other)
      answer gate h (Minted (Right MadeApiKey {key = secret, info}))
      quiet
      -- The key, once, with its examples at the wiki's address.
      valueOf view ".api-key-text" `shouldReturn` secret
      textOf view ".api-key-made" >>= (`shouldSatisfy` T.isInfixOf "not shown again")
      (findAll view ".api-key-example" >>= traverse N.textContent)
        `shouldReturn` [ "curl -H 'Authorization: Bearer " <> secret <> "' 'https://wiki.example/wiki/api/list/'"
                       , "WIKI_TOKEN='" <> secret <> "' WIKI_URL='https://wiki.example/wiki' sh wiki.sh list"
                       ]
      -- Copy: natively there is no clipboard, so the field is selected for
      -- the keyboard, and the button says so.
      textOf view ".api-key-copy" `shouldReturn` "Copy"
      click view ".api-key-copy"
      quiet
      textOf view ".api-key-copy" `shouldReturn` "Select and copy"
      -- Listed at once by its id; a read follows.
      rows view `shouldReturn` [("laptop", "api-key api-key-works")]
      answerAll gate (\case AskList -> Just (Keys (Right [info])); _ -> Nothing)
      quiet
      click view ".api-key-made-close"
      quiet
      isJust <$> find view ".api-key-made" `shouldReturn` False
      T.isInfixOf secret <$> textOfAll view `shouldReturn` False
      rows view `shouldReturn` [("laptop", "api-key api-key-works")]
      -- Revoke, asked once more.
      click view "[data-key=laptop] .api-key-revoke"
      quiet
      textOf view ".api-key-confirm span" `shouldReturn` "Revoke 'laptop'? Programs using it stop at once."
      length <$> held gate `shouldReturn` 0
      click view ".api-key-revoke-yes"
      quiet
      let revoked = info {revokedAt = Just (day 2026 11 23)}
      answerAll gate (\case AskRevoke (ApiKeyId "laptop") -> Just (Keys (Right [revoked])); _ -> Nothing)
      quiet
      rows view `shouldReturn` [("laptop", "api-key api-key-revoked")]
      isJust <$> find view ".api-key-revoke" `shouldReturn` False

  it "makes, lists and revokes a key over the in-memory backend, as the demo does, asking no password" $ do
    wiki <- newMemWiki (demoState demoNow)
    runMemWiki wiki (switchUser (Just (UserId "user-editor")))
    withView (runMemWiki wiki) component editor {demo = True} $ \view -> do
      settle view
      textOf view ".api-keys-demo" >>= (`shouldSatisfy` T.isInfixOf "sign in nowhere")
      textOf view ".api-keys-empty" `shouldReturn` "You have no keys yet."
      isJust <$> find view "input[name=api-key-password]" `shouldReturn` False
      typeInto view "input[name=api-key-name]" "laptop"
      click view "form.api-key-make button[type=submit]"
      settle view
      key <- valueOf view ".api-key-text"
      let base32 c = isAsciiLower c || (c >= '2' && c <= '7')
      case T.splitOn "_" key of
        ["wiki", i, secret] -> (T.length i, T.length secret, T.all base32 (i <> secret)) `shouldBe` (16, 52, True)
        _ -> expectationFailure ("not a key of the server's form: " <> toS key)
      click view ".api-key-made-close"
      settle view
      [(i, cls)] <- rows view
      cls `shouldBe` "api-key api-key-works"
      textOf view ".api-key-name" `shouldReturn` "laptop"
      click view ".api-key-revoke"
      settle view
      click view ".api-key-revoke-yes"
      settle view
      rows view `shouldReturn` [(i, "api-key api-key-revoked")]
  where
    textOfAll view = find view ".api-keys" >>= maybe (pure "") N.textContent

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The sign-in of the API for a page's Markdown ("Wiki.Server.BasicSignIn"),
-- run directly against a test server's database, with a memory of
-- sign-ins and a limiter of the test's own: what happens when the account
-- changes during a check, when several requests carry the same password at
-- once, and what is left of a password once its request is answered.
--
-- A check runs through the hook 'newBasicSignInsWith' takes, which lets a
-- test change the account while the check runs, or hold the check until
-- other requests have joined it.
module Test.Server.BasicSignIn (spec) where

import Data.IORef (atomicModifyIORef', newIORef, writeIORef)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import GHC.Exts (mkWeakNoFinalizer#)
import GHC.IO (IO (IO))
import GHC.Weak (Weak (..), deRefWeak)
import Protolude
import System.IO.Error (userError)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.Database (Backing)
import Test.Hspec
import Test.Server.Fixture
import Test.Server.Hygiene (setUp)
import Wiki.Api (AdminApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Links (links)
import Wiki.Api.Opaque (Stretch (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Server.BasicSignIn
import Wiki.Server.Limiter (Limiter, Verdict (Allowed), defaultLimits, newLimiter)
import Wiki.Server.Limiter qualified as Limiter
import Wiki.Server.Opaque (defaultStretch)
import Wiki.Server.Storage (Storage, withStorageAside)

spec :: Backing -> Spec
spec backing = describe "the API's sign-in" $ do
  budget
  readAgain backing
  shared backing
  alike backing
  leftOfThePassword backing

-- | What a test signs in with: the test server's database, a limiter and
-- a memory of sign-ins of its own.
data Signing = Signing
  { running :: Running
  , storage :: Storage
  , limiter :: Limiter
  , signIns :: BasicSignIns
  }

-- | The demo with passwords for the editor and the reader, and a sign-in
-- whose checks run through the hook given.
withSigning :: Backing -> (Running -> IO Bool -> IO Bool) -> (Signing -> IO a) -> IO a
withSigning backing checkWith test = withDemoRunning backing identity $ \running -> do
  givePassword running "user-editor" "editor's password"
  givePassword running "user-reader" "reader's password"
  withStorageAside running.place $ \storage -> do
    limiter <- newLimiter defaultLimits
    signIns <- newBasicSignInsWith defaultStretch (checkWith running)
    test Signing {running, storage, limiter, signIns}

-- | Sign in with a login and a password, from the tests' address.
signIn :: Signing -> Text -> Text -> IO Outcome
signIn s login password = do
  now <- getCurrentTime
  signInAt s now login password

signInAt :: Signing -> UTCTime -> Text -> Text -> IO Outcome
signInAt s now = signInBasic s.storage s.limiter s.signIns now address

address :: Text
address = "127.0.0.1"

editorId :: UserId
editorId = UserId "user-editor"

budget :: Spec
budget = describe "the budget of stretching" $
  it "takes a record with no more passes and no more memory than it" $ do
    withinBudget defaultStretch defaultStretch `shouldBe` True
    withinBudget defaultStretch defaultStretch {opsLimit = 1, memLimit = 8192} `shouldBe` True
    withinBudget defaultStretch defaultStretch {opsLimit = defaultStretch.opsLimit + 1} `shouldBe` False
    withinBudget defaultStretch defaultStretch {memLimit = defaultStretch.memLimit + 1024} `shouldBe` False

-- | A check whose account changes while it runs: the change is made by
-- the action the test puts in the returned 'IORef', once, right after the
-- check.
readAgain :: Backing -> Spec
readAgain backing = describe "after a check, the account read again" $ do
  it "refuses a password checked against a record replaced during the check, and does not remember it" $ changing $ \s during -> do
    writeIORef during (givePassword s.running "user-editor" "a new password")
    signIn s "editor" "editor's password" `shouldReturn` Refused
    fullChecks s.signIns `shouldReturn` 1
    signIn s "editor" "editor's password" `shouldReturn` Refused
    fullChecks s.signIns `shouldReturn` 2
    signIn s "editor" "a new password" `shouldReturn` SignedIn editorId

  it "answers by the second factor turned on during the check" $ changing $ \s during -> do
    token <- signInAs s.running "editor" "editor's password"
    writeIORef during (void (setUp s.running token))
    signIn s "editor" "editor's password" `shouldReturn` SecondFactorOn
    rememberedLogins s.signIns `shouldReturn` 1
    signIn s "editor" "editor's password" `shouldReturn` SecondFactorOn

  it "refuses an account disabled during the check" $ changing $ \s during -> do
    writeIORef during $ do
      _ :: Account <- sendJson s.running "PUT" (s.running.admin <> "/admin/" <> links.admin.setDisabled editorId) AccountDisabled {disabled = True} >>= jsonAs 200
      pass
    signIn s "editor" "editor's password" `shouldReturn` Refused
    rememberedLogins s.signIns `shouldReturn` 0
  where
    changing test = do
      during <- newIORef pass
      let checkWith _ run = do
            right <- run
            join (atomicModifyIORef' during (pass,))
            pure right
      withSigning backing checkWith $ \s -> test s during

-- | Requests with the same login and password at once.
shared :: Backing -> Spec
shared backing = describe "requests with the same password at once" $ do
  it "share one check, counted once as a failure, and are all let in" $ do
    gate <- newEmptyMVar
    entered <- newEmptyMVar
    let checkWith _ run = void (tryPutMVar entered ()) >> readMVar gate >> run
    withSigning backing checkWith $ \s -> do
      requests <- replicateM 5 (async (signIn s "editor" "editor's password"))
      joined s 5
      takeMVar entered
      -- One failure is counted for the five, so the login may still be
      -- tried: the browser's sign-in is not refused meanwhile.
      now <- getCurrentTime
      Limiter.verdict s.limiter now "editor" address `shouldReturn` Allowed
      putMVar gate ()
      traverse wait requests `shouldReturn` replicate 5 (SignedIn editorId)
      fullChecks s.signIns `shouldReturn` 1
      signIn s "editor" "editor's password" `shouldReturn` SignedIn editorId
      fullChecks s.signIns `shouldReturn` 1

  it "start again when the check they wait for ends without a result, and are let in only by a check" $ do
    gate <- newEmptyMVar
    entered <- newEmptyMVar
    failing <- newIORef True
    let checkWith _ run = do
          void (tryPutMVar entered ())
          readMVar gate
          first' <- atomicModifyIORef' failing (False,)
          if first' then throwIO (userError "the check failed") else run
    withSigning backing checkWith $ \s -> do
      requests <- replicateM 5 (async (signIn s "editor" "editor's password"))
      joined s 5
      takeMVar entered
      putMVar gate ()
      results <- traverse waitCatch requests
      length (lefts results) `shouldBe` 1
      rights results `shouldBe` replicate 4 (SignedIn editorId)
      fullChecks s.signIns `shouldReturn` 1

-- | Wait until this many requests wait for, or run, a check, or fail after
-- ten seconds.
joined :: Signing -> Int -> IO ()
joined s n =
  timeout 10000000 (atomically (sharingChecks s.signIns >>= \k -> unless (k >= n) retry))
    >>= maybe (expectationFailure ("fewer than " <> show n <> " requests share the check")) pure

-- | Logins that cannot sign in with a password, to someone without the
-- password: the same as a usable account given a wrong one.
alike :: Backing -> Spec
alike backing = describe "a login that cannot sign in with a password" $ do
  it "answers five wrong passwords at once and a sixth as a usable account does, and is limited alike" $ withSigning backing (const identity) $ \s -> do
    bursts <- burstsOf s
    map (\(l, (five, sixth, v, _)) -> (l, five, sixth, v)) bursts
      `shouldBe` [(l, replicate 5 Refused, Refused, Allowed) | l <- logins]

  it "runs as many full checks as a usable account does" $ withSigning backing (const identity) $ \s -> do
    bursts <- burstsOf s
    map (\(l, (_, _, _, checks)) -> (l, checks)) bursts `shouldBe` [(l, 2) | l <- logins]
  where
    -- A usable account, a login nobody has, a disabled account and one
    -- without a password.
    logins = ["editor", "nobody", "reader", "admin"]
    burstsOf s = do
      _ :: Account <- sendJson s.running "PUT" (s.running.admin <> "/admin/" <> links.admin.setDisabled (UserId "user-reader")) AccountDisabled {disabled = True} >>= jsonAs 200
      for logins $ \l -> (l,) <$> burst s l
    -- Five requests at once with one wrong password, held until all five
    -- have joined, then a sixth: their outcomes, the limit's verdict after
    -- them, and how many full checks ran; with a limiter and a memory of
    -- sign-ins fresh for the login.
    burst s0 login = do
      gate <- newEmptyMVar
      limiter <- newLimiter defaultLimits
      signIns <- newBasicSignInsWith defaultStretch (\run -> readMVar gate >> run)
      let s = s0 {limiter, signIns}
      requests <- replicateM 5 (async (signIn s login "a wrong password"))
      joined s 5
      putMVar gate ()
      five <- traverse wait requests
      sixth <- signIn s login "a wrong password"
      now <- getCurrentTime
      verdict <- Limiter.verdict limiter now login address
      checks <- fullChecks signIns
      pure (five, sixth, verdict, checks)

-- | What is left of a password once the request that carried it is
-- answered.
leftOfThePassword :: Backing -> Spec
leftOfThePassword backing = describe "the password" $ do
  it "is not held by what the server remembers of the sign-in" $ withSigning backing (const identity) $ \s -> do
    weak <- do
      password <- evaluate (T.copy (T.pack "editor's password"))
      w <- weakOfText password
      signIn s "editor" password `shouldReturn` SignedIn editorId
      pure w
    performMajorGC
    deRefWeak weak >>= (`shouldSatisfy` isNothing)
    -- Read after the collection, so that the memory of sign-ins was alive
    -- through it: the password is gone though what was remembered is not.
    rememberedLogins s.signIns `shouldReturn` 1

  it "is forgotten once remembered too long, by any later request" $ withSigning backing (const identity) $ \s -> do
    now <- getCurrentTime
    signInAt s now "editor" "editor's password" `shouldReturn` SignedIn editorId
    rememberedLogins s.signIns `shouldReturn` 1
    signInAt s (addUTCTime (rememberFor + 1) now) "reader" "not the password" `shouldReturn` Refused
    rememberedLogins s.signIns `shouldReturn` 0

-- | A weak pointer to the bytes of a text: dead once nothing holds them.
-- The 'Text' itself is a box GHC may copy, so the pointer is to its array.
weakOfText :: Text -> IO (Weak ())
weakOfText (TI.Text (TA.ByteArray bytes) _ _) = IO $ \st -> case mkWeakNoFinalizer# bytes () st of
  (# st', w #) -> (# st', Weak w #)

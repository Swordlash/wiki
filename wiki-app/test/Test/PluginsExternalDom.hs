-- | An external plugin in the editor ("Wiki.App.View.Editor",
-- "Wiki.App.Plugin.External.Frames"), the real components, on the
-- in-memory DOM ("Test.Harness.MemDom") over the demo's wiki with one
-- plugin installed for everyone, on a clock the test owns. There is no
-- browser natively, so the test stands in the plugin's frame
-- ('openPluginFrame'): it sees what the editor sends the plugin, and says
-- what the plugin answers, when it chooses.
module Test.PluginsExternalDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External (Contribution (..), ExternalManifest (..), ExternalPlugin (..), ExternalPolicy (..), Permission (..), framePath)
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands (..))
import Wiki.App.Plugin.External.Frame (FrameSaid (..), PluginFrame (..))
import Wiki.App.Plugin.External.Machine (answerWithin)
import Wiki.App.Plugin.External.Protocol (utf16Length)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (id))

-- | 'Stand' is a plugin's frame as the test holds it in the frame's
-- place: the address the editor opened, how the plugin says something,
-- what the editor sent it, and whether the editor closed it.
data Stand = Stand
  { address :: Text
  , say :: FrameSaid -> IO ()
  , posted :: IORef [Value]
  , closed :: IORef Bool
  }

-- | The demo's backend, with the test standing in every plugin's frame.
newtype Plugged m a = Plugged (ReaderT (IORef [Stand]) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Plugged where
  lift = Plugged . lift

runPlugged :: IORef [Stand] -> Plugged m a -> m a
runPlugged stands (Plugged m) = runReaderT m stands

deriving newtype instance (MonadDOM m) => MonadDOM (Plugged m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Plugged m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Plugged m)

instance (MonadUUID m) => MonadUUID (Plugged m) where
  generateV4 = lift generateV4

instance (MonadWiki m) => MonadWiki (Plugged m)

instance (MonadWiki m) => MonadLoad (Plugged m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Plugged m)

instance (MonadNotifications m) => MonadNotifications (Plugged m)

instance (MonadVault m) => MonadVault (Plugged m)

instance (MonadAssistant m) => MonadAssistant (Plugged m)

instance (MonadIslands m, MonadIO m) => MonadIslands (Plugged m) where
  openPluginFrame _ address say = Plugged $ do
    stands <- ask
    liftIO $ do
      posted <- newIORef []
      closed <- newIORef False
      atomicModifyIORef' stands (\s -> (s <> [Stand {address, say, posted, closed}], ()))
      pure PluginFrame {post = \v -> atomicModifyIORef' posted (\vs -> (vs <> [v], ())), close = writeIORef closed True}

-- | The plugin installed for everyone.
doubled :: ExternalPlugin
doubled =
  ExternalPlugin
    { manifest =
        ExternalManifest
          { id = PluginId "doubled-words"
          , name = "Doubled words"
          , version = "1.0.0"
          , description = ""
          , api = 1
          , contributes = Set.singleton Diagnostics
          , permissions = Set.singleton DocumentRead
          , hosts = []
          }
    , bundle = "0123abcd"
    , size = 100
    , mine = False
    , installedAt = UTCTime (fromGregorian 2026 11 25) 0
    }

-- | An ask the editor sent: its number and its text.
askOf :: Value -> Maybe (Int, Text)
askOf v = case Aeson.fromJSON v of
  Aeson.Success (m :: Map Text Value) -> case (Map.lookup "type" m, Map.lookup "id" m, Map.lookup "text" m) of
    (Just (Aeson.String "diagnose"), Just n, Just (Aeson.String t)) | Aeson.Success i <- Aeson.fromJSON n -> Just (i, t)
    _ -> Nothing
  _ -> Nothing

-- | The plugin's answer to an ask: a warning on each "the the" of the text,
-- at its UTF-16 offsets.
answerFor :: (Int, Text) -> FrameSaid
answerFor (n, text) =
  Message
    ( object
        [ "type" .= ("problems" :: Text)
        , "re" .= n
        , "problems"
            .= [ object ["from" .= at, "to" .= (at + 7), "severity" .= ("warning" :: Text), "message" .= ("Doubled word: the" :: Text)]
               | (preceding, _) <- T.breakOnAll "the the" text
               , let at = utf16Length preceding
               ]
        ]
    )

spec :: Spec
spec = describe "Wiki.App.Plugin.External.Frames in the editor, on the in-memory DOM" $
  it "counts an external plugin's problems for exactly the text they answer, and shows the plugin stopped when it does not answer" $
    withTestClock $ \clock -> do
      stands <- newIORef []
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor"), externalPolicy = ExternalWiki, externalWiki = Map.fromList [(PluginId "doubled-words", doubled)]})
      let run :: Plugged (MemWiki MemDOM) a -> MemDOM a
          run = runMemWiki wiki . runPlugged stands
          orPanic = either (panic . show) identity
      router <- runMemDOM (run (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps) <- runMemDOM . run $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        pure (me, spaces, blog, ps)
      let input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
          problems view = find view ".editor-problems" >>= maybe (pure Nothing) (fmap Just . N.textContent)
          asked stand = mapMaybe askOf <$> readIORef stand.posted
      withView run Editor.component input $ \view -> do
        settle view
        -- One frame, at the plugin's address, asked nothing before the
        -- plugin says it is ready.
        stand <-
          readIORef stands >>= \case
            [s] -> pure s
            other -> panic ("frames opened: " <> show (length other))
        stand.address `shouldBe` framePath doubled
        asked stand `shouldReturn` []
        stand.say (Message (object ["type" .= ("ready" :: Text)]))
        settle view
        -- The text as last checked: nothing is checked natively before
        -- the first typing.
        first' <-
          asked stand >>= \case
            [a] -> pure a
            other -> panic ("asks: " <> show other)
        snd first' `shouldBe` ""
        -- The author writes "the the", and the text is checked.
        field <- element view ".editor-text"
        original <- N.textContent field
        let doubledText = original <> "\n\nSaid the the cat.\n"
        typeIntoNode field doubledText
        advance clock Editor.previewDelay
        settle view
        -- The answer for the first text comes now: its text is not the one
        -- checked, so nothing is counted, though the answer holds a
        -- problem (made up for the test, on the first line).
        stand.say (Message (object ["type" .= ("problems" :: Text), "re" .= fst first', "problems" .= [object ["from" .= (0 :: Int), "to" .= (0 :: Int), "severity" .= ("error" :: Text), "message" .= ("old" :: Text)]]]))
        settle view
        problems view `shouldReturn` Nothing
        -- The newest text went out once that answer came, and its answer
        -- is counted.
        second' <-
          asked stand >>= \case
            [_, a] -> pure a
            other -> panic ("asks: " <> show other)
        snd second' `shouldBe` doubledText
        stand.say (answerFor second')
        settle view
        problems view `shouldReturn` Just "1 problem"
        -- More typing: once checked, the problem was of another text.
        typeIntoNode field (doubledText <> "More.\n")
        advance clock Editor.previewDelay
        settle view
        problems view `shouldReturn` Nothing
        third' <-
          asked stand >>= \case
            [_, _, a] -> pure a
            other -> panic ("asks: " <> show other)
        -- The plugin does not answer: it is stopped, its frame closed, and
        -- the editor says why.
        isNothing <$> find view ".editor-plugin-stopped" `shouldReturn` True
        readIORef stand.closed `shouldReturn` False
        advance clock answerWithin
        settle view
        textOf view ".editor-plugin-stopped" >>= (`shouldSatisfy` T.isInfixOf "Doubled words was stopped: it did not answer within 10 seconds")
        readIORef stand.closed `shouldReturn` True
        -- Its late answer changes nothing.
        stand.say (answerFor third')
        settle view
        problems view `shouldReturn` Nothing
        length <$> readIORef stands `shouldReturn` 1
      Router.stop router
  where
    slugs :: [Text] -> [Slug]
    slugs = mapMaybe (rightToMaybe . mkSlug)

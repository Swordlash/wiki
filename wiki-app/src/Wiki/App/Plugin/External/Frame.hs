{-# LANGUAGE TemplateHaskell #-}

-- | An external plugin's frame in the browser (@jsbits/plugin-frames.js@;
-- WP5.84 of docs/PLAN.md, decision 69): the sandboxed @\<iframe>@ that runs
-- a plugin's code, and the 'MessagePort' the editor talks to it over.
--
-- 'openFrame' makes the frame inside an element Halogen does not look
-- inside, so that Halogen never moves it (a frame moved in the DOM loads
-- again). At the frame's first load the JavaScript connects: it gives the
-- frame one end of a new @MessageChannel@ in the message
-- @{wikiPlugin: "connect", api: 1}@, which the wiki's runtime in the frame
-- takes (@web\/plugin\/runtime.js@). Every message after that goes over the
-- port, as a string of JSON, both ways.
--
-- Nothing the plugin sends is worked on beyond what it is, and no more
-- than the host asked for. The JavaScript takes a message only when it is
-- a string of at most 'maxFrameMessage' characters, which it checks
-- before anything else: an object, however small, could stand for a
-- graph whose JSON is exponentially larger. And it takes only as many
-- messages as the host allows: one, its @ready@, once connected, and one
-- more for each message the host posts. A message beyond those closes
-- the port at once, so a plugin that floods the port reaches the program
-- once ('Unasked'). The program checks the length again ('saidOf'), so
-- that its JSON parse is bounded whatever the JavaScript let through.
--
-- The program reaches the frame through 'PluginFrame' only, which
-- the class "Wiki.App.Islands.Monad" hands out ('openPluginFrame'), so a
-- test can stand a plugin of its own in the frame's place.
module Wiki.App.Plugin.External.Frame
  ( PluginFrame (..)
  , FrameSaid (..)
  , openFrame
  , saidOf
  , maxFrameMessage
  )
where

import Data.Aeson (Value, decodeStrict', encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Halogen.JSBits (Callback, JSText, Safety (..), browserJS, freeCallback, inBrowser, jsValText, mkSyncCallback, toJSText)
import Protolude
-- With its constructor: the FFI passes the value inside the newtype.
import Web.DOM.Internal.Types (Element (..))
import Wiki.App.Plugin.External.Protocol (maxFrameMessage)

$( browserJS
     ["jsbits/plugin-frames.js"]
     [ ("js_frame_open", "wiki_plugin_frame_open", Safe, [t|Element -> JSText -> Callback -> IO Int|])
     , ("js_frame_post", "wiki_plugin_frame_post", Safe, [t|Int -> JSText -> IO ()|])
     , ("js_frame_close", "wiki_plugin_frame_close", Safe, [t|Int -> IO ()|])
     ]
 )

-- | 'PluginFrame' is a plugin's frame as the program holds it: how to send
-- the plugin a message ('post'), and how to close the frame ('close'),
-- which ends the plugin's code and frees what the frame holds. A frame is
-- closed once; nothing it says after is heard.
data PluginFrame = PluginFrame
  { post :: Value -> IO ()
  , close :: IO ()
  }

-- | 'FrameSaid' is what comes from a plugin's frame: a message the plugin
-- (or the wiki's runtime around it) sent over the port, as JSON; that the
-- frame went to another page; a message that is not a string of at most
-- 'maxFrameMessage' characters, or not JSON; or a message beyond the ones
-- the host asked for, after which the port is closed.
data FrameSaid
  = Message Value
  | Navigated
  | Unreadable
  | Unasked
  deriving stock (Eq, Show)

-- | Make a plugin's frame of this address in this element, and hand each
-- thing it says to the function. Natively there is no browser: the frame
-- is one that never says anything, and its plugin is stopped once it has
-- not answered in time.
openFrame :: Element -> Text -> (FrameSaid -> IO ()) -> IO PluginFrame
openFrame container address said
  | inBrowser = do
      callback <- mkSyncCallback (said . saidOf . jsValText)
      number <- js_frame_open container (toJSText address) callback
      pure
        PluginFrame
          { post = js_frame_post number . toJSText . decodeUtf8 . BSL.toStrict . encode
          , close = js_frame_close number >> freeCallback callback
          }
  | otherwise = pure PluginFrame {post = \_ -> pass, close = pass}

-- | What the frame's JavaScript said (@jsbits/plugin-frames.js@): a letter,
-- then for a message the plugin's string as it came. @m@ and the message,
-- @n@ the frame went to another page, @u@ a message that is not a string
-- or is too long, @x@ a message beyond those asked for. A message longer
-- than 'maxFrameMessage', or not JSON, is 'Unreadable': the length is
-- checked before the parse, which is then bounded by it.
saidOf :: Text -> FrameSaid
saidOf said = case T.uncons said of
  Just ('m', message)
    | T.compareLength message maxFrameMessage /= GT -> maybe Unreadable Message (decodeStrict' (encodeUtf8 message))
  Just ('n', "") -> Navigated
  Just ('x', "") -> Unasked
  _ -> Unreadable

{-# LANGUAGE TemplateHaskell #-}

-- | What a browser test waits on that is outside the program's loop, where
-- @waitQuiet@ cannot see it: a change of the page that a script of the
-- wiki's own makes when the browser tells it something (@jsbits/toc.js@
-- marks the section being read when an @IntersectionObserver@ calls it; a
-- code block's copy button says "Copied" when the clipboard's promise
-- settles, and goes back by a timer of its own), and a CSS transition.
--
-- Each wait hangs on the browser's own signal (a @MutationObserver@'s
-- records, an animation's @finished@ promise), not on a clock, and is named
-- for what it waits for. It gives up only at 'watchdogMs', which no passing
-- test comes near, and then fails saying what did not come. After it the
-- test reads once.
module Test.Browser.Outside
  ( watchdogMs
  , untilAttribute
  , untilTransitionsDone
  )
where

import Halogen.JSBits (Inert (..), JSText, Safety (..), browserJS, inBrowser, toJSText)
import Halogen.JSBits.Promise (Promise (..), wait)
import Protolude hiding (wait)
import Test.Hspec.Halogen

$( browserJS
     ["jsbits/test-outside.js"]
     [ ("js_until_attributePromise", "wiki_test_until_attribute", Safe, [t|JSText -> JSText -> Bool -> JSText -> Int -> IO (Promise Bool)|])
     , ("js_transitions_donePromise", "wiki_test_transitions_done", Safe, [t|JSText -> Int -> IO (Promise Bool)|])
     ]
 )

js_until_attribute :: JSText -> JSText -> Bool -> JSText -> Int -> IO Bool
js_until_attribute a1 a2 a3 a4 a5 = if inBrowser then wait =<< js_until_attributePromise a1 a2 a3 a4 a5 else pure inert

js_transitions_done :: JSText -> Int -> IO Bool
js_transitions_done a1 a2 = if inBrowser then wait =<< js_transitions_donePromise a1 a2 else pure inert

-- | The suite's watchdog, in milliseconds: how long a wait here hangs
-- before it fails. It stands against a hang, not for how fast the page
-- is: a wait that passes ends on its signal, far sooner. The measure of
-- WP5.137 on GitHub's runners found rightful work of up to 5.4 seconds.
watchdogMs :: Int
watchdogMs = 30000

-- | Wait until the first element a selector finds has the attribute with
-- this value ('Just'), or does not have it ('Nothing'): at once when it is
-- so already, else on the change of the page that makes it so.
untilAttribute :: (HasCallStack) => Text -> Text -> Maybe Text -> PageM s ()
untilAttribute selector attribute value = do
  came <- unsafeIOToPageM (js_until_attribute (toJSText selector) (toJSText attribute) (isJust value) (toJSText (fromMaybe "" value)) watchdogMs)
  unless came . expectationFailure $
    "after " <> show watchdogMs <> " ms, " <> selector <> " still did not " <> maybe ("lack " <> attribute) (\v -> "have " <> attribute <> "=" <> show v) value

-- | Wait until the CSS transitions of the first element a selector finds
-- have finished: at once when it runs none.
untilTransitionsDone :: (HasCallStack) => Text -> PageM s ()
untilTransitionsDone selector = do
  done <- unsafeIOToPageM (js_transitions_done (toJSText selector) watchdogMs)
  unless done . expectationFailure $
    "after " <> show watchdogMs <> " ms, the transitions of " <> selector <> " had not finished, or there was no such element"

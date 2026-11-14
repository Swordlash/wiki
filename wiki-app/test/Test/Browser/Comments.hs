{-# LANGUAGE TemplateHaskell #-}

-- | A page's comments in a browser, over the demo's wiki (WP4.5 of
-- docs/PLAN.md), for what needs one: text selected in a page, after a
-- code block and a formula, commented on from the button beside it, and
-- marked where it is; a link to the selection copied; a quote clicked, and
-- a thread's quote chosen; and someone not signed in, who reads the
-- threads and is told to sign in. The threads' writes and what the page
-- view makes of them are tested natively ("Test.CommentsDom").
module Test.Browser.Comments (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Text qualified as T
import Halogen.JSBits (JSText, Safety (..), browserJS, toJSText)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Browser.Mount (clickButton, slugs, start)
import Test.Hspec.Halogen
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..))

-- Selecting text as a reader does: test/jsbits/test-select.js.
$( browserJS
     ["jsbits/test-select.js"]
     [ ("js_select", "wiki_test_select", Safe, [t|JSText -> JSText -> JSText -> IO Bool|])
     , ("js_unselect", "wiki_test_unselect", Safe, [t|JSText -> IO ()|])
     ]
 )

-- | Select the page's text from one phrase to another, and let go.
select :: (HasCallStack) => Text -> Text -> PageM s ()
select first' last' = do
  found <- unsafeIOToPageM (js_select (toJSText ".page-body") (toJSText first') (toJSText last'))
  found `shouldBe` True

notes, hello :: Route
notes = Page (slugs ["blog"]) (slugs ["notes-on-haskell"])
hello = Page (slugs ["blog"]) (slugs ["hello-world"])

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
spec m = do
  it "comments on text selected after a code block and a formula, and marks it" $ runPage $ do
    ui <- start m (Just "user-reader") notes
    _ <- find ui ".page-view .wiki-code"
    find ui ".comments-toggle" >>= (`shouldHaveText` "Comments (0)")
    select "computed once" "asks for it"
    find ui ".comment-on-selection" >>= click
    find ui ".comments-panel .new-thread .comment-quote" >>= (`shouldHaveText` "computed once something asks for it")
    text <- find ui ".comments-panel .new-thread textarea"
    typeText text "Only *once*?"
    clickButton ui ".comments-panel .new-thread button" "Comment"
    find ui ".page-body mark.comment-mark" >>= (`shouldHaveText` "computed once something asks for it")
    find ui ".comments-panel .comment-thread .comment-body em" >>= (`shouldHaveText` "once")
    find ui ".comments-panel .comment-author" >>= (`shouldHaveText` "Rita Reader")
    find ui ".comments-toggle" >>= (`shouldHaveText` "Comments (1)")
    length <$> findAll ui ".comments-panel .new-thread" `shouldReturn` 0

  it "offers no button for a caret, and none to someone not signed in" $ runPage $ do
    ui <- start m (Just "user-reader") notes
    _ <- find ui ".page-body"
    select "computed once" "asks for it"
    _ <- find ui ".comment-on-selection"
    unsafeIOToPageM (js_unselect (toJSText ".page-body"))
    eventually $ length <$> findAll ui ".comment-on-selection" `shouldReturn` 0
    anonymous <- start m Nothing notes
    _ <- find anonymous ".page-body"
    select "computed once" "asks for it"
    find anonymous ".page-comments .sign-in-to-comment" >>= (`shouldHaveText` "Sign in to comment.")
    length <$> findAll anonymous ".comment-on-selection" `shouldReturn` 0
    length <$> findAll anonymous ".comment-form" `shouldReturn` 0

  it "copies a link to the text selected, with the browser's text fragment, for anyone" $ runPage $ do
    anonymous <- start m Nothing notes
    _ <- find anonymous ".page-body"
    select "computed once" "asks for it"
    length <$> findAll anonymous ".comment-on-selection" `shouldReturn` 0
    find anonymous ".copy-selection-link" >>= click
    eventually $
      find anonymous ".link-copied-address" >>= textContent >>= (`shouldSatisfy` T.isSuffixOf "#:~:text=computed%20once%20something%20asks%20for%20it")
    find anonymous ".link-copied-label" >>= textContent >>= (`shouldSatisfy` (not . T.null))
    length <$> findAll anonymous ".copy-selection-link" `shouldReturn` 0

  it "copies a section's address from a heading's # and a box's, and stays on the page" $ runPage $ do
    ui <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide", "mathematics"]))
    find ui ".page-body a.heading-anchor[href='#macros']" >>= click
    eventually $ find ui ".link-copied-address" >>= textContent >>= (`shouldSatisfy` T.isSuffixOf "/#macros")
    find ui ".page-body h1" >>= (`shouldHaveText` "Mathematics")
    unmount ui
    guide <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    box <- find guide ".page-body .alert-exercise[id='box-exercise-1']"
    findIn box ".alert-title a.heading-anchor[href='#box-exercise-1']" >>= click
    eventually $ find guide ".link-copied-address" >>= textContent >>= (`shouldSatisfy` T.isSuffixOf "/#box-exercise-1")

  it "opens a thread from its quote in the page, and marks the quote of the thread chosen" $ runPage $ do
    ui <- start m Nothing hello
    length <$> findAll ui ".comments-panel" `shouldReturn` 0
    find ui "mark.comment-mark[data-thread='thread-hello-1']" >>= click
    find ui ".comments-panel #comment-thread-thread-hello-1.focused" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "newest first")
    find ui "mark.comment-mark.comment-mark-focused" >>= (`shouldHaveText` "a blog that is also a wiki")
    find ui ".comments-panel .comments-close" >>= click
    eventually $ length <$> findAll ui ".comments-panel" `shouldReturn` 0
    find ui ".comments-toggle" >>= click
    find ui ".comments-panel #comment-thread-thread-hello-2 .comment-quote" >>= click
    find ui ".comments-panel #comment-thread-thread-hello-2.focused" >>= (`shouldHaveClass` "orphaned")
    length <$> findAll ui ".comments-panel .comment-form" `shouldReturn` 0
    find ui ".comments-panel .sign-in-to-comment" >>= (`shouldHaveText` "Sign in to comment.")

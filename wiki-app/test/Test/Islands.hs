-- | The islands as "Halogen.Static" renders them: with no JavaScript to draw
-- them, each element holds its source as text, which is what a prerendered
-- page shows until the program has loaded. In a browser,
-- @Test.Browser.Islands@ checks the drawing.
--
-- And the island's wait for a drawing, on the in-memory DOM
-- ("Test.Harness.MemDom"), with drawings the test holds ('Holding').
module Test.Islands (spec) where

import Control.Concurrent.STM (TMVar, newEmptyTMVarIO, readTMVar)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Halogen.JSBits.Promise (Promise (..))
import Halogen.JSBits.Promise qualified as Promise
import Halogen.JSBits.Value (jsNull)
import Halogen.Static (renderToHtml)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude hiding (find)
import Test.Harness.MemDom (Activity (..), activity, setInput, settle, waitActivity, withView)
import Test.Hspec
import Wiki.App.Islands (Island (..), MonadIslands (..))
import Wiki.App.Islands qualified as Islands
import Wiki.App.Islands.Monad (Drawing (..))

-- | The in-memory DOM's islands, but for a diagram's, a picture's and a
-- frame's drawing: each is one the test holds, which ends when the test
-- fills the variable, and never before.
newtype Holding m a = Holding (ReaderT (TMVar ()) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Holding where
  lift = Holding . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Holding m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Holding m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Holding m)

instance (MonadUUID m) => MonadUUID (Holding m) where
  generateV4 = lift generateV4

instance (MonadIslands m, MonadIO m) => MonadIslands (Holding m) where
  renderMath mode macros tex = lift . renderMath mode macros tex
  checkMacros macros = lift . checkMacros macros
  builtinMacros = lift builtinMacros
  renderMermaid source element = lift (renderMermaid source element) >> held
  renderTikz source document element = lift (renderTikz source document element) >> held
  highlightCode language code = lift . highlightCode language code
  fitFrame element = lift (fitFrame element) >> held
  loadFrame url = lift . loadFrame url
  renderTime at = lift . renderTime at
  awaitDrawing (Drawing done) = liftIO (void (Promise.waitCatch done))

-- | A drawing that ends when the test fills the variable.
held :: (Monad m) => Holding m Drawing
held = Holding $ asks $ \gate -> Drawing Promise {rawPromise = jsNull, settled = Right <$> readTMVar gate}

spec :: Spec
spec = do
  prerendered
  waiting

-- | An island's wait for its drawing, which the program counts as
-- waiting ("Halogen.Activity"), is gone once the island no longer shows
-- that drawing.
waiting :: Spec
waiting = describe "Wiki.App.Islands, waiting for a drawing" $ do
  -- Codex's first round on #319: an input that starts no drawing left the
  -- fork of the one before counted as waiting.
  for_ [("a diagram", Mermaid "graph LR\n  a --> b"), ("a picture", Tikz "\\draw (0,0) -- (1,1);"), ("a frame", HtmlFrame Nothing "<p>a</p>")] $ \(name, drawing) ->
    it ("waits for " <> name <> " held, and for nothing once its input is a listing") $ do
      gate <- newEmptyTMVarIO
      withView (\(Holding p) -> runReaderT p gate) (Islands.component @(Const Void) @Void) drawing $ \view -> do
        waitActivity view (\a -> a.running == 0 && a.waiting == 1)
        setInput view (CodeBlock (Just "haskell") Nothing False "main = pure ()")
        -- Quiet, with the drawing still held: nothing waits for it.
        settle view
        (\a -> a.waiting) <$> activity view `shouldReturn` 0

prerendered :: Spec
prerendered = describe "Wiki.App.Islands, prerendered" $ do
  it "shows inline mathematics as its TeX, in a span" $
    renderToHtml Islands.component (MathInline mempty "x^2")
      `shouldReturn` "<span class=\"wiki-math wiki-math-inline\">x^2</span>"

  it "shows displayed mathematics as its TeX, in a div" $
    renderToHtml Islands.component (MathDisplay mempty "\\frac{1}{2}")
      `shouldReturn` "<div class=\"wiki-math wiki-math-display\">\\frac{1}{2}</div>"

  it "shows a diagram as its Mermaid source" $
    renderToHtml Islands.component (Mermaid "graph LR\n  a --> b")
      `shouldReturn` "<div class=\"wiki-mermaid\">graph LR\n  a --&gt; b</div>"

  it "shows a TikZ picture as its source, as written" $
    renderToHtml Islands.component (Tikz "\\draw (0,0) -- (1,1) node {$a<b$};")
      `shouldReturn` "<div class=\"wiki-tikz\">\\draw (0,0) -- (1,1) node {$a&lt;b$};</div>"

  it "shows code as plain text, escaped" $
    renderToHtml Islands.component (CodeBlock (Just "haskell") Nothing False "x < y && y > z")
      `shouldReturn` "<pre class=\"wiki-code\"><code>x &lt; y &amp;&amp; y &gt; z</code></pre>"

  it "shows numbered code with its gutter, an empty span for each line, before the code" $
    renderToHtml Islands.component (CodeBlock (Just "haskell") (Just 2) True "a\nb\nc")
      `shouldReturn` "<pre class=\"wiki-code numbered code-scroll\" style=\"max-height: 2lh; overflow: auto\"><span aria-hidden=\"true\" class=\"code-lines\"><span></span><span></span><span></span></span><code>a\nb\nc</code></pre>"

  it "shows an html fence's frame with its sandbox and its document in srcdoc, escaped, and nothing inside it" $
    renderToHtml Islands.component (HtmlFrame Nothing "<p title=\"&quot;\">a &amp; b</p></iframe><script>alert(1)</script>")
      `shouldReturn` ( "<iframe class=\"wiki-html\" sandbox=\"allow-same-origin allow-popups allow-popups-to-escape-sandbox\""
                         <> " srcdoc=\"&lt;p title=&quot;&amp;quot;&quot;&gt;a &amp;amp; b&lt;/p&gt;&lt;/iframe&gt;&lt;script&gt;alert(1)&lt;/script&gt;\""
                         <> " title=\"HTML block\"></iframe>"
                     )

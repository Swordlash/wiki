-- | The frames the wiki writes ("Wiki.Core.Markdown.Frame"): an html
-- fence's and an interactive SVG's, their sandboxes, the document an html
-- fence's holds, and which attachments have an interactive form.
module Test.Markdown.Frame (spec) where

import Data.Text qualified as T
import Protolude
import Test.Hspec
import Wiki.Core.Markdown.Frame

spec :: Spec
spec = describe "Markdown.Frame" $ do
  describe "the sandboxes" $ do
    it "never let a frame both run scripts and have the wiki's origin" $
      for_ sandboxes $ \sandbox ->
        (sandbox, all (`elem` T.words sandbox) ["allow-scripts", "allow-same-origin"]) `shouldBe` (sandbox, False)
    it "list both of the wiki's frames" $
      sandboxes `shouldMatchList` [frameSandbox, scriptSandbox]
    it "run no script in an html fence's frame, which keeps its origin and opens links in ordinary tabs" $
      T.words frameSandbox `shouldMatchList` ["allow-same-origin", "allow-popups", "allow-popups-to-escape-sandbox"]
    it "let the frame that runs scripts do nothing else: no origin of the wiki's, no popup" $
      T.words scriptSandbox `shouldBe` ["allow-scripts"]
    it "allow no form, navigation of the page, dialog or download in either" $
      for_ sandboxes $ \sandbox ->
        T.words sandbox `shouldSatisfy` all (`notElem` ["allow-forms", "allow-top-navigation", "allow-top-navigation-by-user-activation", "allow-downloads", "allow-modals"])

  describe "an interactive SVG" $ do
    it "is an attachment stored as an SVG, in any case of letters, with parameters or none" $ do
      framesInteractively "image/svg+xml" `shouldBe` True
      framesInteractively "Image/SVG+XML" `shouldBe` True
      framesInteractively "image/svg+xml; charset=utf-8" `shouldBe` True
    it "is no attachment of another type, an SVG's name notwithstanding" $
      for_ ["image/png", "text/html", "application/xml", "image/svg", "image/svg+xml-compressed", "image/svg+xml; name=\"é\"", "application/octet-stream", ""] $ \t ->
        (t, framesInteractively t) `shouldBe` (t, False)

  describe "the frame's document" $ do
    let key = frameKey "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><style>@import url(\"style.css\") layer(wiki);</style></head><body class=\"html-frame\"><div class=\"page-body\"><p>x</p></div></body></html>"
    it "imports the wiki's stylesheet into a layer, then holds the block in the page's wrapper" $
      frameDocument SystemScheme "<p>x</p>"
        `shouldBe` "<!DOCTYPE html><html data-wiki-frame=\"" <> key <> "\"><head><meta charset=\"utf-8\"><style>@import url(\"style.css\") layer(wiki);</style></head><body class=\"html-frame\"><div class=\"page-body\"><p>x</p></div></body></html>"
    -- The key, which the page's script compares with the document a frame
    -- shows: the same of the same document, on every machine the suite
    -- runs on (the browser's wasm included), and another for another
    -- block or another scheme.
    it "carries on its root the key of the document it is, the same for the same block" $ do
      key `shouldBe` "c4802ff3225bfbfc"
      frameDocument SystemScheme "<p>x</p>" `shouldBe` frameDocument SystemScheme "<p>x</p>"
      let keyOf document = T.takeWhile (/= '"') (T.drop (T.length "<!DOCTYPE html><html data-wiki-frame=\"") document)
          documents = [frameDocument scheme block | scheme <- [SystemScheme, LightScheme, DarkScheme], block <- ["", "<p>x</p>", "<p>y</p>", "<p>x</p> "]]
      length (ordNub (map keyOf documents)) `shouldBe` length documents
      map (T.length . keyOf) documents `shouldSatisfy` all (== 16)
    it "names a slide's colours on its body, and no colours of its own on a page" $ do
      frameDocument LightScheme "" `shouldSatisfy` T.isInfixOf "<body class=\"html-frame html-frame-light\">"
      frameDocument DarkScheme "" `shouldSatisfy` T.isInfixOf "<body class=\"html-frame html-frame-dark\">"

-- | The editor's answer to CodeMirror's question about the list of the
-- wiki's own Markdown ("Wiki.App.Editor.Complete"): against the text the
-- editor holds, from the sources it holds, in the shape
-- @web/src/editor.js@ reads.
module Test.Complete (spec) where

import Data.Aeson (Value, object, (.=))
import Protolude
import Test.Hspec
import Wiki.App.Editor.Complete
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))
import Wiki.Core.Markdown.Complete
import Wiki.Core.PageKind (PageKind (..))

-- | The answer, from no memo of the parser's readings.
answered :: [Source] -> Text -> Request -> Maybe Completion
answered found text request = fst (answer found noReadings text request)

spec :: Spec
spec = describe "Wiki.App.Editor.Complete" $ do
  let text = "# Title\n\n[[!div]]\n\n[[/"
      atEnd = Request {line = 5, before = "[[/", nodes = ["Paragraph", "Document"]}
      document = sources Held {kind = Document, boxes = []}
  describe "answer" $ do
    it "answers from the text the editor holds, at the request's line" $
      (map (.label) . (.candidates) <$> answered document text atEnd) `shouldBe` Just ["[[/div]]"]
    it "answers nothing when the line no longer starts as CodeMirror saw it" $ do
      -- The block is open still, but the line was typed over since.
      answered document "# Title\n\n[[!div]]\n\nText" atEnd `shouldBe` Nothing
      answered document "# Title\n\n[[!div]]\n\n[[" atEnd `shouldBe` Nothing
    it "answers nothing for a line the text does not have" $ do
      answered document text Request {line = 9, before = "[[/", nodes = []} `shouldBe` Nothing
      answered document text Request {line = 0, before = "[[/", nodes = []} `shouldBe` Nothing
    it "answers nothing where the syntax tree says the text is its own" $
      answered document text Request {line = 5, before = "[[/", nodes = ["CodeText", "FencedCode", "Document"]} `shouldBe` Nothing

    it "answers from the memo of the parser's readings as from none, and forgets it when the lines above change" $ do
      let (first', memo) = answer document noReadings text atEnd
          (again, _) = answer document memo text atEnd
          (other, _) = answer document memo "# Title\n\n[[!notes]]\n\n[[/" atEnd
      (map (.label) . (.candidates) <$> first') `shouldBe` Just ["[[/div]]"]
      again `shouldBe` first'
      (map (.label) . (.candidates) <$> other) `shouldBe` Just ["[[/notes]]"]

  describe "sources" $ do
    it "offers a document's markers, and a presentation's" $ do
      let offered kind = map (.label) . (.candidates) <$> answered (sources Held {kind, boxes = []}) "[[!" Request {line = 1, before = "[[!", nodes = []}
      offered Document `shouldBe` Just ["[[!spoiler]]", "[[!div]]", "[[!toc]]"]
      length <$> offered Presentation `shouldBe` Just 7
    it "offers the kinds of box the administration defined" $ do
      let defined = [BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}]
      (map (.label) . (.candidates) <$> answered (sources Held {kind = Document, boxes = defined}) "> [!ex" Request {line = 1, before = "> [!ex", nodes = []})
        `shouldBe` Just ["[!EXERCISE]"]

  describe "completionJson" $ do
    it "gives a candidate from elsewhere than the wiki its origin as the detail, and the wiki's own none" $ do
      let made = Completion {from = 2, candidates = [own, fromPlugin], enter = EnterChooses, escape = EscapeCloses}
          own = Candidate {label = "[[!div]]", skeleton = [Literal "[[!div]]", Field 0 ""], origin = Nothing}
          fromPlugin = Candidate {label = "[[!day]]", skeleton = [Literal "[[!day]]{", Field 1 "date", Literal "}"], origin = Just (FromPlugin "calendar")}
      completionJson made
        `shouldBe` object
          [ "from" .= (2 :: Int)
          , "options"
              .= [ object ["label" .= ("[[!div]]" :: Text), "template" .= ("[[!div]]${0}" :: Text)]
                 , object ["label" .= ("[[!day]]" :: Text), "template" .= ("[[!day]]\\{${1:date}\\}" :: Text), "detail" .= ("from plugin calendar" :: Text)]
                 ]
          , "enter" .= ("chooses" :: Text)
          , "escape" .= ("closes" :: Text)
          ]
    it "says how many characters Enter waits for" $
      completionJson Completion {from = 0, candidates = [], enter = EnterChoosesAfter 2, escape = EscapeCloses}
        `shouldBe` object ["from" .= (0 :: Int), "options" .= ([] :: [Value]), "enter" .= object ["after" .= (2 :: Int)], "escape" .= ("closes" :: Text)]

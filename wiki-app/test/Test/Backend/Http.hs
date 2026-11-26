{-# LANGUAGE MultiWayIf #-}

-- | The backend over HTTP, natively, over a transport the test answers: the
-- requests its client, derived from the API's type, makes (addresses,
-- methods, headers, bodies), how it reads answers and refusals, and what
-- it keeps of what it read. That the
-- server reads those requests as meant is @wiki-server-test@'s
-- (@Test.Server.Http@ runs the whole backend contract over them), and that
-- the browser's @fetch@ carries them is the browser suite's.
module Test.Backend.Http (spec) where

import Data.Aeson (ToJSON, decode, decodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics (C, D, S)
import GHC.Generics qualified as Generics
import Halogen.Net.Fetch qualified as Fetch
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (runMemDOM)
import Protolude
import Servant.API.Generic (AsApi)
import Servant.Client.Core (Response, ResponseF (..))
import Servant.Multipart.API (Mem)
import Test.Backend.Opaque (opaqueServer)
import Test.Bell (noticeOf)
import Test.Browser.Opaque (fakeChallenge, fakeRegistration)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Opaque (fakeChallengeMade, fakeRegistrationMade)
import Test.QuickCheck (Gen, Property, chooseInt, conjoin, counterexample, forAllShrink, ioProperty, listOf1, once, oneof, shrinkList, (===))
import Test.Scripted (bodyOf, formOf, headersOf, jsonResponse, methodOf, responseWith)
import Wiki.Api (EncryptedApi, NotificationsApi (..), PagesApi (..), SpacesApi (..), WikiApi (..))
import Wiki.Api.Account (PasswordChange (..), SignInMethods (..))
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Comments (CommentsApi)
import Wiki.Api.Error qualified as ApiError
import Wiki.Api.Events (PageEvent (..))
import Wiki.Api.Export (ExportFormat (..))
import Wiki.Api.Import (ImportChoices (..), ImportPlan (..), ImportSummary (..), importEverything)
import Wiki.Api.Links (searchUrl)
import Wiki.Api.Notifications (Notifications (..), ReadMark (..))
import Wiki.Api.Opaque (PasswordRecord (..), PasswordRecordChange (..), RegistrationChallenge (..))
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..))
import Wiki.Api.ServerSettings (defaultHomePage)
import Wiki.Api.Stars (StarredPage)
import Wiki.Api.Types (SessionInfo (..))
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Account (MonadAccount (..), SignInError (..))
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Backend
import Wiki.App.Backend.Admin (MonadAdmin (..))
import Wiki.App.Backend.Comments (MonadComments (..))
import Wiki.App.Backend.Http
import Wiki.App.Backend.Http.Cache (Cache (..), CachedDraft (..), Reading (..), Stored (..), afterEvent, afterWrite, cachedPage, changedBy, contentChangedBy, emptyCache, storedAnswer, storedOf, withBacklinks, withDraft, withPageAt, withPageLocation, withPages, withSearch, withSession, withSpaceAt, withSpaces)
import Wiki.App.Backend.Http.Cache qualified as Cache
import Wiki.App.Backend.Http.Client (endpointUrl, fromSessionInfo)
import Wiki.App.Backend.Http.Preload (decodePreload, encodePreload)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications (..))
import Wiki.App.Backend.Stars (MonadStars (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Security (MonadSecurity (..))
import Wiki.App.Vault (changeOwnPassword)
import Wiki.Core.Assistant.Vault (Sealed (..), Vault (..), VaultTag (..), vaultFormat, withSealed)
import Wiki.Core.Attachment (Attachment (..), FileName (..), MimeType (..), Source (..))
import Wiki.Core.Backlink (Backlink (..))
import Wiki.Core.Draft (Draft (..))
import Wiki.Core.Id (AttachmentId (..), BlobId (..), CommentId (..), CommentThreadId (..), NotificationId (..), PageId (..), SpaceId (..), UserId (..))
import Wiki.Core.Notification (Notification (..), PageLocation (..))
import Wiki.Core.Page (Page (..), summaryOf)
import Wiki.Core.Page qualified as P (Page (..))
import Wiki.Core.Path (parsePath, pathOf)
import Wiki.Core.Search (SearchHit, SearchQuery (..), searchQuery)
import Wiki.Core.Slug (Slug)
import Wiki.Core.Space (Space (..))
import Wiki.Core.Space qualified as S
import Wiki.Core.User (Login (..), Password (..), Standing (..), User (..))
import Wiki.Core.Vault (KeyId (..))
import Wiki.Core.Version (Author)

-- | A transport that answers as the function says, and keeps every request
-- it was given, the latest first.
scripted :: (HttpRequest -> Either Text Response) -> IO (Transport, IORef [HttpRequest])
scripted respond = do
  asked <- newIORef []
  pure (Transport (\r -> atomicModifyIORef' asked (\rs -> (r : rs, ())) $> respond r), asked)

-- | A backend over a scripted transport, and the requests it made.
backend :: (HttpRequest -> Either Text Response) -> IO (HttpEnv, IORef [HttpRequest])
backend respond = do
  (transport, asked) <- scripted respond
  (,asked) <$> newHttpEnv transport

-- | 'backend', whose server says yes to every question whether a page's
-- text is plain now ('plainNow'), and the requests it made but those
-- questions: for a test of what a plain write sends.
plainBackend :: (HttpRequest -> Either Text Response) -> IO (HttpEnv, IO [HttpRequest])
plainBackend respond = do
  (env, asked) <- backend (\r -> if asksPlain r then Right (responseWith 204 [] "") else respond r)
  pure (env, filter (not . asksPlain) <$> readIORef asked)
  where
    asksPlain r = methodOf r == "GET" && "/plain" `T.isSuffixOf` requestUrl r

ok :: (ToJSON a) => a -> Either Text Response
ok a = Right (jsonResponse 200 (encode a))

status :: Int -> BSL.ByteString -> Either Text Response
status n body = Right (jsonResponse n body)

demo :: MemState
demo = demoState demoNow

-- | A page of the demo, by path from under its space's home page, with its
-- space's path.
demoPage :: Text -> Text -> Page
demoPage s p =
  fromMaybe (panic ("no page " <> p)) $
    head
      [ page
      | page <- Map.elems demo.pages
      , Just sp <- [pathOf (`Map.lookup` demo.spaces) page.space]
      , sp == slugs s
      , Just space <- [Map.lookup page.space demo.spaces]
      , S.pagePath space (`Map.lookup` demo.pages) page.id == Just (slugs p)
      ]

slugs :: Text -> [Slug]
slugs = either (panic . show) identity . parsePath

-- | A vault as the server stores it; its contents do not matter here.
demoVault :: Vault
demoVault = withSealed Sealed {nonce = "nonce", ciphertext = "ciphertext"} Vault {format = vaultFormat, salt = "salt", items = mempty, wraps = mempty}

spec :: Spec
spec = describe "Wiki.App.Backend.Http" $ do
  pageReads
  plainText
  requests
  answers
  browserFetch
  caching
  accounts
  apiKeys
  preload
  exports

-- | A page or a space fetched as a document (WP5.56 of docs/PLAN.md): its
-- bytes with the name and the media type the server's answer gives, or the
-- server's refusal with its reason, which the Export menu shows.
exports :: Spec
exports = describe "its exports" $ do
  it "fetches a document at its address, named as the answer's Content-Disposition names it" $ do
    (env, asked) <- backend (const (Right (responseWith 200 [("Content-Type", "application/pdf"), ("Content-Disposition", "inline; filename=\"tour.pdf\"")] "%PDF-1.5")))
    runHttpWiki env (fetchExport (ExportPage (PageId "p1")) Pdf) `shouldReturn` Right Document {fileName = "tour.pdf", mediaType = "application/pdf", bytes = "%PDF-1.5"}
    map (\r -> (methodOf r, requestUrl r)) <$> readIORef asked `shouldReturn` [("GET", "api/v1/pages/p1/export/pdf")]

  it "names a document whose answer names none after its format" $ do
    (env, _) <- backend (const (Right (responseWith 200 [] "PK")))
    runHttpWiki env (fetchExport (ExportSpace (SpaceId "s1")) Docx)
      `shouldReturn` Right Document {fileName = "export.docx", mediaType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document", bytes = "PK"}

  it "gives the server's refusal with its reason, rather than a document" $ do
    (env, _) <- backend (const (status 400 "{\"error\":\"Invalid\",\"message\":\"PDF needs xelatex.\"}"))
    runHttpWiki env (fetchExport (ExportSpace (SpaceId "s1")) Pdf) `shouldReturn` Left (Invalid "PDF needs xelatex.")

  it "waits longer for a document than for any other read, since a PDF takes up to a minute to make" $ do
    (env, asked) <- backend (const (Right (responseWith 200 [] "")))
    _ <- runHttpWiki env (fetchExport (ExportPage (PageId "p1")) BeamerPdf)
    _ <- runHttpWiki env (fetchExport (ExportSpace (SpaceId "s1")) Html)
    _ <- runHttpWiki env (getPage (SpaceId "s1") (slugs "export/pdf"))
    map timeLimit <$> readIORef asked `shouldReturn` [requestTimeout, exportTimeout, exportTimeout]

  it "waits as long for a check of the TeX engine, which may hang until the server stops it" $ do
    (env, asked) <- backend (const (status 403 "{\"error\":\"Forbidden\"}"))
    _ <- runHttpWiki env checkTexEngine
    _ <- runHttpWiki env texEngine
    map (\r -> (requestUrl r, timeLimit r)) <$> readIORef asked `shouldReturn` [("api/v1/admin/tex-engine", requestTimeout), ("api/v1/admin/tex-engine/check", exportTimeout)]

-- | What the browser's transport makes of what @fetch@ did, which natively
-- is only that there is no network.
browserFetch :: Spec
browserFetch = describe "the browser's transport" $ do
  it "says why fetch gave no answer, as the application shows it" $ do
    fetchFailure (Fetch.JSError "TypeError" "Failed to fetch") `shouldBe` "the network or the server is down"
    fetchFailure (Fetch.JSError "AbortError" "signal is aborted without reason") `shouldBe` "the request was cancelled"
    fetchFailure (Fetch.JSError "SecurityError" "Refused.") `shouldBe` "Refused."
    fetchFailure (Fetch.JSError "Mystery" "") `shouldBe` "Mystery"

  it "says how long it waited for an answer that did not come" $ do
    noAnswerIn requestTimeout `shouldBe` "no answer in 30 seconds"
    noAnswerIn uploadTimeout `shouldBe` "no answer in 360 seconds"
    noAnswerIn 300 `shouldBe` "no answer in 300 milliseconds"

  it "gives an upload longer than any other request" $ do
    let hello = demoPage "blog" "hello-world"
    -- Each write after the question whether the page's text is plain now
    -- ('plainNow'), which waits as long as any request.
    (env, asked) <- backend (\r -> if "/plain" `T.isSuffixOf` requestUrl r then Right (responseWith 204 [] "") else status 500 "")
    _ <- runHttpWiki env (uploadAttachment hello.id "f" "text/plain" "bytes")
    _ <- runHttpWiki env (savePage hello.id hello.updatedAt "t" "b")
    _ <- runHttpWiki env (getSpace (slugs "docs"))
    map timeLimit <$> readIORef asked `shouldReturn` [requestTimeout, requestTimeout, requestTimeout, uploadTimeout, requestTimeout]

  it "has no network natively, which the backend takes for the wiki out of reach" $ do
    env <- newHttpEnv browserTransport
    runHttpWiki env (getSpace (slugs "docs")) `shouldReturn` Left (Unreachable "there is no network natively")

requests :: Spec
requests = describe "its requests" $ do
  let hello = demoPage "blog" "hello-world"
  it "reads at the API's addresses" $ do
    (env, asked) <- backend (const (ok ([] :: [Space])))
    _ <- runHttpWiki env listSpaces
    _ <- runHttpWiki env (getPage hello.space (slugs "a/b"))
    _ <- runHttpWiki env (searchPages (searchQuery "two words"))
    rs <- reverse <$> readIORef asked
    map (\r -> (methodOf r, requestUrl r, bodyOf r)) rs
      `shouldBe` [ ("GET", "api/v1/spaces", Nothing)
                 , ("GET", "api/v1/spaces/" <> hello.space.text <> "/pages/at/a/b", Nothing)
                 , ("GET", "api/v1/search?q=two%20words&limit=50", Nothing)
                 ]

  it "writes a search's query as the server reads it back" $ do
    (env, asked) <- backend (const (ok ([] :: [SearchHit])))
    let q = (searchQuery "x y") {space = Just hello.space, includeSubspaces = False, templates = True}
    _ <- runHttpWiki env (searchPages q)
    map requestUrl <$> readIORef asked `shouldReturn` ["api/v1/search?q=x%20y&space=" <> hello.space.text <> "&only&limit=50&templates"]
    map requestUrl <$> readIORef asked `shouldReturn` [searchUrl q]

  it "asks for a list in the query a parameter at a time, and names that address without asking" $ do
    (env, asked) <- backend (const (ok (mempty :: Map Text Author)))
    _ <- runHttpWiki env (mentionable ["ada", "edgar"])
    let people = endpointUrl (\e -> e.notifications.people ["ada", "edgar"])
    people `shouldBe` "api/v1/notifications/people?login=ada&login=edgar"
    map requestUrl <$> readIORef asked `shouldReturn` [people]

  it "saves against the version it started from, in If-Match, with the edit as JSON" $ do
    (env, asked) <- plainBackend (const (ok hello))
    _ <- runHttpWiki env (savePage hello.id hello.updatedAt "New title" "New body")
    [r] <- asked
    methodOf r `shouldBe` "PUT"
    requestUrl r `shouldBe` "api/v1/pages/" <> hello.id.text
    headersOf r
      `shouldBe` [ ("If-Match", "\"2026-09-01T12:00:00Z\"")
                 , ("X-Wiki-Request", "1")
                 , ("Accept", "application/json;charset=utf-8,application/json")
                 , ("Content-Type", "application/json;charset=utf-8")
                 ]
    bodyOf r `shouldBe` Just ("application/json;charset=utf-8", "{\"body\":\"New body\",\"title\":\"New title\"}")

  it "sends no plain write or search for a space the server says is encrypted now, though the cached list says it is plain" $ do
    let blog = fromMaybe (panic "no blog") (Map.lookup hello.space demo.spaces)
        nowEncrypted = blog {encryption = Just (KeyId "a-key")}
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == "api/v1/spaces" -> ok [nowEncrypted]
        | requestUrl r == endpointUrl (\e -> e.spaces.pageAt hello.space (slugs "hello-world")) -> ok hello
        | requestUrl r == endpointUrl (\e -> e.pages.plain hello.id) -> status 409 (encode (ApiError.Stale ApiError.SpaceIsEncrypted "Encrypted."))
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    -- Read while the cached list says the space is plain.
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    -- Every write that would carry the page's text asks the server about
    -- the page first ('plainNow'), or for the list of spaces (a page made),
    -- and sends nothing once it says the space is encrypted. (Natively the
    -- encrypted way then stops: there is no WebCrypto.)
    runHttpWiki env (savePage hello.id hello.updatedAt "Hello" "Edited") >>= (`shouldSatisfy` isLeft)
    runHttpWiki env (saveDraft hello.id hello.updatedAt "Hello" "Edited") >>= (`shouldSatisfy` isLeft)
    runHttpWiki env (uploadAttachment hello.id "a.txt" "text/plain" "bytes") >>= (`shouldSatisfy` isLeft)
    runHttpWiki env (openThread hello.id Nothing "A comment") >>= (`shouldSatisfy` isLeft)
    runHttpWiki env (createPage hello.space Nothing "New" "Text") >>= (`shouldSatisfy` isLeft)
    -- A search limited to the space is not sent.
    runHttpWiki env (searchPages (searchQuery "words") {space = Just hello.space, includeSubspaces = True}) `shouldReturn` []
    sent <- readIORef asked
    [(methodOf r, requestUrl r) | r <- sent, methodOf r /= "GET" || "api/v1/search" `T.isPrefixOf` requestUrl r] `shouldBe` []

  -- #255's tenth review: a space that asks each time it is opened, in the
  -- middle of turning its encryption off, after a write emptied the cache.
  -- The list the page started with says it is gated, and that knowledge
  -- outlives the cache: no plain read of its pages is sent, and nothing of
  -- them is given. A plain space read the same way costs no request more.
  it "routes by the list it last read once a write emptied the cache: nothing of a gated space mid-decryption, no request more for a plain one" $ do
    let blog = fromMaybe (panic "no blog") (Map.lookup hello.space demo.spaces)
        blogPages = [summaryOf p | p <- Map.elems demo.pages, p.space == hello.space]
        others = [s | s <- Map.elems demo.spaces, s.id /= blog.id]
        gated = blog {encryption = Just (KeyId "a-key"), entry = S.ConfirmEach, conversion = Just S.Decrypting}
        server listed r =
          if
            | requestUrl r == "api/v1/spaces" -> ok listed
            | methodOf r /= "GET" -> status 404 "{\"error\":\"NotFound\"}"
            | "/pages/at/" `T.isInfixOf` requestUrl r -> ok hello
            | "/pages" `T.isSuffixOf` requestUrl r -> ok blogPages
            | otherwise -> status 404 "{\"error\":\"NotFound\"}"
        plainReads rs = [requestUrl r | r <- rs, ("api/v1/spaces/" <> hello.space.text <> "/pages") `T.isPrefixOf` requestUrl r]
    -- Gated: the plain routes are never asked, and both reads are refused.
    (transport, asked) <- scripted (server (gated : others))
    env <- newHttpEnvWith transport (withSpaces (gated : others) emptyCache)
    _ <- runHttpWiki env (deletePage (PageId "nowhere"))
    (\c -> c.spaces) <$> cacheOf env `shouldReturn` Nothing
    runHttpWiki env (getPage hello.space (slugs "hello-world")) >>= (`shouldSatisfy` isLeft)
    _ <- runHttpWiki env (deletePage (PageId "nowhere"))
    runHttpWiki env (listPages hello.space) >>= (`shouldSatisfy` isLeft)
    plainReads <$> readIORef asked `shouldReturn` []
    -- Plain: the write, then the read, and nothing else.
    (plainTransport, plainAsked) <- scripted (server (blog : others))
    plainEnv <- newHttpEnvWith plainTransport (withSpaces (blog : others) emptyCache)
    _ <- runHttpWiki plainEnv (deletePage (PageId "nowhere"))
    runHttpWiki plainEnv (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    runHttpWiki plainEnv (listPages hello.space) `shouldReturn` Right blogPages
    map (\r -> (methodOf r, requestUrl r)) . reverse <$> readIORef plainAsked
      `shouldReturn` [ ("DELETE", "api/v1/pages/nowhere")
                     , ("GET", endpointUrl (\e -> e.spaces.pageAt hello.space (slugs "hello-world")))
                     , ("GET", endpointUrl (\e -> e.spaces.listPages hello.space))
                     ]

  -- A space the list the tab knows does not name (made, or opened to the
  -- reader, since): a plain read that succeeds is given only after the
  -- list is read again. Gated and mid-decryption there, it gives nothing.
  it "reads the list again before it gives a plain read of a space its list did not name, and gives nothing of one the list now shows gated" $ do
    let blog = fromMaybe (panic "no blog") (Map.lookup hello.space demo.spaces)
        others = [s | s <- Map.elems demo.spaces, s.id /= blog.id]
        gated = blog {encryption = Just (KeyId "a-key"), entry = S.ConfirmEach, conversion = Just S.Decrypting}
        pageAt = endpointUrl (\e -> e.spaces.pageAt hello.space (slugs "hello-world"))
        server listed r =
          if
            | requestUrl r == "api/v1/spaces" -> ok listed
            | requestUrl r == pageAt -> ok hello
            | otherwise -> status 404 "{\"error\":\"NotFound\"}"
        firstTwo env asked = do
          result <- runHttpWiki env (getPage hello.space (slugs "hello-world"))
          sent <- take 2 . map requestUrl . reverse <$> readIORef asked
          pure (result, sent)
    -- Gated now: refused, though the plain route answered.
    (transport, asked) <- scripted (server (gated : others))
    env <- newHttpEnvWith transport (withSpaces others emptyCache)
    (result, sent) <- firstTwo env asked
    result `shouldSatisfy` isLeft
    sent `shouldBe` [pageAt, "api/v1/spaces"]
    -- Plain now: given, for the one list read more.
    (plainTransport, plainAsked) <- scripted (server (blog : others))
    plainEnv <- newHttpEnvWith plainTransport (withSpaces others emptyCache)
    firstTwo plainEnv plainAsked `shouldReturn` (Right hello, [pageAt, "api/v1/spaces"])
    length <$> readIORef plainAsked `shouldReturn` 2

  it "keeps a draft with the version it started from in the body" $ do
    (env, asked) <- plainBackend (const (ok Draft {page = hello.id, author = UserId "admin", title = "t", body = "b", base = hello.updatedAt, savedAt = hello.updatedAt}))
    _ <- runHttpWiki env (saveDraft hello.id hello.updatedAt "t" "b")
    [r] <- asked
    (methodOf r, requestUrl r) `shouldBe` ("PUT", "api/v1/pages/" <> hello.id.text <> "/draft")
    snd <$> bodyOf r `shouldBe` Just "{\"base\":\"2026-09-01T12:00:00Z\",\"body\":\"b\",\"title\":\"t\"}"

  it "deletes with DELETE, and takes a 204 for done" $ do
    (env, asked) <- backend (const (status 204 ""))
    runHttpWiki env (deleteAttachment (AttachmentId "a 1")) `shouldReturn` Right ()
    map (\r -> (methodOf r, requestUrl r)) <$> readIORef asked `shouldReturn` [("DELETE", "api/v1/attachments/a%201")]

  it "uploads a file as a multipart form, its name quoted" $ do
    (env, asked) <- plainBackend (const (status 500 ""))
    _ <- runHttpWiki env (uploadAttachment hello.id "say \"hi\" ü.txt" "text/plain" "the bytes")
    [r] <- asked
    (methodOf r, requestUrl r) `shouldBe` ("POST", "api/v1/pages/" <> hello.id.text <> "/attachments")
    fst <$> bodyOf r `shouldSatisfy` maybe False ("multipart/form-data;boundary=" `T.isPrefixOf`)
    case formOf r of
      Just (boundary, form) ->
        form
          `shouldBe` mconcat
            [ "--" <> boundary <> "\r\n"
            , "Content-Disposition: form-data; name=\"file\"; filename=\"say \\\"hi\\\" "
            , encodeUtf8 "ü"
            , ".txt\"\r\nContent-Type: text/plain\r\n\r\nthe bytes\r\n--" <> boundary <> "--"
            ]
      Nothing -> expectationFailure "no form"

  it "writes each form with a boundary of its own, too long for a file to hold by chance" $ do
    (env, asked) <- plainBackend (const (status 500 ""))
    _ <- runHttpWiki env (uploadAttachment hello.id "a.txt" "text/plain" "a")
    _ <- runHttpWiki env (uploadAttachment hello.id "b.txt" "text/plain" "b")
    boundaries <- mapMaybe (fmap fst . formOf) <$> asked
    length boundaries `shouldBe` 2
    ordNub boundaries `shouldBe` boundaries
    map BS.length boundaries `shouldBe` [55, 55]

  it "asks for an import's preview with the archive as the form's file" $ do
    (env, asked) <- backend (const (ok (ImportPlan [])))
    runHttpWiki env (previewImport "PK zipped") `shouldReturn` Right (ImportPlan [])
    [r] <- readIORef asked
    (methodOf r, requestUrl r) `shouldBe` ("POST", "api/v1/import/preview")
    case formOf r of
      Just (boundary, form) ->
        form
          `shouldBe` ( "--"
                         <> boundary
                         <> "\r\nContent-Disposition: form-data; name=\"archive\"; filename=\"wiki.zip\"\r\nContent-Type: application/zip\r\n\r\nPK zipped\r\n--"
                         <> boundary
                         <> "--"
                     )
      Nothing -> expectationFailure "no form"

  it "applies an import with the archive again and the choices as a field that is not a file" $ do
    let summary = ImportSummary {spacesCreated = 1, spacesMerged = 0, pagesCreated = 2, pagesReplaced = 0, pagesKept = 0, pagesSkipped = 0, attachments = 0}
        choices = importEverything {skipSpaces = [slugs "docs/notes"]}
    (env, asked) <- backend (const (ok summary))
    runHttpWiki env (applyImport "PK zipped" choices) `shouldReturn` Right summary
    [r] <- readIORef asked
    (methodOf r, requestUrl r) `shouldBe` ("POST", "api/v1/import/apply")
    case formOf r of
      Just (boundary, form) ->
        form
          `shouldBe` mconcat
            [ "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"choices\"\r\nContent-Type: text/plain\r\n\r\n"
            , toS (encode choices)
            , "\r\n--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"archive\"; filename=\"wiki.zip\"\r\nContent-Type: application/zip\r\n\r\nPK zipped\r\n"
            , "--" <> boundary <> "--"
            ]
      Nothing -> expectationFailure "no form"

  it "forgets what it read once an import has been applied" $ do
    let summary = ImportSummary {spacesCreated = 0, spacesMerged = 0, pagesCreated = 0, pagesReplaced = 0, pagesKept = 0, pagesSkipped = 0, attachments = 0}
    (env, _) <- backend $ \r ->
      if requestUrl r == "api/v1/spaces" then ok ([] :: [Space]) else ok summary
    _ <- runHttpWiki env listSpaces
    ((\c -> c.spaces) <$> cacheOf env) `shouldReturn` Just []
    _ <- runHttpWiki env (applyImport "PK" importEverything)
    ((\c -> c.spaces) <$> cacheOf env) `shouldReturn` Nothing

  it "gives an attachment's address without asking" $ do
    (env, asked) <- backend (const (Left "not asked"))
    for_ (head hello.attachments) $ \a -> do
      url <- runHttpWiki env (attachmentUrl (UrlOwner 0) a)
      url `shouldSatisfy` (\u -> "api/v1/attachments/" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u)
    readIORef asked `shouldReturn` []

  it "gives a stored SVG's interactive address only where its frame can reach the server: a secure context, online, no kept copy" $ do
    let svg = Attachment {id = AttachmentId "a1", name = FileName "flame.svg", mediaType = MimeType "image/svg+xml", size = 1, source = SourceBlob (BlobId "b1")}
        png = Attachment {id = AttachmentId "a2", name = FileName "flame.png", mediaType = MimeType "image/png", size = 1, source = SourceBlob (BlobId "b2")}
    let reach secure online keptCopy = FrameReach {secure, online, keptCopy}
    frameUrlIn (reach True True False) svg `shouldBe` Just "api/v1/attachments/a1/flame.svg?interactive"
    for_ [(s, o, k) | s <- [False, True], o <- [False, True], k <- [False, True], (s, o, k) /= (True, True, False)] $ \(s, o, k) ->
      ((s, o, k), frameUrlIn (reach s o k) svg) `shouldBe` ((s, o, k), Nothing)
    frameUrlIn (reach True True False) png `shouldBe` Nothing

answers :: Spec
answers = describe "its answers" $ do
  let read' :: Either Text Response -> IO (Either WikiError Space)
      read' answered = do
        (env, _) <- backend (const answered)
        runHttpWiki env (getSpace (slugs "docs"))
  it "reads a refusal by the error its body names" $ do
    read' (status 404 "{\"error\":\"NotFound\"}") `shouldReturn` Left NotFound
    read' (status 403 "{\"error\":\"Forbidden\"}") `shouldReturn` Left Forbidden
    read' (status 409 "{\"error\":\"Conflict\",\"message\":\"Saved since.\"}") `shouldReturn` Left (Conflict "Saved since.")
    read' (status 400 "{\"error\":\"Invalid\",\"message\":\"A title, please.\"}") `shouldReturn` Left (Invalid "A title, please.")

  it "reads a refusal without such a body by its status" $ do
    read' (status 404 "") `shouldReturn` Left NotFound
    read' (status 401 "") `shouldReturn` Left Forbidden
    read' (status 413 "Too large") `shouldReturn` Left (Invalid "Too large")
    read' (status 405 "<html>a proxy's page</html>") `shouldReturn` Left (Invalid "The request was refused (405).")

  it "takes a failing server, a broken answer and no answer for out of reach" $ do
    read' (status 500 "") `shouldReturn` Left (Unreachable "the server failed, with status 500")
    read' (status 502 "<html>Bad gateway</html>") `shouldReturn` Left (Unreachable "the server failed, with status 502")
    read' (Left "the network or the server is down") `shouldReturn` Left (Unreachable "the network or the server is down")
    let outOfReach = \case
          Left (Unreachable _) -> True
          _ -> False
    -- A captive portal's page, which is no JSON.
    read' (Right (responseWith 200 [("Content-Type", "text/html")] "<html>a captive portal</html>")) >>= (`shouldSatisfy` outOfReach)
    -- JSON, but not the space the API declares.
    read' (status 200 "<html>a captive portal</html>") >>= (`shouldSatisfy` outOfReach)
    read' (status 200 "{\"spaces\": 3}") >>= (`shouldSatisfy` outOfReach)
    -- A success, but not the one the API declares for a read.
    read' (status 201 "{}") `shouldReturn` Left (Unreachable "its answer was not one the wiki can read: status 201")

  it "throws from a read without an Either, for attempt to catch" $ do
    (env, _) <- backend (const (Left "down"))
    runHttpWiki env (attempt listSpaces) `shouldReturn` Left (Unreachable "down")
    runHttpWiki env (attempt session) `shouldReturn` Left (Unreachable "down")
    runHttpWiki env (attempt (searchPages (searchQuery "x"))) `shouldReturn` Left (Unreachable "down")
    runHttpWiki env listSpaces `shouldThrow` (== Unreachable "down")

  it "returns a refusal from a read with an Either" $ do
    (env, _) <- backend (const (status 403 "{\"error\":\"Forbidden\"}"))
    runHttpWiki env (getSpace (slugs "team")) `shouldReturn` Left Forbidden

  it "fetches the vault against the version it has, and takes a 304 for that version and a 404 for none" $ do
    let stored = StoredVault {tag = VaultTag "v2", vault = demoVault}
        vaultAnswer = \case
          Just (VaultTag "v2") -> status 304 ""
          Just _ -> ok stored
          Nothing -> status 404 (encode ApiError.NotFound)
    (env, asked) <- backend $ \r -> vaultAnswer (VaultTag <$> lookup "If-None-Match" (headersOf r))
    runHttpWiki env (fetchVault (Just (VaultTag "v2"))) `shouldReturn` Right Unchanged
    runHttpWiki env (fetchVault (Just (VaultTag "v1"))) `shouldReturn` Right (Fetched stored)
    runHttpWiki env (fetchVault Nothing) `shouldReturn` Right NoVault
    map (\r -> (methodOf r, requestUrl r, lookup "If-None-Match" (headersOf r))) . reverse <$> readIORef asked
      `shouldReturn` [ ("GET", "api/v1/assistant/vault", Just "v2")
                     , ("GET", "api/v1/assistant/vault", Just "v1")
                     , ("GET", "api/v1/assistant/vault", Nothing)
                     ]

caching :: Spec
caching = describe "what it keeps" $ do
  let hello = demoPage "blog" "hello-world"
      blogPages = [p | p <- Map.elems demo.pages, p.space == hello.space]
      answering r
        | methodOf r /= "GET" = status 204 ""
        | "/draft" `T.isSuffixOf` requestUrl r = ok (Nothing :: Maybe Draft)
        | "/pages" `T.isSuffixOf` requestUrl r = ok (map summaryOf blogPages)
        | "/pages/at/" `T.isInfixOf` requestUrl r = ok hello
        | "api/v1/search" `T.isPrefixOf` requestUrl r = ok ([] :: [SearchHit])
        | otherwise = ok (Map.elems demo.spaces)
  it "answers at once only what it has read, and asks nothing meanwhile" $ do
    (env, asked) <- backend answering
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Nothing
    readIORef asked `shouldReturn` []
    spaces <- runHttpWiki env listSpaces
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Just spaces
    length <$> readIORef asked `shouldReturn` 1

  it "reads a page by itself, since its space's list of pages has no bodies" $ do
    (env, asked) <- backend answering
    _ <- runHttpWiki env (listPages hello.space)
    runHttpWiki env (atOnce (getPage hello.space (slugs "hello-world"))) `shouldReturn` Nothing
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    runHttpWiki env (atOnce (getPage hello.space (slugs "hello-world"))) `shouldReturn` Just (Right hello)
    length <$> readIORef asked `shouldReturn` 2

  it "caches page content until the route changes, and fetches it on every read after that" $ do
    (env, asked) <- backend answering
    spaces <- runHttpWiki env listSpaces
    _ <- runHttpWiki env (listPages hello.space)
    _ <- runHttpWiki env (getPage hello.space (slugs "hello-world"))
    _ <- runHttpWiki env (getDraft hello.id)
    _ <- runHttpWiki env (searchPages (searchQuery "hello"))
    old <- cacheOf env
    runHttpWiki env endPreload
    left <- cacheOf env
    -- The page content goes; the spaces and this user's draft stay.
    (Map.null left.pages, Map.null left.pageAt, left.searches) `shouldBe` (True, True, [])
    (left.spaces, left.drafts, left.session) `shouldBe` (Just spaces, old.drafts, old.session)
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Just spaces
    runHttpWiki env (atOnce (getDraft hello.id)) `shouldReturn` Just (Right Nothing)
    -- Page content is no longer answered at once, and every read asks the
    -- server, keeping nothing.
    runHttpWiki env (atOnce (getPage hello.space (slugs "hello-world"))) `shouldReturn` Nothing
    runHttpWiki env (atOnce (listPages hello.space)) `shouldReturn` Nothing
    asked' <- length <$> readIORef asked
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    runHttpWiki env (listPages hello.space) `shouldReturn` Right (map summaryOf blogPages)
    runHttpWiki env (searchPages (searchQuery "hello")) `shouldReturn` []
    length <$> readIORef asked `shouldReturn` asked' + 4
    (\c -> (Map.null c.pageAt, Map.null c.pages, c.searches)) <$> cacheOf env `shouldReturn` (True, True, [])

  it "caches the starred pages as page content, and keeps the list when a star is refused" $ do
    (env, asked) <- backend $ \r ->
      if methodOf r == "GET" then ok ([] :: [StarredPage]) else status 404 "{\"error\":\"NotFound\"}"
    runHttpWiki env starred `shouldReturn` Right []
    -- A star refused for one page leaves the list as it was read.
    runHttpWiki env (setStarred (PageId "gone") True) `shouldReturn` Left NotFound
    runHttpWiki env (atOnce starred) `shouldReturn` Just (Right [])
    -- A page saved anywhere, and the route left, drop the list: its pages'
    -- titles and places may have changed.
    read' <- cacheOf env
    (afterEvent hello.id (PageChanged demoNow) read').stars `shouldBe` Nothing
    runHttpWiki env endPreload
    runHttpWiki env (atOnce starred) `shouldReturn` Nothing
    sofar <- length <$> readIORef asked
    runHttpWiki env starred `shouldReturn` Right []
    runHttpWiki env starred `shouldReturn` Right []
    length <$> readIORef asked `shouldReturn` sofar + 2

  it "lets everything go after a write" $ do
    (env, _) <- backend answering
    _ <- runHttpWiki env listSpaces
    _ <- runHttpWiki env (listPages hello.space)
    _ <- runHttpWiki env (deletePage (PageId "x"))
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Nothing
    (\c -> Map.null c.pages) <$> cacheOf env `shouldReturn` True

  it "routes a search by the list of spaces a refresh read, though a star changed the cache while it was out" $ do
    -- The list is read plain; a page made asks for it again, and that read
    -- answers, once a star has changed the cache, that the blog is
    -- encrypted now.
    gate <- newEmptyMVar
    count <- newIORef (0 :: Int)
    let blogId = hello.space
        older = Map.elems demo.spaces
        newer = [if s.id == blogId then s {encryption = Just (KeyId "key-1")} else s | s <- older]
        respond r
          | methodOf r == "GET" && requestUrl r == "api/v1/spaces" =
              atomicModifyIORef' count (\n -> (n + 1, n)) >>= \case
                0 -> pure (ok older)
                1 -> readMVar gate $> ok newer
                _ -> pure (ok newer)
          | methodOf r == "GET" && "/pages/at/" `T.isInfixOf` requestUrl r = pure (ok hello)
          | "api/v1/stars" `T.isPrefixOf` requestUrl r = pure (ok ([] :: [StarredPage]))
          | "api/v1/search" `T.isPrefixOf` requestUrl r = pure (ok ([] :: [SearchHit]))
          | otherwise = pure (status 404 "{\"error\":\"NotFound\"}")
        refresh = readIORef count >>= \n -> unless (n > 1) (threadDelay 1000 >> refresh)
    asked <- newIORef []
    env <- newHttpEnv (Transport (\r -> atomicModifyIORef' asked (\rs -> (r : rs, ())) >> respond r))
    runHttpWiki env listSpaces `shouldReturn` older
    runHttpWiki env (getPage blogId (slugs "hello-world")) `shouldReturn` Right hello
    saving <- async (runHttpWiki env (createPage blogId Nothing "Plans" "Secret."))
    refresh
    _ <- runHttpWiki env (setStarred (PageId "page-x") True)
    putMVar gate ()
    -- The page is not made plain, and the search limited to the blog sends
    -- its words nowhere: both from the list read last.
    wait saving >>= (`shouldSatisfy` isLeft)
    _ <- runHttpWiki env (searchPages SearchQuery {text = "secret", space = Just blogId, includeSubspaces = True, limit = 50, templates = False})
    sent <- readIORef asked
    [requestUrl r | r <- sent, "api/v1/search" `T.isPrefixOf` requestUrl r] `shouldBe` []
    [requestUrl r | r <- sent, methodOf r == "POST", requestUrl r == "api/v1/spaces/" <> blogId.text <> "/pages"] `shouldBe` []

  it "keeps the list of spaces asked for last when one asked for before answers after it" $ do
    -- Two reads of the list cross: the first, from before the blog was
    -- encrypted, answers last.
    gate <- newEmptyMVar
    count <- newIORef (0 :: Int)
    let blogId = hello.space
        older = Map.elems demo.spaces
        newer = [if s.id == blogId then s {encryption = Just (KeyId "key-1")} else s | s <- older]
        respond r
          | methodOf r == "GET" && requestUrl r == "api/v1/spaces" =
              atomicModifyIORef' count (\n -> (n + 1, n)) >>= \case
                0 -> readMVar gate $> ok older
                _ -> pure (ok newer)
          | "api/v1/search" `T.isPrefixOf` requestUrl r = pure (ok ([] :: [SearchHit]))
          | otherwise = pure (status 404 "{\"error\":\"NotFound\"}")
        out = readIORef count >>= \n -> unless (n > 0) (threadDelay 1000 >> out)
    asked <- newIORef []
    env <- newHttpEnv (Transport (\r -> atomicModifyIORef' asked (\rs -> (r : rs, ())) >> respond r))
    early <- async (runHttpWiki env listSpaces)
    out
    runHttpWiki env listSpaces `shouldReturn` newer
    putMVar gate ()
    -- The first read gets the list known: the later one's.
    wait early `shouldReturn` newer
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Just newer
    _ <- runHttpWiki env (searchPages SearchQuery {text = "secret", space = Just blogId, includeSubspaces = True, limit = 50, templates = False})
    sent <- readIORef asked
    [requestUrl r | r <- sent, "api/v1/search" `T.isPrefixOf` requestUrl r] `shouldBe` []

  it "keeps no answer read before a write that came while the read was out" $ do
    -- The first read of the spaces is answered with the list as it was,
    -- and only once a write has come; every read after it, with the list
    -- as the write left it.
    gate <- newEmptyMVar
    count <- newIORef (0 :: Int)
    let older = take 1 (Map.elems demo.spaces)
        newer = Map.elems demo.spaces
        respond r
          | methodOf r /= "GET" = pure (status 204 "")
          | otherwise =
              atomicModifyIORef' count (\n -> (n + 1, n)) >>= \case
                0 -> readMVar gate $> ok older
                _ -> pure (ok newer)
        out = readIORef count >>= \n -> unless (n > 0) (threadDelay 1000 >> out)
    env <- newHttpEnv (Transport respond)
    early <- async (runHttpWiki env listSpaces)
    out
    _ <- runHttpWiki env (deletePage (PageId "x"))
    putMVar gate ()
    -- The read gets the answer it asked for; the cache does not keep it.
    wait early `shouldReturn` older
    runHttpWiki env listSpaces `shouldReturn` newer
    (\c -> c.spaces) <$> cacheOf env `shouldReturn` Just newer

  it "keeps the notifications read last when one read before them answers after them" $ do
    -- Two reads of the notifications cross: the first, from before n1 was
    -- made, answers last. The bell's machine shows the second; the cache
    -- must not answer the next read with the first.
    gate <- newEmptyMVar
    count <- newIORef (0 :: Int)
    let older = Notifications {unread = 0, items = []}
        newer = Notifications {unread = 1, items = [noticeOf 1]}
        respond r
          | methodOf r == "GET" && requestUrl r == "api/v1/notifications" =
              atomicModifyIORef' count (\n -> (n + 1, n)) >>= \case
                0 -> readMVar gate $> ok older
                _ -> pure (ok newer)
          | methodOf r == "GET" && requestUrl r == "api/v1/spaces" = pure (ok (Map.elems demo.spaces))
          | otherwise = pure (status 404 "{\"error\":\"NotFound\"}")
        out = readIORef count >>= \n -> unless (n > 0) (threadDelay 1000 >> out)
    env <- newHttpEnv (Transport respond)
    early <- async (runHttpWiki env notifications)
    out
    runHttpWiki env notifications `shouldReturn` Right newer
    putMVar gate ()
    wait early `shouldReturn` Right older
    (\c -> c.notices) <$> cacheOf env `shouldReturn` Just (Right newer)

  it "forgets the cached notifications when a read went out while a mark was out" $ do
    -- The mark is handled before n2 is made; the read sent after it, after
    -- n2 is made. The mark's answer comes first: put in the cache, it would
    -- keep the read's newer list out of it, and the next read would lack n2.
    marked <- newEmptyMVar
    reading <- newEmptyMVar
    asked <- newIORef ([] :: [Text])
    let markAnswer = Notifications {unread = 0, items = [(noticeOf 1) {read = True}]}
        readAnswer = Notifications {unread = 1, items = [noticeOf 2, (noticeOf 1) {read = True}]}
        respond r = do
          atomicModifyIORef' asked (\rs -> (requestUrl r : rs, ()))
          case (methodOf r, requestUrl r) of
            ("POST", "api/v1/notifications/read") -> readMVar marked $> ok markAnswer
            ("GET", "api/v1/notifications") -> readMVar reading $> ok readAnswer
            ("GET", "api/v1/spaces") -> pure (ok (Map.elems demo.spaces))
            _ -> pure (status 404 "{\"error\":\"NotFound\"}")
        sent url = readIORef asked >>= \rs -> unless (url `elem` rs) (threadDelay 1000 >> sent url)
    env <- newHttpEnv (Transport respond)
    marking <- async (runHttpWiki env (markRead ReadMark {ids = [NotificationId "n1"], everything = False}))
    sent "api/v1/notifications/read"
    readingOut <- async (runHttpWiki env notifications)
    sent "api/v1/notifications"
    putMVar marked ()
    wait marking `shouldReturn` Right markAnswer
    putMVar reading ()
    wait readingOut `shouldReturn` Right readAnswer
    (\c -> c.notices) <$> cacheOf env `shouldReturn` Nothing

  it "stores an answer of page content only if no page was saved while it was read, and any other across a save" $ do
    let sent = storedOf emptyCache
        saved = contentChangedBy (afterEvent hello.id (PageChanged demoNow)) sent
        written = changedBy afterWrite sent
        spaces = withSpaces (Map.elems demo.spaces)
    (storedAnswer ContentRead sent spaces saved).cache `shouldBe` saved.cache
    (storedAnswer PlainRead sent spaces saved).cache `shouldBe` spaces saved.cache
    (storedAnswer PlainRead sent spaces written).cache `shouldBe` written.cache
    (storedAnswer ContentRead sent spaces sent).cache `shouldBe` spaces sent.cache

  it "keeps the rest after a draft's write, which puts the draft in its place" $ do
    (env, asked) <- backend answering
    _ <- runHttpWiki env (listPages hello.space)
    runHttpWiki env (getDraft hello.id) `shouldReturn` Right Nothing
    runHttpWiki env (discardDraft hello.id) `shouldReturn` Right ()
    runHttpWiki env (atOnce ((,) <$> getDraft hello.id <*> listPages hello.space)) `shouldReturn` Just (Right Nothing, Right (map summaryOf blogPages))
    length <$> readIORef asked `shouldReturn` 3

  it "lets the pages read go when the live channel tells of a save, and keeps the rest" $ do
    (env, _) <- backend answering
    spaces <- runHttpWiki env listSpaces
    _ <- runHttpWiki env (listPages hello.space)
    _ <- runHttpWiki env (getDraft hello.id)
    read' <- cacheOf env
    let saved = afterEvent hello.id (PageChanged demoNow) read'
    Map.null saved.pages `shouldBe` True
    cachedPage hello.space (slugs "hello-world") saved `shouldBe` Nothing
    saved.spaces `shouldBe` Just spaces
    saved.drafts `shouldBe` read'.drafts
    saved.session `shouldBe` read'.session
    -- Who is editing changes nothing kept.
    afterEvent hello.id (Editing []) read' `shouldBe` read'

  it "keeps a refusal the wiki decided, and asks again after one it did not" $ do
    (env, asked) <- backend $ \r ->
      if "/team" `T.isSuffixOf` requestUrl r then status 403 "{\"error\":\"Forbidden\"}" else status 503 ""
    runHttpWiki env (getSpace (slugs "team")) `shouldReturn` Left Forbidden
    runHttpWiki env (atOnce (getSpace (slugs "team"))) `shouldReturn` Just (Left Forbidden)
    runHttpWiki env (getSpace (slugs "docs")) `shouldReturn` Left (Unreachable "the server failed, with status 503")
    runHttpWiki env (atOnce (getSpace (slugs "docs"))) `shouldReturn` Nothing
    length <$> readIORef asked `shouldReturn` 2

  it "keeps a search's hits by its query, until a write" $ do
    (env, asked) <- backend (\r -> if methodOf r == "GET" then ok ([] :: [SearchHit]) else status 204 "")
    runHttpWiki env (searchPages (searchQuery "tour")) `shouldReturn` []
    runHttpWiki env (atOnce (searchPages (searchQuery "tour"))) `shouldReturn` Just []
    runHttpWiki env (atOnce (searchPages (searchQuery "other"))) `shouldReturn` Nothing
    _ <- runHttpWiki env (deletePage (PageId "x"))
    runHttpWiki env (atOnce (searchPages (searchQuery "tour"))) `shouldReturn` Nothing
    length <$> readIORef asked `shouldReturn` 2

  it "starts from what it is given, and asks nothing for it" $ do
    (env, asked) <- backend (const (Left "not asked"))
    let spaces = Map.elems demo.spaces
    seeded <- newHttpEnvWith (Transport (\_ -> panic "asked")) . withSpaces spaces =<< cacheOf env
    runHttpWiki seeded (atOnce listSpaces) `shouldReturn` Just spaces
    readIORef asked `shouldReturn` []

  it "asks a transport that answers at once from within atOnce" $ do
    (transport, asked) <- scripted answering
    env <- newImmediateHttpEnv transport
    runHttpWiki env (atOnce (listPages hello.space)) `shouldReturn` Just (Right (map summaryOf blogPages))
    runHttpWiki env (atOnce (getPage hello.space (slugs "hello-world"))) `shouldReturn` Just (Right hello)
    length <$> readIORef asked `shouldReturn` 2
    -- The server's prerender caches page content whatever the route, since
    -- the page it writes carries the cache.
    runHttpWiki env endPreload
    _ <- runHttpWiki env (atOnce (listPages hello.space))
    (\c -> Map.member hello.space c.pages) <$> cacheOf env `shouldReturn` True

-- | Who is asking: the demo's admin, signed in, or nobody, as the server
-- sends them.
-- | That no request's body holds a password.
noPassword :: (HasCallStack) => [HttpRequest] -> Text -> Expectation
noPassword rs password = [r | r <- rs, maybe False ((encodeUtf8 password `BS.isInfixOf`) . snd) (bodyOf r)] `shouldSatisfy` null

-- | The user the OPAQUE server of the tests ('opaqueServer') names.
admin :: UserId
admin = UserId "user-admin"

signedIn, nobody :: SessionInfo
signedIn =
  SessionInfo
    { user = Just User {id = UserId "user-admin", login = Login "admin", displayName = "Ada Admin", avatar = Nothing}
    , groups = mempty
    , grants = mempty
    , standing = SignedIn
    , providers = []
    , exports = []
    , boxes = []
    , homePage = defaultHomePage
    , mathMacros = Nothing
    , assistantServices = []
    , whatsNew = Nothing
    }
nobody = SessionInfo {user = Nothing, groups = mempty, grants = mempty, standing = NotSignedIn, providers = [], exports = [], boxes = [], homePage = defaultHomePage, mathMacros = Nothing, assistantServices = [], whatsNew = Nothing}

accounts :: Spec
accounts = describe "signing in and out" $ do
  it "marks every request as the application's own" $ do
    (env, asked) <- backend (const (status 204 ""))
    _ <- runHttpWiki env (deletePage (PageId "x"))
    _ <- runHttpWiki env (attempt listSpaces)
    rs <- readIORef asked
    map (\r -> lookup "X-Wiki-Request" (headersOf r)) rs `shouldBe` [Just "1", Just "1"]

  it "is answered in the browser tests with what the server's functions make" $
    -- The browser suite cannot use the opaque-hs package, so it carries the
    -- answers as fixed bytes (Test.Browser.Opaque).
    (fakeChallenge, fakeRegistration) `shouldBe` (fakeChallengeMade, fakeRegistrationMade)

  it "signs in through OPAQUE, sending no password, and starts over with the session it answers" $ do
    (env, asked) <- backend (opaqueServer False signedIn (const (ok (Map.elems demo.spaces))))
    _ <- runHttpWiki env listSpaces
    s <- runHttpWiki env (signIn "admin" (Password "correct horse"))
    (\x -> (x.user, x.standing)) <$> s `shouldBe` Right (signedIn.user, SignedIn)
    rs <- reverse <$> readIORef asked
    map (\r -> (methodOf r, requestUrl r)) rs `shouldBe` [("GET", "api/v1/spaces"), ("POST", "api/v1/session/password"), ("PUT", "api/v1/session/password")]
    noPassword rs "correct horse"
    -- What was read as nobody is gone; who is asking is known.
    runHttpWiki env (atOnce listSpaces) `shouldReturn` Nothing
    runHttpWiki env (atOnce session) `shouldReturn` Just (fromSessionInfo signedIn)

  -- A permanent address the server prerendered for someone not signed in
  -- carries its refusal; once they sign in, where the page is is asked
  -- again, so that they arrive at the page (WP5.81).
  it "asks again where a page is after signing in, though the page it started from carried a refusal" $ do
    let there = PageLocation {space = slugs "team", page = slugs "on-call"}
        locationOf r = if requestUrl r == "api/v1/pages/page-9/location" then ok there else ok (Map.elems demo.spaces)
    (transport, asked) <- scripted (opaqueServer False signedIn locationOf)
    env <- newHttpEnvWith transport (withPageLocation (PageId "page-9") (Left NotFound) emptyCache)
    runHttpWiki env (atOnce (pageLocation (PageId "page-9"))) `shouldReturn` Just (Left NotFound)
    readIORef asked `shouldReturn` []
    _ <- runHttpWiki env (signIn "admin" (Password "correct horse"))
    runHttpWiki env (pageLocation (PageId "page-9")) `shouldReturn` Right there
    (\rs -> [requestUrl r | r <- rs, "location" `T.isSuffixOf` requestUrl r]) <$> readIORef asked `shouldReturn` ["api/v1/pages/page-9/location"]

  it "answers a wrong password without sending it, once no account from before OPAQUE is left" $ do
    (env, asked) <- backend (opaqueServer False signedIn (const (status 500 "")))
    runHttpWiki env (signIn "admin" (Password "battery staple")) `shouldReturn` Left WrongCredentials
    runHttpWiki env (signIn "nobody" (Password "correct horse")) `shouldReturn` Left WrongCredentials
    rs <- readIORef asked
    map (\r -> (methodOf r, requestUrl r)) rs `shouldBe` replicate 2 ("POST", "api/v1/session/password")
    noPassword rs "battery staple"

  it "signs an account from before OPAQUE in the old way, with the password once, and registers its record at once" $ do
    let old r
          | requestUrl r == "api/v1/session" = ok signedIn
          | otherwise = status 500 ""
    (env, asked) <- backend (opaqueServer True signedIn old)
    s <- runHttpWiki env (signIn "olga" (Password "olga's old password"))
    (\x -> x.standing) <$> s `shouldBe` Right SignedIn
    rs <- reverse <$> readIORef asked
    map (\r -> (methodOf r, requestUrl r)) rs
      `shouldBe` [("POST", "api/v1/session/password"), ("POST", "api/v1/session"), ("POST", "api/v1/password-registration"), ("PUT", "api/v1/account/password")]
    -- The old sign-in names the attempt that failed, and is the one
    -- request with the password.
    [_, legacy, _, record] <- pure rs
    fmap snd (bodyOf legacy) `shouldSatisfy` maybe False (\b -> "\"attempt\":\"olga " `BS.isInfixOf` b && "olga's old password" `BS.isInfixOf` b)
    noPassword [r | r <- rs, requestUrl r /= "api/v1/session"] "olga's old password"
    fmap snd (bodyOf record) `shouldSatisfy` maybe False ("\"current\":null" `BS.isInfixOf`)

  it "says why signing in failed" $ do
    let failing n body = backend (const (status n body)) >>= \(env, _) -> runHttpWiki env (signIn "admin" (Password "nope"))
    failing 401 "{\"error\":\"Unauthenticated\"}" `shouldReturn` Left WrongCredentials
    failing 429 "{\"error\":\"TooManyRequests\",\"message\":\"Wait 15 minutes.\"}" `shouldReturn` Left (TooManyAttempts "Wait 15 minutes.")
    failing 403 "{\"error\":\"Forbidden\"}" `shouldReturn` Left (SignInFailed Forbidden)
    failing 503 "" `shouldReturn` Left (SignInFailed (Unreachable "the server failed, with status 503"))

  it "changes the password with PUT, the current one proved and the new one registered, keeps what it has read, and says why it could not" $ do
    (env, asked) <- backend (opaqueServer False signedIn (const (ok (Map.elems demo.spaces))))
    _ <- runHttpWiki env listSpaces
    runHttpWiki env (changePassword admin PasswordChange {current = Just (Password "correct horse"), new = Password "a new one"}) `shouldReturn` Right SignInMethods {password = True, identities = []}
    rs <- reverse <$> readIORef asked
    map (\r -> (methodOf r, requestUrl r)) rs
      `shouldBe` [("GET", "api/v1/spaces"), ("POST", "api/v1/account/password/proof"), ("PUT", "api/v1/account/password/proof"), ("POST", "api/v1/password-registration"), ("PUT", "api/v1/account/password")]
    noPassword rs "correct horse"
    noPassword rs "a new one"
    -- The rules for a new password are the backend's to apply, since the
    -- server never sees it.
    runHttpWiki env (changePassword admin PasswordChange {current = Just (Password "correct horse"), new = Password "short"}) >>= (`shouldSatisfy` \case Left (SignInFailed (Invalid _)) -> True; _ -> False)
    -- Who is asking is the same: what was read stays.
    isJust <$> runHttpWiki env (atOnce listSpaces) `shouldReturn` True
    let failing n body = backend (const (status n body)) >>= \(e, _) -> runHttpWiki e (changePassword admin PasswordChange {current = Just (Password "wrong"), new = Password "a new one"})
    failing 401 "{\"error\":\"Unauthenticated\"}" `shouldReturn` Left WrongCredentials
    failing 429 "{\"error\":\"TooManyRequests\",\"message\":\"Wait 15 minutes.\"}" `shouldReturn` Left (TooManyAttempts "Wait 15 minutes.")
    failing 400 "{\"error\":\"Invalid\",\"message\":\"Too short.\"}" `shouldReturn` Left (SignInFailed (Invalid "Too short."))

  it "keeps the session after a wrong current password, which is no session's end" $ do
    changed <- newIORef False
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == "api/v1/session" -> ok signedIn
        | otherwise -> status 401 "{\"error\":\"Unauthenticated\"}"
    env' <- newHttpEnvWith transport (withSession (fromSessionInfo signedIn) emptyCache)
    changes <- runHttpWiki env' sessionChanges
    _ <- HS.subscribe changes (const (writeIORef changed True))
    runHttpWiki env' (changePassword admin PasswordChange {current = Just (Password "wrong"), new = Password "a new one"}) `shouldReturn` Left WrongCredentials
    readIORef changed `shouldReturn` False
    runHttpWiki env' (atOnce session) `shouldReturn` Just (fromSessionInfo signedIn)
    map requestUrl <$> readIORef asked `shouldReturn` ["api/v1/session", "api/v1/account/password/proof"]

  -- A change of password is the user's who asked for it. Here the user
  -- has no password, and the change waits for the vault's lookup; the
  -- session changes meanwhile to another user without a password (a
  -- sign-out and a passkey's sign-in, in another tab). The server names
  -- the new record's credential after the session's user, and takes a
  -- record only for that user: the change must stop, or it sets the second
  -- user's password from what the first one typed.
  it "sets no password for a user signed in while another's change of password waited" $ do
    lookedUp <- newEmptyMVar
    answerLookup <- newEmptyMVar
    sessionUser <- newIORef ("user-alice" :: Text)
    changed <- newIORef ([] :: [Text])
    asked <- newIORef []
    let opaque = opaqueServer False signedIn (const (status 500 ""))
        server r = case (methodOf r, requestUrl r) of
          ("GET", "api/v1/assistant/vault") -> putMVar lookedUp () >> takeMVar answerLookup $> status 404 "{\"error\":\"NotFound\"}"
          ("POST", "api/v1/password-registration") -> do
            u <- readIORef sessionUser
            pure $
              opaque r <&> \answered -> case decode answered.responseBody of
                Just (c :: RegistrationChallenge) -> jsonResponse 200 (encode RegistrationChallenge {credential = u, response = c.response, stretch = c.stretch})
                Nothing -> answered
          ("PUT", "api/v1/account/password") -> do
            u <- readIORef sessionUser
            case bodyOf r >>= decodeStrict . snd of
              Just (c :: PasswordRecordChange)
                | c.password.credential == u -> atomicModifyIORef' changed (\us -> (u : us, ())) $> ok SignInMethods {password = True, identities = []}
              _ -> pure (status 400 "{\"error\":\"Invalid\",\"message\":\"The password was prepared for another account: try again.\"}")
          _ -> pure (opaque r)
    env <- newHttpEnv (Transport (\r -> atomicModifyIORef' asked (\rs -> (r : rs, ())) >> server r))
    done <- newEmptyMVar
    _ <- forkIO (runMemDOM (runHttpWiki env (changeOwnPassword changePassword (UserId "user-alice") PasswordChange {current = Nothing, new = Password "alice's new one"})) >>= putMVar done)
    takeMVar lookedUp
    writeIORef sessionUser "user-bob"
    putMVar answerLookup ()
    result <- takeMVar done
    result `shouldSatisfy` \case Left (SignInFailed (Conflict _)) -> True; _ -> False
    readIORef changed `shouldReturn` []
    filter (\r -> methodOf r == "PUT" && requestUrl r == "api/v1/account/password") <$> readIORef asked `shouldReturn` []

  it "signs out with DELETE, and starts over as nobody" $ do
    (env, asked) <- backend (const (ok nobody))
    runHttpWiki env signOut `shouldReturn` Right (fromSessionInfo nobody)
    map (\r -> (methodOf r, requestUrl r)) <$> readIORef asked `shouldReturn` [("DELETE", "api/v1/session")]
    runHttpWiki env (atOnce session) `shouldReturn` Just anonymous

  it "asks who is asking after a refusal while someone is signed in, and says when that has changed" $ do
    ended <- newIORef False
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == "api/v1/session" -> ok nobody
        | otherwise -> status 403 "{\"error\":\"Forbidden\"}"
    env' <- newHttpEnvWith transport (withSession (fromSessionInfo signedIn) emptyCache)
    changes <- runHttpWiki env' sessionChanges
    _ <- HS.subscribe changes (\s -> when (isNothing s.user) (writeIORef ended True))
    runHttpWiki env' (getSpace (slugs "team")) `shouldReturn` Left Forbidden
    readIORef ended `shouldReturn` True
    runHttpWiki env' (atOnce session) `shouldReturn` Just anonymous
    map requestUrl <$> readIORef asked `shouldReturn` ["api/v1/session", "api/v1/spaces/at/team"]
    -- Nobody is signed in now: another refusal asks nothing more.
    _ <- runHttpWiki env' (getSpace (slugs "docs"))
    length <$> readIORef asked `shouldReturn` 3

  it "asks who is asking once after a refusal, even when that question is refused too" $ do
    -- The question goes through the same client as every request; were it
    -- to ask again after its own refusal, it would never stop.
    (transport, asked) <- scripted (const (status 403 "{\"error\":\"Forbidden\"}"))
    env' <- newHttpEnvWith transport (withSession (fromSessionInfo signedIn) emptyCache)
    runHttpWiki env' (getSpace (slugs "team")) `shouldReturn` Left Forbidden
    map requestUrl <$> readIORef asked `shouldReturn` ["api/v1/session", "api/v1/spaces/at/team"]
    -- What the backend knew of the session stays.
    runHttpWiki env' (atOnce session) `shouldReturn` Just (fromSessionInfo signedIn)

preload :: Spec
preload = describe "what a page it is rendered into carries" $ do
  let hello = demoPage "blog" "hello-world"
      blogPages = [p | p <- Map.elems demo.pages, p.space == hello.space]
      draft = Draft {page = hello.id, author = UserId "admin", title = "t", body = "b <script>", base = hello.updatedAt, savedAt = hello.updatedAt}
      cache =
        withSpaces (Map.elems demo.spaces)
          . withSpaceAt (slugs "blog") (Right (fromMaybe (panic "blog") (Map.lookup hello.space demo.spaces)))
          . withSpaceAt (slugs "team") (Left Forbidden)
          . withPages hello.space (Right (map summaryOf blogPages))
          . withPages (SpaceId "gone") (Left NotFound)
          . withPageAt hello.space (slugs "nowhere") (Left NotFound)
          . withDraft hello.id (HasDraft draft)
          . withDraft (PageId "other") NoDraft
          . withSearch (searchQuery "hello") []
          . withPageLocation (PageId "gone") (Left NotFound)
          . withPageLocation hello.id (Right PageLocation {space = slugs "blog", page = slugs "hello-world"})
          . withBacklinks hello.id (Right [Backlink {page = PageId "notes", title = "Notes", location = PageLocation {space = slugs "blog", page = slugs "notes"}}])
          . withBacklinks (PageId "gone") (Left NotFound)
          $ emptyCache
  it "reads back as the cache it was written from" $
    decodePreload (encodePreload cache) `shouldBe` Right cache

  it "carries which plugins are on, read with the session" $ do
    let known = Plugins {offForWiki = Set.fromList [PluginId "errors"], mine = Map.fromList [(PluginId "assistant", PluginChoice {state = PluginOff, options = Nothing})]}
        withKnown = emptyCache {Cache.plugins = Just (Right known)}
    decodePreload (encodePreload withKnown) `shouldBe` Right withKnown
  it "carries the page shown by itself, beside its space's list of summaries" $ do
    let withPage = withPageAt hello.space (slugs "hello-world") (Right hello) cache
    decodePreload (encodePreload withPage) `shouldBe` Right withPage
    (cachedPage hello.space (slugs "hello-world") <$> decodePreload (encodePreload withPage)) `shouldBe` Right (Just (Right hello))

  it "writes a refusal as the API's error" $
    encodePreload cache `shouldSatisfy` T.isInfixOf "{\"error\":{\"error\":\"Forbidden\"},\"path\":[\"team\"]}"

  it "reads a payload written before the later reads were carried as without them" $
    decodePreload "{\"session\":null,\"spaces\":null,\"spacesAt\":[],\"pages\":[],\"pagesAt\":[],\"drafts\":[],\"searches\":[],\"versionLists\":[],\"versions\":[]}"
      `shouldBe` Right emptyCache

  it "writes what is not kept as nothing, and nothing as null" $
    encodePreload (withSpaceAt (slugs "away") (Left (Unreachable "offline")) emptyCache)
      `shouldBe` "{\"activity\":null,\"backlinks\":null,\"backups\":null,\"directory\":null,\"drafts\":[],\"homeDocument\":null,\"invitations\":null,\"invitationsAt\":null,\"notices\":null,\"pageCounts\":null,\"pageLocations\":null,\"pages\":[],\"pagesAt\":[],\"people\":null,\"plugins\":null,\"postDetails\":null,\"searches\":[],\"server\":null,\"session\":null,\"settings\":null,\"spaces\":null,\"spacesAt\":[],\"stars\":null,\"status\":null,\"texEngine\":null,\"threads\":null,\"userPages\":null,\"versionLists\":[],\"versions\":[],\"watches\":null,\"whatsNew\":null}"

  it "says why a payload cannot be read" $
    decodePreload "{\"spaces\": 3}" `shouldSatisfy` isLeft

-- | The reads of a page by its id whose route depends on how the page
-- reads ('PageRead'), against the API's records of the routes of a page:
-- every field of those records is either such a read, named by a
-- 'PageRead' (which the browser tests go through, both ways, from
-- 'PageRead''s every value), or listed here as no such read, with why. A
-- route added to a page later fails this until it is put in one or the
-- other.
pageReads :: Spec
pageReads = describe "the reads of a page by its id" $
  it "names every route of a page as a read the page's state reroutes, or as none, with why" $ do
    let covered record = sort [field | r <- [minBound .. maxBound], (record', field) <- pageReadRoutes r, record' == record]
        -- Writes, refused by the server while the page is in another
        -- state, and never sent again ('writeOnce').
        writes = ["save", "move", "shift", "template", "kind", "delete", "upload", "saveDraft", "discardDraft", "restore"]
        -- A page's location, its backlinks and its draft: the same route
        -- whatever the page's state (a page of an encrypted space has no
        -- backlinks, and no draft, which the server answers so); its
        -- export: a plain one, which an encrypted space does not offer.
        samePlain = ["location", "backlinks", "draft", "export"]
        -- Whether the page's text is taken plain now: asked before a plain
        -- write, by the same route whatever the page's state; its refusal
        -- is the answer ('plainNow').
        question = ["plain"]
    sort (fieldNames (Proxy @(Rep (PagesApi Mem AsApi)))) `shouldBe` sort (covered "PagesApi" <> writes <> samePlain <> question)
    -- A thread's writes, and its resolving and deleting, which carry no
    -- text: by the routes the page's state names, never sent again.
    sort (fieldNames (Proxy @(Rep (CommentsApi AsApi)))) `shouldBe` sort (covered "CommentsApi" <> ["open", "reply", "edit", "deleteComment", "delete", "resolve"])
    let spaceLevel = ["space", "encrypt", "subspace", "home", "grant", "rotation", "rotate", "pages", "contents", "readers", "createPage", "conversion", "startEncrypting", "startDecrypting", "reverseConversion", "setEntry", "lease"]
        -- By a page's path, through the space's route ('getPage', which
        -- the space's state reroutes); by many pages' ids, answered with
        -- those of them that read encrypted, never refused for it.
        byPathOrMany = ["pageAt", "headings"]
        sealedWrites = ["save", "openThread", "reply", "editComment", "upload"]
        conversion = ["stageSealed", "stagePlain", "stageBytes", "staged", "stagedBytes", "commit"]
    sort (fieldNames (Proxy @(Rep (EncryptedApi Mem AsApi)))) `shouldBe` sort (covered "EncryptedApi" <> spaceLevel <> byPathOrMany <> sealedWrites <> conversion)

-- | The names of the fields of a record, from its generic representation.
class FieldNames f where
  fieldNames :: Proxy f -> [Text]

instance (FieldNames f) => FieldNames (M1 D c f) where
  fieldNames _ = fieldNames (Proxy @f)

instance (FieldNames f) => FieldNames (M1 C c f) where
  fieldNames _ = fieldNames (Proxy @f)

instance (FieldNames f, FieldNames g) => FieldNames (f :*: g) where
  fieldNames _ = fieldNames (Proxy @f) <> fieldNames (Proxy @g)

instance (Generics.Selector s) => FieldNames (M1 S s (K1 i a)) where
  fieldNames _ = [toS (selName (M1 (K1 (panic "never read")) :: M1 S s (K1 i a) ()))]

-- | The user's API keys ("Wiki.Api.ApiKeys"): listed, made with the
-- current password proved first as for a passkey, and revoked, at the
-- account's addresses.
apiKeys :: Spec
apiKeys = describe "the user's API keys" $ do
  let laptop =
        ApiKeyInfo
          { id = ApiKeyId "m3k7q2xa9dwn4zrt"
          , name = "laptop"
          , right = WriteRight
          , createdAt = UTCTime (fromGregorian 2026 11 20) 0
          , expiresAt = Nothing
          , lastUsedAt = Nothing
          , revokedAt = Nothing
          }
      revoked = laptop {revokedAt = Just (UTCTime (fromGregorian 2026 11 21) 0)}
      made = MadeApiKey {key = "wiki_m3k7q2xa9dwn4zrt_" <> T.replicate 52 "a", info = laptop}
      newWith password = NewApiKey {name = "laptop", right = WriteRight, expiresAt = Nothing, password}
      new = newWith (Just (Password "correct horse"))
      keysAt r = (methodOf r, requestUrl r) == ("POST", "api/v1/account/api-keys")

  it "lists them with GET, and revokes one with POST, taking the keys after it" $ do
    (env, asked) <- backend $ \r -> if methodOf r == "GET" then ok [laptop] else ok [revoked]
    runHttpWiki env ownApiKeys `shouldReturn` Right [laptop]
    runHttpWiki env (revokeApiKey laptop.id) `shouldReturn` Right [revoked]
    map (\r -> (methodOf r, requestUrl r)) . reverse <$> readIORef asked
      `shouldReturn` [("GET", "api/v1/account/api-keys"), ("POST", "api/v1/account/api-keys/m3k7q2xa9dwn4zrt/revoke")]

  it "makes one with the current password proved through OPAQUE first, which no request carries, and takes the 201 with the key" $ do
    (env, asked) <- backend (opaqueServer False signedIn (\r -> if keysAt r then Right (jsonResponse 201 (encode made)) else status 500 ""))
    runHttpWiki env (makeApiKey new) >>= (`shouldSatisfy` \case Right m -> m.key == made.key && m.info == laptop; Left _ -> False)
    rs <- reverse <$> readIORef asked
    map (\r -> (methodOf r, requestUrl r)) rs
      `shouldBe` [("POST", "api/v1/account/password/proof"), ("PUT", "api/v1/account/password/proof"), ("POST", "api/v1/account/api-keys")]
    noPassword rs "correct horse"
    [r | r <- rs, keysAt r] `shouldSatisfy` all (\r -> maybe False (("\"password\":null" `BS.isInfixOf`) . snd) (bodyOf r))

  it "makes one with no proof for an account with no password" $ do
    (env, asked) <- backend (\r -> if keysAt r then Right (jsonResponse 201 (encode made)) else status 500 "")
    isRight <$> runHttpWiki env (makeApiKey (newWith Nothing)) `shouldReturn` True
    map requestUrl <$> readIORef asked `shouldReturn` ["api/v1/account/api-keys"]

  it "says why a key was not made: a wrong password, too many attempts, the right to administer, the server's rules" $ do
    let failing n body = backend (\r -> if keysAt r then status n body else status 500 "") >>= \(e, _) -> runHttpWiki e (makeApiKey (newWith Nothing))
    failing 401 "{\"error\":\"Unauthenticated\"}" `shouldReturn` Left WrongCredentials
    failing 429 "{\"error\":\"TooManyRequests\",\"message\":\"Wait 15 minutes.\"}" `shouldReturn` Left (TooManyAttempts "Wait 15 minutes.")
    failing 403 "{\"error\":\"Forbidden\"}" `shouldReturn` Left (SignInFailed Forbidden)
    failing 400 "{\"error\":\"Invalid\",\"message\":\"You have 20 keys that work already: revoke one first.\"}"
      `shouldReturn` Left (SignInFailed (Invalid "You have 20 keys that work already: revoke one first."))
    -- A wrong password at the proof is told before any key is asked for.
    (env, asked) <- backend (opaqueServer False signedIn (const (status 500 "")))
    runHttpWiki env (makeApiKey (newWith (Just (Password "battery staple")))) `shouldReturn` Left WrongCredentials
    readIORef asked >>= (`shouldSatisfy` all (not . keysAt))

-- | The question the backend asks before it sends any plain text of a page
-- ('plainNow', @pages\/\<page>\/plain@): the server's answer for the page
-- where it is now (#389's rounds). A list of spaces read before cannot
-- say that the page moved to a space encrypted since, nor anything of a
-- space the reader may no longer see.
plainText :: Spec
plainText = describe "the question whether a page's text is plain now" $ do
  let hello = demoPage "blog" "hello-world"
      blog = fromMaybe (panic "no blog") (Map.lookup hello.space demo.spaces)
      plainAt p = endpointUrl (\e -> e.pages.plain p)
      stale code = status 409 (encode (ApiError.Stale code "Not as the browser believed."))
      writesOut rs = [(methodOf r, requestUrl r) | r <- rs, methodOf r /= "GET"]
      -- A refusal of the question, as the reader is told it: the space's
      -- state with the server's code, never a conflict of versions.
      refused code why = \case
        Left (Stale c t) -> c == code && why `T.isInfixOf` t && "nothing was sent" `T.isInfixOf` t
        _ -> False

  it "asks the server about the page before each plain write, and sends the write when it says yes" $ do
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == plainAt hello.id -> Right (responseWith 204 [("Cache-Control", "private, no-store")] "")
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    for_ (writesOf hello) $ \(name, write') -> do
      earlier <- length <$> readIORef asked
      _ <- runHttpWiki env write'
      rs <- readIORef asked
      -- The question, then the write, and nothing else.
      (name, length rs - earlier, [(methodOf r == "GET", requestUrl r == plainAt hello.id) | r <- reverse (take 2 rs)])
        `shouldBe` (name, 2, [(True, True), (False, False)])
    -- Every write was sent, and no list of spaces was read for them: what
    -- the service worker could answer from a copy is not asked.
    length . writesOut <$> readIORef asked `shouldReturn` length (writesOf hello)
    filter (== "api/v1/spaces") . map requestUrl <$> readIORef asked `shouldReturn` []

  it "sends nothing once the server says the page's space is encrypted, though the list says the space it was read in is plain" $ do
    -- Read in the blog; moved since, in another tab, to a space then
    -- encrypted. The list still names the blog plain.
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == "api/v1/spaces" -> ok [blog]
        | requestUrl r == endpointUrl (\e -> e.spaces.pageAt hello.space (slugs "hello-world")) -> ok hello
        | requestUrl r == plainAt hello.id -> stale ApiError.SpaceIsEncrypted
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    for_ (writesOf hello) $ \(name, write') -> runHttpWiki env write' >>= \r -> (name, refused ApiError.SpaceIsEncrypted "encrypted since the page was opened" r) `shouldBe` (name, True)
    writesOut <$> readIORef asked `shouldReturn` []
    -- The refusal says the space is not as this tab believed: the list is
    -- read again, as after any such refusal.
    filter (== "api/v1/spaces") . map requestUrl <$> readIORef asked >>= (`shouldSatisfy` (not . null))

  it "sends nothing for a page the server no longer shows the reader, whose space the list no longer names" $ do
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == "api/v1/spaces" -> ok ([] :: [Space])
        | requestUrl r == endpointUrl (\e -> e.spaces.pageAt hello.space (slugs "hello-world")) -> ok hello
        | requestUrl r == plainAt hello.id -> status 404 "{\"error\":\"NotFound\"}"
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    runHttpWiki env (getPage hello.space (slugs "hello-world")) `shouldReturn` Right hello
    -- Refused as the write itself would have been.
    for_ (writesOf hello) $ \(name, write') -> runHttpWiki env write' >>= \r -> (name, r) `shouldBe` (name, Left NotFound)
    writesOut <$> readIORef asked `shouldReturn` []

  it "sends nothing while the page's space is being converted" $ do
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == plainAt hello.id -> stale ApiError.ConversionRuns
        | requestUrl r == "api/v1/spaces" -> ok [blog]
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    for_ (writesOf hello) $ \(name, write') -> runHttpWiki env write' >>= \r -> (name, refused ApiError.ConversionRuns "being encrypted or decrypted" r) `shouldBe` (name, True)
    writesOut <$> readIORef asked `shouldReturn` []

  -- #394's third round: deleting a comment or a thread and resolving a
  -- thread send no text, so no question goes before them, for a page whose
  -- space is encrypted too; a comment's text still waits for the answer.
  it "asks nothing before a change of comments that carries no text, and asks before one that does" $ do
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == plainAt hello.id -> stale ApiError.SpaceIsEncrypted
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    let changes =
          [ void <$> deleteComment hello.id (CommentThreadId "t1") (CommentId "c1")
          , void <$> deleteThread hello.id (CommentThreadId "t1")
          , void <$> setResolved hello.id (CommentThreadId "t1") True
          ]
    for_ changes $ \change -> do
      earlier <- length <$> readIORef asked
      _ <- runHttpWiki env change
      rs <- readIORef asked
      -- The change itself, and no question.
      [(methodOf r /= "GET", requestUrl r == plainAt hello.id) | r <- take (length rs - earlier) rs] `shouldBe` [(True, False)]
    -- A comment's text: the question, and no write on its no (the list of
    -- spaces read again after it, as after any refusal for the space's
    -- state).
    earlier <- length <$> readIORef asked
    runHttpWiki env (openThread hello.id Nothing "A comment") >>= (`shouldSatisfy` isLeft)
    rs <- readIORef asked
    [(methodOf r, requestUrl r) | r <- take (length rs - earlier) rs, requestUrl r /= "api/v1/spaces"] `shouldBe` [("GET", plainAt hello.id)]

  it "sends nothing when the question gets no answer, as offline" $ do
    (transport, asked) <- scripted $ \r ->
      if
        | requestUrl r == plainAt hello.id -> Left "the network or the server is down"
        | otherwise -> status 500 ""
    env <- newHttpEnvWith transport (withSpaces [blog] emptyCache)
    for_ (writesOf hello) $ \(name, write') -> runHttpWiki env write' >>= \r -> (name, r) `shouldBe` (name, Left (Unreachable "the network or the server is down"))
    writesOut <$> readIORef asked `shouldReturn` []

  it "makes no page in a space the list of spaces fetched now no longer names, and makes one in a space it names" $ do
    -- The front page's space, which no list names: another one.
    let front = fromMaybe (panic "no docs") (head [x | x <- Map.elems demo.spaces, x.id /= blog.id, isNothing x.parent, isNothing x.encryption, not x.personalContainer])
        server listed r =
          if
            | requestUrl r == "api/v1/spaces" -> ok listed
            | requestUrl r == "api/v1/home" -> ok (front, hello)
            | otherwise -> status 500 ""
        posts rs = [requestUrl r | r <- rs, methodOf r == "POST"]
    (gone, goneAsked) <- scripted (server ([] :: [Space]))
    goneEnv <- newHttpEnvWith gone (withSpaces [blog] emptyCache)
    runHttpWiki goneEnv (createPage blog.id Nothing "New" "Text") `shouldReturn` Left NotFound
    posts <$> readIORef goneAsked `shouldReturn` []
    -- The front page's space is not refused for it: the server answers.
    _ <- runHttpWiki goneEnv (createPage front.id Nothing "New" "Text")
    posts <$> readIORef goneAsked `shouldReturn` ["api/v1/spaces/" <> front.id.text <> "/pages"]
    (there, thereAsked) <- scripted (server [blog])
    thereEnv <- newHttpEnvWith there (withSpaces [blog] emptyCache)
    _ <- runHttpWiki thereEnv (createPage blog.id Nothing "New" "Text")
    posts <$> readIORef thereAsked `shouldReturn` ["api/v1/spaces/" <> blog.id.text <> "/pages"]

  -- The guard against a model of the server: pages that move between
  -- spaces, spaces encrypted, converted, decrypted, opened to the reader
  -- and closed to them, in any order, with reads and writes of the pages
  -- in between. A plain write reaches the server only for a page whose
  -- space is plain and open to the reader at that moment, and every write
  -- of such a page is sent. A guard that went by the list of spaces and
  -- the space a page was read in (as before #389's rounds) fails it: a
  -- page read, moved to another space, and that space encrypted.
  prop "sends a plain write exactly when the server's page is plain and open to the reader at that moment" $
    forAllShrink (listOf1 (eventFor 2 3)) (shrinkList (const [])) (ioProperty . history)

  -- The history the old guard let through: the page read in the blog,
  -- moved by another tab to the docs, and the docs then encrypted.
  it "sends no plain write of a page read, moved to another space, and that space encrypted" . once . ioProperty $
    history [Read 0, Move 0 1, Encrypt 1, Write 0 0, Write 0 1, Write 0 3]

-- | The guard over a history of events against the model of the server
-- ('Model'): every plain write that reached the server was of a page plain
-- and open to the reader then, and every write of such a page was sent.
history :: [Event] -> IO Property
history events = do
  model <- newIORef (Model {at = Map.fromList (zip [0 ..] [0, 1]), encrypted = mempty, converting = mempty, closed = mempty})
  sent <- newIORef []
  let transport = modelTransport model sent
  env <- newHttpEnvWith transport (withSpaces modelSpaces emptyCache)
  outcomes <- for events $ \event -> case event of
    Write p k -> do
      sentBefore <- length <$> readIORef sent
      wanted <- (`plainFor` p) <$> readIORef model
      _ <- try @SomeException (runHttpWiki env (snd (fromMaybe (panic "no write") (atMay (writesOf (modelPage p 0)) k))))
      sentAfter <- length <$> readIORef sent
      pure [counterexample ("write of page " <> show p <> (if wanted then " not sent" else " sent")) ((sentAfter > sentBefore) === wanted)]
    _ -> [] <$ happen env model event
  writes <- readIORef sent
  pure (counterexample (show events) (conjoin (counterexample ("plain writes sent: " <> show (map fst writes)) (all snd writes) : concat outcomes)))

-- | The plain writes of a page that carry its text or text about it, each
-- named: a save, a draft, an attachment, a thread opened, a reply and a
-- comment edited.
writesOf :: Page -> [(Text, HttpWiki IO (Either WikiError ()))]
writesOf p =
  [ ("save", void <$> savePage p.id p.updatedAt "Hello" "Edited")
  , ("draft", void <$> saveDraft p.id p.updatedAt "Hello" "Edited")
  , ("upload", void <$> uploadAttachment p.id "a.txt" "text/plain" "bytes")
  , ("open", void <$> openThread p.id Nothing "A comment")
  , ("reply", void <$> reply p.id (CommentThreadId "t1") "A reply")
  , ("edit", void <$> editComment p.id (CommentThreadId "t1") (CommentId "c1") "Edited")
  ]

-- | The server as the property models it: which space (by its number in
-- 'modelSpaces') each page (by its number) is in, and which spaces are
-- encrypted, being encrypted, or closed to the reader.
data Model = Model
  { at :: Map Int Int
  , encrypted :: Set Int
  , converting :: Set Int
  , closed :: Set Int
  }

-- | What happens next: the tab reads a page where it is now; another tab
-- moves a page, encrypts a space, starts encrypting it, turns its
-- encryption off, or an administrator closes it to the reader or opens it
-- again; or the tab writes a page (one of 'writesOf').
data Event
  = Read Int
  | Move Int Int
  | Encrypt Int
  | Convert Int
  | Decrypt Int
  | Close Int
  | Open Int
  | Write Int Int
  deriving stock (Show)

-- | An event over so many pages and spaces.
eventFor :: Int -> Int -> Gen Event
eventFor pages spaces =
  oneof
    [ Read <$> page
    , Move <$> page <*> space
    , Encrypt <$> space
    , Convert <$> space
    , Decrypt <$> space
    , Close <$> space
    , Open <$> space
    , Write <$> page <*> chooseInt (0, 5)
    , Write <$> page <*> chooseInt (0, 5)
    ]
  where
    page = chooseInt (0, pages - 1)
    space = chooseInt (0, spaces - 1)

-- | The demo's top-level spaces the model moves its pages between.
modelSpaces :: [Space]
modelSpaces = take 3 (sortOn (\s -> s.slug) [s | s <- Map.elems demo.spaces, isNothing s.parent, not s.personalContainer])

-- | A page of the model, in the space of that number: the demo's first
-- post, or a second page beside it.
modelPage :: Int -> Int -> Page
modelPage p sid =
  let hello = demoPage "blog" "hello-world"
      at' = fromMaybe hello.space ((\s -> s.id) <$> atMay modelSpaces sid)
   in if p == 0
        then hello {P.space = at'}
        else hello {P.id = PageId "second", P.slug = fromMaybe hello.slug (head (slugs "second")), P.title = "Second", P.space = at'}

-- | Whether the server takes the page's text plain now: its space open to
-- the reader, neither encrypted nor being converted.
plainFor :: Model -> Int -> Bool
plainFor m p = case Map.lookup p m.at of
  Just s -> s `Set.notMember` (m.encrypted <> m.converting <> m.closed)
  Nothing -> False

-- | The model's server: the list of spaces open to the reader, as they
-- are now; a page read where it is; the question 'plainNow' asks; and
-- every other request refused. Each write is kept with whether the page
-- was plain when it came.
modelTransport :: IORef Model -> IORef [(Text, Bool)] -> Transport
modelTransport model sent = Transport $ \r -> do
  m <- readIORef model
  let url = requestUrl r
      listed = [shown i s | (i, s) <- zip [0 ..] modelSpaces, i `Set.notMember` m.closed]
      shown i s
        | i `Set.member` m.converting = s {encryption = Just (KeyId "k"), conversion = Just S.Encrypting}
        | i `Set.member` m.encrypted = s {encryption = Just (KeyId "k")}
        | otherwise = s
      pageNamed = head [p | p <- [0, 1], (modelPage p 0).id.text `T.isInfixOf` url]
      stale code = status 409 (encode (ApiError.Stale code "Not as the browser believed."))
      notFound = status 404 "{\"error\":\"NotFound\"}"
      pageReads' =
        [ (endpointUrl (\e -> e.spaces.pageAt (modelPage p s).space [(modelPage p s).slug]), (p, s))
        | p <- [0, 1]
        , s <- [0 .. length modelSpaces - 1]
        ]
  if
    | methodOf r /= "GET" -> do
        for_ pageNamed $ \p -> atomicModifyIORef' sent (\ws -> ((url, plainFor m p) : ws, ()))
        pure (status 500 "")
    | url == "api/v1/spaces" -> pure (ok listed)
    | Just p <- pageNamed
    , url == endpointUrl (\e -> e.pages.plain (modelPage p 0).id) ->
        pure $ case Map.lookup p m.at of
          Just s
            | s `Set.member` m.closed -> notFound
            | s `Set.member` m.converting -> stale ApiError.ConversionRuns
            | s `Set.member` m.encrypted -> stale ApiError.SpaceIsEncrypted
          _ -> Right (responseWith 204 [] "")
    | Just (p, s) <- lookup url pageReads' ->
        pure $
          if
            | Map.lookup p m.at /= Just s || s `Set.member` m.closed -> notFound
            | s `Set.member` m.encrypted -> stale ApiError.SpaceIsEncrypted
            -- A page not converted yet reads plain while its space is
            -- being encrypted.
            | otherwise -> ok (modelPage p s)
    | otherwise -> pure notFound

-- | An event other than a write: the tab's read, or a change on the
-- server. A read the backend refuses, or cannot make natively (an
-- encrypted one: there is no WebCrypto), changes nothing of the model.
happen :: HttpEnv -> IORef Model -> Event -> IO ()
happen env model = \case
  Read p -> do
    m <- readIORef model
    for_ (Map.lookup p m.at) $ \s ->
      void (try @SomeException (runHttpWiki env (getPage (modelPage p s).space [(modelPage p s).slug])))
  Move p s -> change (\m -> m {at = Map.insert p s m.at})
  Encrypt s -> change (\m -> m {encrypted = Set.insert s m.encrypted, converting = Set.delete s m.converting})
  Convert s -> change (\m -> m {converting = Set.insert s m.converting})
  Decrypt s -> change (\m -> m {encrypted = Set.delete s m.encrypted, converting = Set.delete s m.converting})
  Close s -> change (\m -> m {closed = Set.insert s m.closed})
  Open s -> change (\m -> m {closed = Set.delete s m.closed})
  Write _ _ -> pass
  where
    change f = atomicModifyIORef' model (\m -> (f m, ()))

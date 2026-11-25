-- | External plugins (WP5.84 of docs/PLAN.md, decision 69): a hostile
-- plugin installed for everyone by the administrator, run in its frame on
-- a page of the wiki as the editor will run it (a sandboxed frame of the
-- plugin's address, a MessagePort given in the first message), finds that
-- its code runs in a worker with an opaque origin, no page, no WebRTC, no
-- storage, no fetch to the wiki or to a site its manifest does not name,
-- no WebSocket and no script from elsewhere, while a fetch to the host its
-- manifest names arrives; an endless loop in it stops neither the page nor
-- its frame; and the frame's address is refused to a visitor and to a tab.
--
-- What the plugin does (its attempts, its loop) is the frame's own, not the
-- program's: its report, which the page writes into an element of its
-- own, is waited for as a condition.
module Server.Plugin (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (withApplication)
import Server.Site
import Wiki.Smoke.Api
import Wiki.Smoke.Browser
import Wiki.Smoke.Demo (nothingWrongExcept)
import Wiki.Smoke.Walk

spec :: Smoke -> SpecWith Playwright
spec smoke = describe "external plugins" $ do
  step smoke.chain "runs a plugin's code in a worker of its sandboxed frame, which reaches the host its manifest names and nothing of the wiki's or another site's, has no WebRTC, and loops without stopping the page" $ \pw ->
    withElsewhere $ \allowed ->
      withElsewhere $ \other -> plugins smoke pw allowed other
  step smoke.chain "marks in the editor what the example plugin finds, shows a plugin that loops stopped while the page goes on, and leaves the plugin's frame to the network past the service worker" $ \pw ->
    inTheEditor smoke pw

-- | Another site, on localhost: the paths that reached it, in order.
data Elsewhere = Elsewhere {origin :: Text, reached :: IORef [Text]}

withElsewhere :: (Elsewhere -> IO a) -> IO a
withElsewhere body = do
  reached <- newIORef []
  let app request respond = do
        atomicModifyIORef' reached (\xs -> (xs <> [Text.decodeUtf8Lenient (rawPathInfo request)], ()))
        respond (responseLBS status200 [(hContentType, "text/plain"), ("Access-Control-Allow-Origin", "*")] "elsewhere")
  withApplication (pure app) $ \port -> body Elsewhere {origin = "http://localhost:" <> tshow port, reached}

plugins :: Smoke -> Playwright -> Elsewhere -> Elsewhere -> IO ()
plugins smoke pw allowed other = do
  s <- adminSession smoke
  _ <- asAdmin smoke "PUT" "external-plugins/policy" (Just (object ["policy" .= ("wiki" :: Text)])) >>= expectStatus 200 "turning external plugins on"
  let manifest =
        object
          [ "manifest" .= (1 :: Int)
          , "id" .= ("hostile" :: Text)
          , "name" .= ("Hostile" :: Text)
          , "version" .= ("1.0.0" :: Text)
          , "api" .= (1 :: Int)
          , "contributes" .= object ["diagnostics" .= object []]
          , "permissions" .= ["document.read" :: Text]
          , "hosts" .= [allowed.origin]
          ]
  installed <-
    upload smoke.wiki s "external-plugins/wiki" "bundle" "hostile.js" "text/javascript" (Text.encodeUtf8 (hostileWorker smoke.origin allowed.origin other.origin)) [("manifest", Text.decodeUtf8 (LazyByteString.toStrict (Aeson.encode manifest)))]
      >>= expectStatus 200 "installing the hostile plugin"
      >>= apiJson
  bundle <- text (installed .! "bundle")
  let path = "/plugin-frame/wiki/hostile/" <> bundle <> "/diagnostics"
  -- To a visitor, no frame; to the administrator, the frame with the
  -- policy that names the plugin's host.
  (.status) <$> get smoke.wiki Nothing path `shouldReturn` 404
  framed <- get smoke.wiki (Just s) path >>= expectStatus 200 "the plugin's frame"
  expect (maybe False (Text.isInfixOf ("connect-src " <> allowed.origin <> ";")) (header "Content-Security-Policy" framed)) ("the frame's policy: " <> show (header "Content-Security-Policy" framed))

  withSessionContext pw serverContext smoke.origin (Just s) $ \page -> do
    visit page (address smoke "/")
    -- The page frames the plugin and connects to it as the editor will:
    -- the frame's first load, then a MessagePort in the first message.
    -- Each message from the plugin is written into an element of the
    -- page's own, outside the program's.
    _ :: Value <-
      evaluateWith
        page
        "(path) => { \
        \  const out = document.createElement('pre'); out.id = 'plugin-said'; document.body.appendChild(out); \
        \  const frame = document.createElement('iframe'); frame.id = 'plugin-frame'; frame.setAttribute('sandbox', 'allow-scripts'); \
        \  let loads = 0; \
        \  frame.addEventListener('load', () => { \
        \    loads += 1; if (loads !== 1) return; \
        \    const channel = new MessageChannel(); window.pluginPort = channel.port1; \
        \    channel.port1.onmessage = (e) => { out.textContent = JSON.stringify(e.data); }; \
        \    frame.contentWindow.postMessage({ wikiPlugin: 'connect', api: 1 }, '*', [channel.port2]); \
        \  }); \
        \  frame.src = path; document.body.appendChild(frame); return null; }"
        path
    let said = locator page "#plugin-said"
    reportText <- awaitText said ("\"type\":\"report\"" `Text.isInfixOf`)
    told <- either (failure . ("the plugin's report: " <>)) pure (Aeson.eitherDecodeStrict (Text.encodeUtf8 reportText)) :: IO (Map Text Value)
    found <- case Map.lookup "report" told of
      Just r -> case Aeson.fromJSON r of
        Aeson.Success m -> pure (m :: Map Text Text)
        Aeson.Error e -> failure ("the plugin's report: " <> e)
      Nothing -> failure ("no report in " <> show told)
    let at k = Map.findWithDefault "" k found
    at "connected" `shouldBe` "api 1"
    at "origin" `shouldBe` "null"
    at "document" `shouldBe` "undefined"
    -- No WebRTC in a worker, where the frame's page would have one that
    -- reaches any host (docs/PLAN.md, the external plugins' design).
    at "rtc" `shouldBe` "undefined"
    expect ("threw" `Text.isPrefixOf` at "indexedDB") ("IndexedDB, from the plugin: " <> show (at "indexedDB"))
    expect (at "caches" /= "opened") ("the caches, from the plugin: " <> show (at "caches"))
    expect ("threw" `Text.isPrefixOf` at "fetchWiki") ("the wiki's API, from the plugin: " <> show (at "fetchWiki"))
    at "fetchAllowed" `shouldBe` "elsewhere"
    expect ("threw" `Text.isPrefixOf` at "fetchOther") ("another site, from the plugin: " <> show (at "fetchOther"))
    expect (at "socket" /= "open") ("a WebSocket, from the plugin: " <> show (at "socket"))
    expect ("threw" `Text.isPrefixOf` at "importScripts") ("a script from another site, from the plugin: " <> show (at "importScripts"))
    -- The report was sent after every attempt: what reached the two sites
    -- is all that will.
    readIORef allowed.reached `shouldReturn` ["/allowed"]
    readIORef other.reached `shouldReturn` []

    -- An endless loop in the plugin: once it says it loops, the page still
    -- answers, since the loop holds the worker's thread and not the page's
    -- (in the frame's page it would hold the wiki's too, where the browser
    -- does not give the frame a process of its own). Removing the frame
    -- ends the worker.
    _ :: Value <- evaluate page "(() => { window.pluginPort.postMessage({ type: 'loop' }); return null; })()"
    _ <- awaitText said ("\"type\":\"looping\"" `Text.isInfixOf`)
    evaluate page "1 + 1" `shouldReturn` (2 :: Int)
    _ :: Value <- evaluate page "(() => { document.getElementById('plugin-frame').remove(); return null; })()"
    evaluate page "document.querySelectorAll('iframe').length" `shouldReturn` (0 :: Int)
    nothingWrongExcept fromThePlugin serviceWorkerInSandbox page

  -- The wiki as the walk found it: no plugin, and external plugins off.
  _ <- asAdmin smoke "DELETE" "external-plugins/wiki/hostile" Nothing >>= expectStatus 204 "removing the hostile plugin"
  _ <- asAdmin smoke "PUT" "external-plugins/policy" (Just (object ["policy" .= ("off" :: Text)])) >>= expectStatus 200 "turning external plugins off"
  (.status) <$> get smoke.wiki (Just s) path `shouldReturn` 404

-- | What the plugin's frame and its worker write on the console: what the
-- policy refuses them, each attempt of the hostile plugin's.
fromThePlugin :: ConsoleMessage -> Bool
fromThePlugin m = expectedOnServer m || maybe False (\u -> "/plugin-frame/" `Text.isInfixOf` u || "blob:null/" `Text.isPrefixOf` u) m.url

-- | The hostile plugin: a worker's code that tries what a plugin must not
-- be able to do once the wiki connects, posts what happened as its report,
-- and then loops for ever when told to. Each attempt is written as a
-- string: what it gave, or @threw@ and the error's name.
hostileWorker :: Text -> Text -> Text -> Text
hostileWorker wiki allowed other =
  Text.unlines
    [ "const WIKI = " <> quoted wiki <> ", ALLOWED = " <> quoted allowed <> ", OTHER = " <> quoted other <> ";"
    , "const out = {};"
    , "const attempt = async (name, f) => { try { out[name] = String(await f()); } catch (e) { out[name] = 'threw ' + e.name; } };"
    , "self.onmessage = async (event) => {"
    , "  const data = event.data;"
    , "  if (data && data.wikiPlugin === 'connect') {"
    , "    await attempt('connected', () => 'api ' + data.api);"
    , "    await attempt('origin', () => self.origin);"
    , "    await attempt('document', () => typeof document);"
    , "    await attempt('rtc', () => typeof RTCPeerConnection);"
    , "    await attempt('indexedDB', () => new Promise((ok, no) => { const r = indexedDB.open('x'); r.onsuccess = () => ok('opened'); r.onerror = () => no(r.error); }));"
    , "    await attempt('caches', () => caches.keys().then(() => 'opened'));"
    , "    await attempt('fetchWiki', () => fetch(WIKI + '/api/v1/session', { credentials: 'include' }).then((r) => 'status ' + r.status));"
    , "    await attempt('fetchAllowed', () => fetch(ALLOWED + '/allowed').then((r) => r.text()));"
    , "    await attempt('fetchOther', () => fetch(OTHER + '/other').then((r) => 'status ' + r.status));"
    , "    await attempt('socket', () => new Promise((ok) => { const w = new WebSocket(OTHER.replace('http', 'ws') + '/socket'); w.onopen = () => ok('open'); w.onerror = () => ok('error'); }));"
    , "    await attempt('importScripts', () => { importScripts(OTHER + '/script.js'); return 'imported'; });"
    , "    postMessage({ type: 'report', report: out });"
    , "  } else if (data && data.type === 'loop') {"
    , "    postMessage({ type: 'looping' });"
    , "    for (;;) {}"
    , "  }"
    , "};"
    ]
  where
    quoted t = "'" <> t <> "'"

-- | The example plugin installed for everyone over the API, as an
-- administrator would install it (examples/plugins/doubled-words), and a
-- plugin whose diagnostics never return; the editor of a page of the
-- walk's, where the first marks "the the" and the second is stopped
-- while the page goes on; and a frame of the plugin's address, loaded in
-- a page the service worker controls, which the worker does not keep.
inTheEditor :: Smoke -> Playwright -> IO ()
inTheEditor smoke pw = do
  s <- adminSession smoke
  _ <- asAdmin smoke "PUT" "external-plugins/policy" (Just (object ["policy" .= ("wiki" :: Text)])) >>= expectStatus 200 "turning external plugins on"
  manifest <- ByteString.readFile "../examples/plugins/doubled-words/manifest.json"
  code <- ByteString.readFile "../examples/plugins/doubled-words/doubled-words.js"
  installed <-
    upload smoke.wiki s "external-plugins/wiki" "bundle" "doubled-words.js" "text/javascript" code [("manifest", Text.decodeUtf8 manifest)]
      >>= expectStatus 200 "installing the example plugin"
      >>= apiJson
  bundle <- text (installed .! "bundle")
  let looping =
        object
          [ "manifest" .= (1 :: Int)
          , "id" .= ("looping" :: Text)
          , "name" .= ("Looping" :: Text)
          , "version" .= ("1.0.0" :: Text)
          , "api" .= (1 :: Int)
          , "contributes" .= object ["diagnostics" .= object []]
          , "permissions" .= ["document.read" :: Text]
          ]
  _ <-
    upload smoke.wiki s "external-plugins/wiki" "bundle" "looping.js" "text/javascript" "wikiPlugin.plugin({ diagnostics: () => { for (;;) {} } });\n" [("manifest", Text.decodeUtf8 (LazyByteString.toStrict (Aeson.encode looping)))]
      >>= expectStatus 200 "installing the plugin that loops"
  let frameAddress = "/plugin-frame/wiki/doubled-words/" <> bundle <> "/diagnostics"
      editorAt = "/s/smoke-space/p/first-page/edit/"
      stopped page = locator page "#app .editor-plugin-stopped"
      -- What the editor marks, as CodeMirror's host keeps it
      -- (jsbits/editor.js's wiki_editor_problems).
      marked page = evaluate page "JSON.stringify([...document.querySelectorAll('#app *')].map((e) => e.wikiProblems).find((p) => p) ?? [])" :: IO Text
  withSessionContext pw serverContext smoke.origin (Just s) $ \page -> do
    visit page (address smoke editorAt)
    editorReady page
    -- The plugins' frames are loaded and started, and the one that loops
    -- does not answer: the page waits on its pause, which ends with the
    -- plugin stopped.
    waitQuiet page
    hasText (stopped page) "The plugin Looping was stopped: it did not answer within 10 seconds"
    -- The page went on all the while, and the frame that loops is gone:
    -- one frame is left, the example's.
    evaluate page "1 + 1" `shouldReturn` (2 :: Int)
    count (locator page "#app .editor-plugin-frames iframe") `shouldReturn` 1
    -- The author writes a word twice; once the text is checked and the
    -- example has answered, it is marked, with the plugin's name.
    marked page >>= \m -> expect (not ("Doubled word" `Text.isInfixOf` m)) ("marked before the typing: " <> show m)
    pressOn (editorText page) "Control+End"
    typeText (editorText page) "\n\nSaid the the cat.\n"
    waitQuiet page
    marked page >>= \m -> expect ("Doubled words: Doubled word: the" `Text.isInfixOf` m) ("the editor's marks: " <> show m)
    hasText (locator page "#app .editor-problems") "problem"
    nothingWrongExcept fromThePlugin serviceWorkerInSandbox page

  -- The service worker leaves the plugin's frame to the network: a frame
  -- of its address, loaded in a page the worker controls, is not kept,
  -- while a page of the wiki's loaded the same way after it is. What the
  -- worker keeps is its own: a condition.
  withSessionContext pw defaultContextOptions {acceptDialogs = True} smoke.origin (Just s) $ \page -> do
    visit page (address smoke "/s/smoke-space/")
    holdsInPage page "navigator.serviceWorker.controller !== null"
    let framed (path :: Text) = evaluate page ("(() => { const f = document.createElement('iframe'); f.src = " <> Text.pack (show path) <> "; document.body.appendChild(f); return null; })()") :: IO Value
        keptPages = "(async () => { const found = []; for (const name of await caches.keys()) { if (!name.startsWith('wiki-pages ')) continue; const cache = await caches.open(name); for (const r of await cache.keys()) found.push(new URL(r.url).pathname); } return found; })()"
    _ <- framed frameAddress
    _ <- framed "/s/smoke-space/p/first-page/"
    holdsInPage page (keptPages <> ".then((ps) => ps.includes('/s/smoke-space/p/first-page/'))")
    kept :: [Text] <- evaluate page keptPages
    expect (not (any ("/plugin-frame/" `Text.isPrefixOf`) kept)) ("the worker kept the plugin's frame: " <> show kept)
    nothingWrongExcept fromThePlugin serviceWorkerInSandbox page

  _ <- asAdmin smoke "DELETE" "external-plugins/wiki/doubled-words" Nothing >>= expectStatus 204 "removing the example plugin"
  _ <- asAdmin smoke "DELETE" "external-plugins/wiki/looping" Nothing >>= expectStatus 204 "removing the plugin that loops"
  _ <- asAdmin smoke "PUT" "external-plugins/policy" (Just (object ["policy" .= ("off" :: Text)])) >>= expectStatus 200 "turning external plugins off"
  pure ()

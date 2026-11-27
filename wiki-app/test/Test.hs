module Main (main) where

import Protolude
import Test.ApiKeys qualified as ApiKeys
import Test.ApiKeysDom qualified as ApiKeysDom
import Test.Assistant qualified as Assistant
import Test.AssistantMove qualified as AssistantMove
import Test.AssistantSettings qualified as AssistantSettings
import Test.Backend.Http qualified as Http
import Test.Backend.Mem qualified as Mem
import Test.Backend.Persist qualified as Persist
import Test.Bell qualified as Bell
import Test.BellDom qualified as BellDom
import Test.Blog qualified as Blog
import Test.CalendarDom qualified as CalendarDom
import Test.CommentsDom qualified as CommentsDom
import Test.Complete qualified as Complete
import Test.DayPageDom qualified as DayPageDom
import Test.Deck qualified as Deck
import Test.DeckDom qualified as DeckDom
import Test.DecryptedExport qualified as DecryptedExport
import Test.Demo qualified as Demo
import Test.EditorDom qualified as EditorDom
import Test.Elsewhere qualified as Elsewhere
import Test.Entries qualified as Entries
import Test.Export qualified as Export
import Test.Favicon qualified as Favicon
import Test.Guide qualified as Guide
import Test.Help qualified as Help
import Test.Hspec (around_, parallel)
import Test.Hspec.Runner (Config (..), defaultConfig, hspecWith)
import Test.Hydrate qualified as Hydrate
import Test.Import qualified as Import
import Test.Islands qualified as Islands
import Test.Latest qualified as Latest
import Test.Licence qualified as Licence
import Test.Link qualified as Link
import Test.Live qualified as Live
import Test.LivePreviewDom qualified as LivePreviewDom
import Test.Manifest qualified as Manifest
import Test.Passwords qualified as Passwords
import Test.PluginImports qualified as PluginImports
import Test.PluginSecrets qualified as PluginSecrets
import Test.Plugins qualified as Plugins
import Test.PluginsDom qualified as PluginsDom
import Test.PluginsExternal qualified as PluginsExternal
import Test.PluginsExternalDom qualified as PluginsExternalDom
import Test.PluginsSettings qualified as PluginsSettings
import Test.Policy qualified as Policy
import Test.Preview qualified as Preview
import Test.Problems qualified as Problems
import Test.Readme qualified as Readme
import Test.Rect qualified as Rect
import Test.Route qualified as Route
import Test.SecretField qualified as SecretField
import Test.SecretFieldDom qualified as SecretFieldDom
import Test.Server qualified as Server
import Test.SettingsDom qualified as SettingsDom
import Test.Shell qualified as Shell
import Test.Threads qualified as Threads
import Test.Ticking qualified as Ticking
import Test.Users qualified as Users

main :: IO ()
main = do
  -- Most components use detached documents and independent backends. The
  -- page-size and assistant examples need the process's native stores alone.
  -- The views mounted on the in-memory DOM ("Test.Harness.MemDom") each
  -- have a page of their own, and run beside everything else.
  permits <- newQSemN maxBound
  let holding n = bracket_ (waitQSemN permits n) (signalQSemN permits n)
  hspecWith defaultConfig {configConcurrentJobs = Just 4} . parallel $ do
    around_ (holding 1) $ do
      Islands.spec
      Mem.spec
      Http.spec
      Persist.spec
      Demo.spec
      Entries.spec
      Guide.spec
      Help.spec
      Export.spec
      DecryptedExport.spec
      Favicon.spec
      Route.spec
      Rect.spec
      Link.spec
      Live.spec
      Ticking.spec
      Bell.spec
      ApiKeys.spec
      ApiKeysDom.spec
      Threads.spec
      BellDom.spec
      CommentsDom.spec
      CalendarDom.spec
      DayPageDom.spec
      DeckDom.spec
      EditorDom.spec
      LivePreviewDom.spec
      Users.spec
      SettingsDom.spec
      Deck.spec
      Latest.spec
      Shell.spec
      Server.spec
      Passwords.spec
      Blog.spec
      Hydrate.spec
      Import.spec
      Manifest.spec
      Policy.spec
      Preview.spec
      Problems.spec
      Plugins.spec
      PluginsSettings.spec
      PluginsDom.spec
      PluginsExternal.spec
      PluginsExternalDom.spec
      PluginImports.spec
      PluginSecrets.spec
      AssistantMove.spec
      AssistantSettings.spec
      SecretField.spec
      SecretFieldDom.spec
      Complete.spec
      Elsewhere.spec
      Licence.spec
      Readme.spec
    around_ (holding maxBound) $ do
      Hydrate.storageSpec
      Assistant.spec
      PluginSecrets.storageSpec

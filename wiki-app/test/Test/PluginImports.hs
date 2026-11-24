-- | The rule of the plugins' imports (decision 69 of docs/PLAN.md): a
-- built-in plugin imports the plugins' interface ("Wiki.App.Plugin") and
-- nothing else of the application's inside. Its modules are
-- @Wiki.App.Plugin.\<Name\>@ and the modules under it. They may import the
-- interface and its capabilities (@Wiki.App.Plugin.Capability.*@), their
-- own plugin's modules, wiki-core (@Wiki.Core.*@) and packages that are
-- not the wiki's; not another module of the application, of wiki-api or
-- of the server, nor another plugin. The modules of the host under
-- @Wiki.App.Plugin@ ('hostModules') are not plugins. The interface imports no plugin, and of the rest of the
-- application only the registry does.
--
-- The rule reads the sources' import lines, as fourmolu writes them: one
-- import a line, starting with @import@.
module Test.PluginImports (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Protolude
import System.Directory (doesDirectoryExist, listDirectory)
import Test.Hspec

-- | The application's sources, from the package's directory, where the
-- suite runs.
sources :: FilePath
sources = "src"

-- | The modules under @Wiki.App.Plugin@ that are the host's, not a
-- plugin's.
hostModules :: [Text]
hostModules = ["Registry", "Host", "Capability"]

-- | The plugin a module belongs to, if it belongs to one: @Errors@ for
-- @Wiki.App.Plugin.Errors@ and the modules under it.
pluginOf :: Text -> Maybe Text
pluginOf name = case T.splitOn "." <$> T.stripPrefix "Wiki.App.Plugin." name of
  Just (plugin : _) | plugin `notElem` hostModules -> Just plugin
  _ -> Nothing

-- | The imports of a module that the rule forbids it.
forbidden :: Text -> [Text] -> [Text]
forbidden name = filter (not . allowed)
  where
    isPlugin = isJust . pluginOf
    allowed imported = case pluginOf name of
      Just plugin ->
        imported == "Wiki.App.Plugin"
          || "Wiki.App.Plugin.Capability." `T.isPrefixOf` imported
          || imported == "Wiki.App.Plugin." <> plugin
          || ("Wiki.App.Plugin." <> plugin <> ".") `T.isPrefixOf` imported
          || "Wiki.Core." `T.isPrefixOf` imported
          || not ("Wiki." `T.isPrefixOf` imported)
      Nothing
        | name == "Wiki.App.Plugin.Registry" -> True
        | otherwise -> not (isPlugin imported)

-- | The modules a source imports.
importsOf :: Text -> [Text]
importsOf source =
  [ name
  | line <- T.lines source
  , Just rest <- [T.stripPrefix "import " line]
  , name : _ <- [filter (`notElem` ["qualified", "safe", "{-# SOURCE #-}"]) (T.words rest)]
  ]

-- | Every Haskell source under a directory of the sources, given by the
-- module names it holds (@["Wiki"]@), with its module's name and its path.
modulesUnder :: [Text] -> IO [(Text, FilePath)]
modulesUnder parts = do
  let dir = toS (T.intercalate "/" (toS sources : parts))
  entries <- sort <$> listDirectory dir
  fmap concat . for entries $ \entry -> do
    let path = dir <> "/" <> entry
    isDir <- doesDirectoryExist path
    case T.stripSuffix ".hs" (toS entry) of
      _ | isDir -> modulesUnder (parts <> [toS entry])
      Just name -> pure [(T.intercalate "." (parts <> [name]), path)]
      Nothing -> pure []

spec :: Spec
spec = describe "the plugins' imports" $ do
  it "forbid a plugin another module of the application, wiki-api, the server, and another plugin" $ do
    forbidden "Wiki.App.Plugin.Errors" ["Protolude", "Wiki.App.Plugin", "Wiki.App.Plugin.Errors.Parts", "Wiki.Core.Markdown.Problems", "Wiki.App.View.Editor", "Wiki.App.Plugin.Registry", "Wiki.App.Plugin.Other", "Wiki.Api.Types", "Wiki.Server.Api", "Wiki.App.Plugins"]
      `shouldBe` ["Wiki.App.View.Editor", "Wiki.App.Plugin.Registry", "Wiki.App.Plugin.Other", "Wiki.Api.Types", "Wiki.Server.Api", "Wiki.App.Plugins"]
    forbidden "Wiki.App.Plugin.Errors.Parts" ["Wiki.App.Plugin.Errors", "Wiki.App.Plugin.Assistant", "Wiki.App.Plugin.Capability.Assistant", "Wiki.App.Plugin.Host"] `shouldBe` ["Wiki.App.Plugin.Assistant", "Wiki.App.Plugin.Host"]

  it "forbid the interface and the host a plugin, but the registry" $ do
    forbidden "Wiki.App.Plugin" ["Wiki.App.Plugin.Errors", "Wiki.App.Plugin.Registry", "Wiki.App.View.Common"] `shouldBe` ["Wiki.App.Plugin.Errors"]
    forbidden "Wiki.App.View.Editor" ["Wiki.App.Plugin", "Wiki.App.Plugin.Registry", "Wiki.App.Plugin.Errors"] `shouldBe` ["Wiki.App.Plugin.Errors"]
    forbidden "Wiki.App.Plugin.Registry" ["Wiki.App.Plugin.Errors", "Wiki.App.View.Common"] `shouldBe` []
    forbidden "Wiki.App.Plugin.Capability.Assistant" ["Wiki.App.Assistant", "Wiki.App.Plugin.Assistant"] `shouldBe` ["Wiki.App.Plugin.Assistant"]

  it "read a module's imports from its source" $
    importsOf "module A where\n\nimport Data.Text qualified as T\nimport qualified Data.Map as M\nimport Protolude hiding (State)\n-- import Not.This\n"
      `shouldBe` ["Data.Text", "Data.Map", "Protolude"]

  it "hold for every module of the application" $ do
    modules <- modulesUnder ["Wiki"]
    -- The scan reads the plugins themselves, not an empty directory.
    let expected = ["Wiki.App.Plugin", "Wiki.App.Plugin.Capability.Assistant", "Wiki.App.Plugin.Errors", "Wiki.App.Plugin.Host", "Wiki.App.Plugin.Registry"]
    sort [name | (name, _) <- modules, name `elem` expected] `shouldBe` sort expected
    broken <- for modules $ \(name, path) -> do
      source <- decodeUtf8 <$> BS.readFile path
      pure [(name, imported) | imported <- forbidden name (importsOf source)]
    concat broken `shouldBe` []

{-# OPTIONS_GHC -Wno-orphans #-}

-- | The OpenAPI schemas of the types of "Wiki.Core" that answers of the API
-- for programs carry ("Wiki.Server.ProgramApi"): a page's kind, and the
-- marked pieces of a search's hit. They are orphans, kept here and only
-- here: "Wiki.Core" is compiled for the browser too, and the OpenAPI
-- packages are the server's alone. Each says what its 'ToJSON' writes, and
-- the test of the schemas checks it.
module Wiki.Server.ProgramApi.Schemas () where

import Control.Lens ((?~))
import Data.OpenApi (NamedSchema (..), OpenApiItems (..), OpenApiType (..), ToParamSchema (..), ToSchema (..), defaultSchemaOptions, genericDeclareNamedSchema)
import Data.OpenApi qualified as O
import Data.Text qualified as T
import Protolude
import Wiki.Core.PageKind (PageKind, knownKinds, pageKindName)
import Wiki.Core.Search (Piece, Snippet)

-- | A kind is its name: one of the kinds a page is made with, or a kind
-- this server does not know, by its name as stored, which is why the
-- schema names the known ones in its description rather than as an
-- enumeration.
instance ToSchema PageKind where
  declareNamedSchema _ =
    pure . NamedSchema (Just "PageKind") $
      toParamSchema (Proxy @PageKind)
        & O.description ?~ ("A page's kind: " <> T.intercalate " or " (map pageKindName knownKinds) <> ", or a kind another version of the wiki made, by its name.")

-- | A kind in a parameter: its name.
instance ToParamSchema PageKind where
  toParamSchema _ = mempty & O.type_ ?~ OpenApiString

-- | A snippet is its pieces, in order.
instance ToSchema Snippet where
  declareNamedSchema _ = do
    piece <- O.declareSchemaRef (Proxy @Piece)
    pure . NamedSchema (Just "Snippet") $
      mempty
        & O.type_ ?~ OpenApiArray
        & O.items ?~ OpenApiItemsObject piece
        & O.description ?~ "Pieces of text that, joined in order, are the text; a piece with match true is a word of the query."

instance ToSchema Piece where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

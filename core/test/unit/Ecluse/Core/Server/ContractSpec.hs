-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.ContractSpec (spec) where

import Autodocodec (JSONCodec, object, pureCodec)
import Data.List (lookup)
import Network.HTTP.Types (hContentType, status200, status404, statusCode)
import Network.Wai qualified as Wai
import Test.Hspec

import Ecluse.Core.Server.Contract (
    BodySchema (SchemaDocumented, SchemaEmpty, SchemaJson, SchemaOpaque, SchemaPassthrough, SchemaText),
    ResponseContract,
    ResponseDoc (responseBodySchema),
    ResponseValue,
    bodilessContract,
    documentedJsonContract,
    jsonContract,
    mediaContract,
    mediaJsonContract,
    responseDocs,
    responseToWai,
    responseValue,
 )

spec :: Spec
spec = do
    describe "mediaContract" $ do
        it "serves the media type its schema names" $
            servedType (mediaContract status200 "The operator's help message." (SchemaText "text/plain"))
                `shouldBe` Just "text/plain"

        it "documents the same media type it serves" $
            documentedTypes (mediaContract status200 "An index." (SchemaDocumented simpleIndexType "SimpleIndex"))
                `shouldBe` [Just simpleIndexType]

        it "emits no body for a schema that names no media type" $
            servedType (mediaContract status404 "Nothing to say." SchemaEmpty) `shouldBe` Nothing

        it "keeps the exact status it is given" $
            statusOf (mediaContract status404 "Not found." (SchemaText "text/plain")) `shouldBe` 404

    describe "the application/json specialisations" $ do
        it "documentedJsonContract serves and documents application/json" $ do
            let contract = documentedJsonContract status200 "A document." "SomeSchema"
            servedType contract `shouldBe` Just "application/json"
            documentedTypes contract `shouldBe` [Just "application/json"]

        it "jsonContract serves and documents application/json" $ do
            let contract = jsonContract status200 "An empty object." emptyObjectCodec
            servedTypeOf contract (responseValue [] ()) `shouldBe` Just "application/json"
            documentedTypes contract `shouldBe` [Just "application/json"]

    describe "mediaJsonContract" $
        it "encodes through its codec under the media type it is given" $ do
            let contract = mediaJsonContract simpleIndexType status200 "An index." emptyObjectCodec
            servedTypeOf contract (responseValue [] ()) `shouldBe` Just simpleIndexType
            documentedTypes contract `shouldBe` [Just simpleIndexType]

    describe "bodilessContract" $
        it "drops every documented body while keeping the served media type" $ do
            let contract = bodilessContract (mediaContract status200 "An index." (SchemaText "text/plain"))
            documentedTypes contract `shouldBe` [Nothing]
            servedType contract `shouldBe` Just "text/plain"

-- The PyPI Simple index media type, as a media type this module has no opinion about.
simpleIndexType :: ByteString
simpleIndexType = "application/vnd.pypi.simple.v1+json"

emptyObjectCodec :: JSONCodec ()
emptyObjectCodec = object "EmptyObject" (pureCodec ())

-- The @Content-Type@ a contract puts on the wire for a caller-assembled body.
servedType :: ResponseContract (ResponseValue LByteString) -> Maybe ByteString
servedType contract = servedTypeOf contract (responseValue [] "the body bytes")

servedTypeOf :: ResponseContract response -> response -> Maybe ByteString
servedTypeOf contract value = lookup hContentType (Wai.responseHeaders (responseToWai contract value))

statusOf :: ResponseContract (ResponseValue LByteString) -> Int
statusOf contract =
    statusCode (Wai.responseStatus (responseToWai contract (responseValue [] "the body bytes")))

-- The media type each documented response carries, absent for a body with no media type.
documentedTypes :: ResponseContract response -> [Maybe ByteString]
documentedTypes = map (mediaOf . responseBodySchema) . responseDocs
  where
    mediaOf = \case
        SchemaEmpty -> Nothing
        SchemaOpaque media -> Just media
        SchemaText media -> Just media
        SchemaJson media _ -> Just media
        SchemaDocumented media _ -> Just media
        SchemaPassthrough -> Nothing

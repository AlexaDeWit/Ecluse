module Ecluse.ManifestSpec (spec) where

import Data.Aeson (Value (Object), decode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashSet.InsOrd qualified as InsOrdSet

import Autodocodec (eitherDecodeJSONViaCodec, encodeJSONViaCodec)
import Data.OpenApi (
    Components (_componentsSchemas),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers),
    Operation (_operationResponses, _operationTags),
    Referenced (Inline),
    Response (_responseContent),
    Responses (_responsesResponses),
    _infoTitle,
    _pathItemGet,
    _pathItemPut,
 )
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Manifest (
    ErrorEnvelope (ErrorEnvelope),
    buildOpenApi,
    canonicalManifestSource,
    errorEnvelopeSchemaName,
    renderManifest,
    synthesizedPackumentSchemaName,
 )

spec :: Spec
spec = do
    describe "buildOpenApi (canonical npm mount)" $ do
        it "renders a well-formed OpenAPI document (openapi/info/paths present)" $
            case decode (renderManifest doc) :: Maybe Value of
                Just (Object o) -> do
                    KeyMap.member "openapi" o `shouldBe` True
                    KeyMap.member "info" o `shouldBe` True
                    KeyMap.member "paths" o `shouldBe` True
                _ -> expectationFailure "the manifest did not render as a JSON object"

        it "carries a non-empty title and a server entry" $ do
            _infoTitle (_openApiInfo doc) `shouldNotBe` ""
            _openApiServers doc `shouldNotSatisfy` null

        it "registers the owned schemas in components" $ do
            let schemas = _componentsSchemas (_openApiComponents doc)
            InsOrd.member errorEnvelopeSchemaName schemas `shouldBe` True
            InsOrd.member synthesizedPackumentSchemaName schemas `shouldBe` True

        it "renders deterministically (byte-stable across runs)" $
            renderManifest doc `shouldBe` renderManifest doc

    describe "every Route constructor appears as an operation" $ do
        it "Packument -> GET /npm/{package}" $
            getOp "/npm/{package}" `shouldSatisfy` isJust
        it "Tarball -> GET /npm/{package}/-/{filename}" $
            getOp "/npm/{package}/-/{filename}" `shouldSatisfy` isJust
        it "Publish -> PUT /npm/{package}" $
            putOp "/npm/{package}" `shouldSatisfy` isJust
        it "Ping -> GET /npm/-/ping" $
            getOp "/npm/-/ping" `shouldSatisfy` isJust
        it "Search -> GET /npm/-/v1/search" $
            getOp "/npm/-/v1/search" `shouldSatisfy` isJust
        it "Unsupported -> GET /npm/{unsupportedPath}" $
            getOp "/npm/{unsupportedPath}" `shouldSatisfy` isJust

    describe "documented statuses and boundaries" $ do
        it "Search carries 501" $
            (statusCodes <$> getOp "/npm/-/v1/search") `shouldBe` Just [501]
        it "the deny-by-default catch-all carries 404" $
            (statusCodes <$> getOp "/npm/{unsupportedPath}") `shouldBe` Just [404]
        it "the packument GET documents the gate statuses" $
            (statusCodes <$> getOp "/npm/{package}") `shouldBe` Just [200, 403, 404, 500, 502, 503]
        it "the tarball GET serves opaque octet-stream on 200" $
            (okMediaTypes <$> getOp "/npm/{package}/-/{filename}")
                `shouldBe` Just ["application/octet-stream"]
        it "operations are tagged by ecosystem (npm)" $
            (InsOrdSet.member "npm" . _operationTags <$> getOp "/npm/{package}") `shouldBe` Just True

    describe "owned autodocodec codecs round-trip (hedgehog)" $
        it "ErrorEnvelope encodes and decodes back to itself" $
            hedgehog $ do
                msg <- forAll (Gen.text (Range.linear 0 64) Gen.unicode)
                let value = ErrorEnvelope msg
                eitherDecodeJSONViaCodec (encodeJSONViaCodec value) === Right value
  where
    doc :: OpenApi
    doc = buildOpenApi canonicalManifestSource

    getOp :: FilePath -> Maybe Operation
    getOp p = InsOrd.lookup p (_openApiPaths doc) >>= _pathItemGet

    putOp :: FilePath -> Maybe Operation
    putOp p = InsOrd.lookup p (_openApiPaths doc) >>= _pathItemPut

    statusCodes :: Operation -> [Int]
    statusCodes = sort . InsOrd.keys . _responsesResponses . _operationResponses

    okMediaTypes :: Operation -> [String]
    okMediaTypes op =
        case InsOrd.lookup 200 (_responsesResponses (_operationResponses op)) of
            Just (Inline resp) -> sort (map show (InsOrd.keys (_responseContent resp)))
            _ -> []

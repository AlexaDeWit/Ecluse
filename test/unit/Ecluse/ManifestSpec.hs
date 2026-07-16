-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ManifestSpec (spec) where

import Data.Aeson (Value (Object), decode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.Text qualified as T

import Data.OpenApi (
    Components (_componentsSchemas),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers),
    Operation (_operationResponses, _operationTags),
    PathItem,
    Referenced (Inline),
    Response (_responseContent),
    Responses (_responsesResponses),
    _infoTitle,
    _pathItemGet,
    _pathItemPut,
 )
import Network.HTTP.Types.Method (StdMethod (GET, PUT), renderStdMethod)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm), prefixFor)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (serveRoutes), RegistryAdapter (adapterServe))
import Ecluse.Core.Registry.Npm.Route (npmRoutes)
import Ecluse.Core.Server.Route (matchRoute)
import Ecluse.Core.Server.RouteSpec (RouteSpec (rsMethod))
import Ecluse.Manifest (
    buildOpenApi,
    canonicalManifestSource,
    publishDocumentSchemaName,
    renderManifest,
    routePathKey,
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

        it "registers the owned hand-authored schemas in components" $ do
            let schemas = _componentsSchemas (_openApiComponents doc)
            InsOrd.member synthesizedPackumentSchemaName schemas `shouldBe` True
            InsOrd.member publishDocumentSchemaName schemas `shouldBe` True

        it "emits top-level keys in sorted order (deterministic key ordering)" $ do
            let rendered = decodeUtf8 (renderManifest doc) :: Text
                topKeys = ["components", "info", "openapi", "paths", "servers", "tags"]
                marker k = "\n  \"" <> k <> "\":"
                offsetOf k = T.length (fst (T.breakOn (marker k) rendered))
            -- every top-level key is present at the document root ...
            all (\k -> marker k `T.isInfixOf` rendered) topKeys `shouldBe` True
            -- ... and they appear in ascending order, so confCompare = compare is in
            -- effect and the output does not depend on insertion order.
            map offsetOf topKeys `shouldBe` sort (map offsetOf topKeys)

    -- The manifest's specs are the documentation projection ('specOf') of the very
    -- RoutePatterns the classifier routes on, so their paths and methods agree by
    -- construction. These assert that projection reaches the rendered document: every
    -- route is emitted under its declared method at its rendered template, and a path
    -- claimed by no route still denies by default.
    describe "documented routes correspond to the live classifier" $ do
        it "the npm mount exposes a route grammar" $
            null npmSpecs `shouldBe` False

        it "each documented route is rendered under its declared method" $
            for_ npmSpecs $ \rs ->
                (lookupPath rs >>= operationForMethod (rsMethod rs)) `shouldSatisfy` isJust

        it "the manifest's path keys are exactly the rendered route templates" $
            sort (InsOrd.keys (_openApiPaths doc))
                `shouldBe` sort (ordNub (map renderedKey npmSpecs))

        it "a path claimed by no documented route denies by default" $
            -- The catch-all the manifest documents is real: no route in the table claims
            -- this path, so the router answers it with the deny-by-default 404.
            isJust (matchRoute npmRoutes (renderStdMethod GET) ["not", "a", "known", "route"]) `shouldBe` False

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
  where
    doc :: OpenApi
    doc = buildOpenApi canonicalManifestSource

    getOp :: FilePath -> Maybe Operation
    getOp p = InsOrd.lookup p (_openApiPaths doc) >>= _pathItemGet

    -- The npm mount's declarative route grammar, resolved through the same adapter
    -- registry the composition root mounts and the manifest renders.
    npmSpecs :: [RouteSpec]
    npmSpecs = maybe [] (toList . serveRoutes . adapterServe) (adapterFor Npm)

    -- A spec's manifest path key, rendered the way the manifest renders it.
    renderedKey :: RouteSpec -> FilePath
    renderedKey = toString . routePathKey (prefixFor Npm)

    lookupPath :: RouteSpec -> Maybe PathItem
    lookupPath rs = InsOrd.lookup (renderedKey rs) (_openApiPaths doc)

    operationForMethod :: StdMethod -> PathItem -> Maybe Operation
    operationForMethod = \case
        GET -> _pathItemGet
        PUT -> _pathItemPut
        _ -> const Nothing

    statusCodes :: Operation -> [Int]
    statusCodes = sort . InsOrd.keys . _responsesResponses . _operationResponses

    okMediaTypes :: Operation -> [String]
    okMediaTypes op =
        case InsOrd.lookup 200 (_responsesResponses (_operationResponses op)) of
            Just (Inline resp) -> sort (map show (InsOrd.keys (_responseContent resp)))
            _ -> []

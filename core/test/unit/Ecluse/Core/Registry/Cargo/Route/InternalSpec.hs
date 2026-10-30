-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Native response contracts, sparse captures and archive identity validation.
module Ecluse.Core.Registry.Cargo.Route.InternalSpec (spec) where

import Data.Aeson (Value, eitherDecode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types (Method, ResponseHeaders, Status, methodDelete, methodGet, methodHead, methodPut, status200, status400, status401, status403, status404, status405, status500)
import Network.Wai (Request (pathInfo, requestHeaders, requestMethod), responseHeaders, responseStatus)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), defaultRequest, runSession, setPath, srequest)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Package (mkPackageName, renderPackageName)
import Ecluse.Core.Registry.Cargo.Adapter (cargoAdapter)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublish)
import Ecluse.Core.Registry.Cargo.Route.Internal (artifactContract, artifactCoordinate, artifactReplies, cargoRouter, cargoRoutes, indexContract, indexReplies, takeIndexPath)
import Ecluse.Core.Registry.Npm.Route.Internal (npmPackumentContract, npmPackumentReplies, npmTarballContract, npmTarballReplies)
import Ecluse.Core.Security (Limits (maxPublishRequestBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned, withServeAdmission)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), PublishDeps (..), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime (..), runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (packumentForbidden, packumentForbiddenText, packumentInternal, packumentNotFound, packumentPrivateRefused, packumentUnauthorised))
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (tarballError, tarballPrivateRefused))
import Ecluse.Core.Server.Response (RejectReason (Unavailable), Rejection (Rejection), ServeDecision (Reject), Transience (WontResolve), mkHelpMessage, mkRefusal)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Rules.Support (now)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoUpload)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture)
import Ecluse.Test.Registry.Cargo.Reference (referenceDiscovery)
import Ecluse.Test.Server.Handler (responseBytes, testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (serveDepsFor, withPrivateBaseUrl)
import Ecluse.Test.Stub (Captured (..), Stub, allCaptured, stubBaseUrl, withRoutedStub, withStub)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (selfBaseUrlOf)

spec :: Spec
spec = do
    describe "bound native routes" $ do
        it "answers local 404s under the removed private prefix" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                budget <- newByteAdmission deadlines (128 * 1024 * 1024)
                let binding = routeBinding (routeDeps stub) (Just (routePublication stub budget))
                for_ [(methodGet, ["private", "config.json"]), (methodGet, ["private", "unknown"]), (methodPut, ["private", "api", "v1", "crates", "new"])] $ \(method, path) -> do
                    response <- captureCargo runtime binding method path ""
                    simpleStatus response `shouldBe` status404
                    simpleBody response `shouldBe` ""
                allCaptured stub `shouldReturn` []
        it "answers discovery locally without a private origin" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                response <- captureCargo runtime (routeBinding (routeDeps stub) Nothing) methodGet ["config.json"] ""
                simpleStatus response `shouldBe` status200
                (eitherDecode (simpleBody response) :: Either String Value)
                    `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= False])
                lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                allCaptured stub `shouldReturn` []
        it "requires the edge credential before discovery and advertises authentication after it agrees" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                let refused = (routeDeps stub){pdInboundToken = Just (mkSecret "required-edge-token")}
                    admitted = refused{pdInboundToken = Just (mkSecret "cargo-token")}
                response <- captureCargo runtime (routeBinding refused Nothing) methodGet ["config.json"] ""
                simpleStatus response `shouldBe` status401
                simpleBody response `shouldBe` "authentication required"
                lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Just "Cargo"
                lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                control <- captureCargo runtime (routeBinding admitted Nothing) methodGet ["config.json"] ""
                simpleStatus control `shouldBe` status200
                (eitherDecode (simpleBody control) :: Either String Value)
                    `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= True])
                allCaptured stub `shouldReturn` []
        it "answers discovery locally with authentication required whatever the private origin would answer" $
            withSweeper $ \deadlines ->
                for_ [status200, status401, status403, status500] $ \upstream ->
                    withRoutedStub (\captured -> if upstream == status200 then discoveryReply captured else (upstream, [], "upstream refusal")) $ \stub -> do
                        runtime <- testServeRuntime deadlines noopMetricsPort
                        response <- captureCargo runtime (routeBinding (privateDeps stub) Nothing) methodGet ["config.json"] ""
                        simpleStatus response `shouldBe` status200
                        lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                        (eitherDecode (simpleBody response) :: Either String Value)
                            `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= True])
                        allCaptured stub `shouldReturn` []
        it "answers discovery while listing admission is held" $
            withSweeper $ \deadlines -> withRoutedStub discoveryReply $ \stub -> do
                admission <- newServeAdmissionTuned deadlines 1 0 0
                runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
                held <- withServeAdmission noopMetricsPort admission (captureCargo runtime (routeBinding (privateDeps stub) Nothing) methodGet ["config.json"] "")
                response <- expectRight held
                simpleStatus response `shouldBe` status200
                lookup "Retry-After" (simpleHeaders response) `shouldBe` Nothing
                allCaptured stub `shouldReturn` []
        it "keeps publication disabled until configured and preserves local and upstream native responses" $
            withSweeper $ \deadlines -> do
                release <- cargoPublicationFixture
                bytes <- fromStrict <$> expectRight (cargoUpload release)
                for_ [(status200, "{\"warnings\":{}}"), (status401, "native credential required")] $ \(status, responseBody) ->
                    withRoutedStub (\captured -> if capMethod captured == "PUT" then (status, [], responseBody) else discoveryReply captured) $ \stub -> do
                        runtime <- testServeRuntime deadlines noopMetricsPort
                        budget <- newByteAdmission deadlines (128 * 1024 * 1024)
                        let deps = routeDeps stub
                            disabled = routeBinding deps Nothing
                            enabled = routeBinding deps (Just (routePublication stub budget))
                            path = ["api", "v1", "crates", "new"]
                            advertised binding = do
                                discovery <- captureCargo runtime binding methodGet ["config.json"] ""
                                (_, api, _, _) <- expectRight (referenceDiscovery (toStrict (simpleBody discovery)))
                                pure api
                        advertised disabled `shouldReturn` Nothing
                        advertised enabled `shouldReturn` Just "https://proxy.test/cargo"
                        absent <- captureCargo runtime disabled methodPut path bytes
                        simpleStatus absent `shouldBe` status405
                        lookup "Allow" (simpleHeaders absent) `shouldBe` Just "PUT"
                        simpleBody absent `shouldBe` "publishing is not enabled on this view"
                        malformed <- captureCargo runtime enabled methodPut path "broken"
                        simpleStatus malformed `shouldBe` status400
                        simpleBody malformed `shouldBe` "Cargo upload section exceeds its byte bound"
                        lookup "Cache-Control" (simpleHeaders malformed) `shouldBe` Just "no-store"
                        unauthorised <- captureCargo runtime (enabled{bindingPublishDeps = Just ((routePublication stub budget){pubInboundToken = Just (mkSecret "required-edge-token")})}) methodPut path bytes
                        simpleStatus unauthorised `shouldBe` status401
                        simpleBody unauthorised `shouldBe` "authentication required"
                        lookup "WWW-Authenticate" (simpleHeaders unauthorised) `shouldBe` Just "Cargo"
                        allCaptured stub `shouldReturn` []
                        response <- captureCargo runtime enabled methodPut path bytes
                        simpleStatus response `shouldBe` status
                        simpleBody response `shouldBe` responseBody
                        lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` ("Cargo" <$ guard (status == status401))
                        captures <- allCaptured stub
                        map capMethod captures `shouldBe` ["GET", "PUT"]
        it "treats truncated archive paths and invalid native versions as route misses" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                let deps = routeDeps stub
                for_ [["crates"], ["crates", "fixture"], ["crates", "fixture", "1.0.0"], ["crates", "fixture", "invalid", "fixture-invalid.crate"]] $ \path -> do
                    response <- captureCargo runtime (routeBinding deps Nothing) methodGet path ""
                    simpleStatus response `shouldBe` status404
                allCaptured stub `shouldReturn` []
        it "answers native yank and unyank writes locally without reaching the registry" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                budget <- newByteAdmission deadlines (128 * 1024 * 1024)
                let deps = routeDeps stub
                    enabled = routeBinding deps (Just (routePublication stub budget))
                for_ [(methodDelete, "yank"), (methodPut, "unyank")] $ \(method, operation) -> do
                    response <- captureCargo runtime enabled method ["api", "v1", "crates", "fixture", "1.0.0", operation] ""
                    simpleStatus response `shouldBe` status404
                    simpleBody response `shouldBe` ""
                allCaptured stub `shouldReturn` []

    describe "native private refusals" $
        for_ [(401, status401, Just "Cargo"), (403, status403, Nothing)] $ \(code, status, challenge) ->
            it ("retains private HTTP " <> show code <> " in sparse and archive responses") $ do
                let headers = [("Cache-Control", "no-store")]
                    refusal = mkRefusal Nothing "private access was refused"
                    metadata = responseToWai indexContract (packumentPrivateRefused indexReplies code headers refusal)
                    archive = responseToWai artifactContract (tarballPrivateRefused artifactReplies code headers refusal)
                for_ [metadata, archive] $ \response -> do
                    responseStatus response `shouldBe` status
                    lookup "WWW-Authenticate" (responseHeaders response) `shouldBe` challenge
                    lookup "Cache-Control" (responseHeaders response) `shouldBe` Just "no-store"
                    responseBytes response `shouldReturn` ""

    describe "native public refusal diagnostics" $ do
        it "renders the exact policy reason and appends configured help once" $
            for_ [(Nothing, "example@1.0.0 was denied by execution: runs code on install"), (Just (mkHelpMessage "Contact the platform team."), "example@1.0.0 was denied by execution: runs code on install Contact the platform team.")] $ \(help, expected) -> do
                let refusal = mkRefusal help "example@1.0.0 was denied by execution: runs code on install"
                    responses =
                        [ responseToWai indexContract (packumentForbidden indexReplies [] refusal)
                        , responseToWai artifactContract (tarballError artifactReplies status403 [] refusal)
                        ]
                for_ responses $ \response -> do
                    responseStatus response `shouldBe` status403
                    responseBytes response `shouldReturn` expected
        it "keeps detailed public rejection messages in their existing order" $ do
            let decisions = [Reject (Rejection (Unavailable WontResolve) reason) | reason <- ["example@1.0.0 could not be evaluated", "example@2.0.0 could not be evaluated"]]
            packumentForbiddenText indexReplies decisions
                `shouldBe` "example@1.0.0 could not be evaluated; example@2.0.0 could not be evaluated"
        it "renders the exact public evaluation failure without operator-only evidence details" $ do
            let refusal = mkRefusal Nothing "example@1.0.0 could not be evaluated: the rule could not be evaluated"
                responses =
                    [ responseToWai indexContract (packumentInternal indexReplies [] refusal)
                    , responseToWai artifactContract (tarballError artifactReplies status500 [] refusal)
                    ]
            for_ responses $ \response -> do
                responseStatus response `shouldBe` status500
                responseBytes response `shouldReturn` "example@1.0.0 could not be evaluated: the rule could not be evaluated"
        it "keeps inbound authentication, access refusals and misses quiet" $ do
            let refusal = mkRefusal Nothing "request-specific private information"
                responses =
                    [ responseToWai indexContract (packumentUnauthorised indexReplies [] refusal)
                    , responseToWai artifactContract (tarballError artifactReplies status401 [] refusal)
                    , responseToWai indexContract (packumentNotFound indexReplies [] refusal)
                    , responseToWai artifactContract (tarballError artifactReplies status404 [] refusal)
                    , responseToWai indexContract (packumentPrivateRefused indexReplies 403 [] refusal)
                    , responseToWai artifactContract (tarballPrivateRefused artifactReplies 403 [] refusal)
                    ]
            for_ responses $ \response -> responseBytes response `shouldReturn` ""
        it "keeps private credential failures limited to configured operator help" $
            for_ [401, 403] $ \code -> do
                let refusal = mkRefusal (Just (mkHelpMessage "Contact the platform team.")) "request-specific private information"
                    responses =
                        [ responseToWai indexContract (packumentPrivateRefused indexReplies code [] refusal)
                        , responseToWai artifactContract (tarballPrivateRefused artifactReplies code [] refusal)
                        ]
                for_ responses $ \response -> responseBytes response `shouldReturn` "Contact the platform team."

    describe "legacy private refusals" $
        for_ [401, 403] $ \code ->
            it ("keeps npm HTTP " <> show code <> " mapped to 403") $ do
                let refusal = mkRefusal Nothing "private access was refused"
                    responses =
                        [ responseToWai npmPackumentContract (packumentPrivateRefused npmPackumentReplies code [] refusal)
                        , responseToWai npmTarballContract (tarballPrivateRefused npmTarballReplies code [] refusal)
                        ]
                for_ responses $ \response -> do
                    responseStatus response `shouldBe` status403
                    lookup "WWW-Authenticate" (responseHeaders response) `shouldBe` Nothing

    describe "takeIndexPath" $ do
        it "checks native prefixes for every name length" $
            for_ [["1", "a"], ["2", "ab"], ["3", "a", "abc"], ["ab", "cd", "abcde"]] $ \path ->
                fmap (first renderPackageName) (takeIndexPath path) `shouldBe` ((,[]) <$> listToMaybe (reverse path))
        it "rejects wrong prefixes, upper-case lookups and excess segments" $
            for_ [["ab", "xx", "abcde"], ["AB", "cd", "abcde"], ["ab", "cd", "Abcde"], ["3", "a", "abc", "extra"], [".."], []] $ \path ->
                takeIndexPath path `shouldBe` Nothing

    describe "artifactCoordinate" $ do
        let name = mkPackageName Cargo Nothing "my-crate"
            version = mkVersion Cargo "1.2.3-beta.4"
        it "preserves the exact native filename" $
            fmap unFilename (artifactCoordinate name version "my-crate-1.2.3-beta.4.crate")
                `shouldBe` Just "my-crate-1.2.3-beta.4.crate"
        it "rejects disagreement in any archive claim" $
            for_ ["other-1.2.3-beta.4.crate", "my-crate-1.2.3.crate", "My-crate-1.2.3-beta.4.crate", "../my-crate-1.2.3-beta.4.crate"] $ \file ->
                artifactCoordinate name version file `shouldBe` Nothing
        it "rejects a foreign ecosystem with the same rendered name" $
            artifactCoordinate (mkPackageName Npm Nothing "my-crate") version "my-crate-1.2.3-beta.4.crate" `shouldBe` Nothing

    describe "native route selection" $ do
        for_ [methodGet, methodHead] $ \method -> do
            it "claims discovery and sparse records" $ do
                claimed method ["config.json"] `shouldBe` Just (RouteName "discovery")
                claimed method ["se", "rd", "serde"] `shouldBe` Just (RouteName "index")
                claimed method ["private", "config.json"] `shouldBe` Nothing
                claimed method ["private", "se", "rd", "serde"] `shouldBe` Nothing
            it "claims the explicit archive route" $
                claimed method ["crates", "my-crate", "1.2.3-beta.4", "my-crate-1.2.3-beta.4.crate"]
                    `shouldBe` Just (RouteName "archive")
        it "claims the native publication path and no private prefix" $ do
            claimed methodPut ["api", "v1", "crates", "new"] `shouldBe` Just (RouteName "publish")
            claimed methodPut ["private", "api", "v1", "crates", "new"] `shouldBe` Nothing

claimed :: Method -> [Text] -> Maybe RouteName
claimed method path = fmap (routeName . fst) (matchRoute cargoRoutes method [] path)

factors :: ChargeFactors
factors = ChargeFactors 1000 1000 1000

routeDeps :: Stub -> PackumentDeps
routeDeps stub =
    (serveDepsFor (cargoAdapter factors) Nothing (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pure now))
        { pdMountBaseUrl = "https://proxy.test/cargo"
        }

-- The route deps with a private origin on the same stub, which discovery never reads.
privateDeps :: Stub -> PackumentDeps
privateDeps stub = withPrivateBaseUrl (Just (loopbackRegistryUrl (stubBaseUrl stub <> "/private-index"))) (routeDeps stub)

routeBinding :: PackumentDeps -> Maybe PublishDeps -> MountBinding
routeBinding deps publication =
    MountBinding
        { bindingPrefix = "cargo" :| []
        , bindingRouter = cargoRouter
        , bindingCredential = cargoCredential
        , bindingPackumentDeps = deps
        , bindingPublishDeps = publication
        }

captureCargo :: ServeRuntime -> MountBinding -> Method -> [Text] -> LByteString -> IO SResponse
captureCargo runtime binding method path body = do
    logEnv <- newTestLogEnv
    let request = setPath defaultRequest{requestMethod = method, requestHeaders = [("Authorization", "cargo-token"), ("Content-Type", "application/octet-stream")]} ("/" <> encodeUtf8 (T.intercalate "/" (toList (bindingPrefix binding) <> path)))
    runSession (srequest (SRequest request body)) $ \incoming respond ->
        case bindingRouter binding (requestMethod incoming) (requestHeaders incoming) (drop (length (bindingPrefix binding)) (pathInfo incoming)) of
            RouteAction contract action -> case action of
                AnswerLocally value -> respond (responseToWai contract value)
                AnswerRefusal answer -> respond (responseToWai contract (answer (pdHelp (bindingPackumentDeps binding))))
                RunPipeline _ run -> runHandler logEnv mempty (RequestCtx runtime binding) (run incoming (respond . responseToWai contract))

routePublication :: Stub -> ByteAdmission -> PublishDeps
routePublication stub budget =
    PublishDeps
        { pubReadUrl = loopbackRegistryUrl (stubBaseUrl stub <> "/index")
        , pubTargetUrl = loopbackRegistryUrl (stubBaseUrl stub <> "/registry")
        , pubAllowed = \name -> projectName "fixture" == Right name
        , pubStaticCredential = Nothing
        , pubInboundToken = Nothing
        , pubMaxRequestBytes = maxPublishRequestBytes defaultLimits
        , pubLimits = defaultLimits
        , pubBodyBudget = budget
        , pubHelp = Nothing
        , pubProjectName = rightToMaybe . projectName
        , pubAdapter = cargoPublish factors
        }

discoveryReply :: Captured -> (Status, ResponseHeaders, LByteString)
discoveryReply captured =
    let root = selfBaseUrlOf (capHeaders captured)
     in (status200, [], Aeson.encode (object ["dl" .= (root <> "/crates/{crate}/{version}/download"), "api" .= (root <> "/registry"), "auth-required" .= True]))

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
import Network.HTTP.Types (Method, ResponseHeaders, Status, methodDelete, methodGet, methodHead, methodPut, status200, status400, status401, status403, status404, status405, status500, status503)
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
import Ecluse.Core.Registry.Cargo.Route.Internal (CargoView (..), artifactContract, artifactCoordinate, artifactReplies, cargoRouter, cargoRoutes, indexContract, indexReplies, takeIndexPath)
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
import Ecluse.Test.Server.Handler (responseBytes, testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Stub (Captured (..), Stub, allCaptured, headerValue, stubBaseUrl, withRoutedStub, withStub)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (selfBaseUrlOf)

spec :: Spec
spec = do
    describe "bound native routes" $ do
        it "returns local 404s for an absent private binding and unknown private paths" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                let deps = routeDeps stub
                for_ [(Nothing, ["private", "config.json"]), (Just deps, ["private", "unknown"])] $ \(private, path) -> do
                    response <- captureCargo runtime (routeBinding deps private Nothing) methodGet path ""
                    simpleStatus response `shouldBe` status404
                    simpleBody response `shouldBe` ""
                allCaptured stub `shouldReturn` []
        it "keeps public discovery local and reports the missing private origin" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                let deps = routeDeps stub
                    binding = routeBinding deps (Just deps) Nothing
                public <- captureCargo runtime binding methodGet ["config.json"] ""
                simpleStatus public `shouldBe` status200
                (eitherDecode (simpleBody public) :: Either String Value)
                    `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= False])
                private <- captureCargo runtime binding methodGet ["private", "config.json"] ""
                simpleStatus private `shouldBe` status503
                simpleBody private `shouldBe` "private registry discovery is unavailable"
                lookup "Cache-Control" (simpleHeaders private) `shouldBe` Just "no-store"
                allCaptured stub `shouldReturn` []
        it "requires the edge credential before discovery and advertises authentication after it agrees" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                let refused = (routeDeps stub){pdInboundToken = Just (mkSecret "required-edge-token")}
                    admitted = refused{pdInboundToken = Just (mkSecret "cargo-token")}
                response <- captureCargo runtime (routeBinding refused Nothing Nothing) methodGet ["config.json"] ""
                simpleStatus response `shouldBe` status401
                simpleBody response `shouldBe` "authentication required"
                lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Just "Cargo"
                lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                control <- captureCargo runtime (routeBinding admitted Nothing Nothing) methodGet ["config.json"] ""
                simpleStatus control `shouldBe` status200
                (eitherDecode (simpleBody control) :: Either String Value)
                    `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= True])
                allCaptured stub `shouldReturn` []
        it "binds the private origin and preserves trusted discovery refusals" $
            withSweeper $ \deadlines ->
                for_ [(status200, status200, Nothing, Nothing), (status401, status401, Just "Cargo", Just "authentication required"), (status403, status403, Nothing, Just "private registry refused access"), (status500, status503, Nothing, Just "registry discovery is unavailable")] $ \(upstream, expected, challenge, body) ->
                    withRoutedStub (\captured -> if upstream == status200 then discoveryReply captured else (upstream, [], "upstream refusal")) $ \stub -> do
                        runtime <- testServeRuntime deadlines noopMetricsPort
                        let public = routeDeps stub
                            private = (serveDepsFor (cargoAdapter factors) (Just (loopbackRegistryUrl (stubBaseUrl stub <> "/private-index"))) (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pure now)){pdMountBaseUrl = "https://proxy.test/cargo/private"}
                        response <- captureCargo runtime (routeBinding public (Just private) Nothing) methodGet ["private", "config.json"] ""
                        simpleStatus response `shouldBe` expected
                        lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` challenge
                        lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                        for_ body $ \message -> simpleBody response `shouldBe` message
                        when (upstream == status200) $
                            (eitherDecode (simpleBody response) :: Either String Value)
                                `shouldBe` Right (object ["dl" .= ("https://proxy.test/cargo/private/crates/{crate}/{version}/{crate}-{version}.crate" :: Text), "auth-required" .= True])
                        captures <- allCaptured stub
                        map capPath captures `shouldBe` ["/private-index/config.json"]
                        map (headerValue "Authorization") captures `shouldBe` [Just "cargo-token"]
        it "refuses trusted discovery while admission is held and releases capacity for the valid control" $
            withSweeper $ \deadlines -> withRoutedStub discoveryReply $ \stub -> do
                admission <- newServeAdmissionTuned deadlines 1 0 0
                runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
                let public = routeDeps stub
                    private = (serveDepsFor (cargoAdapter factors) (Just (loopbackRegistryUrl (stubBaseUrl stub))) (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pure now)){pdMountBaseUrl = "https://proxy.test/cargo/private"}
                    binding = routeBinding public (Just private) Nothing
                held <- withServeAdmission noopMetricsPort admission (captureCargo runtime binding methodGet ["private", "config.json"] "")
                response <- expectRight held
                simpleStatus response `shouldBe` status503
                lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
                lookup "Retry-After" (simpleHeaders response) `shouldBe` Just "1"
                simpleBody response `shouldBe` "server is busy; retry later"
                allCaptured stub `shouldReturn` []
                control <- captureCargo runtime binding methodGet ["private", "config.json"] ""
                simpleStatus control `shouldBe` status200
                length <$> allCaptured stub `shouldReturn` 1
        it "keeps publication disabled until configured and preserves local and upstream native responses" $
            withSweeper $ \deadlines -> do
                release <- cargoPublicationFixture
                bytes <- fromStrict <$> expectRight (cargoUpload release)
                for_ [(status200, "{\"warnings\":{}}"), (status401, "native credential required")] $ \(status, responseBody) ->
                    withRoutedStub (\captured -> if capMethod captured == "PUT" then (status, [], responseBody) else discoveryReply captured) $ \stub -> do
                        runtime <- testServeRuntime deadlines noopMetricsPort
                        budget <- newByteAdmission deadlines (128 * 1024 * 1024)
                        let deps = routeDeps stub
                            disabled = routeBinding deps (Just deps) Nothing
                            enabled = routeBinding deps (Just deps) (Just (routePublication stub budget))
                            path = ["private", "api", "v1", "crates", "new"]
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
                    response <- captureCargo runtime (routeBinding deps (Just deps) Nothing) methodGet path ""
                    simpleStatus response `shouldBe` status404
                allCaptured stub `shouldReturn` []
        it "answers native yank and unyank writes locally without reaching the registry" $
            withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
                runtime <- testServeRuntime deadlines noopMetricsPort
                budget <- newByteAdmission deadlines (128 * 1024 * 1024)
                let deps = routeDeps stub
                    enabled = routeBinding deps (Just deps) (Just (routePublication stub budget))
                for_ [(methodDelete, "yank"), (methodPut, "unyank")] $ \(method, operation) -> do
                    response <- captureCargo runtime enabled method ["private", "api", "v1", "crates", "fixture", "1.0.0", operation] ""
                    simpleStatus response `shouldBe` status404
                    simpleBody response `shouldBe` ""
                allCaptured stub `shouldReturn` []

    describe "native private refusals" $
        for_ [(401, status401, Just "Cargo"), (403, status403, Nothing)] $ \(code, status, challenge) ->
            it ("retains private HTTP " <> show code <> " in sparse and archive responses") $ for_ [PublicReplacement, PrivateRegistry] $ \view -> do
                let headers = [("Cache-Control", "no-store")]
                    refusal = mkRefusal Nothing "private access was refused"
                    metadata = responseToWai indexContract (packumentPrivateRefused (indexReplies view) code headers refusal)
                    archive = responseToWai artifactContract (tarballPrivateRefused (artifactReplies view) code headers refusal)
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
                        [ responseToWai indexContract (packumentForbidden (indexReplies PublicReplacement) [] refusal)
                        , responseToWai artifactContract (tarballError (artifactReplies PublicReplacement) status403 [] refusal)
                        ]
                for_ responses $ \response -> do
                    responseStatus response `shouldBe` status403
                    responseBytes response `shouldReturn` expected
        it "keeps detailed public rejection messages in their existing order" $ do
            let decisions = [Reject (Rejection (Unavailable WontResolve) reason) | reason <- ["example@1.0.0 could not be evaluated", "example@2.0.0 could not be evaluated"]]
            packumentForbiddenText (indexReplies PublicReplacement) decisions
                `shouldBe` "example@1.0.0 could not be evaluated; example@2.0.0 could not be evaluated"
        it "renders the exact public evaluation failure without operator-only evidence details" $ do
            let refusal = mkRefusal Nothing "example@1.0.0 could not be evaluated: the rule could not be evaluated"
                responses =
                    [ responseToWai indexContract (packumentInternal (indexReplies PublicReplacement) [] refusal)
                    , responseToWai artifactContract (tarballError (artifactReplies PublicReplacement) status500 [] refusal)
                    ]
            for_ responses $ \response -> do
                responseStatus response `shouldBe` status500
                responseBytes response `shouldReturn` "example@1.0.0 could not be evaluated: the rule could not be evaluated"
        it "keeps inbound authentication and first-party exclusion quiet in both views" $
            for_ [PublicReplacement, PrivateRegistry] $ \view -> do
                let refusal = mkRefusal Nothing "request-specific private information"
                    responses =
                        [ responseToWai indexContract (packumentUnauthorised (indexReplies view) [] refusal)
                        , responseToWai artifactContract (tarballError (artifactReplies view) status401 [] refusal)
                        , responseToWai indexContract (packumentNotFound (indexReplies view) [] refusal)
                        , responseToWai artifactContract (tarballError (artifactReplies view) status404 [] refusal)
                        , responseToWai indexContract (packumentForbidden (indexReplies PrivateRegistry) [] refusal)
                        , responseToWai artifactContract (tarballError (artifactReplies PrivateRegistry) status403 [] refusal)
                        ]
                for_ responses $ \response -> responseBytes response `shouldReturn` ""
        it "keeps private credential failures limited to configured operator help in both views" $
            for_ [PublicReplacement, PrivateRegistry] $ \view ->
                for_ [401, 403] $ \code -> do
                    let refusal = mkRefusal (Just (mkHelpMessage "Contact the platform team.")) "request-specific private information"
                        responses =
                            [ responseToWai indexContract (packumentPrivateRefused (indexReplies view) code [] refusal)
                            , responseToWai artifactContract (tarballPrivateRefused (artifactReplies view) code [] refusal)
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
            it "claims each view's discovery and sparse records" $ do
                claimed method ["config.json"] `shouldBe` Just (RouteName "public.discovery")
                claimed method ["private", "config.json"] `shouldBe` Just (RouteName "private.discovery")
                claimed method ["se", "rd", "serde"] `shouldBe` Just (RouteName "public.index")
                claimed method ["private", "se", "rd", "serde"] `shouldBe` Just (RouteName "private.index")
            it "claims the explicit archive route" $
                claimed method ["crates", "my-crate", "1.2.3-beta.4", "my-crate-1.2.3-beta.4.crate"]
                    `shouldBe` Just (RouteName "public.archive")
        it "refuses the public publication path" $
            claimed methodPut ["api", "v1", "crates", "new"] `shouldBe` Nothing
        it "claims only the native private publication path" $
            claimed methodPut ["private", "api", "v1", "crates", "new"] `shouldBe` Just (RouteName "private.publish")

claimed :: Method -> [Text] -> Maybe RouteName
claimed method path = fmap (routeName . fst) (matchRoute cargoRoutes method [] path)

factors :: ChargeFactors
factors = ChargeFactors 1000 1000 1000

routeDeps :: Stub -> PackumentDeps
routeDeps stub =
    (serveDepsFor (cargoAdapter factors) Nothing (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pure now))
        { pdMountBaseUrl = "https://proxy.test/cargo"
        }

routeBinding :: PackumentDeps -> Maybe PackumentDeps -> Maybe PublishDeps -> MountBinding
routeBinding deps private publication =
    MountBinding
        { bindingPrefix = "cargo" :| []
        , bindingRouter = cargoRouter private
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

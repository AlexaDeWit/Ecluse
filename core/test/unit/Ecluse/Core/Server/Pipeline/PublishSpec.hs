-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Pipeline.PublishSpec (spec) where

import Network.HTTP.Types (status201, status400, status401)
import Network.Wai (Request (requestHeaders, requestMethod), defaultRequest, pathInfo)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), runSession, setPath, srequest)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Registry (PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (..), PublishRequestBody (..))
import Ecluse.Core.Registry.Npm.Adapter (npmPublish)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocToken))
import Ecluse.Core.Registry.Publish (PublishCodec (pcPrepareTarget, pcTargetReadPermille))
import Ecluse.Core.Registry.RubyGems.Credential (rubyOtpHeaders, rubyPublishCredential, rubyPublishExtras, rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, meterSnapshot)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (MountBinding (..), PublishDeps (..), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime (srMemoryMeter), runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

spec :: Spec
spec = do
    describe "publication-specific credentials at a protected mount" $ do
        it "accepts the native raw key while the read mapping remains Basic" $
            withFixture Nothing $ \send observed -> do
                response <- send [("Authorization", "edge-key"), ("OTP", "123456")]
                simpleStatus response `shouldBe` status201
                readIORef observed `shouldReturn` [(Just (bareCredential (mkSecret "edge-key")), [("OTP", "123456")], "unchanged-native-body")]
        it "refuses unauthenticated requests before validation or relay" $
            withFixture Nothing $ \send observed -> do
                response <- send []
                simpleStatus response `shouldBe` status401
                readIORef observed `shouldReturn` []
        it "rejects duplicate OTP headers before relay" $
            withFixture Nothing $ \send observed -> do
                response <- send [("Authorization", "edge-key"), ("OTP", "one"), ("OTP", "two")]
                simpleStatus response `shouldBe` status400
                readIORef observed `shouldReturn` []
        it "substitutes a configured native target key only after edge authentication" $
            withFixture (Just (bareCredential (mkSecret "Bearer target-key"))) $ \send observed -> do
                response <- send [("Authorization", "edge-key")]
                simpleStatus response `shouldBe` status201
                readIORef observed `shouldReturn` [(Just (bareCredential (mkSecret "Bearer target-key")), [], "unchanged-native-body")]
    describe "target discovery at a protected mount" $
        it "holds the sum of its full-read chunks, not only the largest" $ do
            held <- newIORef Nothing
            withCodecFixture (discoveryPaying held) Nothing $ \send _ -> do
                response <- send [("Authorization", "edge-key")]
                simpleStatus response `shouldBe` status201
            readIORef held >>= (`shouldSatisfy` maybe False (>= scaleCharge discoveryPermille (3 * discoveryChunk)))
  where
    withFixture = withCodecFixture (const id)
    withCodecFixture adjust static use = withSweeper $ \deadlines -> do
        runtime <- testServeRuntime deadlines noopMetricsPort
        logEnv <- newTestLogEnv
        budget <- newByteAdmission deadlines (1024 * 1024)
        observed <- newIORef []
        let publish =
                npmPublish
                    { publishCredential = rubyPublishCredential
                    , publishRequestExtras = rubyPublishExtras
                    , publishUploadClaims = \_ _ _ _ -> pure (Right ["widget"])
                    , publishCodec = adjust (srMemoryMeter runtime) (publishCodec npmPublish)
                    , publishRelay = \origin _ body -> do
                        modifyIORef' observed (<> [(ocToken origin, rubyOtpHeaders (publishBodyExtras body), publishBodyBytes body)])
                        pure (Right (PublishRelayResponse 201 "created"))
                    }
            deps =
                PublishDeps
                    { pubReadUrl = loopbackRegistryUrl "http://localhost:1"
                    , pubTargetUrl = loopbackRegistryUrl "http://localhost:1"
                    , pubAllowed = const True
                    , pubStaticCredential = static
                    , pubInboundToken = Just (mkSecret "edge-key")
                    , pubMaxRequestBytes = 1024
                    , pubLimits = defaultLimits
                    , pubBodyBudget = budget
                    , pubHelp = Nothing
                    , pubProjectName = rightToMaybe . projectName
                    , pubAdapter = publish
                    }
            binding = MountBinding ("ruby" :| []) rubyGemsRouter rubyReadCredential inertPackumentDeps (Just deps)
            send headers = runSession (srequest (SRequest (setPath defaultRequest{requestMethod = "POST", requestHeaders = headers} "/api/v1/gems") "unchanged-native-body")) $ \incoming respond ->
                case bindingRouter binding (requestMethod incoming) (requestHeaders incoming) (pathInfo incoming) of
                    RouteAction routeContract action -> case action of
                        AnswerLocally answer -> respond (responseToWai routeContract answer)
                        AnswerRefusal answer -> respond (responseToWai routeContract (answer Nothing))
                        RunPipeline _ run -> runHandler logEnv mempty (RequestCtx runtime binding) (run incoming (respond . responseToWai routeContract))
        use send observed

-- Pay three equal chunks, each large enough that one chunk and the entry step stay under their sum.
discoveryPaying :: IORef (Maybe Int) -> MemoryMeter -> PublishCodec -> PublishCodec
discoveryPaying held meter codec =
    codec
        { pcTargetReadPermille = discoveryPermille
        , pcPrepareTarget = \_ origin target -> do
            replicateM_ 3 (ocChargeFullRead origin discoveryChunk)
            meterSnapshot meter >>= writeIORef held . Just . snChargedBytes
            pure (Right target)
        }

discoveryChunk :: Int
discoveryChunk = 8 * 1024 * 1024

discoveryPermille :: Int
discoveryPermille = 1500

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Relay classification and private refusal preserve response commitment.
module Ecluse.Core.Server.Pipeline.Tarball.RelaySpec (spec) where

import Data.Text qualified as T
import Ecluse.Core.Server.Stream (RelayResponder (..))
import Network.HTTP.Types (hContentLength, hContentType, status200, status304, status401, status403, status404, status503)
import Test.Hspec

import Ecluse.Core.Server.Pipeline.Tarball.Relay (RelayVerdict (RelayedArtifact, RelayedNonSuccess, RelayedOddShape), acceptPrivateArtifact, observeRelayAnomaly, privateArtifactResponder, relayVerdict)
import Ecluse.Test.Log (moduleOfLineWhere, runJsonLog)
import Ecluse.Test.Package (npmVersion, thingName)
import Ecluse.Test.Port (noopMetricsPort)

spec :: Spec
spec = do
    privateResponderSpec
    publicVerdictSpec
    anomalyLogSpec

privateResponderSpec :: Spec
privateResponderSpec = describe "private artifact relay" $ do
    it "accepts only artifacts, validators, and access refusals" $
        map acceptPrivateArtifact [status200, status304, status401, status403, status404, status503]
            `shouldBe` [True, True, True, True, False, False]

    for_ [status401, status403] $ \status ->
        it ("replaces " <> show status <> " without touching upstream responders or body") $ do
            let upstream :: RelayResponder Text
                upstream = RelayResponder (\_ _ _ -> fail "stream responder called") (\_ _ -> fail "empty responder called")
                responder = privateArtifactResponder (pure . Left) Right upstream
            relayStreamResponse responder status [] (\_ _ -> fail "body read") `shouldReturn` Left status
            relayEmptyResponse responder status [] `shouldReturn` Left status

    it "retains admitted status and validators for stream and empty responses" $ do
        let headers = [("ETag", "version-one")]
            upstream = RelayResponder (\status hs _ -> pure (status, hs)) (curry pure)
            responder = privateArtifactResponder (\_ -> fail "refusal called") id upstream
        relayStreamResponse responder status200 headers (\_ _ -> pure ()) `shouldReturn` (status200, headers)
        relayEmptyResponse responder status304 headers `shouldReturn` (status304, headers)

publicVerdictSpec :: Spec
publicVerdictSpec = describe "relayVerdict (the public relay judged from status and headers alone)" $ do
    it "a success with a binary content type is the artifact" $
        relayVerdict status200 [(hContentType, "application/octet-stream")] `shouldBe` RelayedArtifact

    it "a success with no content type is taken as the artifact (a tripwire, not a validator)" $
        relayVerdict status200 [] `shouldBe` RelayedArtifact

    it "a 304 is a clean pass-through (the relayed validators matched)" $
        relayVerdict status304 [(hContentType, "text/html")] `shouldBe` RelayedArtifact

    it "a success with a textual content type is the odd shape, its reason carrying the type" $ do
        case relayVerdict status200 [(hContentType, "text/html; charset=utf-8")] of
            RelayedOddShape reason -> reason `shouldSatisfy` T.isInfixOf "text/html"
            other -> expectationFailure ("expected the odd shape, got " <> show other)
        case relayVerdict status200 [(hContentType, "application/json")] of
            RelayedOddShape _ -> pass
            other -> expectationFailure ("expected the odd shape, got " <> show other)

    it "a non-success is the relayed non-success carrying its status" $ do
        relayVerdict status404 [] `shouldBe` RelayedNonSuccess status404
        relayVerdict status503 [(hContentType, "application/octet-stream")] `shouldBe` RelayedNonSuccess status503

    it "the declared length never enters the judgement (npm's declared size is the unpacked tree, not the transfer)" $
        relayVerdict status200 [(hContentLength, "12345"), (hContentType, "application/octet-stream")] `shouldBe` RelayedArtifact

anomalyLogSpec :: Spec
anomalyLogSpec = describe "observeRelayAnomaly" $
    it "logs each anomaly under the pipeline's module filter key" $ do
        logged <- runJsonLog $ for_ [RelayedNonSuccess status503, RelayedOddShape "text/html"] (observeRelayAnomaly noopMetricsPort thingName (npmVersion "1.0.0"))
        moduleOfLineWhere ["msg"] "the public upstream answered a non-success: HTTP 503" logged `shouldBe` Just "Ecluse.Server.Pipeline"
        moduleOfLineWhere ["msg"] "the public upstream answered a success that does not look like the admitted artifact: text/html" logged
            `shouldBe` Just "Ecluse.Server.Pipeline"

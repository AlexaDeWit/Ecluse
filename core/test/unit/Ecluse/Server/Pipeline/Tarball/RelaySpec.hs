-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.Pipeline.Tarball.RelaySpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types (hContentLength, hContentType, status200, status304, status404, status503)
import Test.Hspec

import Ecluse.Core.Server.Pipeline.Tarball.Relay (RelayVerdict (RelayedArtifact, RelayedNonSuccess, RelayedOddShape), relayVerdict)

spec :: Spec
spec = describe "relayVerdict (the public relay judged from status and headers alone)" $ do
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

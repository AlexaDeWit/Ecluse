-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.BootErrorSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..), renderBootError)
import Ecluse.Config (PolicyError (UnknownRuleType))
import Ecluse.Core.Ecosystem (Ecosystem (..))

spec :: Spec
spec = describe "renderBootError" $
    it "renders each boot-error kind as a distinct operator-facing line" $ do
        renderBootError (PolicyBootError (UnknownRuleType "x" "Y")) `shouldSatisfy` infixed "unknown type"
        renderBootError (MissingAdapter PyPI) `shouldSatisfy` infixed "no adapter"
        renderBootError (UnresolvedCredential Npm)
            `shouldSatisfy` infixed "mirror-write credential"
        renderBootError (QueueProviderUnavailable "pubsub") `shouldSatisfy` infixed "not available"
        renderBootError QueueRegionMissing `shouldSatisfy` infixed "AWS_REGION"
        renderBootError QueueRegionMissing `shouldSatisfy` infixed "AWS_ENDPOINT_URL_SQS"
        -- The unrecognised-shape render names the value and the accepted forms.
        renderBootError (QueueUrlUnrecognised "https://queue.example.test/q")
            `shouldSatisfy` infixed "https://queue.example.test/q"
        renderBootError (QueueUrlUnrecognised "x") `shouldSatisfy` infixed "projects/{project}/topics/{topic}"
        renderBootError (QueueEndpointMalformed "x") `shouldSatisfy` infixed "endpoint"
        -- The mint-failure render makes the transient-vs-permanent distinction legible.
        renderBootError (CodeArtifactMintFailed "AccessDenied") `shouldSatisfy` infixed "transient"
        renderBootError (PublishAllowMissing Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW"
        renderBootError (PublishStaticCredentialNeedsEdge Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

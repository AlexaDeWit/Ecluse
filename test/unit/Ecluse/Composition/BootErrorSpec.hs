module Ecluse.Composition.BootErrorSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..), renderBootError)
import Ecluse.Config (
    CredentialBackend (..),
    PolicyError (UnknownRuleType),
    QueueBackend (..),
 )
import Ecluse.Core.Ecosystem (Ecosystem (..))

spec :: Spec
spec = describe "renderBootError" $
    it "renders each boot-error kind as a distinct operator-facing line" $ do
        renderBootError (PolicyBootError (UnknownRuleType "x" "Y")) `shouldSatisfy` infixed "unknown type"
        renderBootError (MissingAdapter PyPI) `shouldSatisfy` infixed "no adapter"
        renderBootError (UnresolvedCredential Npm CodeArtifactCredential)
            `shouldSatisfy` infixed "not initialised"
        renderBootError (QueueProviderUnavailable PubSubQueue) `shouldSatisfy` infixed "not available"
        renderBootError QueueRegionMissing `shouldSatisfy` infixed "AWS_REGION"
        renderBootError (QueueUrlMissing SqsQueue) `shouldSatisfy` infixed "ECLUSE_QUEUE_URL"
        renderBootError (QueueEndpointMalformed "x") `shouldSatisfy` infixed "endpoint"
        renderBootError (MirrorCredentialProviderUnavailable AdcCredential)
            `shouldSatisfy` infixed "adc"
        renderBootError (CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN"
        renderBootError (CodeArtifactConfigInvalid "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id")
            `shouldSatisfy` infixed "12-digit"
        -- The mint-failure render makes the transient-vs-permanent distinction legible.
        renderBootError (CodeArtifactMintFailed "AccessDenied") `shouldSatisfy` infixed "transient"
        renderBootError (PublishScopesMissing Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES"
        renderBootError (PublishStaticCredentialNeedsEdge Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

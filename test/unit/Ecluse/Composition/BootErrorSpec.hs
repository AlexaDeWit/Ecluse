-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.BootErrorSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..), renderBootError, renderBootErrors)
import Ecluse.Config (PolicyError (UnknownRuleType))
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))

spec :: Spec
spec = do
    renderBootErrorSpec
    renderBootErrorsSpec

renderBootErrorsSpec :: Spec
renderBootErrorsSpec =
    describe "renderBootErrors" $
        it "reports every aggregated refusal, one line each, in the order it received them" $
            -- One failed launch shows every problem an operator must fix, so no refusal may be
            -- dropped and none may be reordered ahead of another.
            renderBootErrors [MissingAdapter PyPI, MirrorRoleWithoutMirroring]
                `shouldBe` renderBootError (MissingAdapter PyPI)
                    <> "\n"
                    <> renderBootError MirrorRoleWithoutMirroring
                    <> "\n"

renderBootErrorSpec :: Spec
renderBootErrorSpec = describe "renderBootError" $
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
        -- Each endpoint-override render names its variable, never the value it refused.
        renderBootError (QueueEndpointMalformed (mkSecret "http://u:tok@h"))
            `shouldSatisfy` infixed "AWS_ENDPOINT_URL_SQS"
        renderBootError (QueueEndpointMalformed (mkSecret "http://u:tok@h"))
            `shouldNotSatisfy` infixed "tok"
        renderBootError (AwsEndpointMalformed (mkSecret "http://u:tok@h"))
            `shouldSatisfy` infixed "AWS_ENDPOINT_URL"
        renderBootError (AwsEndpointMalformed (mkSecret "http://u:tok@h"))
            `shouldNotSatisfy` infixed "tok"
        renderBootError (AwsEndpointMalformed (mkSecret "http://u:tok@h"))
            `shouldNotSatisfy` infixed "AWS_ENDPOINT_URL_SQS"
        -- The mint-failure render tells a transient failure from a permanent one.
        renderBootError (CodeArtifactMintFailed "AccessDenied") `shouldSatisfy` infixed "transient"
        renderBootError (PublicationAllowMissing Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW"
        renderBootError (PublishStaticCredentialNeedsEdge Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN"
        -- Each collision render names the offending key, the mount it collided with, and why.
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET (https://store.example.test) shares a host with ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM"
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "publisher's own credential"
        -- Both host-rule refusals name the change that satisfies the rule, as the store-rule
        -- refusals do, so a warned operator never learns more than a refused one.
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "point it at a registry that shares a host with no public upstream"
        renderBootError (PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET is also ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM"
        renderBootError (MirrorTargetOnPublicUpstream Npm Npm "https://store.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__MIRROR_TARGET (https://store.example.test) shares a host with ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM"
        renderBootError (MirrorTargetOnPublicUpstream Npm Npm "https://store.example.test")
            `shouldSatisfy` infixed "point it at a registry that shares a host with no public upstream"
        -- The mirror-store refusal adds the shared registry, which the operator needs to see
        -- because two keys can name one store under different spellings.
        renderBootError (MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://store.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__MIRROR_TARGET is also ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM (https://store.example.test)"
        renderBootError (MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://store.example.test")
            `shouldSatisfy` infixed "the Dredger permanently deletes from the mirror target"
        -- A split-role refusal names the invocation the operator typed and the key that fixes it.
        renderBootError (SplitRoleNeedsDurableQueue "ecluse proxy --no-worker")
            `shouldSatisfy` infixed "ecluse proxy --no-worker"
        renderBootError (SplitRoleNeedsDurableQueue "ecluse mirror") `shouldSatisfy` infixed "ECLUSE_QUEUE__URL"
        renderBootError MirrorRoleWithoutMirroring `shouldSatisfy` infixed "ECLUSE_MOUNTS__<ECOSYSTEM>__MIRROR_TARGET"
        -- The queue backend refuses at the boot's own gate, so its render tells a transient
        -- fault from a permanent one exactly as the credential mint's does.
        renderBootError (MirrorQueueUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "ECLUSE_QUEUE__URL"
        renderBootError (MirrorQueueUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "transient"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.BootErrorSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Ecluse.Composition.BootError (
    Advisory (DredgerQuotaOverrideUnmatched, MirrorTargetOnOwnPublicationTarget, MirrorTargetOnPrivateUpstream, PrivateUpstreamUndecided),
    BootError (..),
    StoreMaintenanceReason (ClientBuildFailed, NoControlPlane),
    renderAdvisory,
    renderBootError,
    renderBootErrors,
 )
import Ecluse.Config (
    PolicyError (UnknownRuleType),
    StoreTag (TagRegistry, TagVerdaccio),
 )
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection),
    PermissionName (PermissionName),
    RepositoryName (RepositoryName),
    UndecidabilityReason (ChainBoundExceeded, NetworkFailure, NoMechanism),
    UnsafeReason (ConfigurationEvidence, InsufficientPermissions),
 )
import Ecluse.Test.Package (unsafeRegistryUrl)

spec :: Spec
spec = do
    renderBootErrorSpec
    renderBootErrorsSpec
    renderAdvisorySpec
    forM_ [(Npm, "NPM"), (PyPI, "PYPI")] $ \(eco, envName) ->
        it ("names the first-party dependency and fix for " <> show eco) $
            TE.encodeUtf8 (renderBootError (FirstPartyWithoutPrivateUpstream eco))
                `shouldBe` "ECLUSE_MOUNTS__"
                    <> envName
                    <> "__FIRST_PARTY is set but ECLUSE_MOUNTS__"
                    <> envName
                    <> "__PRIVATE_UPSTREAM is not: first-party names resolve from the private upstream alone. Configure privateUpstream for these names, or remove firstParty."
    it "names both upstream keys and their repository in the private/public refusal" $
        renderBootError (PrivateUpstreamOnPublicUpstream Npm "https://registry.example.test/npm/public/")
            `shouldBe` "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM and ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM resolve to the same registry (https://registry.example.test/npm/public/): the private leg forwards caller credentials and admits versions without the public rules. Configure distinct repositories."

renderBootErrorsSpec :: Spec
renderBootErrorsSpec =
    describe "renderBootErrors" $
        it "reports every aggregated refusal, one line each, in the order it received them" $
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
        renderBootError (QueueUrlUnrecognised "https://queue.example.test/q")
            `shouldSatisfy` infixed "https://queue.example.test/q"
        renderBootError (QueueUrlUnrecognised "x") `shouldSatisfy` infixed "projects/{project}/topics/{topic}"
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
        renderBootError (CodeArtifactMintFailed ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET" :| []) "AccessDenied") `shouldSatisfy` infixed "transient"
        renderBootError (FirstPartyMissing Npm) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__FIRST_PARTY"
        renderBootError (PublishStaticCredentialNeedsEdge Npm TagVerdaccio) `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__VERDACCIO__TOKEN"
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET (https://store.example.test) shares a host with ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM"
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "publisher's own credential"
        -- Both host-rule refusals name the change that satisfies the rule, as the store-rule
        -- refusals do, so a warned operator never learns more than a refused one.
        renderBootError (PublicationTargetOnPublicUpstream Npm PyPI "https://store.example.test")
            `shouldSatisfy` infixed "point it at a registry that shares a host with no public upstream"
        renderBootError (PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://store.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET is also ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM (https://store.example.test)"
        renderBootError (PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://store.example.test")
            `shouldSatisfy` infixed "point it at a registry that holds no other role"
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
        renderBootError (SplitRoleNeedsDurableQueue "ecluse proxy --no-worker")
            `shouldSatisfy` infixed "ecluse proxy --no-worker"
        renderBootError (SplitRoleNeedsDurableQueue "ecluse mirror") `shouldSatisfy` infixed "ECLUSE_QUEUE__URL"
        renderBootError MirrorRoleWithoutMirroring `shouldSatisfy` infixed "ECLUSE_MOUNTS__<ECOSYSTEM>__MIRROR_TARGET__<TAG>__URL"
        -- The queue backend refuses at the boot's own gate, so its render tells a transient
        -- fault from a permanent one exactly as the credential mint's does.
        renderBootError (MirrorQueueUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "ECLUSE_QUEUE__URL"
        renderBootError (MirrorQueueUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "transient"
        renderBootError (AdvisorySyncUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "ECLUSE_ADVISORIES__URL"
        renderBootError (AdvisorySyncUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "ECLUSE_ADVISORIES__DATA_DIR"
        renderBootError (AdvisorySyncUnavailable "CredentialChainExhausted")
            `shouldSatisfy` infixed "transient"
        renderBootError (StoreMaintenanceUnavailable Npm (NoControlPlane TagRegistry))
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__MIRROR_TARGET has no usable store maintenance backend: its target is a registry store, which carries no store maintenance backend this build can sweep"
        renderBootError (StoreMaintenanceUnavailable Npm (NoControlPlane TagRegistry))
            `shouldSatisfy` infixed "deletes from every mount's mirror target"
        renderBootError (StoreMaintenanceUnavailable Npm (ClientBuildFailed "CredentialChainExhausted"))
            `shouldSatisfy` infixed "building its client failed: CredentialChainExhausted"
        renderBootError (StoreTagConflict Npm "mirrorTarget.codeArtifact" Npm "privateUpstream.verdaccio" "https://one.example.test")
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT and ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO"
        renderBootError (StoreTagConflict Npm "mirrorTarget.codeArtifact" Npm "privateUpstream.verdaccio" "https://one.example.test")
            `shouldSatisfy` infixed "one store has one backend, so declare both endpoints under the same tag"
        renderBootError (DredgerChunkPauseBeneathFloor 1 2)
            `shouldSatisfy` infixed "ECLUSE_DREDGER__CHUNK_PAUSE (dredger.chunkPause) is 1s, beneath the floor of 2s"
        renderBootError (DredgerChunkPauseBeneathFloor 1 2)
            `shouldSatisfy` infixed "may be raised and never lowered"
        renderBootError (DredgerQuotaScopeConflict "shared" "https://one.example.test/" "https://two.example.test/")
            `shouldSatisfy` infixed "one.example.test:443 and two.example.test:443 both define the capacity pool \"shared\""
        renderBootError (DredgerQuotaScopeConflict "shared" "https://one.example.test/" "https://two.example.test/")
            `shouldSatisfy` infixed "give the two entries the same quotas and weights or separate scopes"
        renderBootError PilotWithoutEcosystem
            `shouldSatisfy` infixed "ECLUSE_ADVISORIES__URL is set but no mount is declared"
        renderBootError PilotWithoutEcosystem
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__<ECOSYSTEM>__"
        renderBootError (AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| ["DenyIfEpss"]))
            `shouldSatisfy` infixed "mount \"npm\" enables the advisory deny rules DenyIfCve, DenyIfEpss"
        renderBootError (AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| []))
            `shouldSatisfy` infixed "ECLUSE_ADVISORIES__URL (advisories.url) is unset"
        renderBootError (AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| []))
            `shouldSatisfy` infixed "run ecluse pilot to publish an artifact"
        -- The refusal names the mount, the repository the evidence came from, and the connection,
        -- because the operator fixes it in that repository rather than in this configuration.
        renderBootError (PrivateUpstreamUnsafe Npm (ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs")))
            `shouldSatisfy` infixed "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM admits public content: repository shared carries the external connection public:npmjs"
        renderBootError (PrivateUpstreamUnsafe Npm (ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs")))
            `shouldSatisfy` infixed "point privateUpstream at a repository that has none"
        -- One constructor carries both a refused grant and an identity that never resolved, so
        -- the line names each, and the remedy the second operator needs.
        renderBootError (PrivateUpstreamUnsafe Npm (InsufficientPermissions (PermissionName "codeartifact:DescribeRepository")))
            `shouldSatisfy` infixed "refused codeartifact:DescribeRepository"
        renderBootError (PrivateUpstreamUnsafe Npm (InsufficientPermissions (PermissionName "codeartifact:DescribeRepository")))
            `shouldSatisfy` infixed "Or this role resolved no identity at all."
        renderBootError (PrivateUpstreamUnsafe Npm (InsufficientPermissions (PermissionName "codeartifact:DescribeRepository")))
            `shouldSatisfy` infixed "give this role an AWS identity carrying that grant"
        renderBootError (PrivateUpstreamUnsafe Npm (InsufficientPermissions (PermissionName "codeartifact:DescribeRepository")))
            `shouldSatisfy` infixed "An identity that cannot ask cannot clear the repository"
        -- A probe that threw settled nothing, and it is the proxy's failure as much as a store
        -- role's, so it says what was being checked rather than borrowing a store's vocabulary.
        renderBootError (PrivateUpstreamProbeFailed Npm "NoCredentials")
            `shouldSatisfy` infixed "the check for a connection to a public registry on ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM threw: NoCredentials"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

{- | Every advisory constructor's rendered bytes. A writing role boots on these, so the line is
all an operator gets, and rewording one is a change to the operator-facing contract.
-}
renderAdvisorySpec :: Spec
renderAdvisorySpec = describe "renderAdvisory" $ do
    it "names the pair and the registry a mount's own private upstream collapsed onto" $
        advisoryBytes (MirrorTargetOnPrivateUpstream Npm Npm (unsafeRegistryUrl "https://store.example.test"))
            `shouldBe` "mount \"npm\": mirrorTarget and privateUpstream resolve to the same registry (https://store.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"

    it "names the neighbouring mount whose private upstream the mirror target collapsed onto" $
        advisoryBytes (MirrorTargetOnPrivateUpstream Npm PyPI (unsafeRegistryUrl "https://store.example.test"))
            `shouldBe` "mount \"npm\": mirrorTarget and mount \"pypi\" privateUpstream resolve to the same registry (https://store.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"

    it "names why an unchecked private upstream stayed the operator's to verify" $ do
        advisoryBytes (PrivateUpstreamUndecided Npm NoMechanism)
            `shouldBe` "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM was not checked for a connection to a public registry: its backend does not report the repositories and registries it aggregates. A repository that aggregates a public registry serves public packages as trusted private content, so confirming that this one does not stays yours"
        renderAdvisory (PrivateUpstreamUndecided Npm NetworkFailure) `shouldSatisfy` T.isInfixOf "its backend did not answer"
        renderAdvisory (PrivateUpstreamUndecided Npm ChainBoundExceeded) `shouldSatisfy` T.isInfixOf "crossed this walk's bounds"

    it "quotes the mirror target as configured, trailing slash included" $
        advisoryBytes (MirrorTargetOnOwnPublicationTarget Npm (unsafeRegistryUrl "https://store.example.test/npm/mirror/"))
            `shouldBe` "mount \"npm\": mirrorTarget and publicationTarget resolve to the same registry (https://store.example.test/npm/mirror/); the Dredger refuses this configuration, so pruning this mirror stays manual"

    it "reduces a declared capacity that names no store to its dialled authority, and says it paces nothing" $
        advisoryBytes (DredgerQuotaOverrideUnmatched "https://deploy:hunter2@gone.example.test/npm/")
            `shouldBe` "dredger.quotaOverrides: gone.example.test:443 names no store this deployment declares, so it paces nothing"
  where
    advisoryBytes = TE.encodeUtf8 . renderAdvisory

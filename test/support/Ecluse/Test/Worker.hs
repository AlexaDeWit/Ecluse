-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Adapter-parameterised worker policies and npm admission fixtures.
module Ecluse.Test.Worker (
    policyWith,
    npmPolicyWith,
    admitAllPolicies,
    admitAllPoliciesCapped,
    mirrorPoliciesUnder,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))

import Ecluse.Core.Package (Artifact (artFilename, artHashes), Hash, PackageDetails (pkgArtifacts), PackageName, unscopedName)
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Version (Version, renderVersion)

import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), defaultLimits)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpArtifact, wpArtifactHostHonoured, wpArtifactLimits, wpFirstParty, wpMinIntegrity, wpNow, wpPublish, wpResolveVersion, wpRules))
import Ecluse.Test.Package (defaultMinIntegrity, sampleArtifact, sampleDetails)
import Ecluse.Test.Registry.Npm (sourceVersionDoc)
import Ecluse.Test.Rules (admitRule)

{- | One npm re-evaluation bundle at the caller's clock, artifact byte cap, publish capability,
version resolver, and rules.
-}
npmPolicyWith ::
    IO UTCTime ->
    Int ->
    MirrorPublish ->
    (PackageName -> Version -> IO VersionEvaluation) ->
    [PreparedRule] ->
    WorkerPolicy
npmPolicyWith = policyWith npmAdapter

-- | Build a worker policy using the supplied adapter's artifact request capability.
policyWith ::
    RegistryAdapter ->
    IO UTCTime ->
    Int ->
    MirrorPublish ->
    (PackageName -> Version -> IO VersionEvaluation) ->
    [PreparedRule] ->
    WorkerPolicy
policyWith adapter clock artifactMaxBytes publish resolve rules =
    WorkerPolicy
        { -- Deny by default, as a mount that declares no namespaces owns none. A case
          -- that drives the privilege overrides this field.
          wpFirstParty = const False
        , wpResolveVersion = resolve
        , wpRules = rules
        , wpMinIntegrity = defaultMinIntegrity
        , wpArtifactHostHonoured = const True
        , wpArtifact = adapterArtifact adapter
        , wpPublish = publish
        , wpArtifactLimits = defaultLimits{maxMirrorArtifactBytes = artifactMaxBytes}
        , wpNow = clock
        }

{- | An admit-everything npm worker policy. Pass the true digests of the stub upstream's bytes,
or a mismatching set to drive the tamper refusal.
-}
admitAllPolicies :: MirrorPublish -> NonEmpty Hash -> WorkerPolicies
admitAllPolicies = mirrorPoliciesUnder [admitRule]

{- | 'admitAllPolicies' with an explicit byte cap for the artifact fetch. A body past the cap is
a terminal 'Ecluse.Core.Registry.FetchBoundExceeded' the worker dead-letters.
-}
admitAllPoliciesCapped :: Int -> MirrorPublish -> NonEmpty Hash -> WorkerPolicies
admitAllPoliciesCapped = mirrorPoliciesCapped [admitRule]

{- | 'admitAllPolicies' under the caller's own rules, for a case where one queued job meets workers
that booted different policies.
-}
mirrorPoliciesUnder :: [PreparedRule] -> MirrorPublish -> NonEmpty Hash -> WorkerPolicies
mirrorPoliciesUnder rules = mirrorPoliciesCapped rules (512 * 1024 * 1024)

-- The npm bundle every fixture policy shares, at the caller's rules and artifact byte cap.
mirrorPoliciesCapped :: [PreparedRule] -> Int -> MirrorPublish -> NonEmpty Hash -> WorkerPolicies
mirrorPoliciesCapped rules artifactMaxBytes publish currentDigests =
    Map.singleton
        Npm
        (npmPolicyWith getCurrentTime artifactMaxBytes publish resolve rules)
  where
    resolve name version = pure (VersionPresent (sourceVersionDoc (mirrorableDetails name version)) Nothing)

    -- The sample snapshot renamed to the conventional @{name}-{version}.tgz@ and given the caller's
    -- digest set, so file selection passes and the tamper gate verifies against exactly this set.
    mirrorableDetails :: PackageName -> Version -> PackageDetails
    mirrorableDetails name version =
        (sampleDetails name version)
            { pkgArtifacts =
                one
                    sampleArtifact
                        { artFilename = unscopedName name <> "-" <> renderVersion version <> ".tgz"
                        , artHashes = toList currentDigests
                        }
            }

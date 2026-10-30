-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared worker policy fixtures use production artifact request formation and admission floors.
Source locations belong to the resolver, independently of version triggers.
-}
module Ecluse.Test.Worker (
    npmPolicyWith,
    workerSourceAt,
    admitAllPolicies,
    admitAllPoliciesCapped,
    mirrorPoliciesUnder,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))

import Ecluse.Core.Package (Artifact (artFilename, artHashes, artUrl), Hash, PackageDetails (pkgArtifacts), PackageName, unscopedName)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails), VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Version (Version, renderVersion)

import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpArtifact, wpArtifactHostHonoured, wpArtifactLimits, wpEgressUrl, wpFirstParty, wpMinIntegrity, wpMirrorState, wpNow, wpPublish, wpResolveVersion, wpRules))
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
npmPolicyWith clock artifactMaxBytes publish resolve rules =
    WorkerPolicy
        { -- Deny by default, as a mount that declares no namespaces owns none. A case
          -- that drives the privilege overrides this field.
          wpFirstParty = const False
        , wpResolveVersion = const resolve
        , wpRules = rules
        , wpMinIntegrity = defaultMinIntegrity
        , wpArtifactHostHonoured = const True
        , -- npm's own artifact capability, the record the composition root carries onto the
          -- bundle, so the fetch path forms requests as production does.
          wpArtifact = adapterArtifact npmAdapter
        , wpEgressUrl = Right . loopbackRegistryUrl
        , wpPublish = publish
        , wpArtifactLimits = defaultLimits{maxMirrorArtifactBytes = artifactMaxBytes}
        , wpMirrorState = Nothing
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

-- | Point a fixture's admitted artifact locations at its public stub instead of placing URLs in jobs.
workerSourceAt :: Text -> WorkerPolicies -> WorkerPolicies
workerSourceAt location = Map.map relocate
  where
    relocate policy = policy{wpResolveVersion = \pay name version -> relocateEvaluation location <$> wpResolveVersion policy pay name version}

relocateEvaluation :: Text -> VersionEvaluation -> VersionEvaluation
relocateEvaluation location (VersionPresent doc latest) =
    let details = vdDetails doc
     in VersionPresent doc{vdDetails = details{pkgArtifacts = fmap (\artifact -> artifact{artUrl = location}) (pkgArtifacts details)}} latest
relocateEvaluation _ other = other

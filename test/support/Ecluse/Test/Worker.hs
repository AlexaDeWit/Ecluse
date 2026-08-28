-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Worker-test fixtures: the shared npm re-evaluation bundle, and a ready-made
admit-everything policy over it.

The mirror worker re-runs current policy against a job's version before mirroring it (see
"Ecluse.Core.Worker"), so any end-to-end worker test must supply per-ecosystem policies.
'npmPolicyWith' is the one 'WorkerPolicy' wiring those tests share: npm's real by-URL
request formation, the SHA-256 admission floor, and an open tarball-host gate, with the
clock, byte cap, publish capability, resolver, and rules left to the caller.
-}
module Ecluse.Test.Worker (
    npmPolicyWith,
    admitAllPolicies,
    admitAllPoliciesCapped,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))

import Ecluse.Core.Package (Artifact (artFilename, artHashes), Hash, PackageDetails (pkgArtifacts), PackageName, unscopedName)
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Rules.Types (RuleVerdict (Allow))
import Ecluse.Core.Version (Version, renderVersion)

import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Security (Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpArtifactHostHonoured, wpArtifactLimits, wpBuildArtifactRequest, wpMinIntegrity, wpNow, wpPublish, wpResolveVersion, wpRules))
import Ecluse.Test.Package (defaultMinIntegrity, sampleArtifact, sampleDetails)
import Ecluse.Test.Rules (constRule)

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
        { wpResolveVersion = resolve
        , wpRules = rules
        , wpMinIntegrity = defaultMinIntegrity
        , wpArtifactHostHonoured = const True
        , -- npm's real by-URL request formation, as the composition root
          -- projects it, so the fetch path forms requests as production does.
          wpBuildArtifactRequest = \_ _ baseUrl token -> artifactRequestByUrl baseUrl token
        , wpPublish = publish
        , wpArtifactLimits = defaultLimits{maxBodyBytes = artifactMaxBytes}
        , wpNow = clock
        }

{- | An admit-everything npm worker policy. Pass the true digests of the stub upstream's bytes,
or a mismatching set to drive the tamper refusal.
-}
admitAllPolicies :: MirrorPublish -> NonEmpty Hash -> WorkerPolicies
admitAllPolicies = admitAllPoliciesCapped (512 * 1024 * 1024)

{- | 'admitAllPolicies' with an explicit byte cap for the artifact fetch. A body past the cap is
a terminal 'Ecluse.Core.Registry.FetchBoundExceeded' the worker dead-letters.
-}
admitAllPoliciesCapped :: Int -> MirrorPublish -> NonEmpty Hash -> WorkerPolicies
admitAllPoliciesCapped artifactMaxBytes publish currentDigests =
    Map.singleton
        Npm
        (npmPolicyWith getCurrentTime artifactMaxBytes publish resolve [allowAll])
  where
    resolve name version = pure (VersionPresent (mirrorableDetails name version))

    allowAll :: PreparedRule
    allowAll = constRule "test-allow-all" (Allow "admitted for test")

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

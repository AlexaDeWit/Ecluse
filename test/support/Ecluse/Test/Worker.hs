-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Worker-test fixtures: a ready-made ingest re-evaluation policy.

The mirror worker re-runs current policy against a job's version before mirroring it (see
"Ecluse.Core.Worker"). Any end-to-end worker test must therefore supply per-ecosystem
policies. This module carries the admit-everything policy those tests reuse. Every version
resolves present through an injected resolver, with no real fetch, and an always-allow
rule clears it. The worker's ingest gate then admits, and the test exercises the
fetch → verify → publish path.
-}
module Ecluse.Test.Worker (
    admitAllPolicies,
    admitAllPoliciesCapped,
) where

import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))

import Ecluse.Core.Package (Artifact (artFilename, artHashes), Hash, PackageDetails (pkgArtifacts), PackageName, unscopedName)
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleVerdict (Allow))
import Ecluse.Core.Version (Version, renderVersion)

import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Security (Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpArtifactHostHonoured, wpArtifactLimits, wpBuildArtifactRequest, wpMinIntegrity, wpNow, wpPublish, wpResolveVersion, wpRules))
import Ecluse.Test.Package (defaultMinIntegrity, sampleArtifact, sampleDetails)

{- | An admit-everything npm worker policy: an injected resolver reports every version present and
an always-allow rule clears it. The ingest gate is the shared admission oracle
'Ecluse.Core.Package.Admission.admitArtifact', so the resolver synthesises the conventional
@{name}-{version}.tgz@ artifact carrying the caller's digest set. The tamper gate verifies the
fetched bytes against that set, so pass the true digests of the stub upstream's bytes, or a
mismatching set to drive the tamper refusal.
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
        WorkerPolicy
            { wpResolveVersion = \name version -> pure (VersionPresent (mirrorableDetails name version))
            , wpRules = [allowAll]
            , wpMinIntegrity = defaultMinIntegrity
            , wpArtifactHostHonoured = const True
            , -- npm's real by-URL request formation, as the composition root
              -- projects it, so the fetch path forms requests as production does.
              wpBuildArtifactRequest = \_ _ baseUrl token -> artifactRequestByUrl baseUrl token
            , wpPublish = publish
            , wpArtifactLimits = defaultLimits{maxBodyBytes = artifactMaxBytes}
            , wpNow = getCurrentTime
            }
  where
    allowAll :: PreparedRule
    allowAll =
        PreparedRule
            { prepName = "test-allow-all"
            , prepPrecedence = 0
            , prepResilience = Nothing
            , prepEval = \_ _ -> pure (Allow "admitted for test")
            }

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

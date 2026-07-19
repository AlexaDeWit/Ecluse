-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Worker-test fixtures: a ready-made ingest re-evaluation policy.

The mirror worker re-runs current policy against a job's version before mirroring it (see
"Ecluse.Core.Worker"), so any end-to-end worker test must supply per-ecosystem policies.
This carries the admit-everything policy those tests reuse: every version resolves present
through an injected resolver (no real fetch) and an always-allow rule clears it, so the
worker's ingest gate admits and the test exercises the fetch → verify → publish path.
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

{- | An admit-everything worker policy for the npm ecosystem: every version
resolves present through an injected resolver (no real metadata fetch) and an always-allow
rule clears it, so the worker's ingest gate admits and an end-to-end test exercises the
fetch → verify → publish path unchanged. The caller supplies the bundle's publish
capability (the marriage aimed at its own mirror stub, or a recording double), since
the mirror write rides the bundle.

The worker's ingest gate is the __same shared admission oracle the serve path runs__
('Ecluse.Core.Package.Admission.admitArtifact'), so the resolved snapshot must also
pass artifact selection and the integrity floor: the resolver synthesises the
conventional @{name}-{version}.tgz@ artifact (matching a conventionally-named job's
'Ecluse.Core.Queue.jobArtifactFilename') carrying the caller's digest set, the host
gate honours every host (a test upstream is loopback), and the floor is the
production default (so the set must include a floor-clearing digest).

The digest set is the caller's because the worker's tamper gate verifies the fetched
bytes against the __re-admitted__ artifact's digests, the ones this resolver carries:
a test passes the true digests of the bytes its stub upstream serves for the faithful
posture, or a deliberately mismatching set to drive the tamper refusal.
-}
admitAllPolicies :: MirrorPublish -> NonEmpty Hash -> WorkerPolicies
admitAllPolicies = admitAllPoliciesCapped (512 * 1024 * 1024)

{- | 'admitAllPolicies' with an explicit artifact fetch byte cap, for tests that
exercise the worker's over-cap drop: a body past the cap is a terminal
'Ecluse.Core.Worker.Fetch.ArtifactOverCap' and the job is dropped, not retried.
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

    -- The sample snapshot with its artifact renamed to the conventional
    -- @{name}-{version}.tgz@ and given the caller's digest set, so the shared
    -- admission oracle's file selection passes and the tamper gate verifies the
    -- fetched bytes against exactly this set.
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

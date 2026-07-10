{- | Worker-test fixtures: a ready-made ingest re-evaluation policy.

The mirror worker re-runs current policy against a job's version before mirroring it (see
"Ecluse.Core.Worker"), so any end-to-end worker test must supply per-ecosystem policies.
This carries the admit-everything policy those tests reuse: every version resolves present
through an injected resolver (no real fetch) and an always-allow rule clears it, so the
worker's ingest gate admits and the test exercises the fetch → verify → publish path.
-}
module Ecluse.Test.Worker (
    admitAllPolicies,
) where

import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))

import Ecluse.Core.Package (Artifact (artFilename, artHashes), HashAlg (SRI), PackageDetails (pkgArtifacts), PackageName, unscopedName)
import Ecluse.Core.Package.Integrity (defaultMinIntegrity)
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleVerdict (Allow))
import Ecluse.Core.Version (Version, renderVersion)

import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpArtifactHostHonoured, wpMinIntegrity, wpNow, wpResolveVersion, wpRules))
import Ecluse.Test.Package (sampleArtifact, sampleDetails, unsafeHash, validSha512Sri)

{- | An admit-everything worker re-evaluation policy for the npm ecosystem: every version
resolves present through an injected resolver (no real metadata fetch) and an always-allow
rule clears it, so the worker's ingest gate admits and an end-to-end test exercises the
fetch → verify → publish path unchanged.

The worker's ingest gate is the __same shared admission oracle the serve path runs__
('Ecluse.Core.Package.Admission.admitArtifact'), so the resolved snapshot must also
pass artifact selection and the integrity floor: the resolver synthesises the
conventional @{name}-{version}.tgz@ artifact (matching a conventionally-named job's
'Ecluse.Core.Queue.maFilename') carrying a floor-clearing sha512 SRI digest, the host
gate honours every host (a test upstream is loopback), and the floor is the
production default.
-}
admitAllPolicies :: WorkerPolicies
admitAllPolicies =
    Map.singleton
        Npm
        WorkerPolicy
            { wpResolveVersion = \name version -> pure (VersionPresent (mirrorableDetails name version))
            , wpRules = [allowAll]
            , wpMinIntegrity = defaultMinIntegrity
            , wpArtifactHostHonoured = const True
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
    -- @{name}-{version}.tgz@ and given a floor-clearing digest, so the shared
    -- admission oracle's file selection and integrity floor both pass.
    mirrorableDetails :: PackageName -> Version -> PackageDetails
    mirrorableDetails name version =
        (sampleDetails name version)
            { pkgArtifacts =
                one
                    sampleArtifact
                        { artFilename = unscopedName name <> "-" <> renderVersion version <> ".tgz"
                        , artHashes = [unsafeHash SRI validSha512Sri]
                        }
            }

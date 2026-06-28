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
import Ecluse.Core.Package (
    Artifact (Artifact, artFilename, artHashes, artInterpreter, artKind, artProvenance, artSize, artUrl, artYanked),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    PackageDetails (PackageDetails, pkgArtifacts, pkgAvailability, pkgDependencies, pkgInstallCode, pkgLicenses, pkgMaintainers, pkgName, pkgPublishedAt, pkgPublisher, pkgTrust, pkgVersion),
    PackageName,
    Trust (Untrusted),
 )
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionPresent))
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleResult (Allow))
import Ecluse.Core.Version (Version)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (WorkerPolicy, wpNow, wpResolveVersion, wpRules))

{- | An admit-everything worker re-evaluation policy for the npm ecosystem: every version
resolves present through an injected resolver (no real metadata fetch) and an always-allow
rule clears it, so the worker's ingest gate admits and an end-to-end test exercises the
fetch → verify → publish path unchanged.
-}
admitAllPolicies :: WorkerPolicies
admitAllPolicies =
    Map.singleton
        Npm
        WorkerPolicy
            { wpResolveVersion = \name version -> pure (VersionPresent (detailsFor name version))
            , wpRules = [allowAll]
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

    detailsFor :: PackageName -> Version -> PackageDetails
    detailsFor name version =
        PackageDetails
            { pkgName = name
            , pkgVersion = version
            , pkgPublishedAt = Nothing
            , pkgInstallCode = NoCodeOnInstall
            , pkgTrust = Untrusted
            , pkgAvailability = Available
            , pkgArtifacts = artifact :| []
            , pkgLicenses = []
            , pkgPublisher = Nothing
            , pkgMaintainers = []
            , pkgDependencies = []
            }

    artifact :: Artifact
    artifact =
        Artifact
            { artFilename = "test.tgz"
            , artUrl = "https://registry.npmjs.org/test/-/test.tgz"
            , artKind = Tarball
            , artHashes = []
            , artSize = Nothing
            , artInterpreter = Nothing
            , artYanked = False
            , artProvenance = Nothing
            }

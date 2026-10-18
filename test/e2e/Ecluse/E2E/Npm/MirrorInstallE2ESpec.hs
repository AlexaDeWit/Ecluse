-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native npm observations for the complete mirror contract.
module Ecluse.E2E.Npm.MirrorInstallE2ESpec (npmMirrorFamily) where

import Data.Aeson (Value)
import Data.Set qualified as Set
import Data.Text qualified as T
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Install (TargetExpectation (..), TargetObservation (..), TargetPortability (ArchitectureIndependent))

import Ecluse.E2E.Fixtures.Npm (
    PkgSpec,
    graphDepPkg,
    graphExecutable,
    graphExport,
    graphLeafPkg,
    graphPackages,
    graphPeerPkg,
    graphRootPkg,
    psName,
    psVersion,
 )
import Ecluse.E2E.Harness
import Ecluse.Test.Contract.Mirror

-- | Supply native observations and independent expectations to the canonical mirror contract.
npmMirrorFamily :: ConcreteFixture FamilyMetadata -> (MirrorFixture (Set Resolved) Value, FamilyFixture (MirrorDriver (Set Resolved) Value))
npmMirrorFamily fixture =
    ( MirrorFixture
        { mirrorExpectedGraph = expectedGraph
        , mirrorExpectedTargets = Right (portableTarget, portableTarget)
        , mirrorExpectedExport = graphExport
        , mirrorExpectedVersions = [(psName pkg, [psVersion pkg]) | pkg <- graphPackages]
        , mirrorRequiredPaths = neededOfMirror
        , mirrorExpectedSources = Right [redactedProxy <> npmTarballPath (psName pkg) (psVersion pkg) | pkg <- graphPackages]
        }
    , concreteFamily fixture (\_ action plane -> action (mirrorDriver plane))
    )

mirrorDriver :: GlobalDataPlane -> MirrorDriver (Set Resolved) Value
mirrorDriver plane =
    MirrorDriver
        { mirrorPublicInstall = withE2E installFromPublic plane
        , mirrorOfflineInstall = withPublicUpstreamsDown plane (withE2E installFromMirror plane)
        , mirrorEmptyInstall =
            withPublicUpstreamsDown plane $
                withE2EWith emptyPrivateStore (\proxy -> (,) <$> npmPublicReachable proxy <*> npmRefused proxy rootRequest) plane
        }

installFromPublic :: E2E -> IO (PublicPhase (Set Resolved) Value)
installFromPublic proxy = do
    versionsBefore <- forM graphPackages $ \pkg -> (psName pkg,) <$> verdaccioVersions proxy (psName pkg)
    reachable <- npmPublicReachable proxy
    observed <- observeInstall proxy
    versionsAfter <- forM graphPackages $ \pkg -> (psName pkg,) <$> verdaccioAwaitVersions proxy (psName pkg) [psVersion pkg]
    pure
        PublicPhase
            { publicObserved = observed
            , publicTargetObserved = portableObserved observed
            , publicReachable = reachable
            , publicVersionsBefore = versionsBefore
            , publicVersionsAfter = versionsAfter
            }

installFromMirror :: E2E -> IO (OfflinePhase (Set Resolved) Value)
installFromMirror proxy = do
    reachable <- npmPublicReachable proxy
    seen <- length <$> stubReadsNow plane
    observed <- observeInstall proxy
    answered <- awaitStubReads plane seen ((neededOfMirror `Set.isSubsetOf`) . servedByMirror)
    pure
        OfflinePhase
            { offlineObserved = observed
            , offlineTargetObserved = portableObserved observed
            , offlineUnsupportedTarget = Nothing
            , offlinePublicReachable = reachable
            , offlineMirrorPaths = servedByMirror answered
            , offlinePublicAnswers = map show (filter answeredByPublic answered)
            , offlineLockedSources = lockedSources (obTree observed)
            }
  where
    plane = e2ePlane proxy

observeInstall :: E2E -> IO (Observed (Set Resolved) Value)
observeInstall proxy = withNpmProject proxy $ \project -> do
    sources <- localInstallSources project
    result <- npmInstallIn project rootRequest
    tree <- installedTree proxy project
    Observed
        <$> resolvedGraph project
        <*> loadedExport project (psName graphRootPkg)
        <*> linkedExecutable project graphExecutable
        <*> pure tree
        <*> pure result
        <*> pure sources

-- The request names an exact version, because the `latest` of a packument that only the mirror
-- supplied is the proxy's own choice.
rootRequest :: Text
rootRequest = psName graphRootPkg <> "@" <> psVersion graphRootPkg

-- Every edge npm must resolve: the consumer's one request, then the root's dependency and peer.
expectedGraph :: Set Resolved
expectedGraph =
    Set.fromList
        [ consumerName `resolves` graphRootPkg
        , psName graphRootPkg `resolves` graphDepPkg
        , psName graphDepPkg `resolves` graphLeafPkg
        , psName graphRootPkg `resolves` graphPeerPkg
        ]
  where
    dependent `resolves` pkg = Resolved dependent (psName pkg) (psVersion pkg)

neededOfMirror :: Set Text
neededOfMirror = Set.fromList (concatMap readsOf graphPackages)
  where
    readsOf :: PkgSpec -> [Text]
    readsOf pkg = ["/" <> psName pkg, npmArtifactPath (psName pkg) (psVersion pkg)]

servedByMirror :: [StubRead] -> Set Text
servedByMirror answered =
    Set.fromList [srPath r | r <- answered, srRoute r == Mirror, srMethod r == "GET", srStatus r == 200]

-- A proxy whose private upstream answers every read with a 404, so it has no mirror to read back.
emptyPrivateStore :: E2EConfig
emptyPrivateStore =
    defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl EmptyPrivate)]}

portableTarget :: TargetExpectation
portableTarget =
    TargetExpectation
        ArchitectureIndependent
        [T.takeWhileEnd (/= '/') (npmTarballPath (psName package) (psVersion package)) | package <- graphPackages]
        []

portableObserved :: Observed graph value -> TargetObservation graph
portableObserved observed =
    TargetObservation
        ArchitectureIndependent
        (obClient observed)
        (obGraph observed)
        (Right (map (T.takeWhileEnd (/= '/')) (lockedSources (obTree observed))))
        (obTree observed)

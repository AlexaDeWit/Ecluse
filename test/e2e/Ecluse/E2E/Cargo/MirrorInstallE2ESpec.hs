-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native public and completed-mirror installs supply all shared mirror observations.
module Ecluse.E2E.Cargo.MirrorInstallE2ESpec (
    CargoCoordinates,
    cargoMirrorFamily,
    cargoMirrorConfig,
    withCargoMirrorFixture,
    cargoObservedGraph,
    cargoCoordinates,
    cargoPackageTree,
    cargoMirrorComplete,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath (joinPath, splitDirectories)
import Test.Hspec (shouldReturn)
import UnliftIO (finally)

import Ecluse.E2E.Cargo.InstallE2ESpec (assertCargoAssets, cargoConsumer)
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install (TargetExpectation (..), TargetObservation (..), TargetPortability (ArchitectureIndependent))
import Ecluse.Test.Contract.Mirror
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Serving (ServingResponse (responseStatus))
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Reference

-- | Package IDs vary with native source URLs, while native name and version retain identity.
type CargoCoordinates = [(Text, Text)]

-- | Independent fixture coordinates include both target declarations and both leaf majors.
cargoCoordinates :: CargoCoordinates
cargoCoordinates = sort [(crateName spec, crateVersion spec) | spec <- cargoCrates]

-- | Durable mirror state uses the ordinary S3 emulator and separate native store roles.
cargoMirrorConfig :: E2EConfig
cargoMirrorConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MIRROR_STATE__URL", "s3://" <> advisoryBucket <> "/cargo-mirror")
            , ("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl CargoPrivate)
            , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__URL", stubUrl CargoMirror)
            , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__UPLOAD_URL", T.dropWhileEnd (== '/') (stubUrl CargoMirror))
            , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__TOKEN", cargoRegistryToken)
            ]
                <> ministackAwsEnv
        }

-- | Keep the target, remote state and original archives alive across product restarts.
withCargoMirrorFixture :: GlobalDataPlane -> (CargoRegistry -> [CargoRelease] -> IO a) -> IO a
withCargoMirrorFixture plane action = do
    createAdvisoryBucket plane
    releases <- cargoFixtureReleases
    withCargoRegistry $ \registry ->
        withStubFault plane (CargoBackend (registryPort registry) False) (action registry releases)

-- | Native source archives are portable. Compilation and executable assertions still use the real host toolchain.
cargoMirrorFamily :: (MirrorFixture CargoCoordinates Text, FamilyFixture (MirrorDriver CargoCoordinates Text))
cargoMirrorFamily =
    ( MirrorFixture
        cargoCoordinates
        (Right (targetExpected, targetExpected))
        (cargoExpectedOutput False False)
        expectedVersions
        requiredPaths
        (Right ["registry+https://github.com/rust-lang/crates.io-index"])
    , concreteFamily (cargoWriteFixture "mirror" ["read", "mirror", "client-install"]) $ \recorder action plane ->
        timedFixture recorder (withCargoMirrorFixture plane . curry) $ \(registry, releases) -> do
            queue <- sharedMirrorQueue plane
            action (mirrorDriver plane registry releases cargoMirrorConfig{ecQueueUrl = Just queue})
    )
  where
    expectedVersions = [(name, sort [version | (package, version) <- cargoCoordinates, package == name]) | name <- ordNub (map fst cargoCoordinates)]
    requiredPaths = Set.fromList (["/" <> cargoShard name | name <- ordNub (map fst cargoCoordinates)] <> ["/" <> cargoArchivePath spec | spec <- cargoCrates])
    targetExpected = TargetExpectation ArchitectureIndependent [crateName spec <> "-" <> crateVersion spec <> ".crate" | spec <- cargoCrates] []

mirrorDriver :: GlobalDataPlane -> CargoRegistry -> [CargoRelease] -> E2EConfig -> MirrorDriver CargoCoordinates Text
mirrorDriver plane registry releases config =
    MirrorDriver
        { mirrorPublicInstall = do
            before <- versions
            (observed, target, reachable) <-
                withObservedNativeProxy
                    config{ecArgs = ["proxy", "--no-worker"]}
                    plane
                    ( \proxy -> do
                        reachable <- cargoPublicReachable (observedProxy proxy)
                        (\(observed, target, _) -> (observed, target, reachable)) <$> cargoObservedGraph proxy
                    )
            cargoRegistryFiles registry `shouldReturn` Map.empty
            withObservedNativeProxy config plane $ \proxy -> do
                awaitCargoRegistry registry releases `shouldReturn` True
                for_ releases $ \release -> awaitProxyLog (observedProxy proxy) (cargoMirrorComplete (crateName (releaseCrate release)) (crateVersion (releaseCrate release))) 120 `shouldReturn` True
            PublicPhase observed target reachable before <$> versions
        , mirrorOfflineInstall = withStubFault plane (CargoBackend (registryPort registry) True) $
            withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \proxy -> do
                reachable <- cargoPublicReachable (observedProxy proxy)
                before <- nativeTraffic proxy
                (observed, target, sources) <- cargoObservedGraph proxy
                after <- nativeTraffic proxy
                let requests = drop (length (nativeUpstreamRequests before)) (nativeUpstreamRequests after)
                pure
                    ( OfflinePhase
                        observed
                        target
                        Nothing
                        reachable
                        (Set.fromList [nativeRequestPath request | request <- requests, nativeRequestHost request == "cargo-mirror", nativeRequestStatus request == 200])
                        [show request | request <- requests, nativeRequestHost request == "cargo-upstream", nativeRequestStatus request /= 444]
                        sources
                    )
        , mirrorEmptyInstall = do
            files <- cargoRegistryFiles registry
            atomically (writeTVar (registryFiles registry) Map.empty)
            flip finally (atomically (writeTVar (registryFiles registry) files)) $
                withStubFault plane (CargoBackend (registryPort registry) True) $
                    withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \proxy ->
                        do
                            reachable <- cargoPublicReachable (observedProxy proxy)
                            cargoConsumer (observedProxy proxy) currentCargo $ \project -> (reachable,) <$> runCargo project ["build"]
        }
  where
    versions = forM (ordNub (map fst cargoCoordinates)) $ \name -> (name,) <$> cargoRegistryVersions registry "cargo-mirror" name

-- | Capture a real build, install, graph, executable and complete native package sources.
cargoObservedGraph :: ObservedNativeProxy -> IO (Observed CargoCoordinates Text, TargetObservation CargoCoordinates, [Text])
cargoObservedGraph observed = cargoConsumer proxy currentCargo $ \project -> do
    before <- nativeTraffic observed
    built <- runCargo project ["build"] >>= shouldSucceed
    metadata <- cargoMetadata project [] >>= shouldSucceed
    graph <- either (fail . toString) pure (cargoGraph metadata)
    let coordinates = sort [(cargoPackageName package, cargoPackageVersion package) | package <- Map.elems (cargoGraphPackages graph), isJust (cargoPackageSource package)]
    ran <- cargoRun project [] >>= shouldSucceed
    void (cargoInstall project cargoRoot cargoVersion [] >>= shouldSucceed)
    executable <- cargoInstalledExecutable project cargoExecutable >>= shouldSucceed
    tree <- cargoPackageTree project
    after <- nativeTraffic observed
    interval <- nativeTrafficSince before after
    assertCargoAssets project interval
    locked <- cargoLock project >>= either (fail . toString) pure
    let archives = sort [toText filename | (path, TreeFile _ _) <- Map.toList tree, let filename = fromMaybe path (listToMaybe (reverse (splitDirectories path))), ".crate" `T.isSuffixOf` toText filename]
        sources = ordNub [source | entry <- locked, Just source <- [cargoLockedSource entry]]
        output = T.strip (crStdout ran)
        localSources = [cargoPackageManifestPath package | (identifier, package) <- Map.toList (cargoGraphPackages graph), Just identifier /= cargoGraphRoot graph, isNothing (cargoPackageSource package)]
        result = Observed (Right coordinates) (Right output) (Just (Right (T.strip (crStdout executable)))) tree built localSources
        target = TargetObservation ArchitectureIndependent built (Right coordinates) (Right archives) tree
    pure (result, target, sources)
  where
    proxy = observedProxy observed

-- | Compare exact native archives and extracted package files, excluding compiler output and transport caches.
cargoPackageTree :: CargoProject -> IO InstalledTree
cargoPackageTree project = Map.fromList . mapMaybe nativePath . Map.toList <$> snapshotTree (cargoDirectory project) ["home/registry/cache", "home/registry/src"]
  where
    nativePath (path, entry) = case splitDirectories path of
        "home" : "registry" : kind : _source : rest | not (null rest) -> Just (joinPath (kind : rest), entry)
        _ -> Nothing

-- | Read the worker's structured completion message for one exact native identity.
cargoMirrorComplete :: Text -> Text -> Text -> Bool
cargoMirrorComplete = mirrorVersionComplete

cargoPublicReachable :: E2E -> IO Bool
cargoPublicReachable proxy = (== 200) . responseStatus <$> proxyHead proxy ("/cargo/" <> cargoShard "ecluse-e2e-advisory")

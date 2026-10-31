-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Real Cargo installation and independent wire evidence supply the canonical installation laws.
module Ecluse.E2E.Cargo.InstallE2ESpec (
    cargoInstallationFamily,
    cargoInstallInto,
    cargoConsumer,
    assertCargoRouting,
    assertCargoAssets,
    assertCargoOriginRouting,
    cargoDeniedConfig,
) where

import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (splitDirectories, takeFileName, (</>))
import Test.Hspec (shouldBe, shouldMatchList, shouldSatisfy)

import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoRegistry (cargoRegistryToken)
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.InstallationE2ESpec (codedArtifact, probedArtifact)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Accepted and denied clients own fresh native caches and compiler outputs.
cargoInstallationFamily :: FamilyFixture (InstallationDriver Bool)
cargoInstallationFamily = concreteFamily (cargoReadFixture "installation" ["read", "client-install"]) $ \recorder action plane ->
    timedFixture recorder (withObservedNativeProxy defaultE2EConfig plane) (action . installation)

installation :: ObservedNativeProxy -> InstallationDriver Bool
installation observed =
    InstallationDriver
        { installationAccepted = InstallDriver install True
        , installationDenied =
            InstallDriver
                ( withObservedNativeProxy
                    cargoDeniedConfig
                    plane
                    ( \denied -> cargoConsumer (observedProxy denied) currentCargo $ \project -> do
                        result <- runCargo project ["build"]
                        built <- doesFileExist (cargoDirectory project </> "target/debug/ecluse-e2e-consumer")
                        files <- snapshotTree (cargoDirectory project) ["home/registry/src", "home/registry/cache", "installed/bin", "target/debug"]
                        pure (result, (built, files))
                    )
                )
                (False, mempty)
        , installationArtifact = (,) <$> advertised <*> proxyGetCoded proxy archive
        , installationDigest = hexSha256Of
        , installationHead = probedArtifact proxy CargoPublic upstream archive
        , installationCodedArtifact = codedArtifact proxy CargoPublic upstream archive advertised
        }
  where
    proxy = observedProxy observed
    plane = e2ePlane proxy
    archive = cargoArchiveRoute cargoRoot cargoVersion
    upstream = T.drop (T.length "/cargo") archive
    advertised = do
        rows <- cargoReadIndex proxy "/cargo/" cargoRoot
        case [checksum | row <- rows, Just (String version) <- [KeyMap.lookup "vers" row], version == cargoVersion, Just (String checksum) <- [KeyMap.lookup "cksum" row]] of
            [checksum] -> pure checksum
            _ -> fail "Cargo did not advertise one exact checksum"
    install = cargoConsumer proxy currentCargo $ \project -> do
        invocation <- observeCargo (nativeTraffic observed) project ["build"]
        when (crExit (cargoInvocationClient invocation) == ExitSuccess) $ do
            assertCargoRouting (cargoInvocationTraffic invocation)
            assertCargoAssets project (cargoInvocationTraffic invocation)
        result <- cargoRun project []
        pure (cargoInvocationClient invocation, crExit result == ExitSuccess && T.strip (crStdout result) == cargoExpectedOutput False False)

{- | Every client command uses the crates.io replacement and a fresh owned project. The token answers
discovery's authentication requirement whenever the mount has a private upstream.
-}
cargoConsumer :: E2E -> CargoToolchain -> (CargoProject -> IO a) -> IO a
cargoConsumer proxy toolchain = withCargoProject toolchain CargoConsumer proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/") (Just cargoRegistryToken)

-- | Compile the selected registry coordinate using a fresh consumer project.
cargoInstallInto :: E2E -> CargoToolchain -> Text -> Text -> IO ClientResult
cargoInstallInto proxy toolchain name version = cargoConsumer proxy toolchain $ \project -> do
    cargoSelectDependency name version project
    runCargo project ["build"]

-- | Require archive requests at both proxy listeners and at the public fixture.
assertCargoRouting :: NativeTraffic -> IO ()
assertCargoRouting = assertCargoOriginRouting "cargo-upstream"

-- | Require matching native archive exchanges through the registry and its actual origin.
assertCargoOriginRouting :: Text -> NativeTraffic -> IO ()
assertCargoOriginRouting origin traffic = do
    let archives = filter (T.isSuffixOf ".crate" . nativeRequestPath)
        root = "/cargo"
        throughRoot = filter (T.isPrefixOf (root <> "/crates/") . nativeRequestPath) . archives
        clients = throughRoot (nativeClientRequests traffic)
        forwards = throughRoot (nativeProxyRequests traffic)
        upstream = filter ((== origin) . nativeRequestHost) (archives (nativeUpstreamRequests traffic))
    let discovery = filter ((== root <> "/config.json") . nativeRequestPath) (nativeClientRequests traffic)
    discovery `shouldSatisfy` (not . null)
    map nativeRequestScheme discovery `shouldSatisfy` all (== "https")
    clients `shouldSatisfy` (not . null)
    map nativeRequestScheme clients `shouldSatisfy` all (== "https")
    ordNub (map nativeRequestHost clients) `shouldBe` ordNub (map nativeRequestHost discovery)
    forwards `shouldSatisfy` (not . null)
    upstream `shouldSatisfy` (not . null)
    map nativeRequestPath clients `shouldMatchList` map nativeRequestPath forwards
    map (T.drop (T.length root) . nativeRequestPath) clients `shouldMatchList` map nativeRequestPath upstream

-- | Every cached native archive needs a successful HTTPS request and a matching product exchange.
assertCargoAssets :: CargoProject -> NativeTraffic -> IO ()
assertCargoAssets project traffic = do
    tree <- cargoTree project
    let archives = filter (T.isSuffixOf ".crate" . nativeRequestPath)
        clients = archives (nativeClientRequests traffic)
        forwards = archives (nativeProxyRequests traffic)
        cached = [toText (takeFileName path) | (path, TreeFile _ _) <- Map.toList tree, take 3 (splitDirectories path) == ["home", "registry", "cache"], ".crate" `T.isSuffixOf` toText path]
        discovery = filter (T.isSuffixOf "/config.json" . nativeRequestPath) (nativeClientRequests traffic)
        throughCargoView = T.isPrefixOf "/cargo/crates/"
        exchange request = (nativeRequestMethod request, nativeRequestPath request, nativeRequestStatus request)
    clients `shouldSatisfy` (not . null)
    discovery `shouldSatisfy` (not . null)
    map nativeRequestScheme (clients <> discovery) `shouldSatisfy` all (== "https")
    map nativeRequestPath clients `shouldSatisfy` all throughCargoView
    map nativeRequestStatus clients `shouldSatisfy` all (== 200)
    ordNub (map nativeRequestHost clients) `shouldBe` ordNub (map nativeRequestHost discovery)
    map exchange clients `shouldMatchList` map exchange forwards
    ordNub (map (T.takeWhileEnd (/= '/') . nativeRequestPath) clients) `shouldMatchList` ordNub cached

-- | Disable all ordinary admission paths for the native denial control.
cargoDeniedConfig :: E2EConfig
cargoDeniedConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__CARGO__RULES", "{\"min-age\":{\"enabled\":false},\"remediation-fast-track\":{\"enabled\":false}}")]}

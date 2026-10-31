-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | First-party native publication and a fresh authenticated Cargo installation use the product image.
module Ecluse.E2E.Cargo.PublishE2ESpec (
    cargoPublicationFamily,
    cargoPublicationConfig,
    cargoPublishFixture,
    cargoPrivateConsumer,
    cargoPublishName,
    cargoRetainedName,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec (expectationFailure, shouldBe, shouldReturn)

import Ecluse.E2E.Cargo.InstallE2ESpec (assertCargoAssets, assertCargoOriginRouting)
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Publication
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Independent owned names isolate accepted publication from the refusal controls.
cargoPublishName, cargoRetainedName :: Text
cargoPublishName = "ecluse-e2e-build"
cargoRetainedName = "ecluse-e2e-macro"

{- | Undeclared, and held by the public index at 1.0.0 only: Cargo's duplicate check passes and the upload reaches Écluse.
The sparse index answers a name in no index 404, which Cargo also reads as a new crate.
-}
cargoRefusedName, cargoRefusedVersion :: Text
cargoRefusedName = "ecluse-e2e-extra"
cargoRefusedVersion = "1.0.1"

-- | Read and upload authority share one declared native registry, with the API base set explicitly.
cargoPublicationConfig :: E2EConfig
cargoPublicationConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl CargoPrivate)
            , ("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", cargoPublishName <> "," <> cargoRetainedName)
            , ("ECLUSE_MOUNTS__CARGO__PUBLICATION_TARGET__REGISTRY__URL", stubUrl CargoPrivate)
            , ("ECLUSE_MOUNTS__CARGO__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL", T.dropWhileEnd (== '/') (stubUrl CargoPrivate))
            , ("ECLUSE_MOUNTS__CARGO__PUBLICATION_TARGET__REGISTRY__TOKEN", cargoRegistryToken)
            , ("ECLUSE_SERVER__AUTH_TOKEN", cargoRegistryToken)
            ]
        }

-- | Every shared publication law uses the native Cargo command and independently reads the target.
cargoPublicationFamily :: FamilyFixture PublicationFixture
cargoPublicationFamily = concreteFamily (cargoWriteFixture "publish" ["read", "publication", "client-install"]) $ \recorder action plane ->
    timedFixture recorder withCargoRegistry $ \registry ->
        withStubFault plane (CargoBackend (registryPort registry) False) $
            withObservedNativeProxy cargoPublicationConfig plane $ \observed ->
                action
                    ( PublicationFixture
                        cargoVersion
                        (publicationDriver observed registry cargoPublishName)
                        (refusedDriver observed registry)
                        (yankDriver observed registry)
                    )

publicationDriver :: ObservedNativeProxy -> CargoRegistry -> Text -> PublicationDriver
publicationDriver observed registry name =
    PublicationDriver
        { publicationRun = cargoPublishFixture proxy currentCargo name
        , publicationInstall = cargoPrivateConsumer proxy currentCargo name cargoVersion $ \project -> do
            invocation <- observeCargo (nativeTraffic observed) project ["build"]
            when (crExit (cargoInvocationClient invocation) == ExitSuccess) $ do
                assertCargoOriginRouting "cargo-private" (cargoInvocationTraffic invocation)
                assertCargoAssets project (cargoInvocationTraffic invocation)
            pure (cargoInvocationClient invocation)
        , publicationVersions = cargoRegistryVersions registry "cargo-private" name
        , publicationAwait = cargoRegistryVersions registry "cargo-private" name
        , publicationSettle = pass
        }
  where
    proxy = observedProxy observed

-- | The guard itself answers the upload: one 403 that names the guard, and no write at the target.
refusedDriver :: ObservedNativeProxy -> CargoRegistry -> PublicationDriver
refusedDriver observed registry =
    (publicationDriver observed registry cargoRefusedName)
        { publicationRun = do
            before <- nativeTraffic observed
            received <- length <$> readTVarIO (registryRequests registry)
            uploads <- readTVarIO (registryUploads registry)
            result <- cargoPublishAt (observedProxy observed) currentCargo cargoRefusedName cargoRefusedVersion
            traffic <- nativeTraffic observed >>= nativeTrafficSince before
            let sent = [(nativeRequestPath request, nativeRequestStatus request) | request <- nativeClientRequests traffic, nativeRequestMethod request == "PUT"]
                refuse reason = expectationFailure (toString (clientReport result reason))
            unless (sent == [("/cargo/api/v1/crates/new", 403)]) (refuse ("sent the uploads " <> show sent <> ", where the guard refuses one with 403"))
            unless ("outside the first-party namespaces" `T.isInfixOf` (crStdout result <> crStderr result)) (refuse "omitted the first-party guard's refusal")
            readTVarIO (registryUploads registry) `shouldReturn` uploads
            writes <- filter ((`notElem` ["GET", "HEAD"]) . nativeRequestMethod) . drop received <$> readTVarIO (registryRequests registry)
            writes `shouldBe` []
            pure result
        }

yankDriver :: ObservedNativeProxy -> CargoRegistry -> NonUploadDriver
yankDriver observed registry =
    NonUploadDriver
        { nonUploadRelease = cargoPublishFixture proxy currentCargo cargoRetainedName
        , nonUploadWrite = cargoPrivateConsumer proxy currentCargo cargoRetainedName cargoVersion $ \project ->
            runCargo project ["yank", "--registry", "ecluse-test", "--version", toString cargoVersion, toString cargoRetainedName]
        , nonUploadRefusal = ["404"]
        , nonUploadDestination = do
            files <- cargoRegistryFiles registry
            requests <- readTVarIO (registryRequests registry)
            pure $
                [host <> "/" <> name <> "/" <> version <> " " <> hexSha256Of (storedArchive stored) | ((host, name, version), stored) <- Map.toList files]
                    <> ["write received " <> nativeRequestMethod request <> " " <> nativeRequestHost request <> nativeRequestPath request | request <- requests, nativeRequestMethod request `notElem` ["GET", "HEAD"]]
        , nonUploadSettle = pass
        }
  where
    proxy = observedProxy observed

-- | Publish repository-owned leaf sources with the real client's verification and native framing.
cargoPublishFixture :: E2E -> CargoToolchain -> Text -> IO ClientResult
cargoPublishFixture proxy toolchain name = cargoPublishAt proxy toolchain name cargoVersion

-- | Publish one owned fixture's sources under the given native version.
cargoPublishAt :: E2E -> CargoToolchain -> Text -> Text -> IO ClientResult
cargoPublishAt proxy toolchain name version = do
    spec <- maybe (fail "owned Cargo publication fixture is absent") pure (find (\crate -> crateName crate == name && crateVersion crate == cargoVersion) cargoCrates)
    withCargoProject toolchain CargoPublisher proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/") (Just cargoRegistryToken) $ \project -> do
        cargoUseFixture project "test/fixtures/cargo/graph" spec
        let manifest = cargoDirectory project </> "Cargo.toml"
        original <- decodeUtf8 <$> readFileBS manifest
        writeFileText manifest (T.replace ("\nversion = \"" <> cargoVersion <> "\"\n") ("\nversion = \"" <> version <> "\"\n") original)
        cargoRestrictPublication project
        cargoPublish project

-- | Resolve a private crate as a plain dependency through the merged registry with a fresh native cache.
cargoPrivateConsumer :: E2E -> CargoToolchain -> Text -> Text -> (CargoProject -> IO a) -> IO a
cargoPrivateConsumer proxy toolchain name version action =
    withCargoProject toolchain CargoConsumer proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/") (Just cargoRegistryToken) $ \project -> do
        cargoSelectDependency name version project
        action project

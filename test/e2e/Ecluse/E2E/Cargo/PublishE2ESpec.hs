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

import Ecluse.E2E.Cargo.InstallE2ESpec (assertCargoAssets, assertCargoViewRouting)
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Publication
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Independent owned names isolate accepted publication from the refusal controls.
cargoPublishName, cargoRetainedName :: Text
cargoPublishName = "ecluse-e2e-build"
cargoRetainedName = "ecluse-e2e-macro"

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
                        (publicationDriver observed registry "ecluse-e2e-extra")
                        (yankDriver observed registry)
                    )

publicationDriver :: ObservedNativeProxy -> CargoRegistry -> Text -> PublicationDriver
publicationDriver observed registry name =
    PublicationDriver
        { publicationRun = cargoPublishFixture proxy currentCargo name
        , publicationInstall = cargoPrivateConsumer proxy currentCargo name cargoVersion $ \project -> do
            invocation <- observeCargo (nativeTraffic observed) project ["build"]
            when (crExit (cargoInvocationClient invocation) == ExitSuccess) $ do
                assertCargoViewRouting "/cargo/private" "cargo-private" (cargoInvocationTraffic invocation)
                assertCargoAssets project (cargoInvocationTraffic invocation)
            pure (cargoInvocationClient invocation)
        , publicationVersions = cargoRegistryVersions registry "cargo-private" name
        , publicationAwait = cargoRegistryVersions registry "cargo-private" name
        , publicationSettle = pass
        }
  where
    proxy = observedProxy observed

yankDriver :: ObservedNativeProxy -> CargoRegistry -> NonUploadDriver
yankDriver observed registry =
    NonUploadDriver
        { nonUploadRelease = cargoPublishFixture proxy currentCargo cargoRetainedName
        , nonUploadWrite = cargoPrivateConsumer proxy currentCargo cargoRetainedName cargoVersion $ \project ->
            runCargo project ["yank", "--registry", "ecluse-test", "--version", toString cargoVersion, toString cargoRetainedName]
        , nonUploadRefusal = ["404"]
        , nonUploadDestination = do
            files <- cargoRegistryFiles registry
            pure [host <> "/" <> name <> "/" <> version <> " " <> hexSha256Of (storedArchive stored) | ((host, name, version), stored) <- Map.toList files]
        , nonUploadSettle = pass
        }
  where
    proxy = observedProxy observed

-- | Publish repository-owned leaf sources with the real client's verification and native framing.
cargoPublishFixture :: E2E -> CargoToolchain -> Text -> IO ClientResult
cargoPublishFixture proxy toolchain name = do
    spec <- maybe (fail "owned Cargo publication fixture is absent") pure (find (\crate -> crateName crate == name && crateVersion crate == cargoVersion) cargoCrates)
    withCargoProject toolchain CargoAlternate proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/private/") (Just cargoRegistryToken) $ \project -> do
        cargoUseFixture project "test/fixtures/cargo/graph" spec
        cargoPublish project

-- | Resolve an arbitrary private coordinate through the private-only view with a fresh native cache.
cargoPrivateConsumer :: E2E -> CargoToolchain -> Text -> Text -> (CargoProject -> IO a) -> IO a
cargoPrivateConsumer proxy toolchain name version action =
    withCargoProject toolchain CargoAlternate proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/private/") (Just cargoRegistryToken) $ \project -> do
        cargoSelectDependency CargoAlternate name version project
        action project

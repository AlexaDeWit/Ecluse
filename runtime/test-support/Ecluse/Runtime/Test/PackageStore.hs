-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact conformance over native data planes and an independent typed control plane.
module Ecluse.Runtime.Test.PackageStore (
    codeArtifactDriver,
    codeArtifactPyPIDriver,
    codeArtifactCargoDriver,
    codeArtifactCatalogue,
    codeArtifactSuiteGroups,
    validateCodeArtifact,
) where

import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((?~), (^.))

import Data.Text qualified as T
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Registry.Maintenance (StoreFault, StoreMaintenance, StoreManifestRead, protocolFault)
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), codeArtifactFormat)
import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (ControlPlane (..), maintenanceFor)
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (ReadPlane (..))
import Ecluse.Test.Contract.Catalogue (combineCatalogues)
import Ecluse.Test.Contract.Completion (validateCompletion)
import Ecluse.Test.Contract.Manifest (failRunner)
import Ecluse.Test.Contract.PackageStore (FullPackageStoreDriver, PackageStoreRelease (psrVersion), packageStoreCatalogueFor, packageStoreCompletion)
import Ecluse.Test.Contract.PackageStore.Cargo (cargoStoreDriver, readCargoStoreManifestOver)
import Ecluse.Test.Contract.PackageStore.Protocol (npmStoreDriver, readStoreManifestOver)
import Ecluse.Test.Contract.PackageStore.Provider (StoreBoundary (..))
import Ecluse.Test.Contract.PackageStore.PyPI (pypiStoreDriver, readPyPIStoreManifestOver)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Catalogue (..), Group (..))

-- | This proves Écluse's adapter behaviour, not remote AWS retention, IAM, or service fidelity.
codeArtifactDriver :: FullPackageStoreDriver
codeArtifactDriver = npmStoreDriver "codeartifact" (codeArtifactMaintenance Npm CA.PackageFormat_Npm readStoreManifestOver)

-- | PyPI executes every law with six distinct distributions across two releases.
codeArtifactPyPIDriver :: FullPackageStoreDriver
codeArtifactPyPIDriver = pypiStoreDriver "codeartifact-pypi" (codeArtifactMaintenance PyPI CA.PackageFormat_Pypi readPyPIStoreManifestOver)

-- | Native Cargo executes every law through prepared authenticated metadata and physical deletion.
codeArtifactCargoDriver :: FullPackageStoreDriver
codeArtifactCargoDriver = cargoStoreDriver "codeartifact-cargo" (codeArtifactMaintenance Cargo (CA.PackageFormat' "cargo") readCargoStoreManifestOver)

-- | All native profiles belong to the same backend and fixed conformance job.
codeArtifactCatalogue :: Catalogue
codeArtifactCatalogue =
    combineCatalogues
        [ packageStoreCatalogueFor "npm" "codeartifact" "package-store-codeartifact" "codeartifact"
        , packageStoreCatalogueFor "pypi" "codeartifact" "package-store-codeartifact" "codeartifact-pypi"
        , packageStoreCatalogueFor "cargo" "codeartifact" "package-store-codeartifact" "codeartifact-cargo"
        ]

-- | Preserve each concrete registration while assigning the existing backend job.
codeArtifactSuiteGroups :: [DeclaredGroup] -> [DeclaredGroup]
codeArtifactSuiteGroups = map (\declaration -> declaration{declaredGroup = (declaredGroup declaration){groupPartition = "package-store-codeartifact"}})

-- | Independent completion claims require every format, regardless of actual registrations.
validateCodeArtifact :: IO ()
validateCodeArtifact = for_ ["codeartifact", "codeartifact-pypi", "codeartifact-cargo"] $ \subject ->
    either (failRunner . T.unlines) (const (pure ())) (validateCompletion (packageStoreCompletion subject) [codeArtifactCatalogue])

codeArtifactMaintenance :: Ecosystem -> CA.PackageFormat -> (OriginClient -> StoreManifestRead) -> OriginClient -> StoreBoundary -> IO StoreMaintenance
codeArtifactMaintenance ecosystem expectedFormat readManifest origin boundary = do
    format <- maybe (fail "ecosystem has no CodeArtifact format") pure (codeArtifactFormat ecosystem)
    let store =
            CodeArtifactStore
                { casDomain = "contract-domain"
                , casDomainOwner = "123456789012"
                , casRegion = "us-east-1"
                , casRepository = "contract-store"
                , casFormat = format
                }
    pure (maintenanceFor (mkNameAlphabet "c") (readManifest origin) store (controlPlane expectedFormat boundary))

controlPlane :: CA.PackageFormat -> StoreBoundary -> ControlPlane
controlPlane expectedFormat boundary =
    ControlPlane
        { cpRead =
            ReadPlane
                { rpListPackages = \request -> do
                    releases <- sbReleases boundary
                    pure $ do
                        checkRepository (request ^. CAL.listPackages_domain) (request ^. CAL.listPackages_domainOwner) (request ^. CAL.listPackages_repository)
                        unless (request ^. CAL.listPackages_format == Just expectedFormat) (Left (protocolFault "list packages used the wrong format"))
                        unless (isNothing (request ^. CAL.listPackages_namespace)) (Left (protocolFault "list packages used a namespace"))
                        unless (isNothing (request ^. CAL.listPackages_nextToken)) (Left (protocolFault "unexpected package pagination token"))
                        pure $
                            CA.newListPackagesResponse 200
                                & CAL.listPackagesResponse_packages
                                ?~ [CA.newPackageSummary & CAL.packageSummary_package ?~ "contract-package" | not (null releases)]
                , rpListVersions = \request -> do
                    releases <- sbReleases boundary
                    pure $ do
                        checkRepository (request ^. CAL.listPackageVersions_domain) (request ^. CAL.listPackageVersions_domainOwner) (request ^. CAL.listPackageVersions_repository)
                        checkPackage expectedFormat (request ^. CAL.listPackageVersions_package) (request ^. CAL.listPackageVersions_namespace) (request ^. CAL.listPackageVersions_format)
                        unless (isNothing (request ^. CAL.listPackageVersions_nextToken)) (Left (protocolFault "unexpected version pagination token"))
                        pure $
                            CA.newListPackageVersionsResponse 200
                                & CAL.listPackageVersionsResponse_versions
                                ?~ [CA.newPackageVersionSummary (psrVersion release) CA.PackageVersionStatus_Published | release <- releases]
                , rpDescribeRepository = \_ -> fail "unexpected DescribeRepository in persistence law"
                , rpDescribeUpstream = \_ -> fail "unexpected upstream probe in persistence law"
                , rpListTags = \_ -> fail "unexpected ListTagsForResource in persistence law"
                }
        , cpDeleteVersions = \request -> do
            blocked <- sbTakeDeleteFailure boundary
            let versions = request ^. CAL.deletePackageVersions_versions
                valid = do
                    checkRepository (request ^. CAL.deletePackageVersions_domain) (request ^. CAL.deletePackageVersions_domainOwner) (request ^. CAL.deletePackageVersions_repository)
                    checkPackage expectedFormat (request ^. CAL.deletePackageVersions_package) (request ^. CAL.deletePackageVersions_namespace) (request ^. CAL.deletePackageVersions_format)
                    when blocked (Left (protocolFault "injected delete transport failure"))
            case valid of
                Left fault -> pure (Left fault)
                Right () -> do
                    sbRemove boundary versions
                    pure $
                        Right $
                            CA.newDeletePackageVersionsResponse 200
                                & CAL.deletePackageVersionsResponse_successfulVersions
                                ?~ fromList [(version, CA.newSuccessfulPackageVersionInfo) | version <- versions]
        , cpTagResource = \_ -> fail "unexpected TagResource in persistence law"
        , cpUntagResource = \_ -> fail "unexpected UntagResource in persistence law"
        }

checkRepository :: Text -> Maybe Text -> Text -> Either StoreFault ()
checkRepository domain owner repository =
    unless
        (domain == "contract-domain" && owner == Just "123456789012" && repository == "contract-store")
        (Left (protocolFault "control-plane request addressed the wrong repository"))

checkPackage :: CA.PackageFormat -> Text -> Maybe Text -> CA.PackageFormat -> Either StoreFault ()
checkPackage expectedFormat package namespace format =
    unless
        (package == "contract-package" && isNothing namespace && format == expectedFormat)
        (Left (protocolFault "control-plane request addressed the wrong package or format"))

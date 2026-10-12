-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact adapter conformance over npm HTTP and a deterministic typed control plane.
module Ecluse.Runtime.Test.PackageStore (codeArtifactDriver) where

import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Maintenance (StoreFault, StoreMaintenance, protocolFault)
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), codeArtifactFormat)
import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (ControlPlane (..), maintenanceFor)
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (ReadPlane (..))
import Ecluse.Test.Contract.PackageStore (FullPackageStoreDriver, PackageStoreRelease (psrVersion))
import Ecluse.Test.Contract.PackageStore.Protocol (NpmStoreBoundary (..), npmStoreDriver, readStoreManifestOver)

-- | This proves Écluse's adapter behaviour, not remote AWS retention, IAM, or service fidelity.
codeArtifactDriver :: FullPackageStoreDriver
codeArtifactDriver = npmStoreDriver "codeartifact" codeArtifactMaintenance

codeArtifactMaintenance :: OriginClient -> NpmStoreBoundary -> IO StoreMaintenance
codeArtifactMaintenance origin boundary = do
    format <- maybe (fail "npm has no CodeArtifact format") pure (codeArtifactFormat Npm)
    let store =
            CodeArtifactStore
                { casDomain = "contract-domain"
                , casDomainOwner = "123456789012"
                , casRegion = "us-east-1"
                , casRepository = "contract-store"
                , casFormat = format
                }
    pure (maintenanceFor (mkNameAlphabet "c") (readStoreManifestOver origin) store (controlPlane boundary))

controlPlane :: NpmStoreBoundary -> ControlPlane
controlPlane boundary =
    ControlPlane
        { cpRead =
            ReadPlane
                { rpListPackages = \request -> do
                    releases <- nsbReleases boundary
                    pure $ do
                        checkRepository (request ^. CAL.listPackages_domain) (request ^. CAL.listPackages_domainOwner) (request ^. CAL.listPackages_repository)
                        unless (request ^. CAL.listPackages_format == Just CA.PackageFormat_Npm) (Left (protocolFault "list packages used the wrong format"))
                        unless (isNothing (request ^. CAL.listPackages_nextToken)) (Left (protocolFault "unexpected package pagination token"))
                        pure $
                            CA.newListPackagesResponse 200
                                & CAL.listPackagesResponse_packages
                                ?~ [CA.newPackageSummary & CAL.packageSummary_package ?~ "contract-package" | not (null releases)]
                , rpListVersions = \request -> do
                    releases <- nsbReleases boundary
                    pure $ do
                        checkRepository (request ^. CAL.listPackageVersions_domain) (request ^. CAL.listPackageVersions_domainOwner) (request ^. CAL.listPackageVersions_repository)
                        checkPackage (request ^. CAL.listPackageVersions_package) (request ^. CAL.listPackageVersions_format)
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
            blocked <- nsbTakeDeleteFailure boundary
            let versions = request ^. CAL.deletePackageVersions_versions
                valid = do
                    checkRepository (request ^. CAL.deletePackageVersions_domain) (request ^. CAL.deletePackageVersions_domainOwner) (request ^. CAL.deletePackageVersions_repository)
                    checkPackage (request ^. CAL.deletePackageVersions_package) (request ^. CAL.deletePackageVersions_format)
                    when blocked (Left (protocolFault "injected delete transport failure"))
            case valid of
                Left fault -> pure (Left fault)
                Right () -> do
                    nsbRemove boundary versions
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

checkPackage :: Text -> CA.PackageFormat -> Either StoreFault ()
checkPackage package format =
    unless
        (package == "contract-package" && format == CA.PackageFormat_Npm)
        (Left (protocolFault "control-plane request addressed the wrong package or format"))

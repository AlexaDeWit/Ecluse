-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete mirror inventories and verified per-file upload data.
module Ecluse.Core.Registry.MirrorUpload (
    MirrorReadData (..),
    mirrorReadDocument,
    readDataCompanions,
    MirrorInventory,
    ExpectedMirrorFile,
    VerifiedArtifactBytes,
    MirrorUploadData,
    checkedMirrorInventory,
    splitMirrorInventory,
    expectedIdentityMatches,
    expectedTargetMatchesWith,
    targetIdentityConflict,
    inventoryPackage,
    inventoryVersion,
    inventoryDetails,
    inventoryFiles,
    inventoryReadData,
    expectedArtifact,
    expectedDescriptor,
    verifyMirrorBytes,
    verifiedFile,
    verifiedBytes,
    verifiedTargetHash,
    uploadInventory,
    uploadFile,
    uploadBytes,
    uploadTargetHash,
    uploadMetadata,
) where

import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion), PackageName)
import Ecluse.Core.Registry.MirrorUpload.Internal
import Ecluse.Core.Version (Version)

-- | The inventory's validated package identity.
inventoryPackage :: MirrorInventory -> PackageName
inventoryPackage = pkgName . inventoryDetails

-- | The inventory's validated version identity.
inventoryVersion :: MirrorInventory -> Version
inventoryVersion = pkgVersion . inventoryDetails

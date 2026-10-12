-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete mirror inventories and verified per-file upload data.
module Ecluse.Core.Registry.MirrorUpload (
    MirrorReadData (..),
    MirrorInventory,
    ExpectedMirrorFile,
    VerifiedArtifactBytes,
    MirrorUploadData,
    checkedMirrorInventory,
    splitMirrorInventory,
    expectedIdentityMatches,
    expectedTargetMatches,
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

import Ecluse.Core.Registry.MirrorUpload.Internal

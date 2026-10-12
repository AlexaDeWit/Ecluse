-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Publication fixtures pass through production inventory and byte verification.
module Ecluse.Test.Registry.Publish (publishOnlyFile, checkedNpmUpload) where

import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion))
import Ecluse.Core.Registry (PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Npm.Publish (NpmUploadMetadata, prepareNpmSource, prepareNpmUpload)
import Ecluse.Core.Registry.Publish

-- | Exercise a single-file codec through the bound transport without bypassing byte verification.
publishOnlyFile :: MirrorPublish -> VersionDoc -> PublishPlan -> ByteString -> IO (Either PublishFault ())
publishOnlyFile publish doc plan bytes = case mpPrepareVersion publish (pkgName details) (pkgVersion details) doc of
    Left fault -> pure (Left fault)
    Right prepared -> case inventoryFiles (pmvInventory prepared) of
        file :| [] -> case verifyMirrorBytes file bytes of
            Left fault -> pure (Left fault)
            Right verified -> pmvPrepareUpload prepared file verified >>= either (pure . Left) (`publishPrepared` plan)
        _ -> pure (Left (PublishSourceUnavailable "the publication fixture requires one physical file"))
  where
    details = vdDetails doc

-- | Build npm encoder inputs with the same checked bytes as the worker.
checkedNpmUpload :: VersionDoc -> ByteString -> Either PublishFault (MirrorUploadData NpmUploadMetadata)
checkedNpmUpload doc bytes = do
    (inventory, metadata) <- prepareNpmSource (pkgName details) (pkgVersion details) doc
    case inventoryFiles inventory of
        file :| [] -> verifyMirrorBytes file bytes >>= \verified -> prepareNpmUpload inventory metadata file verified
        _ -> Left (PublishSourceUnavailable "the npm fixture requires one physical file")
  where
    details = vdDetails doc

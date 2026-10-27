-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded mirror transport with opaque prepared uploads and complete source inventories.
module Ecluse.Core.Registry.Publish (
    PublishPlan (..),
    PreparedMirrorVersion,
    pmvInventory,
    pmvPrepareUpload,
    PreparedMirrorUpload,
    publishPrepared,
    PublishCodec (..),
    MirrorCompanion (..),
    fetchVersionList,
    relayPublishRequest,
    MirrorTransport (..),
    MirrorPublish (..),
    newMirrorPublish,
) where

import Ecluse.Core.Registry (PublishFault)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, MirrorInventory, VerifiedArtifactBytes)
import Ecluse.Core.Registry.Publish.Internal

-- | The complete expected source inventory bound to this preparation capability.
pmvInventory :: PreparedMirrorVersion -> MirrorInventory
pmvInventory = pmvInventoryValue

-- | Prepare one verified member without accepting replacement bytes.
pmvPrepareUpload :: PreparedMirrorVersion -> (Int -> IO ()) -> ExpectedMirrorFile -> VerifiedArtifactBytes -> IO (Either PublishFault PreparedMirrorUpload)
pmvPrepareUpload = pmvPrepareUploadAction

-- | Run the bound write with its captured verified body and required upload metadata.
publishPrepared :: PreparedMirrorUpload -> PublishPlan -> IO (Either PublishFault ())
publishPrepared = runPreparedUpload

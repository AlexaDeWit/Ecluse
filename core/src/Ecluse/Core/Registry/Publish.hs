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
    fetchVersionList,
    MirrorTransport (..),
    MirrorPublish (..),
    newMirrorPublish,
) where

import Ecluse.Core.Registry.Publish.Internal

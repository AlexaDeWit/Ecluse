-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure rendering operations let measurement run before production charges are calibrated.
module Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..), renderingOf) where

import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry.Adapter.Capability qualified as Adapter
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.ServedDocument (RenderRefused)
import Ecluse.Core.Snapshot (Snapshot)

-- | Assembly and serialisation need no admission charge or live adapter registration.
data MetadataRendering = MetadataRendering
    { metadataAssemble :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
    , metadataSerialise :: CachedDoc -> Either RenderRefused LByteString
    }

-- | Reuse the registered adapter's rendering functions without reading its factors.
renderingOf :: Adapter.AdapterMetadata -> MetadataRendering
renderingOf adapter = MetadataRendering (Adapter.metadataAssemble adapter) (Adapter.metadataSerialise adapter)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepare metadata reads only after the caller selects an origin and its credentials.
module Ecluse.Core.Registry.Metadata.Fetch.Preparation (
    PrepareRead (..),
    staticRead,
    prepareManifest,
    prepareVersion,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Metadata (MetadataError, VersionRead)
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest, fetchVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead, ManifestFetch)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version)

-- | Resolve one operation's metadata reader under its selected origin's authority and limits.
newtype PrepareRead = PrepareRead
    { prepareRead :: TracingPort -> OriginClient -> IO (Either MetadataError EcosystemRead)
    }

-- | Use a reader that needs no origin discovery or other preparation exchange.
staticRead :: EcosystemRead -> PrepareRead
staticRead nativeRead = PrepareRead (\_ _ -> pure (Right nativeRead))

-- | Prepare within the full-read operation, so cache hits need no preparation.
prepareManifest :: PrepareRead -> ManifestFetch
prepareManifest factory tracing origin name =
    prepareRead factory tracing origin >>= either (pure . Left) (\nativeRead -> fetchManifest nativeRead tracing origin name)

-- | Prepare independently for each selected-read operation under its actual credentials.
prepareVersion :: PrepareRead -> TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
prepareVersion factory tracing origin name version =
    prepareRead factory tracing origin >>= either (pure . Left) (\nativeRead -> fetchVersion nativeRead tracing origin name version)

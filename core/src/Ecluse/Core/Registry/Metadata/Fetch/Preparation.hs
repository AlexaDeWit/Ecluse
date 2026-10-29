-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepare metadata reads only after the caller selects an origin and its credentials.
module Ecluse.Core.Registry.Metadata.Fetch.Preparation (
    PrepareRead (PrepareRead),
    staticRead,
    prepareRead,
    bindToOrigin,
    canChargeFullReadWorkspace,
    prepareManifest,
    prepareVersion,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionRead)
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest, fetchVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..), ManifestFetch)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.Request (baseRequestUrl)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version)

-- | Origin-bound factories may capture operation callbacks. Static factories receive no origin.
data PrepareRead
    = StaticRead ~EcosystemRead
    | PrepareRead (TracingPort -> OriginClient -> IO (Either MetadataError EcosystemRead))

-- | Reuse a reader that cannot bind callbacks from the selected origin.
staticRead :: EcosystemRead -> PrepareRead
staticRead = StaticRead

-- | Prepare a reader under the caller-selected origin's authority and limits.
prepareRead :: PrepareRead -> TracingPort -> OriginClient -> IO (Either MetadataError EcosystemRead)
prepareRead = \case
    StaticRead nativeRead -> \_ _ -> pure (Right nativeRead)
    PrepareRead prepare -> prepare

{- | Hold a prepared reader to the origin at @root@: a request under another base is refused with
@refusal@, and a finish located against another base is undecodable.
-}
bindToOrigin :: UrlFormationError -> Text -> EcosystemRead -> EcosystemRead
bindToOrigin refusal root EcosystemRead{erRequest, erUniqueFields, erWalkFull, erFinishFull, erWalkSelected, erFinishSelected} =
    EcosystemRead
        { erRequest = \base credential name -> do
            unless (registryUrlText (baseRequestUrl base) == root) (Left refusal)
            erRequest base credential name
        , erUniqueFields
        , erWalkFull
        , erFinishFull = \limits name base streamed -> do
            unless (base == root) (Left MetadataUndecodable)
            erFinishFull limits name base streamed
        , erWalkSelected
        , erFinishSelected = \limits name base version streamed -> do
            unless (base == root) (Left MetadataUndecodable)
            erFinishSelected limits name base version streamed
        }

-- | Whether preparation can bind the selected origin's full-read workspace callback into its reader.
canChargeFullReadWorkspace :: PrepareRead -> Bool
canChargeFullReadWorkspace = \case
    StaticRead{} -> False
    PrepareRead{} -> True

-- | Prepare within the full-read operation, so cache hits need no preparation.
prepareManifest :: PrepareRead -> ManifestFetch
prepareManifest = \case
    StaticRead nativeRead -> fetchManifest nativeRead
    PrepareRead prepare -> \tracing origin name ->
        prepare tracing origin >>= either (pure . Left) (\nativeRead -> fetchManifest nativeRead tracing origin name)

-- | Prepare independently for each selected-read operation under its actual credentials.
prepareVersion :: PrepareRead -> TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
prepareVersion = \case
    StaticRead nativeRead -> fetchVersion nativeRead
    PrepareRead prepare -> \tracing origin name version ->
        prepare tracing origin >>= either (pure . Left) (\nativeRead -> fetchVersion nativeRead tracing origin name version)

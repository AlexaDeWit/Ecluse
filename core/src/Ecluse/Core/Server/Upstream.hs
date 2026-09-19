-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A mount's configured upstreams and the tarball-host gate derived from them, as one opaque
cluster with a private constructor.

The 'Ecluse.Core.Security.TarballHostGate' is built once per mount from the three upstream URLs,
so the hot artifact path re-parses nothing. A gate that disagreed with those URLs would silently
authorise the wrong authorities, so 'mountUpstreams' is the only builder and neither the
constructor nor the selectors are exported: an @upstreams{...}@ update alone would break the pair.
-}
module Ecluse.Core.Server.Upstream (
    -- * Mirror serve plan
    MirrorServePlan (..),

    -- * The upstream cluster
    MountUpstreams,
    mountUpstreams,
    upstreamPrivateBaseUrl,
    upstreamPublicBaseUrl,
    upstreamMirror,
    upstreamTarballHostGate,
) where

import Ecluse.Core.Security (TarballHostGate, tarballHostGate)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

{- | Whether an admitted public artifact is enqueued for the demand-driven mirror, and where that
write lands. A serve-only mount opens no producer span and emits no enqueue metric.
-}
data MirrorServePlan
    = {- | Enqueue admitted public artifacts for publication to this mirror-target endpoint. The
      worker resolves its publish capability from the same configuration.
      -}
      MirrorOnAdmit RegistryUrl
    | -- | Serve-only: admitted public artifacts stream to the client and are mirrored nowhere.
      NoMirrorWrite
    deriving stock (Eq, Show)

{- | A mount's three configured upstreams and the tarball-host gate they derive. Exported
__abstract__, so the carried gate is always the gate of the carried URLs.
-}
data MountUpstreams = MountUpstreams
    { muPrivateBaseUrl :: Maybe RegistryUrl
    , muPublicBaseUrl :: RegistryUrl
    , muMirror :: MirrorServePlan
    , muTarballHostGate :: TarballHostGate
    }
    deriving stock (Eq, Show)

{- | Bind a mount's upstreams. This is the only caller of 'Ecluse.Core.Security.tarballHostGate'
outside that gate's own specs, so the allowlist and the reference authorities have one derivation.
-}
mountUpstreams :: [Text] -> Maybe RegistryUrl -> RegistryUrl -> MirrorServePlan -> MountUpstreams
mountUpstreams ecosystemHostUrls privateBaseUrl publicBaseUrl mirror =
    MountUpstreams
        { muPrivateBaseUrl = privateBaseUrl
        , muPublicBaseUrl = publicBaseUrl
        , muMirror = mirror
        , -- The gate reasons over authorities, so it takes the URLs as text. That is the
          -- one place the egress witness is read for its characters.
          muTarballHostGate =
            tarballHostGate
                ecosystemHostUrls
                (registryUrlText <$> privateBaseUrl)
                (registryUrlText publicBaseUrl)
                (registryUrlText <$> mirrorTargetUrl mirror)
        }

-- The mirror target's URL, or 'Nothing' for a serve-only mount. It is the third
-- authority that feeds the gate's allowlist.
mirrorTargetUrl :: MirrorServePlan -> Maybe RegistryUrl
mirrorTargetUrl = \case
    MirrorOnAdmit url -> Just url
    NoMirrorWrite -> Nothing

{- | The private upstream base URL. 'Nothing' when the mount has no private upstream, so
the private leg is structurally absent rather than misconfigured.
-}
upstreamPrivateBaseUrl :: MountUpstreams -> Maybe RegistryUrl
upstreamPrivateBaseUrl = muPrivateBaseUrl

-- | The public upstream base URL.
upstreamPublicBaseUrl :: MountUpstreams -> RegistryUrl
upstreamPublicBaseUrl = muPublicBaseUrl

-- | The mirror serve plan, carrying the mirror-target endpoint when there is one.
upstreamMirror :: MountUpstreams -> MirrorServePlan
upstreamMirror = muMirror

{- | The tarball-host gate of these upstreams: the canonicalised @host:port@ allowlist and the
private and public reference authorities the per-request SSRF check decides against.
-}
upstreamTarballHostGate :: MountUpstreams -> TarballHostGate
upstreamTarballHostGate = muTarballHostGate

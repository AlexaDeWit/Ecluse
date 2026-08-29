-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A mount's configured upstreams and the tarball-host gate derived from them, held
together as one __opaque cluster with a private constructor__.

The 'Ecluse.Core.Security.TarballHostGate' is built once per mount from the three upstream
URLs, so the hot artifact path re-parses nothing. A gate that disagreed with those URLs would
silently authorise the wrong authorities, so 'mountUpstreams' is the only builder and neither
the constructor nor the selectors are exported: @upstreams{...}@ alone would break the pair.
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

{- | Whether an admitted public artifact is enqueued for the demand-driven mirror, and
where that write lands. The discriminant is an absent capability, not a no-op handle.
A serve-only mount opens no mirror producer span and emits no enqueue metric, so the
telemetry never claims work that cannot happen.
-}
data MirrorServePlan
    = {- | Enqueue admitted public artifacts for publication to this mirror-target
      endpoint, the mount's declared destination. The worker resolves its publish
      capability from the same configuration.
      -}
      MirrorOnAdmit RegistryUrl
    | {- | Serve-only: admitted public artifacts stream to the client and are never
      mirrored anywhere. Every artifact stays on the gated public leg.
      -}
      NoMirrorWrite
    deriving stock (Eq, Show)

{- | A mount's three configured upstreams and the tarball-host gate they derive, as one
value. Exported __abstract__: 'mountUpstreams' is the only way to obtain one, so the
carried gate is always the gate of the carried URLs. The derived 'Eq' and 'Show' serve
tests and debugging (an assertion's failure message, a fixture comparison). They are not
a way around the builder.
-}
data MountUpstreams = MountUpstreams
    { muPrivateBaseUrl :: Maybe RegistryUrl
    , muPublicBaseUrl :: RegistryUrl
    , muMirror :: MirrorServePlan
    , muTarballHostGate :: TarballHostGate
    }
    deriving stock (Eq, Show)

{- | Bind a mount's upstreams: the ecosystem's canonical artifact hosts, the private upstream
base URL, the public upstream base URL, and the mirror serve plan. The artifact-host list is
empty for an ecosystem like npm that serves artifacts from its registry host, and the private
base URL is 'Nothing' for a serve-only public gate.

This builder is the only caller of 'Ecluse.Core.Security.tarballHostGate' outside that gate's
own specs, so a mount's allowlist and reference authorities have one derivation that no second
argument list can drift from.
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

{- | The tarball-host gate of these upstreams: the canonicalised @host:port@ allowlist
and the private and public reference authorities. The per-request SSRF check decides an
honoured artifact location against them.
-}
upstreamTarballHostGate :: MountUpstreams -> TarballHostGate
upstreamTarballHostGate = muTarballHostGate

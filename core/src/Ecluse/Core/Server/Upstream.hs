-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A mount's configured upstreams and the tarball-host gate derived from them, held
together as one __opaque cluster with a private constructor__.

The SSRF gate on an honoured @dist.tarball@ location is decided against a
'Ecluse.Core.Security.TarballHostGate' built from a mount's three configured upstream
URLs: the private upstream, the public upstream, and the mirror target. Building it once
per mount is what keeps the hot artifact path from rebuilding a host set and re-parsing
those URLs on every request, but it also makes the gate a __cached projection__: a gate
that disagrees with the URLs beside it authorises the wrong authorities, and does so
silently, because nothing about a stale pair is ill-typed.

'MountUpstreams' is the answer: the URLs and the gate are one value, 'mountUpstreams' is
its only builder, and that builder derives the gate from the very URLs it stores. A
divergent pair is therefore unconstructible rather than merely discouraged, and the
per-request gate can trust what it reads.

Neither the constructor nor the record selectors are exported. Hiding the constructor
alone would not be enough: record-update syntax needs only a selector in scope, so an
exported selector would leave @upstreams{...}@ free to replace a URL and leave the gate
behind, which is exactly the divergence the type exists to forbid. The exported surface
is four accessor functions, which read and cannot write.
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

{- | Whether an admitted public artifact is enqueued for the demand-driven mirror, and
where that write lands. The discriminant is an absent capability, not a no-op handle:
a serve-only mount opens no mirror producer span and emits no enqueue metric, so the
telemetry never claims work that cannot happen.
-}
data MirrorServePlan
    = {- | Enqueue admitted public artifacts for publication to this mirror-target
      endpoint (the mount's declared destination; the worker resolves its publish
      capability from the same configuration).
      -}
      MirrorOnAdmit Text
    | {- | Serve-only: admitted public artifacts stream to the client and are never
      mirrored anywhere. Every artifact stays on the gated public leg.
      -}
      NoMirrorWrite
    deriving stock (Eq, Show)

{- | A mount's three configured upstreams and the tarball-host gate they derive, as one
value. Exported __abstract__: 'mountUpstreams' is the only way to obtain one, so the
carried gate is always the gate of the carried URLs. The derived 'Eq' and 'Show' are a
test and debug affordance (an assertion's failure message, a fixture comparison), not a
way around the builder.
-}
data MountUpstreams = MountUpstreams
    { muPrivateBaseUrl :: Maybe Text
    , muPublicBaseUrl :: Text
    , muMirror :: MirrorServePlan
    , muTarballHostGate :: TarballHostGate
    }
    deriving stock (Eq, Show)

{- | Bind a mount's upstreams: the ecosystem's canonical artifact hosts (empty for an
ecosystem, like npm, that serves artifacts from its registry host), the private upstream
base URL ('Nothing' for a serve-only pure public gate), the public upstream base URL, and
the mirror serve plan.

The tarball-host gate falls out of these arguments rather than being supplied beside
them: across the library and the application this is the only caller of
'Ecluse.Core.Security.tarballHostGate' (the gate's own specs call it directly, as an
oracle), so a mount's allowlist and reference authorities have one derivation and cannot
be assembled from a second, drifting argument list. The composition root calls this once
per mount (test fixtures likewise); the result is carried on the serve dependencies, so
the per-request gate reads precomputed fields.
-}
mountUpstreams :: [Text] -> Maybe Text -> Text -> MirrorServePlan -> MountUpstreams
mountUpstreams ecosystemHostUrls privateBaseUrl publicBaseUrl mirror =
    MountUpstreams
        { muPrivateBaseUrl = privateBaseUrl
        , muPublicBaseUrl = publicBaseUrl
        , muMirror = mirror
        , muTarballHostGate = tarballHostGate ecosystemHostUrls privateBaseUrl publicBaseUrl (mirrorTargetUrl mirror)
        }

-- The mirror target's URL, or 'Nothing' for a serve-only mount: the third authority the
-- gate's allowlist is built from.
mirrorTargetUrl :: MirrorServePlan -> Maybe Text
mirrorTargetUrl = \case
    MirrorOnAdmit url -> Just url
    NoMirrorWrite -> Nothing

{- | The private upstream base URL; 'Nothing' when the mount has no private upstream, so
the private leg is structurally absent rather than misconfigured.
-}
upstreamPrivateBaseUrl :: MountUpstreams -> Maybe Text
upstreamPrivateBaseUrl = muPrivateBaseUrl

-- | The public upstream base URL.
upstreamPublicBaseUrl :: MountUpstreams -> Text
upstreamPublicBaseUrl = muPublicBaseUrl

-- | The mirror serve plan, carrying the mirror-target endpoint when there is one.
upstreamMirror :: MountUpstreams -> MirrorServePlan
upstreamMirror = muMirror

{- | The tarball-host gate of these upstreams: the canonicalised @host:port@ allowlist
and the private and public reference authorities the per-request SSRF check is decided
against.
-}
upstreamTarballHostGate :: MountUpstreams -> TarballHostGate
upstreamTarballHostGate = muTarballHostGate

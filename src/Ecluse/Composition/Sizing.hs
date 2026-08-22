-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The config-derived runtime sizings of the composition root: serve-admission
capacity, the two connection-pool sizes, the file-descriptor datapoint behind them,
and the mirror-enqueue buffer tunables. A separate partition of the heap ceiling
covers the byte-valued bounds ("Ecluse.Composition.MemoryPlan").

Each resolution is a pure function of the validated configuration, plus, for the
pools, the process file-descriptor limit 'openFileSoftLimit' reads once. An explicit
config value always wins. A computed default returns its boot-log line alongside the
number, so the decision's provenance lands in the standard boot log. Nothing here
opens a socket or reads a clock. The composition root applies the results when it
builds the managers and the admission gate.
-}
module Ecluse.Composition.Sizing (
    -- * Connection pools and admission
    connectionPoolSettings,
    resolveServeAdmission,
    resolvePrivateConnections,
    resolvePublicConnections,
    openFileSoftLimit,

    -- * Mirror-enqueue buffering
    mirrorEnqueueBufferDepth,
    mirrorEnqueueReportInterval,
) where

import Network.HTTP.Client (ManagerSettings (managerConnCount))
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (ResourceLimit, ResourceLimitInfinity, ResourceLimitUnknown), ResourceLimits (softLimit), getResourceLimit)

{- | Apply an explicit per-host connection bound to an HTTP manager's settings.

The public and private managers call this independently after telemetry
instrumentation, so changing the pool size cannot discard the instrumented request and
response hooks.
-}
connectionPoolSettings :: Int -> ManagerSettings -> ManagerSettings
connectionPoolSettings connections settings = settings{managerConnCount = connections}

{- | The effective serve-admission capacity and its boot-log line. It is the explicit
@serveMaxInFlight@ when configured, else __computed from the resolved capability
count__ as @max 8 (10 x capabilities)@.

The multiplier is empirical, not modelled. In the saturation model, an admitted
metadata materialisation alternates upstream wait @W@ and CPU work @P@. Keeping @C@
capabilities busy therefore wants about @C x (W + P) \/ P@ in flight. At a round-trip
@W\/P@ of 2-3 that is about 4 per capability. The load bench's measured dose-response
kept climbing well past that and levelled only near 10 per capability. One request
holds a slot across every upstream leg plus GC pauses and scheduling delay, so the
effective @W\/P@ is nearer 9-10. The floor keeps a tiny pod admitting a useful burst
should the multiplier ever drop below it. The capability count must be the
__post-runtime-posture__ one (see "Ecluse.Rts"), so callers resolve this after
'Ecluse.Runtime.applyRuntimePosture' runs.

The returned line carries the decision's provenance for the standard boot log,
alongside the runtime posture lines. This bounds only __metadata materialisation__:
whole packument requests, and a tarball miss's public-metadata gate. It does __not__
size the __private__ connection pool (see 'resolvePrivateConnections'). A trusted
tarball hit __streams outside admission__, so the private pool's demand is the
inbound hit concurrency, not the admission capacity. Tying the two would undersize
that pool under a private-hit fan-out. Beyond the pool, http-client opens throwaway
connections and pays a TLS handshake per overflow request.
-}
resolveServeAdmission :: Maybe Int -> Int -> (Int, Text)
resolveServeAdmission explicit capabilities = case explicit of
    Just n -> (n, "runtime: serve admission " <> show n <> " (from config)")
    Nothing ->
        let computed = max serveAdmissionFloor (serveAdmissionPerCapability * capabilities)
         in (computed, "runtime: serve admission " <> show computed <> " (computed from " <> show capabilities <> " capabilities)")

-- The computed-admission constants. The multiplier is empirically about 10 per
-- capability (see 'resolveServeAdmission'). The floor keeps a tiny pod admitting a
-- useful burst if the multiplier ever drops below it.
serveAdmissionPerCapability :: Int
serveAdmissionPerCapability = 10

serveAdmissionFloor :: Int
serveAdmissionFloor = 8

{- | The effective private-upstream connection-pool size and its boot-log line. It is
the explicit @privateConnectionsPerHost@ when configured, else __computed from the
process file-descriptor limit__ as @clamp 64 4096 (nofile \/ 4)@.

The private pool caches idle connections to the trusted upstream for __reuse across
concurrent private-hit tarball streams__. Those streams are __IO-bound__ and, unlike
metadata materialisation, stream __outside serve admission__. Their concurrency, and
so the pool's real demand, is the inbound hit fan-out, not the CPU-saturation model
'resolveServeAdmission' uses. That is why this size comes from a different datapoint
and is __not__ tied to @serveMaxInFlight@: the private pool also serves the
un-admitted streaming path.

Each pooled connection is one file descriptor, so the file-descriptor limit is the
pool's real physical ceiling. The default takes a __quarter of the soft
@RLIMIT_NOFILE@__ as the reuse cache. The floor at 'privateConnectionsFloor' keeps a
small-limit host reusing connections across an install fan-out. The cap at
'privateConnectionsCap' stops an enormous-limit host retaining an absurd idle cache
to a single upstream. A larger pool never opens more sockets than the concurrency
already demands, because http-client opens a connection per in-flight request
regardless. It only decides how many to __retain for reuse__ rather than
re-handshake, so sizing up is safe. An operator who knows their fan-out can override
it outright.

The returned line carries the decision's provenance for the standard boot log.
-}
resolvePrivateConnections :: Maybe Int -> Int -> (Int, Text)
resolvePrivateConnections explicit fdLimit = case explicit of
    Just n -> (n, "runtime: private connection pool " <> show n <> " (from config)")
    Nothing ->
        let computed = clampPrivateConnections (fdLimit `div` privateConnectionsFdShare)
         in (computed, "runtime: private connection pool " <> show computed <> " (computed from file-descriptor limit " <> show fdLimit <> ")")

-- Clamp a computed private-pool size into the sane band. The floor keeps a small
-- file-descriptor limit reusing a useful number of connections. The cap stops an
-- enormous limit retaining an absurd idle cache to one upstream.
clampPrivateConnections :: Int -> Int
clampPrivateConnections = max privateConnectionsFloor . min privateConnectionsCap

-- The private pool takes a quarter of the file-descriptor budget as its reuse cache,
-- one descriptor per pooled connection. The other three quarters stay for the
-- listener, the public pool, telemetry, the worker, and the runtime.
privateConnectionsFdShare :: Int
privateConnectionsFdShare = 4

privateConnectionsFloor :: Int
privateConnectionsFloor = 64

privateConnectionsCap :: Int
privateConnectionsCap = 4096

{- | The effective public-upstream connection-pool size and its boot-log line. It is
the explicit @publicConnectionsPerHost@ when configured, else __computed from the
process file-descriptor limit__ as @clamp 32 1024 (nofile \/ 8)@.

The public pool's metadata demand is small by construction: same-key misses coalesce
under single-flight, and admission bounds them. The pool is __not__ metadata-only,
though. The onboarding fail-over's artifact streams and the mirror worker's back-fill
fetches ride the same manager, and neither coalesces. During a cold fleet's
onboarding burst the concurrent public streams track the inbound fan-out.
'Network.HTTP.Client.managerConnCount' is a keep-alive __retention__ cap, not a
concurrency cap: overflow opens throwaway connections, each paying a TLS handshake to
the public origin per request.

So this pool follows the private one, sized from the file-descriptor budget at __half
the private share__. That is an eighth of @nofile@, out of the three quarters the
private sizing reserves for everything else. The public leg is transient by the
traffic model, because the worker retires it artifact by artifact. It earns retention
for the burst, not the steady state. Sizing up is safe for the same reason as the
private pool. It never opens more sockets than the concurrency already demands, and
only retains more for reuse.

The returned line carries the decision's provenance for the standard boot log.
-}
resolvePublicConnections :: Maybe Int -> Int -> (Int, Text)
resolvePublicConnections explicit fdLimit = case explicit of
    Just n -> (n, "runtime: public connection pool " <> show n <> " (from config)")
    Nothing ->
        let computed = clampPublicConnections (fdLimit `div` publicConnectionsFdShare)
         in (computed, "runtime: public connection pool " <> show computed <> " (computed from file-descriptor limit " <> show fdLimit <> ")")

-- Clamp a computed public-pool size into its sane band, for the same reasons as
-- 'clampPrivateConnections'. The floor keeps a small limit reusing connections
-- across an onboarding burst. The cap stops an enormous limit retaining an absurd
-- idle cache to one public origin.
clampPublicConnections :: Int -> Int
clampPublicConnections = max publicConnectionsFloor . min publicConnectionsCap

-- The public pool takes an eighth of the file-descriptor budget: half the private
-- share, drawn from the reserve the private sizing leaves. The public leg is the
-- transient onboarding ramp, not the steady-state workhorse.
publicConnectionsFdShare :: Int
publicConnectionsFdShare = 8

publicConnectionsFloor :: Int
publicConnectionsFloor = 32

publicConnectionsCap :: Int
publicConnectionsCap = 1024

{- | The depth of the producer-side hand-off buffer the composition root wraps in
front of the mirror queue ('Ecluse.Core.Queue.newEnqueueBuffer'). It absorbs a cold
@npm ci@'s enqueue burst (a lockfile fan-out enqueues one job per public-served
tarball) while bounding memory. A job dropped at the cap re-enqueues on the next
demand for its artifact, so overflow costs a deferred mirror, never correctness.
-}
mirrorEnqueueBufferDepth :: Int
mirrorEnqueueBufferDepth = 1024

{- | How many enqueue-buffer drops or delivery failures pass between warning-log
reports at the composition root (the first is always reported, then every multiple
of this). The buffer's callbacks fire per event, so the failure counter stays exact.
A sustained flood then logs one line per this many events rather than one per job.
-}
mirrorEnqueueReportInterval :: Int
mirrorEnqueueReportInterval = 100

{- | The process soft file-descriptor limit (@RLIMIT_NOFILE@), the datapoint
'resolvePrivateConnections' sizes the private pool against. An __infinite__ or
__unknown__ limit falls back to @privateConnectionsCap x privateConnectionsFdShare@, so
the computed pool lands at the cap rather than overflowing.
-}
openFileSoftLimit :: IO Int
openFileSoftLimit = do
    limits <- getResourceLimit ResourceOpenFiles
    pure $ case softLimit limits of
        ResourceLimit n -> fromInteger n
        ResourceLimitInfinity -> privateConnectionsCap * privateConnectionsFdShare
        ResourceLimitUnknown -> privateConnectionsCap * privateConnectionsFdShare

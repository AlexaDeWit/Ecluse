-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The config-derived runtime sizings of the composition root: serve-admission
capacity, the two connection-pool sizes, the file-descriptor datapoint behind them,
and the mirror-enqueue buffer tunables. A separate partition of the heap ceiling
covers the byte-valued bounds ("Ecluse.Composition.MemoryPlan").

Each resolution is a pure function of the validated configuration, plus, for the
pools, the process file-descriptor limit 'openFileSoftLimit' reads once. That read is
the module's only effect: nothing here opens a socket or reads a clock. An explicit
config value always wins, and 'resolveSized' pairs the result with the boot-log line
naming its provenance. The composition root applies the results when it builds the
managers and the admission gate.
-}
module Ecluse.Composition.Sizing (
    -- * A resolved bound and its boot-log line
    resolveSized,
    renderSized,

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

import Data.Ord (clamp)
import Network.HTTP.Client (ManagerSettings (managerConnCount))
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (ResourceLimit, ResourceLimitInfinity, ResourceLimitUnknown), ResourceLimits (softLimit), getResourceLimit)

{- | A resolved bound and its boot-log line: an explicit config value wins, else the
computed default. Every sizing and every memory-plan bound resolves through this.
-}
resolveSized :: Text -> Maybe Int -> Int -> Text -> (Int, Text)
resolveSized subject explicit computed computedClause =
    (value, renderSized subject value explicit computedClause)
  where
    value = fromMaybe computed explicit

{- | The boot-log line for a bound already resolved elsewhere. The explicit config value
decides the provenance clause, and the caller supplies the computed alternative.
-}
renderSized :: Text -> Int -> Maybe Int -> Text -> Text
renderSized subject value explicit computedClause =
    subject <> " " <> show value <> " (" <> provenance <> ")"
  where
    provenance = if isJust explicit then "from config" else computedClause

{- | Apply an explicit per-host connection bound to an HTTP manager's settings. Callers
apply it after telemetry instrumentation, so it cannot discard the instrumented hooks.
-}
connectionPoolSettings :: Int -> ManagerSettings -> ManagerSettings
connectionPoolSettings connections settings = settings{managerConnCount = connections}

{- | The effective serve-admission capacity and its boot-log line: the explicit
@serveMaxInFlight@, else @max 8 (10 x capabilities)@. The multiplier is empirical, not
modelled, because the load bench's dose-response levelled near 10 per capability.
Callers resolve this after 'Ecluse.Runtime.applyRuntimePosture' runs, so the capability
count is the post-posture one. It bounds metadata materialisation only.
-}
resolveServeAdmission :: Maybe Int -> Int -> (Int, Text)
resolveServeAdmission explicit capabilities =
    resolveSized
        "runtime: serve admission"
        explicit
        (max serveAdmissionFloor (serveAdmissionPerCapability * capabilities))
        ("computed from " <> show capabilities <> " capabilities")

-- The floor keeps a tiny pod admitting a useful burst. 'resolveServeAdmission' explains
-- the multiplier.
serveAdmissionPerCapability :: Int
serveAdmissionPerCapability = 10

serveAdmissionFloor :: Int
serveAdmissionFloor = 8

{- | The effective private-upstream connection-pool size and its boot-log line: the
explicit @privateConnectionsPerHost@, else @clamp (64, 4096) (nofile \/ 4)@. It is not tied
to @serveMaxInFlight@, because private-hit tarball streams run outside serve admission
and their concurrency is the inbound fan-out. 'Network.HTTP.Client.managerConnCount'
caps retention, not concurrency, so sizing up retains more idle connections for reuse
and never opens more sockets.
-}
resolvePrivateConnections :: Maybe Int -> Int -> (Int, Text)
resolvePrivateConnections explicit fdLimit =
    resolveSized
        "runtime: private connection pool"
        explicit
        (clampPrivateConnections (fdLimit `div` privateConnectionsFdShare))
        (fdLimitClause fdLimit)

-- The floor keeps a small file-descriptor limit reusing a useful number of connections.
-- The cap stops an enormous limit retaining an absurd idle cache to one upstream.
clampPrivateConnections :: Int -> Int
clampPrivateConnections = clamp (privateConnectionsFloor, privateConnectionsCap)

-- One descriptor per pooled connection. The private pool takes a quarter of the budget
-- and leaves the rest to the listener, the public pool, telemetry, the worker, and the runtime.
privateConnectionsFdShare :: Int
privateConnectionsFdShare = 4

privateConnectionsFloor :: Int
privateConnectionsFloor = 64

privateConnectionsCap :: Int
privateConnectionsCap = 4096

{- | The effective public-upstream connection-pool size and its boot-log line: the
explicit @publicConnectionsPerHost@, else @clamp (32, 1024) (nofile \/ 8)@, half the private
share. The pool is not metadata-only: onboarding fail-over artifact streams and the
worker's back-fill fetches ride the same manager and do not coalesce, so an onboarding
burst tracks the inbound fan-out. Sizing up is safe for the reason
'resolvePrivateConnections' gives.
-}
resolvePublicConnections :: Maybe Int -> Int -> (Int, Text)
resolvePublicConnections explicit fdLimit =
    resolveSized
        "runtime: public connection pool"
        explicit
        (clampPublicConnections (fdLimit `div` publicConnectionsFdShare))
        (fdLimitClause fdLimit)

fdLimitClause :: Int -> Text
fdLimitClause fdLimit = "computed from file-descriptor limit " <> show fdLimit

-- The floor keeps a small limit reusing connections across an onboarding burst. The cap
-- and the reasoning match 'clampPrivateConnections'.
clampPublicConnections :: Int -> Int
clampPublicConnections = clamp (publicConnectionsFloor, publicConnectionsCap)

-- An eighth of the file-descriptor budget, drawn from the reserve the private sizing
-- leaves. The public leg is the transient onboarding ramp, not the steady-state load.
publicConnectionsFdShare :: Int
publicConnectionsFdShare = 8

publicConnectionsFloor :: Int
publicConnectionsFloor = 32

publicConnectionsCap :: Int
publicConnectionsCap = 1024

{- | The depth of the hand-off buffer in front of the mirror queue
('Ecluse.Core.Queue.newEnqueueBuffer'). It absorbs a cold @npm ci@ burst while bounding
memory. A job dropped at the cap re-enqueues on the next demand, so overflow defers a
mirror rather than losing it.
-}
mirrorEnqueueBufferDepth :: Int
mirrorEnqueueBufferDepth = 1024

{- | How many enqueue-buffer drops or delivery failures pass between warning-log reports.
The composition root reports the first, then every multiple of this. The buffer's
callbacks still fire per event, so the counter stays exact.
-}
mirrorEnqueueReportInterval :: Int
mirrorEnqueueReportInterval = 100

{- | The process soft file-descriptor limit (@RLIMIT_NOFILE@). An infinite or unknown
limit falls back to @privateConnectionsCap x privateConnectionsFdShare@, so the computed
pool lands on the cap rather than overflowing.
-}
openFileSoftLimit :: IO Int
openFileSoftLimit = do
    limits <- getResourceLimit ResourceOpenFiles
    pure $ case softLimit limits of
        ResourceLimit n -> fromInteger n
        ResourceLimitInfinity -> privateConnectionsCap * privateConnectionsFdShare
        ResourceLimitUnknown -> privateConnectionsCap * privateConnectionsFdShare

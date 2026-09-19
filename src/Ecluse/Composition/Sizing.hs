-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The config-derived runtime sizings of the composition root: serve-admission capacity,
the two connection-pool sizes and the managers built under them, and the mirror-enqueue
buffer tunables. The byte-valued bounds live in "Ecluse.Composition.MemoryPlan".

Each resolution is a pure function of the validated configuration plus the process
file-descriptor limit. An explicit config value always wins, and 'resolveSized' pairs the
result with the boot-log line naming its provenance.
-}
module Ecluse.Composition.Sizing (
    -- * A resolved bound and its boot-log line
    resolveSized,
    renderSized,

    -- * Connection pools and admission
    newPooledManager,
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
import Network.HTTP.Client (Manager, ManagerSettings (managerConnCount), newManager)
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (ResourceLimit, ResourceLimitInfinity, ResourceLimitUnknown), ResourceLimits (softLimit), getResourceLimit)

{- | A resolved bound and its boot-log line: an explicit config value wins, else the
computed default. Every sizing and every memory-plan bound resolves through this.
-}
resolveSized :: (Show a) => Text -> Maybe a -> a -> Text -> (a, Text)
resolveSized subject explicit computed computedClause =
    (value, renderSized subject value explicit computedClause)
  where
    value = fromMaybe computed explicit

{- | The boot-log line for a bound already resolved elsewhere. The explicit config value
decides the provenance clause, and the caller supplies the computed alternative.
-}
renderSized :: (Show a) => Text -> a -> Maybe a -> Text -> Text
renderSized subject value explicit computedClause =
    subject <> " " <> show value <> " (" <> provenance <> ")"
  where
    provenance = if isJust explicit then "from config" else computedClause

{- | Open an HTTP manager under an explicit per-host connection bound. Every manager the
composition root builds comes from here, so one pool bound applies whatever dials through it.
-}
newPooledManager :: Int -> ManagerSettings -> IO Manager
newPooledManager connections = newManager . connectionPoolSettings connections

{- | Apply an explicit per-host connection bound to an HTTP manager's settings. Callers
apply it after telemetry instrumentation, so it cannot discard the instrumented hooks.
-}
connectionPoolSettings :: Int -> ManagerSettings -> ManagerSettings
connectionPoolSettings connections settings = settings{managerConnCount = connections}

{- | The serve-admission capacity and its boot-log line: explicit @serveMaxInFlight@, else
@max 8 (10 x capabilities)@ on the post-posture count. The multiplier is where the bench levelled.
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

{- | The private pool size and its boot-log line: @privateConnectionsPerHost@, else
@clamp (64, 4096) (nofile \/ 4)@. 'managerConnCount' caps retention, not concurrency.
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

{- | The public pool size and its boot-log line: @publicConnectionsPerHost@, else
@clamp (32, 1024) (nofile \/ 8)@. Onboarding fail-over and back-fill streams ride it too.
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

{- | The depth of the hand-off buffer in front of the mirror queue. It absorbs a cold @npm ci@
burst, and a job dropped at the cap re-enqueues on the next demand, so overflow defers a mirror.
-}
mirrorEnqueueBufferDepth :: Int
mirrorEnqueueBufferDepth = 1024

{- | How many enqueue-buffer drops or delivery failures pass between warning-log reports. The
buffer's callbacks still fire per event, so the counter beside the log stays exact.
-}
mirrorEnqueueReportInterval :: Int
mirrorEnqueueReportInterval = 100

{- | The process soft file-descriptor limit (@RLIMIT_NOFILE@). An infinite or unknown limit falls
back to a value that lands the computed pool on its cap rather than overflowing.
-}
openFileSoftLimit :: IO Int
openFileSoftLimit = do
    limits <- getResourceLimit ResourceOpenFiles
    pure $ case softLimit limits of
        ResourceLimit n -> fromInteger n
        ResourceLimitInfinity -> privateConnectionsCap * privateConnectionsFdShare
        ResourceLimitUnknown -> privateConnectionsCap * privateConnectionsFdShare

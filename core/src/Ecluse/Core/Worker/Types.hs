-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror worker's vocabulary: the 'WorkerRuntime' the composition root assembles, the
per-ecosystem 'WorkerPolicy' bundles a job is re-evaluated against, and the 'WorkerM' reader
the loop runs in.

The mirror write is not a runtime slot. It rides each ecosystem's bundle, so a job publishes
only through its own ecosystem's capability. The loop is in "Ecluse.Core.Worker.Loop".
-}
module Ecluse.Core.Worker.Types (
    -- * The runtime and its policy bundles
    WorkerRuntime (..),
    WorkerPolicy (..),
    WorkerPolicies,

    -- * The worker monad
    WorkerM,
    runWorkerM,
    recordWorkerProgress,

    -- * Shared job vocabulary
    queueOp,
    renderJob,
) where

import Data.Time (UTCTime, getCurrentTime)
import Katip (Katip, KatipContext, KatipContextT, LogEnv, SimpleLogPayload, runKatipContextT)
import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Package.Integrity (MinIntegrity)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobVersion), MirrorQueue)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact)
import Ecluse.Core.Registry.Metadata (VersionEvaluation)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (HostPort, Limits)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Telemetry.Span (WorkerTracingPort)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Core.Worker.Liveness (WorkerHeartbeat, recordPoll)

-- | The effectful backends the mirror worker closes over, read through the 'WorkerM' reader.
data WorkerRuntime = WorkerRuntime
    { wrQueue :: MirrorQueue
    -- ^ The mirror-queue handle the consume loop long-polls and acks against.
    , wrManager :: Manager
    -- ^ The validating-TLS manager for the __untrusted__ artifact fetch.
    , wrHeartbeat :: WorkerHeartbeat
    -- ^ Advanced on every successful poll and every completed job, and read by the liveness probe.
    , wrMetrics :: WorkerMetricsPort
    -- ^ The port the @ecluse.mirror.*@ job signals are emitted through.
    , wrTracing :: WorkerTracingPort
    -- ^ The port the per-job span is opened through.
    , wrInjectTraceContext :: forall m a. (KatipContext m, MonadIO m) => m a -> m a
    -- ^ Inject the current OpenTelemetry correlation payload into the @katip@ context.
    , wrPolicies :: WorkerPolicies
    {- ^ Current policy is re-run before a mirror write, so a policy that tightened toward
    deny since the enqueue drops the job instead of freezing it into the trusted mirror.
    -}
    }

{- | The per-ecosystem bundle every job is dispatched through. Its resolver, rules, and gates
are the serve path's own, so ingest and serve reach one decision over one policy.
-}
data WorkerPolicy = WorkerPolicy
    { wpFirstParty :: PackageName -> Bool
    -- ^ Whether a name belongs to a namespace this deployment owns.
    , wpResolveVersion :: PackageName -> Version -> IO VersionEvaluation
    {- ^ Resolve one version's metadata through the guarded public origin. Total by type:
    every failure, transport included, classifies as a 'VersionMetadataUnavailable' value.
    -}
    , wpRules :: [PreparedRule]
    -- ^ The prepared rule set re-evaluated against the resolved version.
    , wpMinIntegrity :: MinIntegrity
    -- ^ The mount's public-integrity floor, re-applied through the shared admission gate.
    , wpArtifactHostHonoured :: Maybe HostPort -> Bool
    {- ^ The mount's tarball-host gate, re-checked on the job's fetch URL. An unextractable
    authority ('Nothing') is refused, because the queue payload is a trust boundary.
    -}
    , wpArtifact :: AdapterArtifact
    -- ^ The mount ecosystem's artifact capability. A job's @GET@ rides its by-URL member.
    , wpPublish :: MirrorPublish
    {- ^ The mirror write bound to the mount's declared target, so a job's presence probe and
    publish reach only its own ecosystem's mirror.
    -}
    , wpArtifactLimits :: Limits
    {- ^ The bounded-fetch budget for the artifact download, set from the memory plan's
    mirror-artifact tenant so a publish envelope cannot breach the heap ceiling.
    -}
    , wpNow :: IO UTCTime
    -- ^ Wall-clock now for the rules, injected so the quarantine rule is deterministic.
    }

{- | The bundles keyed by a job's package ecosystem. A job whose ecosystem is absent is
fail-closed: dropped, never mirrored unvetted.
-}
type WorkerPolicies = Map Ecosystem WorkerPolicy

{- | A reader over the 'WorkerRuntime' on @katip@'s logging context. That base is a reader,
never a 'StateT', so the context behaves across the loop.
-}
newtype WorkerM a = WorkerM
    { unWorkerM :: ReaderT WorkerRuntime (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader WorkerRuntime
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

{- | Run a 'WorkerM' at the caller's @katip@ environment, so the application owns the log
stream and the trace-correlation identity every line carries.
-}
runWorkerM :: LogEnv -> SimpleLogPayload -> WorkerRuntime -> WorkerM a -> IO a
runWorkerM logEnv initialContext runtime action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unWorkerM action) runtime)

{- | Record a unit of demonstrated progress. The loop beats on every successful poll, an empty
long-poll included, and after every completed job, so the staleness bound covers one job.
-}
recordWorkerProgress :: WorkerM ()
recordWorkerProgress = do
    heartbeat <- asks wrHeartbeat
    now <- liftIO getCurrentTime
    liftIO (recordPoll heartbeat now)

{- | Run one operation on the worker's queue handle, handing its typed failure to @onFault@. A
fault costs at most a redelivery, which idempotent publishing makes harmless, so it never fails.
-}
queueOp :: (MirrorQueue -> IO (Either TransportFault a)) -> (TransportFault -> WorkerM ()) -> WorkerM ()
queueOp op onFault = do
    queue <- asks wrQueue
    outcome <- liftIO (op queue)
    whenLeft_ outcome onFault

-- | A one-line identifier for a job, for log lines and audit reasons.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)

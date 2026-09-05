-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.Types (
    WorkerRuntime (..),
    WorkerPolicy (..),
    WorkerPolicies,
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

{- | The effectful backends the mirror worker closes over: a record of concrete handles
and abstract ports. The composition root assembles it ('Ecluse.Env.workerRuntimeOf') and
the loop reads it through the 'WorkerM' reader.

The mirror write is not a slot here. It rides each ecosystem's bundle ('wpPublish'), so a
job publishes through its own ecosystem's capability. There is no log field either. The
loop logs through the ambient @katip@ context the entry point establishes.
-}
data WorkerRuntime = WorkerRuntime
    { wrQueue :: MirrorQueue
    -- ^ The mirror-queue handle the consume loop long-polls and acks against.
    , wrManager :: Manager
    {- ^ The validating-TLS data-plane manager for the __untrusted__ artifact fetch (over
    an https-only @dist.tarball@).
    -}
    , wrHeartbeat :: WorkerHeartbeat
    {- ^ The consume-loop heartbeat, advanced on every successful poll and every
    completed job (see 'recordWorkerProgress') and read by the liveness probe.
    -}
    , wrMetrics :: WorkerMetricsPort
    -- ^ The metric-recording port the worker emits its @ecluse.mirror.*@ job signals through.
    , wrTracing :: WorkerTracingPort
    -- ^ The tracing port the worker opens its per-job span through.
    , wrInjectTraceContext :: forall m a. (KatipContext m, MonadIO m) => m a -> m a
    {- ^ Evaluate and inject the current OpenTelemetry correlation payload into the
    @katip@ context for the inner action.
    -}
    , wrPolicies :: WorkerPolicies
    {- ^ The per-ecosystem re-evaluation bundles, keyed by a job's ecosystem. The worker
    re-runs current policy before it mirrors a version, so a policy that tightened toward
    deny since the enqueue drops the job instead of freezing it into the trusted mirror.
    -}
    }

{- | The per-ecosystem bundle the worker dispatches every job through: the version
resolver, the rules, the gates, the request formation, the mirror write, and the clock.

The resolver, rules, and gates are the serve path's own, so the worker's ingest decision
and the serve-time decision run one codepath
('Ecluse.Core.Package.Admission.admitArtifact') over one policy and share its per-source
breaker state. The publish capability is the mount's own, so a job's presence probe and
mirror write reach only its own ecosystem's declared mirror target.
-}
data WorkerPolicy = WorkerPolicy
    { wpFirstParty :: PackageName -> Bool
    {- ^ Whether a name belongs to a namespace this deployment owns, the predicate the serve
    and publish paths read ('Ecluse.Core.Server.Context.pdFirstParty').
    -}
    , wpResolveVersion :: PackageName -> Version -> IO VersionEvaluation
    {- ^ Resolve and project one version's metadata through the guarded public origin,
    classifying the outcome ('Ecluse.Core.Registry.Metadata.fetchVersionDetails').
    Total by type: the fetch reports every failure, transport included, in its typed
    channel, and each classifies as a 'VersionMetadataUnavailable' value.
    -}
    , wpRules :: [PreparedRule]
    {- ^ The prepared rule set evaluated against the resolved version under current policy
    (the same rules the serve path gates the public version set with).
    -}
    , wpMinIntegrity :: MinIntegrity
    {- ^ The mount's own public-integrity floor
    ('Ecluse.Core.Server.Context.pdMinIntegrity'), re-applied at ingest through the
    shared admission gate.
    -}
    , wpArtifactHostHonoured :: Maybe HostPort -> Bool
    {- ^ The mount's own tarball-host gate
    ('Ecluse.Core.Server.Context.tarballHostHonoured', closed against the public
    upstream authority), re-checked on the extracted @host:port@ of the job's fetch
    URL. The gate refuses an unextractable authority ('Nothing'), because the queue
    payload is a trust boundary.
    -}
    , wpArtifact :: AdapterArtifact
    {- ^ The mount ecosystem's artifact capability, the same record the serve deps carry
    ('Ecluse.Core.Server.Context.pdArtifact'). A job's @GET@ rides its by-URL member.
    -}
    , wpPublish :: MirrorPublish
    {- ^ The mount's married mirror-write capability
    ('Ecluse.Core.Registry.Publish.newMirrorPublish', the adapter's protocol codec over the
    shared publish transport, bound to the mount's declared mirror target). The presence probe
    and the verified-bytes publish both ride it, so a job reaches only its own ecosystem's
    mirror target.
    -}
    , wpArtifactLimits :: Limits
    {- ^ The bounded-fetch budget for the artifact download
    ('Ecluse.Core.Worker.Fetch.fetchArtifactBytes'). The composition root sets @maxBodyBytes@
    from the memory plan's mirror-artifact tenant (@Ecluse.Composition.MemoryPlan@), so the
    worker never buffers a tarball whose publish envelope would breach the heap ceiling.
    -}
    , wpNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'EvalContext', injected so the
    time-sensitive age gate is deterministic under test.
    -}
    }

{- | The worker's per-ecosystem re-evaluation bundles, keyed by a job's package ecosystem
('Ecluse.Core.Package.pkgEcosystem') and shared with the serve mounts. A job whose
ecosystem is absent is fail-closed: dropped, never mirrored unvetted.
-}
type WorkerPolicies = Map Ecosystem WorkerPolicy

{- | The mirror worker's monad: a reader over the 'WorkerRuntime' layered on @katip@'s
logging context.

The @katip@ base is a reader, never a 'StateT', so the logging context behaves correctly
across the loop (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").
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

{- | Run a 'WorkerM' against the 'WorkerRuntime' and the @katip@ environment the entry
point supplies. This is the boundary where the worker's code becomes 'IO'.

The caller passes the 'LogEnv' and the initial context, so the application owns the log
stream and the @dd@ trace-correlation identity every line carries.
-}
runWorkerM :: LogEnv -> SimpleLogPayload -> WorkerRuntime -> WorkerM a -> IO a
runWorkerM logEnv initialContext runtime action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unWorkerM action) runtime)

{- | Advance the worker heartbeat to the current instant, recording a unit of demonstrated
progress. The consume loop beats on every successful poll, an empty long-poll included,
and 'Ecluse.Core.Worker.Realise.processBatch' beats after every completed job, so the
staleness bound covers one job's worst case rather than a whole batch.
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

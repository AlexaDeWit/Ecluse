-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Worker capabilities bound to one mounted ecosystem and its current policy.
The supervised loop owns receipts and records progress through this runtime.
-}
module Ecluse.Core.Worker.Types (
    -- * The runtime and its policy bundles
    WorkerRuntime (..),
    WorkerPolicy (..),
    WorkerMirrorState (..),
    WorkerStateBudget (..),
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
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Mirror.State (MirrorState, StateCharge, StateTarget)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Package.Integrity (MinIntegrity)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobVersion), MirrorQueue)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Metadata (VersionEvaluation)
import Ecluse.Core.Registry.Public (PublicControl)
import Ecluse.Core.Registry.Publish (MirrorPublish)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (HostPort, Limits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Telemetry.Span (WorkerTracingPort)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Core.Worker.Liveness (WorkerHeartbeat, recordPoll)

-- | The effectful backends the mirror worker closes over, read through the 'WorkerM' reader.
data WorkerRuntime = WorkerRuntime
    { wrQueue :: MirrorQueue
    -- ^ The mirror-queue handle the consume loop long-polls and acks against.
    , wrClient :: ExchangeClient
    -- ^ The validating-TLS pool and the deadline sweeper for the __untrusted__ artifact fetch.
    , wrPublicControl :: PublicControl
    -- ^ Public request admission and observation shared by all mounts in the process.
    , wrHeartbeat :: WorkerHeartbeat
    -- ^ Advanced on every successful poll and every completed job, and read by the liveness probe.
    , wrMetrics :: WorkerMetricsPort
    -- ^ The port the @ecluse.mirror.*@ job signals are emitted through.
    , wrTracing :: WorkerTracingPort
    -- ^ The port the per-job span is opened through.
    , wrInjectTraceContext :: forall m a. (KatipContext m, MonadIO m) => m a -> m a
    -- ^ Inject the current OpenTelemetry correlation payload into the @katip@ context.
    , wrPolicies :: WorkerPolicies
    -- ^ Recheck policy before writing, including changes since enqueue.
    }

-- | Use the served mount's resolver, rules, and gates for worker admission.
data WorkerPolicy = WorkerPolicy
    { wpFirstParty :: PackageName -> Bool
    -- ^ Whether a name belongs to a namespace this deployment owns.
    , wpResolveVersion :: StateCharge -> PackageName -> Version -> IO VersionEvaluation
    -- ^ Resolve through the guarded public origin, charging retained source fields before parsing.
    , wpRules :: [PreparedRule]
    -- ^ The prepared rule set re-evaluated against the resolved version.
    , wpMinIntegrity :: MinIntegrity
    -- ^ The mount's public-integrity floor, re-applied through the shared admission gate.
    , wpArtifactHostHonoured :: Maybe HostPort -> Bool
    -- ^ Apply the mount's artifact authority gate, refusing an unextractable authority.
    , wpEgressUrl :: Text -> Either Text RegistryUrl
    -- ^ The same URL validation policy used by the mounted serving path.
    , wpArtifact :: AdapterArtifact
    -- ^ The mount ecosystem's artifact capability. A job's @GET@ rides its by-URL member.
    , wpPublish :: MirrorPublish
    -- ^ Bind inventory reads and publication to the mount's declared mirror target.
    , wpMirrorFormat :: MirrorStateFormat
    -- ^ How the mount's mirror records keep its adapter's read data, durable or not.
    , wpArtifactLimits :: Limits
    -- ^ Bound artifact downloads by the memory plan's mirror-artifact allocation.
    , wpMirrorState :: Maybe WorkerMirrorState
    -- ^ Durable inventory storage within the worker admission reservation.
    , wpNow :: IO UTCTime
    -- ^ Wall-clock now for the rules, injected so the quarantine rule is deterministic.
    }

-- | Composition owns the admission ticket until the managed job returns.
data WorkerMirrorState = WorkerMirrorState
    { wmsStore :: MirrorState
    , wmsTarget :: StateTarget
    , wmsLimits :: Limits
    , wmsWithinBudget :: forall a. (WorkerStateBudget -> IO a) -> IO (Maybe a)
    -- ^ Keep all admission regions alive until final proof finishes.
    }

-- | Independent regions keep transient work separate from overlapping records, source bytes and archive data.
data WorkerStateBudget = WorkerStateBudget
    { wsbState :: StateCharge
    , wsbRecords :: StateCharge
    , wsbCompanions :: StateCharge
    , wsbArtifacts :: StateCharge
    , wsbArchive :: StateCharge
    , wsbSource :: StateCharge
    , wsbTargets :: StateCharge
    }

-- | Key bundles by ecosystem. Jobs without a bundle are dropped without publication.
type WorkerPolicies = Map Ecosystem WorkerPolicy

-- | Carry worker runtime and logging context through ReaderT, without mutable request state.
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

-- | Run under the caller's logging environment and trace context.
runWorkerM :: LogEnv -> SimpleLogPayload -> WorkerRuntime -> WorkerM a -> IO a
runWorkerM logEnv initialContext runtime action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unWorkerM action) runtime)

-- | Record progress after each completed poll and job, including an empty poll.
recordWorkerProgress :: WorkerM ()
recordWorkerProgress = do
    heartbeat <- asks wrHeartbeat
    now <- liftIO getCurrentTime
    liftIO (recordPoll heartbeat now)

-- | Hand typed queue failures to the caller without failing the batch.
queueOp :: (MirrorQueue -> IO (Either TransportFault a)) -> (TransportFault -> WorkerM ()) -> WorkerM ()
queueOp op onFault = do
    queue <- asks wrQueue
    outcome <- liftIO (op queue)
    whenLeft_ outcome onFault

-- | A one-line identifier for a job, for log lines and audit reasons.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.Types (
    WorkerRuntime (..),
    WorkerPolicy (..),
    WorkerPolicies,
    WorkerM,
    runWorkerM,
) where

import Data.Time (UTCTime)
import Katip (Katip, KatipContext, KatipContextT, LogEnv, SimpleLogPayload, runKatipContextT)
import Network.HTTP.Client (Manager, Request)
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Integrity (MinIntegrity)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry (RegistryClient, UrlFormationError)
import Ecluse.Core.Registry.Metadata (VersionEvaluation)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Telemetry.Span (WorkerTracingPort)
import Ecluse.Core.Version (Version)
import Ecluse.Core.Worker.Liveness (WorkerHeartbeat)

{- | The runtime backends the mirror worker is closed over: exactly the effectful
capabilities the consume loop needs to poll, fetch, verify, publish, and record. A
record of concrete handles and abstract ports (the Handle pattern), assembled by the
composition root ('Ecluse.Env.workerRuntimeOf') and read by the loop through the
'WorkerM' reader.

The mirror queue is the demand-driven hand-off the loop consumes; the publish-side
registry client writes approved artifacts to the mirror target; the untrusted
data-plane manager fetches the artifact bytes (the validating TLS manager, over an
https-only @dist.tarball@); the heartbeat is the loop's liveness surface. The metric and
tracing ports are the abstract recording interfaces ("Ecluse.Core.Telemetry.Record",
"Ecluse.Core.Telemetry.Span"); the application supplies their OpenTelemetry-backed
implementations, so the loop records without naming a telemetry backend. There is no log
field: the loop logs through the ambient @katip@ context the entry point establishes.
-}
data WorkerRuntime = WorkerRuntime
    { wrQueue :: MirrorQueue
    -- ^ The mirror-queue handle the consume loop long-polls and acks against.
    , wrRegistry :: RegistryClient
    {- ^ The publish-side registry handle approved artifacts are written to the mirror
    target through.
    -}
    , wrManager :: Manager
    {- ^ The validating-TLS data-plane manager for the __untrusted__ artifact fetch (over
    an https-only @dist.tarball@).
    -}
    , wrBuildArtifactRequest :: Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request
    {- ^ Form the artifact @GET@ request for a job's authoritative artifact URL: the
    ecosystem's request formation, projected from its registered adapter at the
    composition root so the fetch step speaks the registry protocol without naming
    an ecosystem.
    -}
    , wrHeartbeat :: WorkerHeartbeat
    {- ^ The consume-loop heartbeat, advanced on every successful poll and read by the
    liveness probe.
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
    re-runs current policy against a job's version before it mirrors it, so a policy that
    has tightened toward deny since the job was enqueued drops the job rather than freezing
    a now-disallowed version into the trusted mirror store.
    -}
    }

{- | The per-ecosystem re-evaluation bundle the worker re-runs current policy through
before it mirrors a job: a resolver that fetches and projects the single version's
metadata, the prepared rule set, the integrity floor, the tarball-host gate, and the
wall-clock the age rules read.

The resolver is the __shared__ single-version fetch-and-project
('Ecluse.Core.Registry.Metadata.fetchVersionDetails' over the guarded public origin,
wired by the composition root); the rules are the __same__ prepared rules the serve
path gates with; and the floor and host gate are the mount's __own__ configured
policy values -- so the worker's ingest decision and the serve-time decision run one
codepath ('Ecluse.Core.Package.Admission.admitArtifact') over one policy, and any
per-source breaker state is shared, never forked.
-}
data WorkerPolicy = WorkerPolicy
    { wpResolveVersion :: PackageName -> Version -> IO VersionEvaluation
    {- ^ Resolve and project one version's metadata through the guarded public origin,
    classifying the outcome ('Ecluse.Core.Registry.Metadata.fetchVersionDetails'). Total
    by type: the fetch reports every failure -- transport included -- in its typed
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
    , wpArtifactHostHonoured :: Text -> Bool
    {- ^ The mount's own tarball-host gate
    ('Ecluse.Core.Server.Context.tarballHostHonoured', closed against the public
    upstream host), re-checked on the job's fetch URL: the queue payload is a trust
    boundary.
    -}
    , wpNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'EvalContext'; injected so the time-sensitive
    age gate is deterministic under test.
    -}
    }

{- | The worker's per-ecosystem re-evaluation bundles, keyed by the ecosystem a job's
package belongs to ('Ecluse.Core.Package.pkgEcosystem'). Built once at boot and shared
with the serve mounts; a job whose ecosystem is absent here is fail-closed (dropped), never
mirrored unvetted.
-}
type WorkerPolicies = Map Ecosystem WorkerPolicy

{- | The mirror worker's monad: a reader over the 'WorkerRuntime' layered on @katip@'s
logging context.

A @newtype@ over @'ReaderT' 'WorkerRuntime' ('KatipContextT' 'IO')@ so its instances are
this module's to control and call sites name one concrete monad. The derived instances
give reader access to the runtime ('MonadReader' 'WorkerRuntime'), arbitrary effects
('MonadIO'), the unlift capability ('MonadUnliftIO') the loop's @tryAny@ and the per-job
span bracket need, and the @katip@ classes ('Katip', 'KatipContext') so a structured log
call composes through the ambient context the entry point establishes.

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

{- | Run a 'WorkerM' against the 'WorkerRuntime' and the @katip@ logging environment and
initial context the entry point supplies, yielding the underlying 'IO' action. This is
the boundary where the worker's 'WorkerM' code is discharged to 'IO'.

The 'LogEnv' (the structured-log scribes) and the initial context payload are passed in
rather than read from the runtime, so the application owns the log stream and the
trace-correlation @dd@ enrichment: it resolves the @dd@ identity and hands it here as the
initial context, so every line the loop emits carries @dd@. The loop narrows the
namespace with @katip@'s combinators on top as it logs.
-}
runWorkerM :: LogEnv -> SimpleLogPayload -> WorkerRuntime -> WorkerM a -> IO a
runWorkerM logEnv initialContext runtime action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unWorkerM action) runtime)

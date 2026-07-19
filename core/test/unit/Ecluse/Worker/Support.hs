-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-orphans #-}

module Ecluse.Worker.Support where

import Data.Aeson (Key, Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Katip (
    Environment (Environment),
    Namespace (Namespace),
    initLogEnv,
 )
import Network.HTTP.Client (Manager, Request, defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg (SRI),
    PackageDetails (..),
    PackageName,
    mkPackageName,
 )
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (ack, deadLetter, receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    queueTransportFault,
 )
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact,
    ParseError (ParseError),
    PublishFault,
    RegistryResponse (RegistryResponse),
    UrlFormationError,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError,
    VersionEvaluation (VersionPresent),
 )
import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.Publish (MirrorPublish (..))
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (FailureAlignment (FailDeny), RuleVerdict (Allow, CannotVet, Deny))
import Ecluse.Core.Security (HostPort, Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
 )
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Core.Worker (
    IntegrityResult (IntegrityMismatch, IntegrityVerified),
    JobOutcome (Dropped, Retried),
    WorkerHeartbeat,
    WorkerM,
    WorkerPolicies,
    WorkerPolicy (WorkerPolicy, wpArtifactHostHonoured, wpArtifactLimits, wpBuildArtifactRequest, wpMinIntegrity, wpNow, wpPublish, wpResolveVersion, wpRules),
    WorkerRuntime (WorkerRuntime, wrHeartbeat, wrInjectTraceContext, wrManager, wrMetrics, wrPolicies, wrQueue, wrTracing),
    newWorkerHeartbeat,
    runWorkerM,
 )
import Ecluse.Test.Package (
    defaultMinIntegrity,
    unsafeHash,
    validBlake2b,
    validMd5,
    validSha1,
    validSha256,
    validSha256Sri,
 )
import Ecluse.Test.Package qualified as Package
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)

{- | Unit cover for the core mirror worker ("Ecluse.Core.Worker") driven __directly__
over a 'WorkerRuntime' of test doubles -- no application 'Ecluse.Env.Env', no
OpenTelemetry SDK.

This is the partition's proof that the worker is genuinely core: it constructs the worker
runtime from a recording publish double on every bundle, the production in-memory queue (test-configured
through "Ecluse.Test.Queue"), a real HTTP manager, a fresh heartbeat, and the worker
metric/tracing port doubles, then runs the loop and the per-job processing through the
core 'runWorkerM' against a scribe-less @katip@ environment. The integrity gate, the
publish outcomes, the worker's ack decisions (observed at the handle; see
'recordingAckQueue'), the heartbeat, and the loop's catch-log-backoff supervision are all
exercised over those doubles. The same paths are covered through a __real SQS queue__ in
the integration suite's @Ecluse.WorkerSpec@; this pins that the loop runs over the ports.
-}

-- ── fixtures ──────────────────────────────────────────────────────────────────

{- | The tarball bytes the stub upstream serves; the digests in the job fixtures
are computed over exactly these.
-}
tarballBytes :: ByteString
tarballBytes = "the-real-artifact-bytes"

-- | The lower-cased hex SHA-1 of 'tarballBytes' -- the shasum a faithful job carries.
trueSha1 :: Text
trueSha1 = Package.hexSha1Of tarballBytes

-- | The SRI (@sha512-<base64>@) of 'tarballBytes'.
trueSri :: Text
trueSri = Package.sriSha512Of tarballBytes

{- | The lower-cased hex SHA-512 of 'tarballBytes' -- the form a __raw 'SHA512'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha512Hex :: Text
trueSha512Hex = Package.hexSha512Of tarballBytes

-- | The lower-cased hex SHA-256 of 'tarballBytes' (a digest the worker now computes).
trueSha256 :: Text
trueSha256 = Package.hexSha256Of tarballBytes

-- | The SRI (@sha256-<base64>@) of 'tarballBytes', the resolved-and-computable SRI form.
trueSha256Sri :: Text
trueSha256Sri = Package.sriSha256Of tarballBytes

-- | The lower-cased hex Blake2b-512 of 'tarballBytes' (a digest the worker now computes).
trueBlake2b :: Text
trueBlake2b = Package.hexBlake2bOf tarballBytes

-- | The SRI (@sha384-<base64>@) of 'tarballBytes' -- a genuine sha384 the worker computes.
trueSha384Sri :: Text
trueSha384Sri = Package.sriSha384Of tarballBytes

{- | The lower-cased hex SHA-384 of 'tarballBytes' -- the form a __raw 'SHA384'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha384Hex :: Text
trueSha384Hex = Package.hexSha384Of tarballBytes

{- | A well-formed sha384 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) -- the sha384 tamper-direction fixture: a real sha384 that fails.
-}
falseSha384Sri :: Text
falseSha384Sri = Package.sriSha384Of "completely-different-bytes"

{- | A well-formed sha512 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) -- for the tamper-direction regression: a real sha512 that fails.
-}
falseSri :: Text
falseSri = Package.sriSha512Of "completely-different-bytes"

{- | A well-formed sha512 SRI whose base64 body is the correct digest with its
letter case flipped. base64 is case-sensitive, so this must NOT verify -- a
case-folding comparison would wrongly admit it.
-}
caseVariantSri :: Text
caseVariantSri = "sha512-" <> T.toUpper (fromMaybe "" (T.stripPrefix "sha512-" trueSri))

{- | The canonical empty-input SHA-1 fixture ('Ecluse.Test.Package.validSha1'), used here as
a well-formed digest that does not match 'tarballBytes': the mismatch fixture, distinct
from a malformed one.
-}
wrongSha1 :: Text
wrongSha1 = validSha1

-- The canonical empty-input digest fixtures ('Ecluse.Test.Package'), used here as well-formed
-- digests that do not match 'tarballBytes'. 'someMd5' feeds the still-uncomputable MD5 arm
-- (fail-closed); the others are the now-computable algorithms' tamper fixtures the worker
-- recomputes and finds do not match.
someBlake2b, someSha256, someMd5, someSha256Sri :: Text
someBlake2b = validBlake2b
someSha256 = validSha256
someMd5 = validMd5
someSha256Sri = validSha256Sri

-- | A fixed reference instant for the heartbeat-staleness assertions.
epoch :: UTCTime
epoch = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)

pkg :: PackageName
pkg = mkPackageName Npm Nothing "thing"

ver :: Version
ver = mkVersion Npm "1.0.0"

{- | A different version of the same package -- a present-at-mirror probe fixture lists
it to prove presence is judged per version, never per package.
-}
otherVer :: Version
otherVer = mkVersion Npm "0.9.0"

{- | A mirror job for the conventional @thing-1.0.0.tgz@ artifact at the given stub
upstream. The payload names the artifact by filename only; the digests the worker
verifies against live on the policies' resolved snapshot (see
'admitPoliciesWithDigests'), never on the job.
-}
jobWith :: Text -> MirrorJob
jobWith url =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , -- The flag-gated loopback former: these suites point jobs at in-process
          -- http stubs, which the production https-only former would refuse.
          jobArtifactUrl = loopbackRegistryUrl url
        , jobArtifactFilename = "thing-1.0.0.tgz"
        , jobTraceContext = Nothing
        }

-- ── a recording publish capability ──────────────────────────────────────────────

{- | What a publish captured: the raw verified bytes it was handed, and the artifact
descriptor whose digests the real codec's publish document is assembled from (the
descriptor-sourcing case pins it to the re-admitted artifact's exactly).
-}
data PublishLog = PublishLog
    { plDocuments :: [ByteString]
    , plArtifacts :: [MirrorArtifact]
    }

{- | A publish-capability double whose 'mpPublishArtifact' records each call and
returns the given fixed outcome. The mirror-presence probe (the worker's first
step) answers __absent__ -- an unparseable metadata body, the same shape a
production mirror gives a package it does not hold -- so every test drives the
full pipeline unless it swaps in 'mirrorListingPublish'.
-}
recordingPublish :: IORef PublishLog -> Either PublishFault () -> MirrorPublish
recordingPublish logRef outcome =
    MirrorPublish
        { mpProbeMetadata = const (pure (Right (RegistryResponse "")))
        , mpParseVersionList = const (Left (ParseError "absent: nothing mirrored yet"))
        , mpPublishArtifact = \_ _ artifact document -> do
            atomicModifyIORef' logRef (\l -> (l{plDocuments = document : plDocuments l, plArtifacts = artifact : plArtifacts l}, ()))
            pure outcome
        }

{- | 'recordingPublish' whose mirror-presence probe __confirms__ the given versions
present at the mirror target, for the dedup short-circuit tests.
-}
mirrorListingPublish :: IORef PublishLog -> Either PublishFault () -> [Version] -> MirrorPublish
mirrorListingPublish logRef outcome versions =
    (recordingPublish logRef outcome)
        { mpParseVersionList = const (Right versions)
        }

{- | 'recordingPublish' whose mirror-presence probe reports a __transport fault__ (a
mirror outage) as the typed 'FetchTransport' value, for the probe-cannot-tell
fall-through tests.
-}
probeUnreachablePublish :: IORef PublishLog -> Either PublishFault () -> MirrorPublish
probeUnreachablePublish logRef outcome =
    (recordingPublish logRef outcome)
        { mpProbeMetadata = const (pure (Left (FetchTransport (transportFault TransportUnreachable "simulated mirror outage"))))
        }

{- | Give every bundle in the map the same publish capability: the runtime builders
below inject their recording double this way, mirroring how the composition root
gives each mount its own married capability.
-}
withPublish :: MirrorPublish -> WorkerPolicies -> WorkerPolicies
withPublish publish = Map.map (\p -> p{wpPublish = publish})

-- ── building a worker runtime over doubles ──────────────────────────────────────

{- | Build a 'WorkerRuntime' whose every bundle carries the caller-supplied publish
double (given the publish log, so its publishes still record), over a real no-TLS
manager (for the stub upstream), a fresh queue + heartbeat, the given worker
metrics port, and the given per-ecosystem policies, then run the body against it.
The queue and the publish log are returned so a test can drive and inspect them.
The probe tests use this directly to swap in 'mirrorListingPublish' or
'probeUnreachablePublish'; 'withRuntimePolicies' is this over 'recordingPublish'.
-}
withRuntimeRegistry :: (IORef PublishLog -> MirrorPublish) -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeRegistry mkPublish policies metricsPort body = do
    queue <- newTestMemoryQueue
    withRuntimeQueue queue mkPublish policies metricsPort (`body` queue)

{- | 'withRuntimeRegistry' over a __caller-supplied__ queue, so a test can observe
the worker's queue-side decisions on a wrapped handle (see 'recordingAckQueue') or
drive the loop against a misbehaving one.
-}
withRuntimeQueue :: MirrorQueue -> (IORef PublishLog -> MirrorPublish) -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> IORef PublishLog -> IO a) -> IO a
withRuntimeQueue queue mkPublish policies metricsPort body = do
    logRef <- newIORef (PublishLog [] [])
    withWiredRuntime queue (withPublish (mkPublish logRef) policies) metricsPort (`body` logRef)

{- | The base runtime builder over bundles that already carry their own publish
capabilities (nothing injected), so a test wiring distinct capabilities per
ecosystem -- the foreign-bundle decoy pins -- observes exactly what it wired.
-}
withWiredRuntime :: MirrorQueue -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> IO a) -> IO a
withWiredRuntime queue policies metricsPort body = do
    heartbeat <- newWorkerHeartbeat
    withWiredRuntimeHeartbeat heartbeat queue policies metricsPort body

{- | 'withWiredRuntime' over a __caller-supplied__ heartbeat, so a test can observe
the heartbeat a mid-batch step (a publish, an ack) reads while the loop runs. The
plain 'withWiredRuntime' is this over a fresh one.
-}
withWiredRuntimeHeartbeat :: WorkerHeartbeat -> MirrorQueue -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> IO a) -> IO a
withWiredRuntimeHeartbeat heartbeat queue policies metricsPort body = do
    manager <- newManager defaultManagerSettings
    body
        WorkerRuntime
            { wrQueue = queue
            , wrManager = manager
            , wrHeartbeat = heartbeat
            , wrMetrics = metricsPort
            , wrTracing = passthroughWorkerTracingPort
            , wrInjectTraceContext = id
            , wrPolicies = policies
            }

{- | 'withRuntimeRegistry' with the recording publish double answering the given
publish outcome -- the common case.
-}
withRuntimePolicies :: WorkerPolicies -> WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimePolicies policies metricsPort outcome =
    withRuntimeRegistry (`recordingPublish` outcome) policies metricsPort

{- | 'withRuntimePolicies' with the default admitting policy ('admitPolicies'), so the
integrity-gate and publish tests exercise their own path while ingest re-evaluation always
admits; the re-evaluation tests pass their own policies through 'withRuntimePolicies'.
-}
withRuntimeWith :: WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeWith = withRuntimePolicies admitPolicies

-- | 'withRuntimeWith' with the inert worker metrics port -- the common case.
withRuntime :: Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntime = withRuntimeWith noopWorkerMetricsPort

{- | Build a 'WorkerRuntime' over a caller-supplied queue (the publish double is the
never-consulted recording one) and run the body against it. Lets a test drive
the supervised loop against a queue whose @receive@ misbehaves.
-}
withQueueRuntime :: MirrorQueue -> (WorkerRuntime -> IO a) -> IO a
withQueueRuntime queue body =
    withRuntimeQueue queue (`recordingPublish` Right ()) admitPolicies noopWorkerMetricsPort (\runtime _logRef -> body runtime)

-- ── ingest re-evaluation fixtures ───────────────────────────────────────────────

{- | A prepared rule with a fixed verdict, built directly through the engine's injection
point so a re-evaluation reaches a chosen decision independent of the version's details.
-}
constRule :: Text -> RuleVerdict -> PreparedRule
constRule name result =
    PreparedRule
        { prepName = name
        , prepPrecedence = 0
        , prepResilience = Nothing
        , prepEval = \_ _ -> pure result
        }

-- | An always-admitting prepared rule: a re-evaluation reaches an admit decision.
admitRule :: PreparedRule
admitRule = constRule "test-admit" (Allow "admitted for test")

{- | An always-blocking prepared rule: a re-evaluation reaches a block decision, modelling a
denylist/advisory/config that has tightened to deny since the job was enqueued.
-}
denyRule :: PreparedRule
denyRule = constRule "test-deny" (Deny "denied by current policy")

{- | A fail-closed cannot-vet rule: models the advisory database being absent, so a
re-evaluation reaches an undecidable decision (the serve path's transient 503; the
worker's leave-for-redelivery).
-}
cannotVetRule :: PreparedRule
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny "no advisory database is loaded")

{- | A resolver whose resolved snapshot carries the given artifact, for the
ingest-gate cases where current metadata has changed shape since the job was
enqueued: a digest stripped or downgraded below the floor, a file renamed away.
-}
resolverWithArtifact :: Artifact -> PackageName -> Version -> IO VersionEvaluation
resolverWithArtifact art rName rVersion =
    pure (VersionPresent ((sampleDetails rName rVersion){pkgArtifacts = art :| []}))

{- | Override the tarball-host gate of every policy in the map: the payload
re-gating tests refuse (or admit) every authority wholesale.
-}
withHostGate :: (Maybe HostPort -> Bool) -> WorkerPolicies -> WorkerPolicies
withHostGate gate = Map.map (\p -> p{wpArtifactHostHonoured = gate})

{- | Override the artifact request formation of every policy in the map: the
builder-keying tests swap in a refusing builder to prove which bundle's formation
a job rides.
-}
withArtifactRequest :: (Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request) -> WorkerPolicies -> WorkerPolicies
withArtifactRequest builder = Map.map (\p -> p{wpBuildArtifactRequest = builder})

{- | The artifact of a projected version snapshot. The injected rules never inspect
it, but the shared admission oracle does: its filename must match the job fixture's
'Ecluse.Core.Queue.jobArtifactFilename' (file selection) and it carries the
floor-clearing sha512 SRI of 'tarballBytes'. The re-admitted artifact's digests are
what the tamper gate verifies the fetched bytes against, so the current-metadata
double must carry the true digest of the bytes the stub upstream serves (the
faithful, immutable-version posture); a tamper case swaps this set through
'admitPoliciesWithDigests'.
-}
sampleArtifact :: Artifact
sampleArtifact =
    Package.sampleArtifact
        { artUrl = "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
        , artHashes = [unsafeHash SRI trueSri]
        }

{- | A minimal projected version snapshot. The injected rules ignore its contents, so only
its validity matters; it stands in for what the shared single-version fetch would project.
-}
sampleDetails :: PackageName -> Version -> PackageDetails
sampleDetails name version =
    (Package.sampleDetails name version){pkgArtifacts = sampleArtifact :| []}

{- | A resolver that always reports the version present (projected), so the worker runs the
rules over its 'PackageDetails'.
-}
presentResolver :: PackageName -> Version -> IO VersionEvaluation
presentResolver name version = pure (VersionPresent (sampleDetails name version))

{- | A worker-policies map for the npm ecosystem with the given single-version resolver and
prepared rules, clocked at the fixed 'epoch' (the injected rules are not time-sensitive).
-}
npmPolicies :: (PackageName -> Version -> IO VersionEvaluation) -> [PreparedRule] -> WorkerPolicies
npmPolicies resolve rules = Map.singleton Npm (npmPolicy resolve rules)

{- | One npm re-evaluation bundle (the entry 'npmPolicies' keys under npm), for a test
that assembles its own multi-ecosystem map. The request formation is npm's real
by-URL builder, so the fetch path forms requests exactly as production does.
-}
npmPolicy :: (PackageName -> Version -> IO VersionEvaluation) -> [PreparedRule] -> WorkerPolicy
npmPolicy resolve rules =
    WorkerPolicy
        { wpResolveVersion = resolve
        , wpRules = rules
        , wpMinIntegrity = defaultMinIntegrity
        , wpArtifactHostHonoured = const True
        , wpBuildArtifactRequest = \_ _ baseUrl token -> artifactRequestByUrl baseUrl token
        , wpPublish = unwiredPublish
        , -- A generous artifact cap: these tests fetch tiny fixtures, so the cap never
          -- bites; it matches the pre-plan worker default so fetch behaviour is unchanged.
          wpArtifactLimits = defaultLimits{maxBodyBytes = 512 * 1024 * 1024}
        , wpNow = pure epoch
        }

{- | The publish placeholder 'npmPolicy' carries: the runtime builders swap in the
recording double ('withPublish'), so an effectful use of this one is a broken test
premise, failed loudly rather than fabricating an outcome.
-}
unwiredPublish :: MirrorPublish
unwiredPublish =
    MirrorPublish
        { mpProbeMetadata = const (throwIO (SimulatedContractEscape "unwiredPublish: probe consulted"))
        , mpParseVersionList = const (Left (ParseError "unwiredPublish: nothing to parse"))
        , mpPublishArtifact = \_ _ _ _ -> throwIO (SimulatedContractEscape "unwiredPublish: publish consulted")
        }

{- | The default admitting policy the integrity-gate and publish tests run under: the version
resolves present and an always-admit rule clears it, so re-evaluation never blocks and the
existing tests exercise the integrity gate and publish outcomes unchanged.
-}
admitPolicies :: WorkerPolicies
admitPolicies = npmPolicies presentResolver [admitRule]

{- | 'admitPolicies' with the resolved artifact's digest set replaced: the
current-metadata double for the verification-source cases. The tamper gate verifies
fetched bytes against the re-admitted artifact's digests, so a test chooses here
whether current metadata matches the stub upstream's bytes (a faithful mirror) or
deliberately mismatches them (a tamper), independent of what the job payload carries.
-}
admitPoliciesWithDigests :: [Hash] -> WorkerPolicies
admitPoliciesWithDigests hashes =
    npmPolicies (resolverWithArtifact sampleArtifact{artHashes = hashes}) [admitRule]

{- | A 'MetadataClient' double whose single-version op returns a fixed result (the
full-manifest op is unused here and refuses loudly).
-}
versionClient :: Either MetadataError (Maybe PackageDetails) -> MetadataClient
versionClient result =
    MetadataClient
        { fetchFullManifest = const (throwIO (SimulatedContractEscape "versionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> pure result
        }

{- | A 'MetadataClient' double whose single-version op __escapes its total contract__
(the typed channel reports every real failure, so a throw here is an invariant break),
pinning that the classification boundary propagates rather than absorbs it.
-}
throwingVersionClient :: MetadataClient
throwingVersionClient =
    MetadataClient
        { fetchFullManifest = const (throwIO (SimulatedContractEscape "throwingVersionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> throwIO (SimulatedContractEscape "simulated contract escape")
        }

{- | The loop tests' supervision policy: everything transient, retried at a fixed
one-second pace -- the composition root's worker policy shape without the shell's
wiring-fault classifications (which live with the shell's types).
-}
testSupervision :: SupervisionPolicy
testSupervision =
    SupervisionPolicy
        { spLabel = "worker-test"
        , spClassify = const Transient
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 1_000_000}
        }

{- | Discharge a 'WorkerM' to 'IO' over the worker runtime against a scribe-less @katip@
environment (its log lines have nowhere to go, which is what these tests want) and an
empty initial context (no @dd@). This is the core 'runWorkerM' boundary the application
entry point uses.
-}
runWM :: WorkerRuntime -> WorkerM a -> IO a
runWM runtime action = do
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    runWorkerM logEnv mempty runtime action

{- | A typed stand-in for an exception escaping a dependency's typed contract (an
invariant break), so the residue-supervision cases pin the escape channel without
a stringly exception.
-}
newtype SimulatedContractEscape = SimulatedContractEscape Text
    deriving stock (Eq, Show)

instance Exception SimulatedContractEscape

{- | A queue whose @receive@ always reports the handle's typed fault, counting
each call. Stands in for a persistently-failing backend so the loop's typed
log-and-back-off arm can be exercised: the loop must survive a faulted poll and
poll again, not die.
-}
faultingReceiveQueue :: IORef Int -> IO MirrorQueue
faultingReceiveQueue calls = do
    base <- newTestMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                pure (Left (queueTransportFault (transportFault TransportUnreachable "receive: simulated queue outage")))
            }

{- | A queue whose @receive@ always __throws__, counting each call: the handle's
typed contract broken rather than honoured. Stands in for residue (an invariant
break escaping the value channel), so the loop's residual catch-log-backoff arm
stays pinned by a test.
-}
throwingReceiveQueue :: IORef Int -> IO MirrorQueue
throwingReceiveQueue calls = do
    base <- newTestMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                throwIO (SimulatedContractEscape "receive: simulated queue outage")
            }

{- | Run a stub upstream that serves 'tarballBytes' and yields its base URL to the
body.
-}
withUpstream :: (Text -> IO a) -> IO a
withUpstream body =
    testWithApplication (pure app) $ \port -> body ("http://127.0.0.1:" <> show port)
  where
    app :: Application
    app _ respond = respond (responseLBS status200 [] (toLazy tarballBytes))

{- | An address with nothing listening -- a fetch against it is refused at connect,
the genuine transient fault. Port 1 is in the privileged range and never bound.
-}
unreachableUrl :: Text
unreachableUrl = "http://127.0.0.1:1/thing/-/thing-1.0.0.tgz"

{- | A job artifact URL that cannot be parsed into a request at all (a space and no
scheme), so the worker's by-URL request build fails before any fetch -- the
unformable-URL arm, distinct from a reachable-but-failing fetch.
-}
unformableUrl :: Text
unformableUrl = "not a url"

-- Enqueue a job on the test queue, unwrapping its never-faulting typed
-- channel: a 'Left' is a broken test premise, failed loudly.
enqueue_ :: MirrorQueue -> MirrorJob -> IO ()
enqueue_ queue job =
    enqueue queue job >>= \case
        Left fault -> fail ("enqueue faulted on the test queue: " <> show fault)
        Right () -> pass

-- Receive the currently-queued batch, unwrapping the never-faulting typed channel.
receive_ :: MirrorQueue -> IO [QueueMessage]
receive_ queue =
    receive queue >>= \case
        Left fault -> fail ("receive faulted on the test queue: " <> show fault)
        Right messages -> pure messages

-- Enqueue a job, receive it, and return its receipt handle so the per-job processing
-- can be driven with a real handle.
enqueueAndReceive :: MirrorQueue -> MirrorJob -> IO (ReceiptHandle, MirrorJob)
enqueueAndReceive queue job = do
    enqueue_ queue job
    receive_ queue >>= \case
        [message] -> pure (msgReceipt message, job)
        other -> fail ("expected exactly one message, got " <> show other)

{- | The test queue with its 'ack' field wrapped to record each acked receipt, so
a test asserts the worker's retire-vs-retry decision directly at the handle. The
production memory backend removes a job at delivery and never redelivers, so the
decision is not observable through the queue's own state; the redelivery
consequence of an un-acked message over a redelivering backend is pinned against
real SQS in the integration suite's @Ecluse.WorkerSpec@.
-}
recordingAckQueue :: IO (MirrorQueue, IO [ReceiptHandle])
recordingAckQueue = do
    base <- newTestMemoryQueue
    acked <- newIORef []
    let recording = base{ack = \receipt -> atomicModifyIORef' acked (\rs -> (receipt : rs, ())) >> ack base receipt}
    pure (recording, reverse <$> readIORef acked)

{- | The test queue with its 'deadLetter' field wrapped to record each dead-lettered
receipt, so a test asserts the worker routed a terminal fault to the backend's
dead-letter terminus (never 'ack'). The in-memory backend's 'deadLetter' is a no-op
drop, so the recorded receipts are the only observable signal here; the not-deleted
redelivery consequence over a durable backend is pinned against real SQS in
@Ecluse.MirrorQueueSpec@.
-}
recordingDeadLetterQueue :: IO (MirrorQueue, IO [ReceiptHandle])
recordingDeadLetterQueue = do
    base <- newTestMemoryQueue
    dead <- newIORef []
    let recording = base{deadLetter = \receipt -> atomicModifyIORef' dead (\rs -> (receipt : rs, ())) >> deadLetter base receipt}
    pure (recording, reverse <$> readIORef dead)

-- | Set every bundle's artifact fetch cap, so a test drives an over-cap fetch.
withArtifactCap :: Int -> WorkerPolicies -> WorkerPolicies
withArtifactCap cap = Map.map (\p -> p{wpArtifactLimits = defaultLimits{maxBodyBytes = cap}})

-- ── small predicates ─────────────────────────────────────────────────────────────

-- Follow a path of object keys into a decoded JSON 'Value', returning the string at
-- the leaf (or 'Nothing' if any step is absent or not the expected shape).
stringAt :: [Key] -> Value -> Maybe Text
stringAt [] (String t) = Just t
stringAt (k : ks) (Object o) = KeyMap.lookup k o >>= stringAt ks
stringAt _ _ = Nothing

isMismatch :: IntegrityResult -> Bool
isMismatch = \case
    IntegrityMismatch _ -> True
    IntegrityVerified -> False

-- The operator-facing detail of an integrity mismatch, or 'Nothing' when verified.
mismatchDetail :: IntegrityResult -> Maybe Text
mismatchDetail = \case
    IntegrityMismatch detail -> Just detail
    IntegrityVerified -> Nothing

isDropped :: JobOutcome -> Bool
isDropped = \case
    Dropped _ -> True
    _ -> False

isRetried :: JobOutcome -> Bool
isRetried = \case
    Retried _ -> True
    _ -> False

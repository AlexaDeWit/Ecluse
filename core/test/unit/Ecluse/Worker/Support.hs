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
import Ecluse.Test.Log (newTestLogEnv)
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
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

{- | Unit cover for the core mirror worker ("Ecluse.Core.Worker") driven __directly__ over a
'WorkerRuntime' of test doubles, with no application 'Ecluse.Env.Env' and no OpenTelemetry SDK.
This is the partition's proof that the worker is genuinely core.
-}

-- ── fixtures ──────────────────────────────────────────────────────────────────

{- | The tarball bytes the stub upstream serves. The digests in the job fixtures are
computed over exactly these bytes.
-}
tarballBytes :: ByteString
tarballBytes = "the-real-artifact-bytes"

-- | The lower-cased hex SHA-1 of 'tarballBytes': the shasum a faithful job carries.
trueSha1 :: Text
trueSha1 = Package.hexSha1Of tarballBytes

-- | The SRI (@sha512-<base64>@) of 'tarballBytes'.
trueSri :: Text
trueSri = Package.sriSha512Of tarballBytes

{- | The lower-cased hex SHA-512 of 'tarballBytes': the form a __raw 'SHA512'-tagged__
digest carries, as opposed to the base64 inside an SRI string.
-}
trueSha512Hex :: Text
trueSha512Hex = Package.hexSha512Of tarballBytes

-- | The lower-cased hex SHA-256 of 'tarballBytes' (a digest the worker computes).
trueSha256 :: Text
trueSha256 = Package.hexSha256Of tarballBytes

-- | The SRI (@sha256-<base64>@) of 'tarballBytes', the resolved-and-computable SRI form.
trueSha256Sri :: Text
trueSha256Sri = Package.sriSha256Of tarballBytes

-- | The lower-cased hex Blake2b-512 of 'tarballBytes' (a digest the worker computes).
trueBlake2b :: Text
trueBlake2b = Package.hexBlake2bOf tarballBytes

-- | The SRI (@sha384-<base64>@) of 'tarballBytes': a genuine sha384 the worker computes.
trueSha384Sri :: Text
trueSha384Sri = Package.sriSha384Of tarballBytes

{- | The lower-cased hex SHA-384 of 'tarballBytes': the form a __raw 'SHA384'-tagged__
digest carries, as opposed to the base64 inside an SRI string.
-}
trueSha384Hex :: Text
trueSha384Hex = Package.hexSha384Of tarballBytes

{- | A well-formed sha384 SRI that does NOT match 'tarballBytes', because it is the digest
of different bytes. This is the sha384 tamper-direction fixture: a real sha384 that fails.
-}
falseSha384Sri :: Text
falseSha384Sri = Package.sriSha384Of "completely-different-bytes"

{- | A well-formed sha512 SRI that does NOT match 'tarballBytes', because it is the digest
of different bytes. This is the tamper-direction fixture: a real sha512 that fails.
-}
falseSri :: Text
falseSri = Package.sriSha512Of "completely-different-bytes"

{- | A well-formed sha512 SRI whose base64 body is the correct digest with its letter
case flipped. base64 is case-sensitive, so this must NOT verify. A case-folding
comparison would wrongly admit it.
-}
caseVariantSri :: Text
caseVariantSri = "sha512-" <> T.toUpper (fromMaybe "" (T.stripPrefix "sha512-" trueSri))

{- | The canonical empty-input SHA-1 fixture ('Ecluse.Test.Package.validSha1'), used here as
a well-formed digest that does not match 'tarballBytes': the mismatch fixture, distinct
from a malformed one.
-}
wrongSha1 :: Text
wrongSha1 = validSha1

-- Canonical empty-input digests ('Ecluse.Test.Package') that do not match 'tarballBytes'.
-- 'someMd5' drives the uncomputable MD5 arm (fail-closed). The worker recomputes the rest.
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

{- | A different version of the same package. A present-at-mirror probe fixture lists it
to prove the worker judges presence per version, never per package.
-}
otherVer :: Version
otherVer = mkVersion Npm "0.9.0"

{- | A mirror job for the conventional @thing-1.0.0.tgz@ artifact at the given stub upstream.
The digests the worker verifies against live on the policies' resolved snapshot, never on the job.
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

{- | What a publish captured: the raw verified bytes it received, and the artifact descriptor whose
digests the real codec assembles its publish document from.
-}
data PublishLog = PublishLog
    { plDocuments :: [ByteString]
    , plArtifacts :: [MirrorArtifact]
    }

{- | A publish double that records each call and returns the given fixed outcome. Its
mirror-presence probe answers absent, so a test drives the full pipeline unless it swaps in
'mirrorListingPublish'.
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

-- | Give every bundle in the map the same publish capability.
withPublish :: MirrorPublish -> WorkerPolicies -> WorkerPolicies
withPublish publish = Map.map (\p -> p{wpPublish = publish})

-- ── building a worker runtime over doubles ──────────────────────────────────────

{- | Build a 'WorkerRuntime' over the caller's publish double and run the body against it. It hands
back the queue and the publish log, so a test can drive and inspect them.
-}
withRuntimeRegistry :: (IORef PublishLog -> MirrorPublish) -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeRegistry mkPublish policies metricsPort body = do
    queue <- newTestMemoryQueue
    withRuntimeQueue queue mkPublish policies metricsPort (`body` queue)

{- | 'withRuntimeRegistry' over a __caller-supplied__ queue, so a test observes the worker's
queue-side decisions or drives the loop against a misbehaving queue.
-}
withRuntimeQueue :: MirrorQueue -> (IORef PublishLog -> MirrorPublish) -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> IORef PublishLog -> IO a) -> IO a
withRuntimeQueue queue mkPublish policies metricsPort body = do
    logRef <- newIORef (PublishLog [] [])
    withWiredRuntime queue (withPublish (mkPublish logRef) policies) metricsPort (`body` logRef)

{- | The base runtime builder over bundles that already carry their own publish capabilities, with
nothing injected, so a test observes exactly what it wired.
-}
withWiredRuntime :: MirrorQueue -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> IO a) -> IO a
withWiredRuntime queue policies metricsPort body = do
    heartbeat <- newWorkerHeartbeat
    withWiredRuntimeHeartbeat heartbeat queue policies metricsPort body

{- | 'withWiredRuntime' over a __caller-supplied__ heartbeat, so a test observes the heartbeat a
mid-batch step reads while the loop runs.
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

-- | 'withRuntimeRegistry' with the recording publish double answering the given publish outcome.
withRuntimePolicies :: WorkerPolicies -> WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimePolicies policies metricsPort outcome =
    withRuntimeRegistry (`recordingPublish` outcome) policies metricsPort

{- | 'withRuntimePolicies' with the default admitting policy ('admitPolicies'), so ingest
re-evaluation always admits.
-}
withRuntimeWith :: WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeWith = withRuntimePolicies admitPolicies

-- | 'withRuntimeWith' with the inert worker metrics port: the common case.
withRuntime :: Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntime = withRuntimeWith noopWorkerMetricsPort

{- | Build a 'WorkerRuntime' over a caller-supplied queue, so a test drives the supervised loop
against a queue whose @receive@ misbehaves.
-}
withQueueRuntime :: MirrorQueue -> (WorkerRuntime -> IO a) -> IO a
withQueueRuntime queue body =
    withRuntimeQueue queue (`recordingPublish` Right ()) admitPolicies noopWorkerMetricsPort (\runtime _logRef -> body runtime)

-- ── ingest re-evaluation fixtures ───────────────────────────────────────────────

{- | A prepared rule with a fixed verdict, so a re-evaluation reaches a chosen decision independent
of the version's details.
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

{- | An always-blocking prepared rule: a re-evaluation reaches a block decision. It models
a denylist, advisory, or config that tightened to deny after the job was enqueued.
-}
denyRule :: PreparedRule
denyRule = constRule "test-deny" (Deny "denied by current policy")

{- | A fail-closed cannot-vet rule. It models an absent advisory database, so a
re-evaluation reaches an undecidable decision: the serve path's transient 503, and the
worker's leave-for-redelivery.
-}
cannotVetRule :: PreparedRule
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny "no advisory database is loaded")

{- | A resolver whose resolved snapshot carries the given artifact, for the ingest-gate cases where
current metadata changed shape after the job was enqueued.
-}
resolverWithArtifact :: Artifact -> PackageName -> Version -> IO VersionEvaluation
resolverWithArtifact art rName rVersion =
    pure (VersionPresent ((sampleDetails rName rVersion){pkgArtifacts = art :| []}))

{- | Override the tarball-host gate of every policy in the map: the payload
re-gating tests refuse (or admit) every authority wholesale.
-}
withHostGate :: (Maybe HostPort -> Bool) -> WorkerPolicies -> WorkerPolicies
withHostGate gate = Map.map (\p -> p{wpArtifactHostHonoured = gate})

{- | Override the artifact request formation of every policy in the map. The
builder-keying tests swap in a refusing builder to prove which bundle's formation a job
rides.
-}
withArtifactRequest :: (Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request) -> WorkerPolicies -> WorkerPolicies
withArtifactRequest builder = Map.map (\p -> p{wpBuildArtifactRequest = builder})

{- | The artifact of a projected version snapshot. Its filename must match the job fixture's
'Ecluse.Core.Queue.jobArtifactFilename', and it carries the floor-clearing sha512 SRI of
'tarballBytes', because the tamper gate verifies fetched bytes against the re-admitted artifact.
-}
sampleArtifact :: Artifact
sampleArtifact =
    Package.sampleArtifact
        { artUrl = "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
        , artHashes = [unsafeHash SRI trueSri]
        }

{- | A minimal projected version snapshot. The injected rules ignore its contents, so only
its validity matters. It stands in for what the shared single-version fetch would project.
-}
sampleDetails :: PackageName -> Version -> PackageDetails
sampleDetails name version =
    (Package.sampleDetails name version){pkgArtifacts = sampleArtifact :| []}

{- | A resolver that always reports the version present (projected), so the worker runs the
rules over its 'PackageDetails'.
-}
presentResolver :: PackageName -> Version -> IO VersionEvaluation
presentResolver name version = pure (VersionPresent (sampleDetails name version))

{- | Worker policies for npm, clocked at the fixed 'epoch'. The injected rules are not
time-sensitive.
-}
npmPolicies :: (PackageName -> Version -> IO VersionEvaluation) -> [PreparedRule] -> WorkerPolicies
npmPolicies resolve rules = Map.singleton Npm (npmPolicy resolve rules)

{- | One npm re-evaluation bundle. It forms artifact requests with npm's real by-URL
builder, so the fetch path matches production.
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
          -- bites. It matches the worker default.
          wpArtifactLimits = defaultLimits{maxBodyBytes = 512 * 1024 * 1024}
        , wpNow = pure epoch
        }

{- | The publish placeholder 'npmPolicy' carries. The runtime builders swap in the
recording double ('withPublish'), so any effectful use here fails loudly.
-}
unwiredPublish :: MirrorPublish
unwiredPublish =
    MirrorPublish
        { mpProbeMetadata = const (throwIO (TestContractEscape "unwiredPublish: probe consulted"))
        , mpParseVersionList = const (Left (ParseError "unwiredPublish: nothing to parse"))
        , mpPublishArtifact = \_ _ _ _ -> throwIO (TestContractEscape "unwiredPublish: publish consulted")
        }

{- | The default admitting policy. The version resolves present and an always-admit rule
clears it, so re-evaluation never blocks.
-}
admitPolicies :: WorkerPolicies
admitPolicies = npmPolicies presentResolver [admitRule]

{- | 'admitPolicies' with the resolved artifact's digests replaced. The tamper gate verifies
fetched bytes against these digests, so a test chooses a faithful mirror or a tamper here,
independent of what the job payload carries.
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
        { fetchFullManifest = const (throwIO (TestContractEscape "versionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> pure result
        }

{- | A 'MetadataClient' double whose single-version op __escapes its total contract__. The
typed channel reports every real failure, so a throw here is an invariant break. It pins
that the classification boundary propagates the escape rather than absorbing it.
-}
throwingVersionClient :: MetadataClient
throwingVersionClient =
    MetadataClient
        { fetchFullManifest = const (throwIO (TestContractEscape "throwingVersionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> throwIO (TestContractEscape "simulated contract escape")
        }

{- | The loop tests' supervision policy. It drops the shell's wiring-fault classifications,
which live with the shell's types.
-}
testSupervision :: SupervisionPolicy
testSupervision =
    SupervisionPolicy
        { spLabel = "worker-test"
        , spClassify = const Transient
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 1_000_000}
        }

{- | Discharge a 'WorkerM' to 'IO' over the worker runtime. The @katip@ environment has no
scribe, so log lines are discarded.
-}
runWM :: WorkerRuntime -> WorkerM a -> IO a
runWM runtime action = do
    logEnv <- newTestLogEnv
    runWorkerM logEnv mempty runtime action

{- | A queue whose @receive@ always reports the handle's typed fault, counting each call. The
loop must survive a faulted poll and poll again, not die.
-}
faultingReceiveQueue :: IORef Int -> IO MirrorQueue
faultingReceiveQueue calls = do
    base <- newTestMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                pure (Left (transportFault TransportUnreachable "receive: simulated queue outage"))
            }

{- | A queue whose @receive@ always throws, counting each call. The throw breaks the handle's
typed contract, so it drives the loop's residual catch-log-backoff arm.
-}
throwingReceiveQueue :: IORef Int -> IO MirrorQueue
throwingReceiveQueue calls = do
    base <- newTestMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                throwIO (TestContractEscape "receive: simulated queue outage")
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

{- | An address with nothing listening. A fetch against it is refused at connect, the
genuine transient fault. Port 1 is in the privileged range and never bound.
-}
unreachableUrl :: Text
unreachableUrl = "http://127.0.0.1:1/thing/-/thing-1.0.0.tgz"

{- | 'unreachableUrl' dressed as a hostile artifact location: userinfo and a signed query, the
two places a @dist.tarball@ can hide a credential. The fetch still fails at connect, so a test
asserts the fault text with no network.
-}
credentialBearingUnreachableUrl :: Text
credentialBearingUnreachableUrl = "http://deploy:hunter2@127.0.0.1:1/x?sig=abc"

{- | A job artifact URL that cannot be parsed into a request, so the by-URL build fails before
any fetch.
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

-- Enqueue a job, receive it, and return its receipt handle, so a test drives the per-job
-- processing with a real handle.
enqueueAndReceive :: MirrorQueue -> MirrorJob -> IO (ReceiptHandle, MirrorJob)
enqueueAndReceive queue job = do
    enqueue_ queue job
    receive_ queue >>= \case
        [message] -> pure (msgReceipt message, job)
        other -> fail ("expected exactly one message, got " <> show other)

{- | The test queue with 'ack' wrapped to record each acked receipt. The memory backend removes
a job at delivery and never redelivers, so the queue's own state does not show the worker's
retire-vs-retry decision.
-}
recordingAckQueue :: IO (MirrorQueue, IO [ReceiptHandle])
recordingAckQueue = do
    base <- newTestMemoryQueue
    acked <- newIORef []
    let recording = base{ack = \receipt -> atomicModifyIORef' acked (\rs -> (receipt : rs, ())) >> ack base receipt}
    pure (recording, reverse <$> readIORef acked)

{- | The test queue with 'deadLetter' wrapped to record each dead-lettered receipt. The
in-memory 'deadLetter' is a no-op drop, so the recorded receipts are the only signal that a
terminal fault went to the terminus rather than 'ack'.
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

-- Follow a path of object keys into a decoded JSON 'Value' and return the string at the
-- leaf. Any step that is absent or the wrong shape yields 'Nothing'.
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

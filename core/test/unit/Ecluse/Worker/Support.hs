-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-orphans #-}

module Ecluse.Worker.Support where

import Crypto.Hash (Blake2b_512, Digest, SHA1, SHA256, SHA384, SHA512, hashlazy)
import Data.Aeson (Key, Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Katip (
    Environment (Environment),
    Namespace (Namespace),
    initLogEnv,
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg (SRI),
    PackageDetails (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
 )
import Ecluse.Core.Package.Integrity (defaultMinIntegrity)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    queueFault,
 )
import Ecluse.Core.Queue.Memory (newInMemoryQueue)
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact,
    ParseError (ParseError),
    PublishFault,
    RegistryClient (..),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError,
    VersionEvaluation (VersionPresent),
 )
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (FailureAlignment (FailDeny), RuleVerdict (Allow, CannotVet, Deny))
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
    WorkerM,
    WorkerPolicies,
    WorkerPolicy (WorkerPolicy, wpArtifactHostHonoured, wpMinIntegrity, wpNow, wpResolveVersion, wpRules),
    WorkerRuntime (WorkerRuntime, wrHeartbeat, wrInjectTraceContext, wrManager, wrMetrics, wrPolicies, wrQueue, wrRegistry, wrTracing),
    newWorkerHeartbeat,
    runWorkerM,
 )
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort)

{- | Unit cover for the core mirror worker ("Ecluse.Core.Worker") driven __directly__
over a 'WorkerRuntime' of test doubles -- no application 'Ecluse.Env.Env', no
OpenTelemetry SDK.

This is the partition's proof that the worker is genuinely core: it constructs the worker
runtime from a recording publish client, an in-memory queue, a real HTTP manager, a fresh
heartbeat, and the worker metric/tracing port doubles, then runs the loop and the per-job
processing through the core 'runWorkerM' against a scribe-less @katip@ environment. The
integrity gate, the publish/ack/redeliver outcomes, the heartbeat, and the loop's
catch-log-backoff supervision are all exercised over those doubles. The same paths are
covered through a __real SQS queue__ in the integration suite's @Ecluse.WorkerSpec@; this
pins that the loop runs over the ports.
-}

-- ── fixtures ──────────────────────────────────────────────────────────────────

{- | The tarball bytes the stub upstream serves; the digests in the job fixtures
are computed over exactly these.
-}
tarballBytes :: ByteString
tarballBytes = "the-real-artifact-bytes"

-- | The lower-cased hex SHA-1 of 'tarballBytes' -- the shasum a faithful job carries.
trueSha1 :: Text
trueSha1 = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA1) :: ByteString)

-- | The SRI (@sha512-<base64>@) of 'tarballBytes'.
trueSri :: Text
trueSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy (toLazy tarballBytes) :: Digest SHA512) :: ByteString)

{- | The lower-cased hex SHA-512 of 'tarballBytes' -- the form a __raw 'SHA512'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha512Hex :: Text
trueSha512Hex = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA512) :: ByteString)

-- | The lower-cased hex SHA-256 of 'tarballBytes' (a digest the worker now computes).
trueSha256 :: Text
trueSha256 = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA256) :: ByteString)

-- | The SRI (@sha256-<base64>@) of 'tarballBytes', the resolved-and-computable SRI form.
trueSha256Sri :: Text
trueSha256Sri = "sha256-" <> decodeUtf8 (convertToBase Base64 (hashlazy (toLazy tarballBytes) :: Digest SHA256) :: ByteString)

-- | The lower-cased hex Blake2b-512 of 'tarballBytes' (a digest the worker now computes).
trueBlake2b :: Text
trueBlake2b = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest Blake2b_512) :: ByteString)

-- | The SRI (@sha384-<base64>@) of 'tarballBytes' -- a genuine sha384 the worker computes.
trueSha384Sri :: Text
trueSha384Sri = "sha384-" <> decodeUtf8 (convertToBase Base64 (hashlazy (toLazy tarballBytes) :: Digest SHA384) :: ByteString)

{- | The lower-cased hex SHA-384 of 'tarballBytes' -- the form a __raw 'SHA384'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha384Hex :: Text
trueSha384Hex = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA384) :: ByteString)

{- | A well-formed sha384 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) -- the sha384 tamper-direction fixture: a real sha384 that fails.
-}
falseSha384Sri :: Text
falseSha384Sri = "sha384-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA384) :: ByteString)

{- | A well-formed sha512 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) -- for the tamper-direction regression: a real sha512 that fails.
-}
falseSri :: Text
falseSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA512) :: ByteString)

{- | A well-formed sha512 SRI whose base64 body is the correct digest with its
letter case flipped. base64 is case-sensitive, so this must NOT verify -- a
case-folding comparison would wrongly admit it.
-}
caseVariantSri :: Text
caseVariantSri = "sha512-" <> T.toUpper (fromMaybe "" (T.stripPrefix "sha512-" trueSri))

{- | A well-formed SHA-1 digest that does NOT match 'tarballBytes' (it is sha1 of the
empty string) -- the mismatch fixture, distinct from a malformed one.
-}
wrongSha1 :: Text
wrongSha1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

-- Well-formed digests of the EMPTY input (not of 'tarballBytes'), so each is a real digest
-- of its algorithm that does not match the fetched bytes. 'someMd5' feeds the still-uncomputable
-- MD5 arm (fail-closed); the others are the now-computable algorithms' tamper fixtures (a real,
-- well-formed digest the worker recomputes and finds does not match).
someBlake2b, someSha256, someMd5, someSha256Sri :: Text
someBlake2b = "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
someSha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
someMd5 = "d41d8cd98f00b204e9800998ecf8427e"
someSha256Sri = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="

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

-- ── a recording publish client ─────────────────────────────────────────────────

{- | What a publish captured: the raw verified bytes it was handed, and the artifact
descriptor whose digests the real client's publish document is assembled from (the
descriptor-sourcing case pins it to the re-admitted artifact's exactly).
-}
data PublishLog = PublishLog
    { plDocuments :: [ByteString]
    , plArtifacts :: [MirrorArtifact]
    }

{- | A registry-handle double whose 'publishArtifact' records each call and returns
the given fixed outcome. The mirror-presence probe (the worker's first step) answers
__absent__ -- an unparseable metadata body, the same shape a production mirror gives a
package it does not hold -- so every test drives the full pipeline unless it swaps in
'mirrorListingClient'. The parse fields the worker never exercises return an inert
'ParseError' rather than a fabricated success.
-}
recordingClient :: IORef PublishLog -> Either PublishFault () -> RegistryClient
recordingClient logRef outcome =
    RegistryClient
        { fetchMetadata = const (pure (Right (RegistryResponse "")))
        , publishArtifact = \_ _ artifact document -> do
            atomicModifyIORef' logRef (\l -> (l{plDocuments = document : plDocuments l, plArtifacts = artifact : plArtifacts l}, ()))
            pure outcome
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "absent: nothing mirrored yet"))
        }

{- | 'recordingClient' whose mirror-presence probe __confirms__ the given versions
present at the mirror target, for the dedup short-circuit tests.
-}
mirrorListingClient :: IORef PublishLog -> Either PublishFault () -> [Version] -> RegistryClient
mirrorListingClient logRef outcome versions =
    (recordingClient logRef outcome)
        { parseVersionList = const (Right versions)
        }

{- | 'recordingClient' whose mirror-presence probe reports a __transport fault__ (a
mirror outage) as the typed 'FetchTransport' value, for the probe-cannot-tell
fall-through tests.
-}
probeUnreachableClient :: IORef PublishLog -> Either PublishFault () -> RegistryClient
probeUnreachableClient logRef outcome =
    (recordingClient logRef outcome)
        { fetchMetadata = const (pure (Left (FetchTransport (transportFault TransportUnreachable "simulated mirror outage"))))
        }

-- ── building a worker runtime over doubles ──────────────────────────────────────

{- | Build a 'WorkerRuntime' over a caller-supplied registry-handle double (given the
publish log, so its publishes still record), a real no-TLS manager (for the stub
upstream), a fresh queue + heartbeat, the given worker metrics port, and the given
per-ecosystem re-evaluation policies, then run the body against it. The queue and the
publish log are returned so a test can drive and inspect them. The probe tests use this
directly to swap in 'mirrorListingClient' or 'probeUnreachableClient';
'withRuntimePolicies' is this over 'recordingClient'.
-}
withRuntimeRegistry :: (IORef PublishLog -> RegistryClient) -> WorkerPolicies -> WorkerMetricsPort -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeRegistry mkClient policies metricsPort body = do
    logRef <- newIORef (PublishLog [] [])
    queue <- newInMemoryQueue
    manager <- newManager defaultManagerSettings
    heartbeat <- newWorkerHeartbeat
    let runtime =
            WorkerRuntime
                { wrQueue = queue
                , wrRegistry = mkClient logRef
                , wrManager = manager
                , wrHeartbeat = heartbeat
                , wrMetrics = metricsPort
                , wrTracing = passthroughWorkerTracingPort
                , wrInjectTraceContext = id
                , wrPolicies = policies
                }
    body runtime queue logRef

{- | 'withRuntimeRegistry' with the recording publish client answering the given
publish outcome -- the common case.
-}
withRuntimePolicies :: WorkerPolicies -> WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimePolicies policies metricsPort outcome =
    withRuntimeRegistry (`recordingClient` outcome) policies metricsPort

{- | 'withRuntimePolicies' with the default admitting policy ('admitPolicies'), so the
integrity-gate and publish tests exercise their own path while ingest re-evaluation always
admits; the re-evaluation tests pass their own policies through 'withRuntimePolicies'.
-}
withRuntimeWith :: WorkerMetricsPort -> Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntimeWith = withRuntimePolicies admitPolicies

-- | 'withRuntimeWith' with the inert worker metrics port -- the common case.
withRuntime :: Either PublishFault () -> (WorkerRuntime -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withRuntime = withRuntimeWith noopWorkerMetricsPort

{- | Build a 'WorkerRuntime' over a caller-supplied queue (the publish client is the
never-succeeding recording double, unused here) and run the body against it. Lets a
test drive the supervised loop against a queue whose @receive@ misbehaves.
-}
withQueueRuntime :: MirrorQueue -> (WorkerRuntime -> IO a) -> IO a
withQueueRuntime queue body = do
    logRef <- newIORef (PublishLog [] [])
    manager <- newManager defaultManagerSettings
    heartbeat <- newWorkerHeartbeat
    let runtime =
            WorkerRuntime
                { wrQueue = queue
                , wrRegistry = recordingClient logRef (Right ())
                , wrManager = manager
                , wrHeartbeat = heartbeat
                , wrMetrics = noopWorkerMetricsPort
                , wrTracing = passthroughWorkerTracingPort
                , wrInjectTraceContext = id
                , wrPolicies = admitPolicies
                }
    body runtime

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
re-gating tests refuse (or admit) every host wholesale.
-}
withHostGate :: (Text -> Bool) -> WorkerPolicies -> WorkerPolicies
withHostGate gate = Map.map (\p -> p{wpArtifactHostHonoured = gate})

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
    Artifact
        { artFilename = "thing-1.0.0.tgz"
        , artUrl = "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = [unsafeHash SRI trueSri]
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A minimal projected version snapshot. The injected rules ignore its contents, so only
its validity matters; it stands in for what the shared single-version fetch would project.
-}
sampleDetails :: PackageName -> Version -> PackageDetails
sampleDetails name version =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = []
        , pkgPublisher = Nothing
        }

{- | A resolver that always reports the version present (projected), so the worker runs the
rules over its 'PackageDetails'.
-}
presentResolver :: PackageName -> Version -> IO VersionEvaluation
presentResolver name version = pure (VersionPresent (sampleDetails name version))

{- | A worker-policies map for the npm ecosystem with the given single-version resolver and
prepared rules, clocked at the fixed 'epoch' (the injected rules are not time-sensitive).
-}
npmPolicies :: (PackageName -> Version -> IO VersionEvaluation) -> [PreparedRule] -> WorkerPolicies
npmPolicies resolve rules =
    Map.singleton
        Npm
        WorkerPolicy
            { wpResolveVersion = resolve
            , wpRules = rules
            , wpMinIntegrity = defaultMinIntegrity
            , wpArtifactHostHonoured = const True
            , wpNow = pure epoch
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
    base <- newInMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                pure (Left (queueFault TransportUnreachable "receive: simulated queue outage"))
            }

{- | A queue whose @receive@ always __throws__, counting each call: the handle's
typed contract broken rather than honoured. Stands in for residue (an invariant
break escaping the value channel), so the loop's residual catch-log-backoff arm
stays pinned by a test.
-}
throwingReceiveQueue :: IORef Int -> IO MirrorQueue
throwingReceiveQueue calls = do
    base <- newInMemoryQueue
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

-- Enqueue a job on the in-memory double, unwrapping its never-faulting typed
-- channel: a 'Left' is a broken test premise, failed loudly.
enqueue_ :: MirrorQueue -> MirrorJob -> IO ()
enqueue_ queue job =
    enqueue queue job >>= \case
        Left fault -> fail ("enqueue faulted on the in-memory double: " <> show fault)
        Right () -> pass

-- Receive the currently-visible batch, unwrapping the never-faulting typed channel.
receive_ :: MirrorQueue -> IO [QueueMessage]
receive_ queue =
    receive queue >>= \case
        Left fault -> fail ("receive faulted on the in-memory double: " <> show fault)
        Right messages -> pure messages

-- Enqueue a job, receive it, and return its receipt handle so the per-job processing
-- can be driven with a real handle.
enqueueAndReceive :: MirrorQueue -> MirrorJob -> IO (ReceiptHandle, MirrorJob)
enqueueAndReceive queue job = do
    enqueue_ queue job
    receive_ queue >>= \case
        [message] -> pure (msgReceipt message, job)
        other -> fail ("expected exactly one message, got " <> show other)

-- ── spec ────────────────────────────────────────────────────────────────────────

-- Poll the queue up to @n@ times, returning 'True' as soon as a message reappears
-- (the un-acked job redelivered). The in-memory double may hold a visibility-extended
-- message past one reclaim pass, so more than one poll can be needed.
pollUntilRedelivered :: MirrorQueue -> Int -> IO Bool
pollUntilRedelivered _ 0 = pure False
pollUntilRedelivered queue n =
    receive queue >>= \case
        Right [] -> pollUntilRedelivered queue (n - 1)
        Right _ -> pure True
        Left fault -> fail ("receive faulted on the in-memory double: " <> show fault)

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

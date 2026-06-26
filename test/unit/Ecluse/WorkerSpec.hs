module Ecluse.WorkerSpec (spec) where

import Crypto.Hash (Digest, SHA1, SHA384, SHA512, hashlazy)
import Data.Aeson (Key, Value (Object, String), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, secondsToDiffTime)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Exception (throwString)

import Ecluse.App (runApp)
import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Env (Env, envWorkerHeartbeat, lastPoll, newEnv, newWorkerHeartbeat)
import Ecluse.Package (Hash, HashAlg (Blake2b, MD5, SHA1, SHA256, SRI), PackageName, mkPackageName)
import Ecluse.Package qualified as Pkg
import Ecluse.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    newInMemoryQueue,
 )
import Ecluse.Registry (
    ParseError (ParseError),
    PublishError (PublishError),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (..),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Registry.Npm (npmPublishDocument)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Version (Version, mkVersion)
import Ecluse.Worker (
    IntegrityResult (IntegrityMismatch, IntegrityVerified),
    JobOutcome (Dropped, Retried, Succeeded),
    heartbeatHealthy,
    processBatch,
    processJob,
    verifyIntegrity,
    workerHeartbeatStaleAfter,
    workerLoop,
 )

-- ── fixtures ──────────────────────────────────────────────────────────────────

{- | The tarball bytes the stub upstream serves; the digests in the job fixtures
are computed over exactly these.
-}
tarballBytes :: ByteString
tarballBytes = "the-real-artifact-bytes"

-- | The lower-cased hex SHA-1 of 'tarballBytes' — the shasum a faithful job carries.
trueSha1 :: Text
trueSha1 = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA1) :: ByteString)

-- | The SRI (@sha512-<base64>@) of 'tarballBytes'.
trueSri :: Text
trueSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy (toLazy tarballBytes) :: Digest SHA512) :: ByteString)

{- | The lower-cased hex SHA-512 of 'tarballBytes' — the form a __raw 'SHA512'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha512Hex :: Text
trueSha512Hex = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA512) :: ByteString)

-- | The SRI (@sha384-<base64>@) of 'tarballBytes' — a genuine sha384 the worker computes.
trueSha384Sri :: Text
trueSha384Sri = "sha384-" <> decodeUtf8 (convertToBase Base64 (hashlazy (toLazy tarballBytes) :: Digest SHA384) :: ByteString)

{- | The lower-cased hex SHA-384 of 'tarballBytes' — the form a __raw 'SHA384'-tagged__
digest carries (as opposed to the base64 inside an SRI string).
-}
trueSha384Hex :: Text
trueSha384Hex = decodeUtf8 (convertToBase Base16 (hashlazy (toLazy tarballBytes) :: Digest SHA384) :: ByteString)

{- | A well-formed sha384 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) — the sha384 tamper-direction fixture: a real sha384 that fails.
-}
falseSha384Sri :: Text
falseSha384Sri = "sha384-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA384) :: ByteString)

{- | A well-formed sha512 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) — for the tamper-direction regression: a real sha512 that fails.
-}
falseSri :: Text
falseSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA512) :: ByteString)

{- | A well-formed sha512 SRI whose base64 body is the correct digest with its
letter case flipped. base64 is case-sensitive, so this must NOT verify — a
case-folding comparison would wrongly admit it.
-}
caseVariantSri :: Text
caseVariantSri = "sha512-" <> T.toUpper (fromMaybe "" (T.stripPrefix "sha512-" trueSri))

{- | A well-formed SHA-1 digest that does NOT match 'tarballBytes' (it is sha1 of the
empty string) — the mismatch fixture, distinct from a malformed one.
-}
wrongSha1 :: Text
wrongSha1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

-- Well-formed digests of algorithms the worker cannot recompute (the empty-input
-- digest of each), so the fail-closed-on-uncomputable arm sees a real digest rather
-- than a malformed one; their values are immaterial since the worker never computes them.
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

{- | A mirror job whose artifact descriptor carries the given integrity hashes; its
artifact URL is the stub upstream the test points it at.
-}
jobWith :: Text -> NonEmpty Hash -> MirrorJob
jobWith url hashes =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , jobArtifactUrl = url
        , jobMirrorTarget = "https://mirror.test/thing/-/thing-1.0.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "thing-1.0.0.tgz"
                , maHashes = hashes
                , maSize = Just (BS.length tarballBytes)
                }
        }

-- ── a recording publish client ─────────────────────────────────────────────────

-- | What a publish captured: the bytes (the publish document) it was handed.
newtype PublishLog = PublishLog {plDocuments :: [ByteString]}

{- | A registry-handle double whose 'publishArtifact' records each call and returns
the given fixed outcome; the read/parse fields refuse loudly (unused here).
-}
recordingClient :: IORef PublishLog -> Either PublishFault () -> RegistryClient
recordingClient logRef outcome =
    RegistryClient
        { fetchMetadata = const (refuse "fetchMetadata")
        , fetchArtifact = \_ _ -> refuse "fetchArtifact"
        , publishArtifact = \_ _ document -> do
            atomicModifyIORef' logRef (\l -> (l{plDocuments = document : plDocuments l}, ()))
            pure outcome
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }
  where
    -- The worker only ever publishes through this double; the read fields are wired
    -- to refuse loudly so a test that wrongly reaches one fails rather than passing
    -- on a fabricated result.
    refuse :: Text -> IO a
    refuse field = throwString (toString ("recordingClient: the worker must not use the handle field " <> field))

{- | Build an 'Env' with the recording publish client, a real no-TLS manager (for the
stub upstream), and a fresh queue + heartbeat, then run the body against it. The
queue and the publish log are returned so a test can drive and inspect them.
-}
withWorkerEnv :: Either PublishFault () -> (Env -> MirrorQueue -> IORef PublishLog -> IO a) -> IO a
withWorkerEnv outcome body = do
    logRef <- newIORef (PublishLog [])
    queue <- newInMemoryQueue
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    env <-
        newEnv
            (recordingClient logRef outcome)
            queue
            credentials
            manager
            manager
            metadataCache
            logEnv
            telemetryDisabled
            heartbeat
    body env queue logRef
  where
    credentials :: CredentialProvider
    credentials = staticProvider AuthToken{authSecret = mkSecret "tok", authExpiresAt = Nothing}

{- | Build an 'Env' over a caller-supplied queue (the publish client is the
never-succeeding recording double, unused here) and run the body against it. Lets a
test drive the supervised loop against a queue whose @receive@ misbehaves.
-}
withQueueEnv :: MirrorQueue -> (Env -> IO a) -> IO a
withQueueEnv queue body = do
    logRef <- newIORef (PublishLog [])
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    env <-
        newEnv
            (recordingClient logRef (Right ()))
            queue
            credentials
            manager
            manager
            metadataCache
            logEnv
            telemetryDisabled
            heartbeat
    body env
  where
    credentials :: CredentialProvider
    credentials = staticProvider AuthToken{authSecret = mkSecret "tok", authExpiresAt = Nothing}

{- | A queue whose @receive@ always throws, counting each call. Stands in for a
persistently-failing dependency so the supervised loop's catch-log-backoff arm can
be exercised: the loop must survive a throwing iteration and poll again, not die.
-}
throwingReceiveQueue :: IORef Int -> IO MirrorQueue
throwingReceiveQueue calls = do
    base <- newInMemoryQueue
    pure
        base
            { receive = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                throwString "receive: simulated queue outage"
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

{- | An address with nothing listening — a fetch against it is refused at connect,
the genuine transient fault. Port 1 is in the privileged range and never bound.
-}
unreachableUrl :: Text
unreachableUrl = "http://127.0.0.1:1/thing/-/thing-1.0.0.tgz"

{- | A job artifact URL that cannot be parsed into a request at all (a space and no
scheme), so the worker's by-URL request build fails before any fetch — the
unformable-URL arm, distinct from a reachable-but-failing fetch.
-}
unformableUrl :: Text
unformableUrl = "not a url"

-- Enqueue a job, receive it, and return its receipt handle so the per-job processing
-- can be driven with a real handle.
enqueueAndReceive :: MirrorQueue -> MirrorJob -> IO (ReceiptHandle, MirrorJob)
enqueueAndReceive queue job = do
    enqueue queue job
    receive queue >>= \case
        [message] -> pure (msgReceipt message, job)
        other -> fail ("expected exactly one message, got " <> show (length other))

-- ── spec ────────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
    describe "verifyIntegrity" $ do
        it "verifies a sha1-only artifact against its sha1 (no stronger digest present)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha512)-only artifact against its sha512" $
            verifyIntegrity (unsafeHash SRI trueSri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha384)-only artifact against its sha384 (the worker computes sha384)" $
            -- The whole point of modelling sha384: the worker must be able to RECOMPUTE it,
            -- so a sha384-admitted artifact is verifiable rather than admit-but-uncomputable.
            verifyIntegrity (unsafeHash SRI trueSha384Sri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies a raw SHA384-tagged digest against its hex sha384 (the tag arm, not SRI)" $
            -- A digest carried under the raw 'SHA384' tag (hex), distinct from the same hash
            -- inside an SRI string: the worker computes hex SHA-384 and matches it.
            verifyIntegrity (unsafeHash Pkg.SHA384 trueSha384Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha384 SRI that does not match the fetched bytes (tamper guard)" $
            -- The tamper direction for the new compute path: a real, well-formed sha384 that
            -- is the digest of OTHER bytes must fail closed, naming the algorithm.
            verifyIntegrity (unsafeHash SRI falseSha384Sri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha384 digest did not match the fetched bytes"

        it "prefers and verifies a co-present sha384 over a matching sha1 (strongest wins, and is computable)" $
            -- sha384 outranks sha1, so the gate selects it; because the worker can now compute
            -- sha384 it verifies against it rather than failing closed or downgrading to sha1.
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSha384Sri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "verifies a raw SHA512-tagged digest against its hex sha512 (the tag arm, not SRI)" $
            -- A digest carried under the raw 'SHA512' tag (hex), distinct from the same
            -- hash inside an SRI string: the worker computes hex SHA-512 and matches it,
            -- so this exercises the SHA512-tag compute arm rather than the SRI path.
            -- (Pkg.SHA512 is the HashAlg constructor; the bare SHA512 here is Crypto's.)
            verifyIntegrity (unsafeHash Pkg.SHA512 trueSha512Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies against the strongest digest when both sha512 and sha1 match" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS bytes that match the weak sha1 but fail the strong sha512 (tamper guard)" $
            -- The security crux of the most-authoritative-digest rule: a collision
            -- against the broken SHA-1 must NOT admit an artifact whose sha512 fails.
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI falseSri]) tarballBytes
                `shouldSatisfy` isMismatch

        it "reports a mismatch when the sole digest does not match" $
            verifyIntegrity (unsafeHash SHA1 wrongSha1 :| []) tarballBytes
                `shouldSatisfy` isMismatch

        it "fails closed when the strongest present digest is in an uncomputable algorithm" $
            -- A blake2b ranks at the top but the worker cannot compute it, so it must
            -- NOT fall back to the (matching) sha1 — fail closed.
            verifyIntegrity (unsafeHash Blake2b someBlake2b :| [unsafeHash SHA1 trueSha1]) tarballBytes
                `shouldSatisfy` isMismatch

        it "names the uncomputable strongest algorithm in the fail-closed detail" $
            -- The fail-closed detail is operator-facing: it must name WHICH algorithm
            -- the worker could not verify, so a refused publish is diagnosable. Asserting
            -- the message (not just the constructor) pins that diagnostic.
            mismatchDetail (verifyIntegrity (unsafeHash Blake2b someBlake2b :| [unsafeHash SHA1 trueSha1]) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (Blake2b) is in an algorithm the worker cannot verify"

        it "fails closed on a sha256-only digest (the worker cannot compute SHA-256)" $
            -- SHA-256 outranks a SHA-1, but the worker has no SHA-256 computation, so a
            -- sha256-only artifact must fail closed rather than be admitted unverified —
            -- it must NOT silently fall through to a (non-present) weaker digest.
            mismatchDetail (verifyIntegrity (unsafeHash SHA256 someSha256 :| []) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (SHA256) is in an algorithm the worker cannot verify"

        it "fails closed on an md5-only digest (the worker cannot compute MD5)" $
            -- MD5 is cryptographically broken AND uncomputable here; an md5-only artifact
            -- fails closed, never admitted on the strength of a forgeable hash.
            mismatchDetail (verifyIntegrity (unsafeHash MD5 someMd5 :| []) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (MD5) is in an algorithm the worker cannot verify"

        it "fails closed on an SRI whose inner algorithm is not sha512 (uncomputable)" $
            -- An SRI string names its own algorithm; only sha512 is computable here. A
            -- sha256 SRI ranks below everything (its inner alg does not resolve) and is
            -- uncomputable, so it fails closed — and the detail names it as an SRI.
            mismatchDetail (verifyIntegrity (unsafeHash SRI someSha256Sri :| []) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (SRI sha256) is in an algorithm the worker cannot verify"

        it "fails closed on an unrecognised SRI that outranks a matching weaker digest (no downgrade)" $
            -- The downgrade guard, multi-digest: a present-but-uncomputable strong SRI
            -- (sha256) must OUTRANK a co-present, matching SHA-1 the attacker also
            -- controls, so the gate fails closed instead of admitting the artifact on
            -- the weaker digest. The lone-SRI test above cannot see this — with no
            -- weaker digest present there is nothing to downgrade to.
            mismatchDetail (verifyIntegrity (unsafeHash SRI someSha256Sri :| [unsafeHash SHA1 trueSha1]) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (SRI sha256) is in an algorithm the worker cannot verify"

        it "names the algorithm in a plain (computable) digest mismatch too" $
            -- The non-uncomputable mismatch branch: a sha512 SRI that simply does not
            -- match. The detail names the algorithm via the SRI 'describe' arm, so a
            -- genuine tamper is reported with its algorithm, not anonymously.
            mismatchDetail (verifyIntegrity (unsafeHash SRI falseSri :| []) tarballBytes)
                `shouldBe` Just "the SRI sha512 digest did not match the fetched bytes"

        it "is case-insensitive on the hex shasum" $
            verifyIntegrity (unsafeHash SHA1 (T.toUpper trueSha1) :| []) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS an SRI whose base64 body matches only after case-folding (base64 is case-sensitive)" $
            -- The hex arms fold case (hex is case-insensitive), but an SRI carries a
            -- base64 digest, which is case-sensitive: a body matching the bytes only after
            -- a case change must NOT verify, or the tamper gate is silently weakened.
            verifyIntegrity (unsafeHash SRI caseVariantSri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

    describe "npmPublishDocument" $ do
        it "assembles a PUT document with the version, dist integrity, and base64 attachment" $ do
            let document =
                    npmPublishDocument pkg ver "thing-1.0.0.tgz" (Just trueSri) (Just trueSha1) tarballBytes
                decoded :: Either String Value
                decoded = eitherDecodeStrict' document
            case decoded of
                Left err -> expectationFailure ("publish document is not valid JSON: " <> err)
                Right value -> do
                    stringAt ["name"] value `shouldBe` Just "thing"
                    stringAt ["dist-tags", "latest"] value `shouldBe` Just "1.0.0"
                    stringAt ["versions", "1.0.0", "dist", "integrity"] value `shouldBe` Just trueSri
                    stringAt ["_attachments", "thing-1.0.0.tgz", "data"] value
                        `shouldBe` Just (decodeUtf8 (convertToBase Base64 tarballBytes :: ByteString))

    describe "processJob — the integrity gate" $ do
        it "publishes and reports success when the bytes match the admitted digest" $
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a sha384-only job end to end (fetch, compute sha384, verify, publish)" $
            -- The end-to-end proof that a sha384-admitted artifact is not admit-but-
            -- uncomputable: the worker fetches, recomputes sha384, matches, and publishes.
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SRI trueSha384Sri :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "refuses to publish (no publish) when the bytes do not match the digest" $
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue logRef -> do
                    -- A tampered/substituted artifact: the threaded digest does not match
                    -- the fetched bytes, so the worker must NOT publish.
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "leaves the job for redelivery on a transient fetch failure (no publish)" $
            -- An unreachable upstream (connection refused) is a transient fault: the
            -- fetch throws, so the job is left for redelivery and nothing is published.
            withWorkerEnv (Right ()) $ \env queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runApp env (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "treats a registry rejection as retryable (job left for redelivery)" $
            withUpstream $ \url ->
                withWorkerEnv (Left (PublishRejected (PublishError "503"))) $ \env queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldSatisfy` isRetried

        it "leaves the job for redelivery when the artifact URL is unformable (no publish)" $
            -- A job whose artifact URL cannot be parsed into a request never reaches a
            -- fetch: the by-URL build fails, which the worker treats as a transient
            -- reason (Retried) rather than crashing the iteration. Nothing is published.
            withWorkerEnv (Right ()) $ \env queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unformableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runApp env (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job (non-retryable) when the publish URL is unformable (a config fault)" $
            -- An unformable PUBLISH URL is a misconfiguration redelivery cannot fix, so
            -- the registry handle surfaces it as PublishUrlUnformable and the worker
            -- DROPS the job rather than re-enqueueing it forever — the non-retryable
            -- terminal outcome, distinct from a retryable registry rejection.
            withUpstream $ \url ->
                withWorkerEnv (Left (PublishUrlUnformable EmptyBaseUrl)) $ \env queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldSatisfy` isDropped

    describe "processBatch — ack semantics over the in-memory queue" $ do
        it "acks a successfully-mirrored job so it is not redelivered" $
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue _logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runApp env (processBatch messages)
                    -- A redelivery pass past the (immediate) visibility window yields
                    -- nothing: the job was acked.
                    redelivered <- receive queue
                    redelivered `shouldBe` []

        it "does not ack a transiently-failed job, so it redelivers" $
            withUpstream $ \url ->
                withWorkerEnv (Left (PublishRejected (PublishError "503"))) $ \env queue _logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runApp env (processBatch messages)
                    -- The publish was rejected (retryable), so the job is left un-acked
                    -- and redelivers. The worker extended the message's visibility before
                    -- the (failing) publish, so the in-memory double holds it past one
                    -- reclaim pass; poll a few times so the held message reappears.
                    redelivered <- pollUntilRedelivered queue 5
                    redelivered `shouldBe` True

        it "acks a DROPPED job so a tampered artifact is retired, not redelivered forever" $
            -- An integrity mismatch is non-retryable: redelivery can never make the
            -- bytes match, so the worker must ack the job to retire it from the queue
            -- (having alarmed at the mismatch) rather than leave it to redeliver
            -- indefinitely. A second poll past the visibility window yields nothing.
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    messages <- receive queue
                    runApp env (processBatch messages)
                    -- Nothing was published (the mismatch refused the publish)...
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []
                    -- ...and the job was acked: it does not redeliver.
                    redelivered <- pollUntilRedelivered queue 5
                    redelivered `shouldBe` False

    describe "heartbeat" $ do
        it "advances the last-successful-poll once the loop has polled the queue" $
            withWorkerEnv (Right ()) $ \env _queue _logRef -> do
                pollBefore <- lastPoll (envWorkerHeartbeat env)
                pollBefore `shouldBe` Nothing
                -- Run the consume loop briefly against the (empty) queue, then cancel
                -- it. Even an empty long-poll is a healthy poll, so the heartbeat must
                -- have advanced from 'Nothing'.
                _ <- timeout 200000 (runApp env workerLoop)
                pollAfter <- lastPoll (envWorkerHeartbeat env)
                pollAfter `shouldSatisfy` isJust

    describe "workerLoop — supervision (one bad iteration must not kill the loop)" $
        it "survives a throwing receive: catches, backs off, and polls again" $ do
            -- A persistently-failing queue: every poll throws. The loop is wrapped in
            -- tryAny, so a throwing iteration must be caught, logged, and retried after a
            -- backoff — never escape and tear the worker thread down. The witness is the
            -- receive count: more than one call across the window proves the loop polled
            -- AGAIN after the first throw (it recovered), rather than dying on it.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueEnv queue $ \env -> do
                -- The backoff after a failed iteration is ~1s, so a ~2.5s window admits a
                -- couple of attempts; assert at least a second poll occurred.
                _ <- timeout 2_500_000 (runApp env workerLoop)
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

    describe "heartbeatHealthy (the /livez staleness rule)" $ do
        it "is healthy before the first poll (the worker is starting, not stalled)" $
            heartbeatHealthy epoch Nothing `shouldBe` True

        it "is healthy for a poll within the staleness window" $
            heartbeatHealthy (addUTCTime 10 epoch) (Just epoch) `shouldBe` True

        it "is unhealthy once the last poll is staler than the threshold" $
            heartbeatHealthy (addUTCTime (workerHeartbeatStaleAfter + 1) epoch) (Just epoch)
                `shouldBe` False

-- Poll the queue up to @n@ times, returning 'True' as soon as a message reappears
-- (the un-acked job redelivered). The in-memory double may hold a visibility-extended
-- message past one reclaim pass, so more than one poll can be needed.
pollUntilRedelivered :: MirrorQueue -> Int -> IO Bool
pollUntilRedelivered _ 0 = pure False
pollUntilRedelivered queue n =
    receive queue >>= \case
        [] -> pollUntilRedelivered queue (n - 1)
        _ -> pure True

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

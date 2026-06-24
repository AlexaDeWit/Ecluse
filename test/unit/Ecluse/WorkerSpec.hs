module Ecluse.WorkerSpec (spec) where

import Crypto.Hash (Digest, SHA1, SHA512, hashlazy)
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
import Ecluse.Package (Hash (Hash), HashAlg (Blake2b, SHA1, SRI), PackageName, mkPackageName)
import Ecluse.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue,
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    newInMemoryQueue,
    receive,
 )
import Ecluse.Registry (
    ParseError (ParseError),
    PublishError (PublishError),
    PublishFault (PublishRejected),
    RegistryClient (..),
 )
import Ecluse.Registry.Npm (npmPublishDocument)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled)
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

{- | A well-formed sha512 SRI that does NOT match 'tarballBytes' (it is the digest of
different bytes) — for the tamper-direction regression: a real sha512 that fails.
-}
falseSri :: Text
falseSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA512) :: ByteString)

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
        , parsePackageInfo = const (Left (ParseError "unused"))
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
            verifyIntegrity (Hash SHA1 trueSha1 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha512)-only artifact against its sha512" $
            verifyIntegrity (Hash SRI trueSri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies against the strongest digest when both sha512 and sha1 match" $
            verifyIntegrity (Hash SHA1 trueSha1 :| [Hash SRI trueSri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS bytes that match the weak sha1 but fail the strong sha512 (tamper guard)" $
            -- The security crux of the most-authoritative-digest rule: a collision
            -- against the broken SHA-1 must NOT admit an artifact whose sha512 fails.
            verifyIntegrity (Hash SHA1 trueSha1 :| [Hash SRI falseSri]) tarballBytes
                `shouldSatisfy` isMismatch

        it "reports a mismatch when the sole digest does not match" $
            verifyIntegrity (Hash SHA1 "deadbeef" :| []) tarballBytes
                `shouldSatisfy` isMismatch

        it "fails closed when the strongest present digest is in an uncomputable algorithm" $
            -- A blake2b ranks at the top but the worker cannot compute it, so it must
            -- NOT fall back to the (matching) sha1 — fail closed.
            verifyIntegrity (Hash Blake2b "whatever" :| [Hash SHA1 trueSha1]) tarballBytes
                `shouldSatisfy` isMismatch

        it "is case-insensitive on the hex shasum" $
            verifyIntegrity (Hash SHA1 (T.toUpper trueSha1) :| []) tarballBytes
                `shouldBe` IntegrityVerified

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
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (Hash SHA1 trueSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "refuses to publish (no publish) when the bytes do not match the digest" $
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue logRef -> do
                    -- A tampered/substituted artifact: the threaded digest does not match
                    -- the fetched bytes, so the worker must NOT publish.
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (Hash SHA1 "deadbeef" :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "leaves the job for redelivery on a transient fetch failure (no publish)" $
            -- An unreachable upstream (connection refused) is a transient fault: the
            -- fetch throws, so the job is left for redelivery and nothing is published.
            withWorkerEnv (Right ()) $ \env queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (Hash SHA1 trueSha1 :| []))
                outcome <- runApp env (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "treats a registry rejection as retryable (job left for redelivery)" $
            withUpstream $ \url ->
                withWorkerEnv (Left (PublishRejected (PublishError "503"))) $ \env queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (Hash SHA1 trueSha1 :| []))
                    outcome <- runApp env (processJob receipt job)
                    outcome `shouldSatisfy` isRetried

    describe "processBatch — ack semantics over the in-memory queue" $ do
        it "acks a successfully-mirrored job so it is not redelivered" $
            withUpstream $ \url ->
                withWorkerEnv (Right ()) $ \env queue _logRef -> do
                    enqueue queue (jobWith url (Hash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runApp env (processBatch messages)
                    -- A redelivery pass past the (immediate) visibility window yields
                    -- nothing: the job was acked.
                    redelivered <- receive queue
                    redelivered `shouldBe` []

        it "does not ack a transiently-failed job, so it redelivers" $
            withUpstream $ \url ->
                withWorkerEnv (Left (PublishRejected (PublishError "503"))) $ \env queue _logRef -> do
                    enqueue queue (jobWith url (Hash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runApp env (processBatch messages)
                    -- The publish was rejected (retryable), so the job is left un-acked
                    -- and redelivers. The worker extended the message's visibility before
                    -- the (failing) publish, so the in-memory double holds it past one
                    -- reclaim pass; poll a few times so the held message reappears.
                    redelivered <- pollUntilRedelivered queue 5
                    redelivered `shouldBe` True

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

isDropped :: JobOutcome -> Bool
isDropped = \case
    Dropped _ -> True
    _ -> False

isRetried :: JobOutcome -> Bool
isRetried = \case
    Retried _ -> True
    _ -> False

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory fast lane, proven end to end in two phases against one
booted proxy (the ratified acceptance criterion on the sync slice):

1. __Control__: the bucket is empty, so the young security fix is denied
   (@403@), and the audit body carries both the fast lane's abstain reason
   (no advisory database is loaded) and the quarantine's: proof the CVE rule
   ran, abstained for the stated cause, and the ordinary policy governed.
2. The @osv.db@ compiled by __Pilot's real pipeline__ over the shared advisory
   corpus is uploaded to the ministack S3 bucket; the running sync task's next
   poll detects, verifies, and shadow-swaps it, with no restart and no
   configuration change.
3. The identical request now returns @200@ with the version served: the fast
   lane opened because a synced advisory names it as the exact fix.

Hermetic and gating, but requires a Docker daemon (for @ministack@'s S3).
-}
module Ecluse.CveSyncSpec (spec) where

import Crypto.Hash (Digest, SHA1, SHA512, hashlazy)
import Data.Aeson (Value, encode, object, (.=))
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (Environment (Environment), KatipContextT, LogEnv, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (simpleBody), defaultRequest, request, runSession, setPath)
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (tryAny)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Conduit (runResourceT)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Test qualified as WaiTest

import Ecluse.Composition.MirrorQueue (parseEndpointUrl)
import Ecluse.Config (AppConfig (cfgAwsEndpointUrl), Config (configApp), loadConfig)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve.Slot (currentAdvisoryEtag, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package.Merge (DivergencePolicy (Warn))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (RuleDeps (..), prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan, AllowIfRemediatesCve))
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, tarballHostGate)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Integration.Ministack (endpointFor, withMinistack)
import Ecluse.Runtime.Cve.Sync (CveFetch (fetchDownload), OsvDbFetchFault (OsvDbTooLarge), SyncEnv (..), SyncSchedule (..), runCveSync, s3CveFetch)
import Ecluse.Runtime.Env (newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Pilot.Export (buildS3Env)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1))
import Ecluse.Test.OsvDb (withFixtureOsvDb)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (atDefaultPrecedence)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)

import Ecluse.Runtime.Queue.Sqs (SqsEndpoint (endpointHost, endpointPort))

spec :: Spec
spec =
    aroundAll withMinistack $
        describe "advisory sync + shadow swap (ministack S3, one proxy, two phases)" $
            it "denies the young fix with no database, then admits it once the synced artifact swaps in" $ \container ->
                withSystemTempDir $ \dataDir ->
                    withPublicUpstream $ \publicUrl ->
                        withPrivateUpstream $ \privateUrl -> do
                            -- The bucket, addressed exactly as the released image would
                            -- (the standard AWS_ENDPOINT_URL override), created empty.
                            let endpoint = endpointFor container
                                endpointUrl = "http://" <> endpointHost endpoint <> ":" <> show (endpointPort endpoint)
                                bucket = "cve-sync-spec"
                            appCfg <-
                                either (fail . ("CveSyncSpec fixture env: " <>) . show) (pure . configApp) $
                                    loadConfig (s3EnvVars endpointUrl bucket) Nothing
                            awsEnv <- buildS3Env (cfgAwsEndpointUrl appCfg >>= parseEndpointUrl)
                            createBucketWithRetry awsEnv bucket 30

                            -- One proxy wiring: the slot, the fast-lane policy over it,
                            -- and the sync task polling the (empty) bucket.
                            slot <- newCveSlot
                            let ruleDeps =
                                    RuleDeps
                                        { rdWithCveLookup = withSlotLookup slot
                                        , rdCurrentAdvisoryEtag = currentAdvisoryEtag slot
                                        , rdBreakerReporter = noBreakerReporter
                                        }
                                syncEnv =
                                    SyncEnv
                                        { syncFetch = s3CveFetch awsEnv bucket "npm-osv-schema3.db" (512 * 1024 * 1024)
                                        , syncEcosystem = Npm
                                        , syncDbPath = dataDir <> "/npm-osv-schema3.db"
                                        , syncSlot = slot
                                        }
                                schedule = SyncSchedule{schedBootBackoff = [50_000, 50_000], schedPollDelay = 100_000}
                            app <- proxyApp ruleDeps privateUrl publicUrl
                            withAsync (runQuiet (runCveSync syncEnv schedule pass)) $ \_ -> do
                                -- Phase 1 (control): same configuration, no database.
                                -- The fix is too young for the quarantine and the fast
                                -- lane can only abstain, so the packument has no
                                -- survivors: a 403 whose audit body names both causes.
                                denied <- getThrough app "/npm/corpus-vuln"
                                status denied `shouldBe` 403
                                let deniedBody = decodeUtf8 (simpleBody denied) :: Text
                                deniedBody `shouldSatisfy` T.isInfixOf "no advisory database is loaded"
                                deniedBody `shouldSatisfy` T.isInfixOf "minimum age"

                                -- Publish the artifact Pilot's real pipeline compiles
                                -- from the shared corpus; the running task's next poll
                                -- verifies and swaps it in. No restart, no new config.
                                withFixtureOsvDb CorpusV1 (uploadArtifact awsEnv bucket)

                                -- Phase 2: the identical request is admitted, and the
                                -- served document carries the fixed version.
                                served <- awaitAdmitted app "/npm/corpus-vuln"
                                (decodeUtf8 (simpleBody served) :: Text) `shouldSatisfy` T.isInfixOf "\"1.2.0\""

                                -- The byte cap against the real S3 leg: a fetch whose
                                -- cap the published artifact's declared length
                                -- oversteps fails fast, before any bytes sink, as
                                -- the typed value on the 'CveFetch' channel.
                                let cappedFetch = s3CveFetch awsEnv bucket "npm-osv-schema3.db" 16
                                fetchDownload cappedFetch (dataDir <> "/capped.db.tmp")
                                    `shouldReturn` Left (OsvDbTooLarge 16)
  where
    withSystemTempDir = withSystemTempDirectory "ecluse-cve-sync-spec"

-- The environment layer for the S3 side: the standard endpoint override and
-- credential keys, the required upstream, and the vulnerability bucket.
s3EnvVars :: Text -> Text -> [(String, String)]
s3EnvVars endpointUrl bucket =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.invalid")
    , ("ECLUSE_VULNERABILITY_DATABASE_BUCKET", toString bucket)
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ENDPOINT_URL", toString endpointUrl)
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

-- Create the bucket, retrying while the container's S3 gateway finishes waking
-- (the readiness wait only proves the port accepts connections).
createBucketWithRetry :: AWS.Env -> Text -> Int -> IO ()
createBucketWithRetry awsEnv bucket attempts =
    tryAny (runResourceT (AWS.send awsEnv (S3.newCreateBucket (S3.BucketName bucket)))) >>= \case
        Right _ -> pass
        Left err
            | attempts <= 1 -> fail ("CveSyncSpec: bucket never became creatable: " <> show err)
            | otherwise -> threadDelay 500_000 >> createBucketWithRetry awsEnv bucket (attempts - 1)

-- Upload the compiled artifact under its own (stable, epoch-carrying) file name,
-- the key the sync task polls.
uploadArtifact :: AWS.Env -> Text -> FilePath -> IO ()
uploadArtifact awsEnv bucket dbPath = runResourceT $ do
    body <- liftIO (AWS.chunkedFile 1_048_576 dbPath)
    void (AWS.send awsEnv (S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey (toText (takeFileName dbPath))) body))

-- The in-process proxy: the real serve application over the fast-lane policy
-- (the quarantine plus AllowIfRemediatesCve, both at their shipped defaults),
-- a 404 private upstream, and the packument stub as the public origin.
proxyApp :: RuleDeps -> Text -> Text -> IO Application
proxyApp ruleDeps privateUrl publicUrl = do
    prepared <- prepare ruleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay)), atDefaultPrecedence AllowIfRemediatesCve]
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    queue <- newTestMemoryQueue
    admission <- testServeAdmission
    env <- newEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat
    let deps =
            PackumentDeps
                { pdPrivateBaseUrl = privateUrl
                , pdPublicBaseUrl = publicUrl
                , pdMountBaseUrl = "https://proxy.test/npm"
                , pdMirrorTarget = privateUrl
                , pdRules = prepared
                , pdTarballHostPolicy = SameHostAsPackument
                , pdAdditionalBlockedRanges = []
                , pdTarballHostGate = tarballHostGate privateUrl publicUrl privateUrl
                , pdLimits = defaultLimits
                , pdInboundToken = Nothing
                , pdNow = pure fixedNow
                , pdAdvisoryEtag = pure Nothing
                , pdHelp = Nothing
                , pdMinIntegrity = defaultMinIntegrity
                , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
                , pdDivergencePolicy = Warn
                , pdNewMetadataClient = \t p u c f1 f2 f3 l m b s -> newNpmMetadataClient t p u c f1 f2 f3 (NpmClientConfig b m s l)
                , pdBuildArtifactRequestByFile = \_ _ t s -> artifactRequestByFile t s
                , pdBuildArtifactRequestByUrl = \_ _ t s -> artifactRequestByUrl t s
                , pdAssemble = assembleMergedPackument
                , pdEgressUrl = Right . loopbackRegistryUrl
                }
        binding =
            MountBinding
                { bindingPrefix = "npm" :| []
                , bindingClassifier = Npm.classify
                , bindingPackumentDeps = Just deps
                , bindingPublishDeps = Nothing
                , bindingRenderer = npmRenderer
                }
    pure (application (mkServerConfig [binding]) env)

{- The public upstream: a single-version packument for @corpus-vuln\@1.2.0@,
the exact fixed version the corpus's GHSA-corpus-0001 names, published one day
before the fixed clock, so the quarantine alone always denies it and only the
fast lane can admit it.
-}
withPublicUpstream :: (Text -> IO a) -> IO a
withPublicUpstream k = testWithApplication (pure app) (k . loopbackUrl)
  where
    app :: Application
    app _req respond = respond (responseLBS status200 [] (encode packument))

{- The private upstream: it resolves (the pull-through store knows the package)
but holds no versions yet, so the public leg, and therefore the rules, decide
every version. A private 404 would instead classify as needed-but-unavailable
and turn a total public denial into a retryable 503; resolving keeps the
no-survivors outcome on the policy arm (403), which is the phase-1 control this
test pins.
-}
withPrivateUpstream :: (Text -> IO a) -> IO a
withPrivateUpstream k = testWithApplication (pure app) (k . loopbackUrl)
  where
    app :: Application
    app _req respond =
        respond
            ( responseLBS
                status200
                []
                (encode (object ["name" .= ("corpus-vuln" :: Text), "dist-tags" .= object [], "versions" .= object []]))
            )

packument :: Value
packument =
    object
        [ "name" .= ("corpus-vuln" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.2.0" :: Text)]
        , "versions"
            .= object
                [ "1.2.0"
                    .= object
                        [ "name" .= ("corpus-vuln" :: Text)
                        , "version" .= ("1.2.0" :: Text)
                        , "dist"
                            .= object
                                [ "tarball" .= ("http://localhost:1/corpus-vuln/-/corpus-vuln-1.2.0.tgz" :: Text)
                                , "integrity" .= sha512Integrity
                                , "shasum" .= sha1Shasum
                                ]
                        ]
                ]
        , "time" .= object ["1.2.0" .= ("2026-06-19T00:00:00.000Z" :: Text)]
        ]

-- Placeholder artifact bytes carrying honest digests, so integrity admission
-- never confounds what the rules decided.
artifactBytes :: LByteString
artifactBytes = "corpus-vuln-1.2.0-artifact-bytes"

sha1Shasum :: Text
sha1Shasum = decodeUtf8 (convertToBase Base16 (hashlazy artifactBytes :: Digest SHA1) :: ByteString)

sha512Integrity :: Text
sha512Integrity = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy artifactBytes :: Digest SHA512) :: ByteString)

-- A fixed clock one day after the stub packument's publish time: always inside
-- the 7-day quarantine.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 20) 0

loopbackUrl :: Int -> Text
loopbackUrl port = "http://localhost:" <> show port

getThrough :: Application -> ByteString -> IO SResponse
getThrough app path = runSession (request (setPath defaultRequest path)) app

status :: SResponse -> Int
status = statusCode . WaiTest.simpleStatus

-- Poll the same request until the packument is served (the swap landed), bounded
-- so a broken sync fails the test rather than hanging it.
awaitAdmitted :: Application -> ByteString -> IO SResponse
awaitAdmitted app path = go (150 :: Int)
  where
    go 0 = fail "the artifact never swapped in: the request was still denied after the patience window"
    go n = do
        resp <- getThrough app path
        if status resp == 200
            then pure resp
            else threadDelay 100_000 >> go (n - 1)

newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

runQuiet :: KatipContextT IO a -> IO a
runQuiet action = do
    logEnv <- newTestLogEnv
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

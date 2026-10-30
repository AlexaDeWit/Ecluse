-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise remediation and MAL decisions across accepted Pilot generations, under each
remediation unavailability choice. The running proxy loads it through the Docker-backed S3 fixture.
-}
module Ecluse.Runtime.Cve.SyncIntegrationSpec (spec) where

import Data.Aeson (Value (String), encode, object, (.=))
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Katip (KatipContextT, SimpleLogPayload, runKatipContextT)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import Network.Wai.Test (SResponse (simpleBody))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (tryAny)
import UnliftIO.Async (withAsync)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Conduit (runResourceT)
import Ecluse.Config (loadConfig)
import Ecluse.Core.Cve.Slot (newCveSlot)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Rules (RuleDeps, prepare)
import Ecluse.Core.Rules.Types (
    Decision (BlockedByDefault),
    EvalContext (EvalContext),
    FailureAlignment (FailDeny, FailNoDecision),
    PrecededRule,
    Reason (NoKnownMaliciousAdvisory),
    Rule (AllowIfOlderThan, DenyIfKnownMalicious),
    identityEvidence,
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Integration.Ministack (MinistackPool, endpointFor, quietLogEnv, withMinistack)
import Ecluse.Pilot (PilotCompileOptions (..), UploadRequest (Upload), runPilotCompile)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (fetchDownload), OsvDbFetchFault (OsvDbTooLarge), SyncEnv (..), SyncHooks (SyncHooks, hookFirstSync, hookPushAge), SyncSchedule (..), newS3CveSource, runCveSync, s3CveFetchFor)
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Server.Pipeline.TestSupport (getPath)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Corpus.Advisories (configuredShippedPolicy)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), osvCorpusZip)
import Ecluse.Test.Osv.Withdrawal (maliciousZip)
import Ecluse.Test.OsvDb (epssFixtureFile, withSourceStubs)
import Ecluse.Test.Package (hexSha1Of, npmVersion, sriSha512Of, unscopedNpm)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort, passthroughAdvisorySyncTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, blockedBy, differentialRemediation, evalRules, slotRuleDeps)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Stub (Captured (capHeaders), stubLocalhostUrl, withRoutedStub, withStub)
import Ecluse.Test.Wai (rebaseAuthority, selfBaseUrlOf, status)

import Ecluse.Runtime.Aws.Env (AwsEndpoint (endpointHost, endpointPort))

spec :: MinistackPool -> Spec
spec pool =
    aroundAll (withMinistack pool) $
        describe "advisory sync + shadow swap (ministack S3, one proxy, two phases)" $
            it (caseLabel "integration/advisory-sync/native-001" "refuses or quarantines the young fix with no database, then admits it once the synced artifact swaps in") $ \container ->
                withSystemTempDir $ \dataDir ->
                    withPublicUpstream $ \publicUrl ->
                        withPrivateUpstream $ \privateUrl -> withSweeper $ \deadlines -> do
                            -- The bucket, addressed exactly as the released image would
                            -- (the standard AWS_ENDPOINT_URL override), created empty.
                            let endpoint = endpointFor container
                                endpointUrl = "http://" <> endpointHost endpoint <> ":" <> show (endpointPort endpoint)
                                bucket = "cve-sync-spec"
                            awsEnv <- buildS3Env (Just endpoint)
                            createBucketWithRetry awsEnv bucket 30
                            cveSource <- newS3CveSource (Just endpoint)

                            -- One proxy wiring: the slot, the fast-lane policy over it,
                            -- and the sync task polling the (empty) bucket.
                            slot <- newCveSlot
                            let ruleDeps = slotRuleDeps slot
                                syncEnv =
                                    SyncEnv
                                        { syncFetch = s3CveFetchFor cveSource bucket "npm-osv-schema5.db" (512 * 1024 * 1024)
                                        , syncEcosystem = Npm
                                        , syncEpssRequirement = EpssOptional
                                        , syncDbPath = dataDir <> "/npm-osv-schema5.db"
                                        , syncSlot = slot
                                        , syncStoreRef = "s3://" <> bucket
                                        }
                                schedule = SyncSchedule{schedBootBackoff = [50_000, 50_000], schedPollDelay = 100_000, schedAbsentReport = 600_000_000}
                            app <- proxyApp deadlines ruleDeps configuredShippedPolicy privateUrl publicUrl
                            refusing <- proxyApp deadlines ruleDeps (fastLaneUnder FailDeny) privateUrl publicUrl
                            skipping <- proxyApp deadlines ruleDeps (fastLaneUnder FailNoDecision) privateUrl publicUrl
                            withAsync (runQuiet (runCveSync noopAdvisorySyncMetricsPort passthroughAdvisorySyncTracingPort syncEnv schedule (SyncHooks{hookFirstSync = pass, hookPushAge = pass}))) $ \_ -> do
                                -- The configured MAL deny cannot vet before the first accepted generation.
                                denied <- getPath "/npm/corpus-vuln" app
                                status denied `shouldBe` 503
                                let deniedBody = decodeUtf8 (simpleBody denied) :: Text
                                deniedBody `shouldSatisfy` T.isInfixOf "no advisory database loaded"

                                -- Alone, the remediation rule refuses under deny and leaves the quarantine to decide under skip.
                                refused <- getPath "/npm/corpus-vuln" refusing
                                status refused `shouldBe` 503
                                (decodeUtf8 (simpleBody refused) :: Text) `shouldSatisfy` T.isInfixOf "AllowIfRemediatesCve: no advisory database loaded"
                                quarantined <- getPath "/npm/corpus-vuln" skipping
                                status quarantined `shouldBe` 403

                                -- The running sync task's next poll verifies the new artifact and
                                -- swaps it in, with no restart and no config change.
                                publishViaPilot (Just endpoint) (s3EnvVars endpointUrl bucket) CorpusV1

                                -- Phase 2: the proxy admits the identical request, and
                                -- the served document carries the fixed version.
                                served <- awaitAdmitted app "/npm/corpus-vuln"
                                (decodeUtf8 (simpleBody served) :: Text) `shouldSatisfy` T.isInfixOf "\"1.2.0\""
                                for_ [refusing, skipping] $ \fastLane ->
                                    awaitAdmitted fastLane "/npm/corpus-vuln" >>= \admitted -> (decodeUtf8 (simpleBody admitted) :: Text) `shouldSatisfy` T.isInfixOf "\"1.2.0\""

                                malRules <- prepare ruleDeps [atDefaultPrecedence (DenyIfKnownMalicious FailDeny)]
                                let evidence = identityEvidence (unscopedNpm "corpus-vuln") (npmVersion "1.2.0")
                                    malDecision = evalRules (EvalContext fixedNow Nothing) malRules evidence
                                maliciousZip Npm [("corpus-vuln", ["1.2.0"])] Nothing >>= publishArchive (Just endpoint) (s3EnvVars endpointUrl bucket)
                                pollUntil 120 100_000 ((== Just "DenyIfKnownMalicious") . blockedBy) malDecision
                                    >>= (\decision -> blockedBy decision `shouldBe` Just "DenyIfKnownMalicious")
                                maliciousZip Npm [("corpus-vuln", ["1.2.0"])] (Just (String "2026-10-01T00:00:00Z")) >>= publishArchive (Just endpoint) (s3EnvVars endpointUrl bucket)
                                pollUntil 120 100_000 (== BlockedByDefault [NoKnownMaliciousAdvisory]) malDecision
                                    `shouldReturn` BlockedByDefault [NoKnownMaliciousAdvisory]

                                -- The fetch fails fast on the declared length, before any bytes
                                -- sink to disk.
                                let cappedFetch = s3CveFetchFor cveSource bucket "npm-osv-schema5.db" 16
                                fetchDownload cappedFetch (dataDir <> "/capped.db.tmp")
                                    `shouldReturn` Left (OsvDbTooLarge 16)
  where
    withSystemTempDir = withSystemTempDirectory "ecluse-cve-sync-spec"

-- The environment layer for the S3 side: the standard endpoint override and
-- credential keys, the required upstream, and the advisory store.
s3EnvVars :: Text -> Text -> [(String, String)]
s3EnvVars endpointUrl bucket =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "test-token")
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ENDPOINT_URL", toString endpointUrl)
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

-- Create the bucket, retrying while the container's S3 gateway finishes waking
-- (the readiness wait only proves the port accepts connections).
createBucketWithRetry :: AWS.Env -> Text -> Int -> IO ()
createBucketWithRetry awsEnv bucket attempts =
    pollUntil attempts 500_000 isRight attempt >>= \case
        Right _ -> pass
        Left err -> fail ("CveSyncSpec: bucket never became creatable: " <> show err)
  where
    attempt = tryAny (runResourceT (AWS.send awsEnv (S3.newCreateBucket (S3.BucketName bucket))))

-- The same compile-then-upload cycle the Pilot worker runs, never a direct PutObject, under @env@
-- with the feeds pointed at stubs. The output lands in its own temp dir, apart from the sync's.
publishViaPilot :: Maybe AwsEndpoint -> [(String, String)] -> CorpusVersion -> IO ()
publishViaPilot s3Endpoint env v = osvCorpusZip v >>= publishArchive s3Endpoint env

publishArchive :: Maybe AwsEndpoint -> [(String, String)] -> LByteString -> IO ()
publishArchive s3Endpoint env zipBytes = do
    epssBytes <- readFileLBS epssFixtureFile
    logEnv <- quietLogEnv
    withSourceStubs zipBytes (status200, epssBytes) $ \sources _ -> do
        config <-
            either (fail . ("CveSyncSpec fixture env: " <>) . show) pure $
                loadConfig (env <> sources) Nothing
        withSystemTempDirectory "ecluse-pilot-out" $ \pilotDir ->
            void $
                runPilotCompile
                    logEnv
                    telemetryDisabled
                    s3Endpoint
                    config
                    PilotCompileOptions{pcoEcosystem = "npm", pcoOutDir = pilotDir, pcoUpload = Upload}

-- The quarantine and the shipped remediation limits, under the given unavailability choice.
fastLaneUnder :: FailureAlignment -> [PrecededRule]
fastLaneUnder alignment = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), differentialRemediation alignment]

-- A proxy under this policy, with the packument stub as its public origin.
proxyApp :: Deadlines -> RuleDeps -> [PrecededRule] -> Text -> Text -> IO Application
proxyApp deadlines ruleDeps policy privateUrl publicUrl = do
    prepared <- prepare ruleDeps policy
    manager <- newManager defaultManagerSettings
    queue <- newTestMemoryQueue
    env <- newTestEnvWith deadlines queue (manager, manager) telemetryDisabled
    let deps =
            ( npmServeDeps
                (Just (loopbackRegistryUrl privateUrl))
                (loopbackRegistryUrl publicUrl)
                (MirrorOnAdmit (loopbackRegistryUrl privateUrl))
                prepared
                (pure fixedNow)
            )
                { pdMountBaseUrl = "https://proxy.test/npm"
                , pdEgressUrl = Right . loopbackRegistryUrl
                }
    pure (application (mkServerConfig (maybeToList (mountBindingFor Npm deps Nothing))) env)

-- A young fixed version whose artifact URL resolves against the local stub.
withPublicUpstream :: (Text -> IO a) -> IO a
withPublicUpstream k = withRoutedStub selfHosted (k . stubLocalhostUrl)
  where
    selfHosted cap = (status200, [], rebaseAuthority artifactPlaceholder (selfBaseUrlOf (capHeaders cap)) (encode packument))

-- The authority the committed packument names, replaced by the stub's own before it is served.
artifactPlaceholder :: Text
artifactPlaceholder = "http://localhost:1"

-- The private upstream resolves with no versions, so the public leg and the rules decide.
-- A 404 would turn a total public denial into a retryable 503, hiding the 403 this test pins.
withPrivateUpstream :: (Text -> IO a) -> IO a
withPrivateUpstream k = withStub status200 (encode emptyPackument) (k . stubLocalhostUrl)

-- A well-formed packument naming no versions at all.
emptyPackument :: Value
emptyPackument = object ["name" .= ("corpus-vuln" :: Text), "dist-tags" .= object [], "versions" .= object []]

packument :: Value
packument =
    packumentValue
        "corpus-vuln"
        "1.2.0"
        [
            ( "1.2.0"
            , versionValue
                ( (versionSpec "corpus-vuln" "1.2.0" (artifactPlaceholder <> "/corpus-vuln/-/corpus-vuln-1.2.0.tgz"))
                    { vsIntegrity = Just sha512Integrity
                    , vsShasum = Just sha1Shasum
                    }
                )
            )
        ]
        ["1.2.0" .= ("2026-06-19T00:00:00.000Z" :: Text)]
        []

-- Placeholder artifact bytes carrying honest digests, so integrity admission
-- never confounds what the rules decided.
artifactBytes :: LByteString
artifactBytes = "corpus-vuln-1.2.0-artifact-bytes"

sha1Shasum :: Text
sha1Shasum = hexSha1Of (toStrict artifactBytes)

sha512Integrity :: Text
sha512Integrity = sriSha512Of (toStrict artifactBytes)

-- A fixed clock one day after the stub packument's publish time: always inside
-- the 7-day quarantine.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 20) 0

-- Poll the same request until the proxy serves the packument (the swap landed),
-- bounded so a broken sync fails the test rather than hanging it.
awaitAdmitted :: Application -> ByteString -> IO SResponse
awaitAdmitted app path = do
    resp <- pollUntil 150 100_000 admitted (getPath path app)
    if admitted resp
        then pure resp
        else fail "the artifact never swapped in: the request was still denied after the patience window"
  where
    admitted resp = status resp == 200

runQuiet :: KatipContextT IO a -> IO a
runQuiet action = do
    logEnv <- quietLogEnv
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.AwsEndToEndSpec (spec) where

import Data.Aeson (Value, encode, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Types (status200, status201, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (simpleBody))
import Test.Hspec
import TestContainers (Container)

import Ecluse (mountBindingFor)
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), planMirrorQueue)
import Ecluse.Config (Config (configApp), loadConfig)
import Ecluse.Config.Ambient (ambientAwsFromEnv)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (SRI))
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Integration.Ministack (
    endpointFor,
    freshQueueUrl,
    quietLogEnv,
    withMinistack,
 )
import Ecluse.Integration.WorkerLoop (
    mirrorPoliciesAt,
    newQueueEnv,
    publishedAtLeast,
    runLoopUntil,
    withMirrorTarget,
 )
import Ecluse.Runtime.Aws.Env (AwsEndpoint (endpointHost, endpointPort))
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsWaitSeconds), newSqsQueue)
import Ecluse.Runtime.Server (MountBinding, application, mkServerConfig)
import Ecluse.Server.Pipeline.TestSupport (getPath)
import Ecluse.Test.Package (hexSha1Of, sriSha512Of, unsafeHash)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Stub (stubLocalhostUrl, withStub)
import Ecluse.Test.Wai (localhost, selfBaseUrl, servedVersions, status)

{- | The AWS-backed path end to end through the real composition root: the serve
'Ecluse.Server.application' and the mirror worker 'Ecluse.runWorker' over a real SQS queue in a
@ministack@ container, with WAI npm stubs for both upstreams and the mirror target. Requires a
Docker daemon and never real AWS.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "AWS-backed Écluse end to end (ministack SQS + WAI npm stubs)" $ do
            it "filters public versions by the rules on a packument request" $ \container ->
                withAwsProxy container "aws-e2e-packument" $ \proxy -> do
                    resp <- getPath "/npm/left-pad" (tpApp proxy)
                    status resp `shouldBe` 200
                    -- The 7-day quarantine admits the 2020 version and denies the
                    -- 2-day-old one, so only the survivor is in the served document.
                    servedVersions resp `shouldMatchList` ["1.0.0"]

            it "gates a tarball, enqueues a real SQS job, and the worker mirrors it (fetch → verify → publish)" $ \container ->
                withAwsProxy container "aws-e2e-tarball" $ \proxy -> do
                    resp <- getPath "/npm/left-pad/-/left-pad-1.0.0.tgz" (tpApp proxy)
                    status resp `shouldBe` 200
                    simpleBody resp `shouldBe` tarballBytes
                    -- The worker long-polls the same SQS queue, fetches the artifact,
                    -- verifies it against the re-admitted digest, and publishes it.
                    runLoopUntil (tpPolicies proxy) (tpEnv proxy) (publishedAtLeast (tpMirrorLog proxy) 1)
                    published <- readIORef (tpMirrorLog proxy)
                    length published `shouldSatisfy` (>= 1)
                    published `shouldSatisfy` all (== "/left-pad")

{- | The in-process Écluse under test: the serve 'Application', the composition-root
'Env' the worker runs over, and the mirror-target stub's publish log.
-}
data TestProxy = TestProxy
    { tpApp :: Application
    , tpEnv :: Env
    , tpPolicies :: WorkerPolicies
    , tpMirrorLog :: IORef [ByteString]
    }

{- The queue comes from the production composition root, driven by the AWS-SDK-standard
@AWS_ENDPOINT_URL_SQS@ override pointed at the container, so this fixture runs no test-only path. -}
withAwsProxy :: Container -> Text -> (TestProxy -> IO a) -> IO a
withAwsProxy container queueName body =
    withPrivateUpstream $ \privateUrl ->
        withPublicUpstream $ \publicUrl ->
            withMirrorTarget status201 $ \mirrorUrl mirrorLog -> do
                queue <- configDrivenQueue container queueName
                env <- newQueueEnv queue
                policies <- mirrorPoliciesAt Nothing mirrorUrl (unsafeHash SRI sha512Integrity :| [])
                binding <- mountBinding privateUrl publicUrl mirrorUrl
                let app = application (mkServerConfig (maybeToList binding)) env
                body TestProxy{tpApp = app, tpEnv = env, tpPolicies = policies, tpMirrorLog = mirrorLog}

{- The released image resolves the backend the same way, from @AWS_ENDPOINT_URL_SQS@ and the
standard credential keys. A one-second long poll keeps the worker loop brisk. -}
configDrivenQueue :: Container -> Text -> IO MirrorQueue
configDrivenQueue container queueName = do
    queueUrl <- freshQueueUrl container queueName
    let endpoint = endpointFor container
        endpointUrl = "http://" <> endpointHost endpoint <> ":" <> show (endpointPort endpoint)
    env <- either (fail . ("AwsEndToEndSpec fixture env: " <>) . show) (pure . configApp) (loadConfig (sqsEnvVars queueUrl endpointUrl) Nothing)
    let ambient = ambientAwsFromEnv (sqsEnvVars queueUrl endpointUrl)
    plan <- either (fail . toString . T.unlines . map renderBootError) pure (planMirrorQueue ambient env)
    logEnv <- quietLogEnv
    case plan of
        -- The wire decode's egress former: the loopback dev former, since this
        -- suite's artifact URLs are in-process http servers.
        SqsBackend sqsConfig -> newSqsQueue logEnv (Right . loopbackRegistryUrl) sqsConfig{sqsWaitSeconds = 1}
        MemoryBackend -> fail "AwsEndToEndSpec fixture: expected the SQS backend, got the in-memory one"

-- The environment layer the released image would run with to target a ministack SQS:
-- the standard endpoint override and credential keys, plus the required upstreams.
sqsEnvVars :: Text -> Text -> [(String, String)]
sqsEnvVars queueUrl endpointUrl =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.invalid")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "test-token")
    , ("ECLUSE_QUEUE__URL", toString queueUrl)
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ENDPOINT_URL_SQS", toString endpointUrl)
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

-- The private origin 404s, so every request misses to public. The fixed clock and the week-long
-- quarantine make the rule gate deterministic.
mountBinding :: Text -> Text -> Text -> IO (Maybe MountBinding)
mountBinding privateUrl publicUrl mirrorUrl = do
    prepared <- prepare inertRuleDeps admitOldEnough
    let deps =
            ( npmServeDeps
                (Just (loopbackRegistryUrl privateUrl))
                (loopbackRegistryUrl publicUrl)
                (MirrorOnAdmit (loopbackRegistryUrl mirrorUrl))
                prepared
                (pure fixedNow)
            )
                { pdMountBaseUrl = "https://proxy.test/npm"
                , pdEgressUrl = Right . loopbackRegistryUrl
                }
    pure (mountBindingFor Npm deps Nothing)

{- The packument's @dist.tarball@ names this same loopback host and port, learned from the
request's @Host@ header, so the honoured location is reachable. -}
withPublicUpstream :: (Text -> IO a) -> IO a
withPublicUpstream k = testWithApplication (pure app) (k . localhost)
  where
    app :: Application
    app req respond =
        respond $
            if ".tgz" `BS.isSuffixOf` rawPathInfo req
                then responseLBS status200 [] tarballBytes
                else responseLBS status200 [] (encode (packument (selfBaseUrl req)))

-- The private upstream: a clean 404 miss for everything, so every request falls
-- through to the public origin.
withPrivateUpstream :: (Text -> IO a) -> IO a
withPrivateUpstream k = withStub status404 "{}" (k . stubLocalhostUrl)

-- The artifact bytes the public upstream serves and the worker verifies + publishes.
tarballBytes :: LByteString
tarballBytes = "left-pad-1.0.0-artifact-bytes"

-- The true lower-cased hex SHA-1 (npm @dist.shasum@) of the served bytes.
sha1Shasum :: Text
sha1Shasum = hexSha1Of (toStrict tarballBytes)

-- The true SRI @sha512-<base64>@ (npm @dist.integrity@) of the served bytes: the
-- strongest digest, the one the worker verifies the fetched bytes against.
sha512Integrity :: Text
sha512Integrity = sriSha512Of (toStrict tarballBytes)

{- @1.0.0@ (2020) clears the quarantine and @2.0.0@ (two days before the fixed clock) does not.
Both carry a real integrity digest, so the rule is the only distinguishing factor. -}
packument :: Text -> Value
packument baseUrl =
    packumentValue
        "left-pad"
        "1.0.0"
        [ ("1.0.0", versionObject "1.0.0" "left-pad-1.0.0.tgz" baseUrl)
        , ("2.0.0", versionObject "2.0.0" "left-pad-2.0.0.tgz" baseUrl)
        ]
        [ "1.0.0" .= ("2020-01-01T00:00:00.000Z" :: Text)
        , "2.0.0" .= ("2026-05-30T00:00:00.000Z" :: Text)
        ]
        []

versionObject :: Text -> Text -> Text -> Value
versionObject version file baseUrl =
    versionValue
        ( (versionSpec "left-pad" version (baseUrl <> "/left-pad/-/" <> file))
            { vsIntegrity = Just sha512Integrity
            , vsShasum = Just sha1Shasum
            }
        )

-- A fixed clock. The fixture publishes 2.0.0 two days earlier, well inside the 7-day
-- window.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 1) 0

-- The shipped quarantine: admit a version only once it is older than a week.
admitOldEnough :: [PrecededRule]
admitOldEnough = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

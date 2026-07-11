module Ecluse.Server.Pipeline.SharedSpec (spec) where

import Data.Aeson (Value (Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Ecluse.Server.Pipeline.TestSupport
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Exception (impureThrow, throwString)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Queue.Memory (newInMemoryQueue)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (EffectfulConfig (..), PreparedRule (..), Resilience (..), defaultEffectfulConfig, newBreaker, noBreakerReporter)
import Ecluse.Core.Rules.Types (FailureAlignment (..), RuleVerdict (..))
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Runtime.Env (newEnv, newWorkerHeartbeat)
import Ecluse.Runtime.Log (LogFormat (JsonLog), newLogEnv)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Katip (Environment (Environment), closeScribes)
import Network.Wai.Handler.Warp (testWithApplication)

spec :: Spec
spec = do
    effectfulSpec
    boundsSpec
    boundsLogSpec
    perimeterSpec

mkEffectful :: Text -> Int -> EffectfulConfig -> FailureAlignment -> (PackageDetails -> IO RuleVerdict) -> IO PreparedRule
mkEffectful name prec cfg align eval = do
    breaker <- newBreaker
    pure
        PreparedRule
            { prepName = name
            , prepPrecedence = prec
            , prepResilience = Just (Resilience cfg align breaker noBreakerReporter getCurrentTime)
            , prepEval = \_ pd -> eval pd
            }

downEffectfulRule :: IO PreparedRule
downEffectfulRule =
    mkEffectful "DownAdvisory" 400 defaultEffectfulConfig{ecBackoff = []} FailDeny (\_ -> throwString "advisory source down")

denyingEffectfulRule :: IO PreparedRule
denyingEffectfulRule =
    mkEffectful "DenyAdvisory" 400 defaultEffectfulConfig FailDeny (\_ -> pure (Deny "affected by a known advisory"))

allowingEffectfulRule :: IO PreparedRule
allowingEffectfulRule =
    mkEffectful "AllowAdvisory" 400 defaultEffectfulConfig FailNoDecision (\_ -> pure (Allow "remediates a known advisory"))

effectfulSpec :: Spec
effectfulSpec = describe "effectful rule tier" $ do
    it "filters an Undecidable version out of a packument and 503s when nothing survives" $ do
        rule <- downEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            status resp `shouldNotBe` 404

    it "excludes a version a reachable effectful deny rejects (403 when nothing else survives)" $ do
        rule <- denyingEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403

    it "serves a version a reachable effectful allow admits (a 200 with that version)" $ do
        rule <- allowingEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    (packument [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 1)])
                )
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "serves a tarball a reachable effectful allow admits on the artifact path" $ do
        rule <- allowingEffectfulRule
        privateUp <- privateArtifactMiss
        let young base = packument [("1.0.0", selfHostedVersion base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 1)]
        publicUp <- artifactUpstreamServing (encodePackument . young) publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "maps a concrete-artifact Undecidable to a 503 on the tarball path" $ do
        rule <- downEffectfulRule
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503

tightLimits :: Limits
tightLimits =
    defaultLimits
        { maxBodyBytes = 4096
        , maxVersionCount = 3
        , maxNestingDepth = 8
        }

withLimits :: Limits -> PackumentDeps -> PackumentDeps
withLimits limits d = d{pdLimits = limits}

{- | The typed impure exception the bottoming assembly hook plants: a stand-in
for an invariant break inside the pure render.
-}
newtype AssembleBottom = AssembleBottom Text
    deriving stock (Show)

instance Exception AssembleBottom

-- A deps transform breaking the pdAssemble never-throws contract on purpose,
-- for the request-perimeter case: the assembled render bottoms when forced.
withBottomingAssemble :: PackumentDeps -> PackumentDeps
withBottomingAssemble d = d{pdAssemble = \_ _ _ _ -> impureThrow (AssembleBottom "simulated render invariant break")}

oversizedPackument :: Text -> LByteString
oversizedPackument v =
    encodePackument $ case admittingPublic v of
        Object o -> Object (KeyMap.insert "_padding" (String (T.replicate 8192 "x")) o)
        other -> other

versionFloodPackument :: LByteString
versionFloodPackument =
    encodePackument
        ( packument
            [(v, plainVersion v) | v <- floodVersions]
            "4.0.0"
            [(v, publishedDaysAgo 30) | v <- floodVersions]
        )
  where
    floodVersions :: [Text]
    floodVersions = ["1.0.0", "2.0.0", "3.0.0", "4.0.0"]

undecodableBody :: LByteString
undecodableBody = "!"

deeplyNestedBody :: LByteString
deeplyNestedBody = encodePackument (nest 32 (String "deep"))
  where
    nest :: Int -> Value -> Value
    nest 0 v = v
    nest n v = nest (n - 1) (Aeson.toJSON [v])

boundsSpec :: Spec
boundsSpec = describe "response bounds through the request path (security.md invariant 4)" $ do
    it "refuses an oversized private packument fail-closed, serving only the public set" $ do
        privateUp <- servingUpstream (oversizedPackument "9.9.9")
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "503s when the only (public) packument is oversized and the private upstream is down" $ do
        privateUp <- failingUpstream
        publicUp <- servingUpstream (oversizedPackument "1.0.0")
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "refuses a version-flood public packument fail-closed (nothing served from it)" $ do
        privateUp <- failingUpstream
        publicUp <- servingUpstream versionFloodPackument
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "serves the public set when a version-flood document arrives on the private leg" $ do
        privateUp <- servingUpstream versionFloodPackument
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "refuses a deeply-nested public document fail-closed (503 with the private upstream down)" $ do
        privateUp <- failingUpstream
        publicUp <- servingUpstream deeplyNestedBody
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "serves the public set when a deeply-nested document arrives on the private leg" $ do
        privateUp <- servingUpstream deeplyNestedBody
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "serves normally when every document is within the bounds (no false refusal)" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0", "3.0.0"]

captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-pipeline-log.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved

captureBreachLog :: LByteString -> IO Text
captureBreachLog privateBody = do
    privateUp <- servingUpstream privateBody
    publicUp <- failingUpstream
    queue <- newInMemoryQueue
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            metadataCache <- newMetadataCache defaultCacheConfig
            logEnv <- newLogEnv JsonLog (Environment "test")
            heartbeat <- newWorkerHeartbeat
            env <- newEnv fakeRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat
            baseDeps <- deps privatePort publicPort Nothing
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (withLimits tightLimits baseDeps)
                            , bindingPublishDeps = Nothing
                            , bindingRenderer = npmRenderer
                            }
                        ]
            captureStdout $ do
                _ <- getThing Nothing (application cfg env)
                _ <- closeScribes logEnv
                pure ()

boundsLogSpec :: Spec
boundsLogSpec = describe "serve-path warnings are logged before degrading" $ do
    it "logs a WARNING naming the version-count bound, distinct from a plain parse failure" $ do
        logged <- captureBreachLog versionFloodPackument
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"version-count\""
        logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""

    it "logs a WARNING naming the body-size bound on an oversized body" $ do
        logged <- captureBreachLog (oversizedPackument "9.9.9")
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"body-size\""

    it "logs a WARNING naming the nesting-depth bound on a deeply-nested body" $ do
        logged <- captureBreachLog deeplyNestedBody
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"nesting-depth\""

    it "logs a WARNING on an undecodable upstream body (the case the bound guards leave silent)" $ do
        logged <- captureBreachLog undecodableBody
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "did not decode"

    it "tags every serve-path log line with the emitting module" $ do
        logged <- captureBreachLog versionFloodPackument
        logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline\""

perimeterSpec :: Spec
perimeterSpec = describe "the typed request perimeter (an escaped pre-commit fault)" $
    it "answers a bottoming assembly with the mount-shaped neutral 500, never a torn session" $ do
        -- Both origins serve happily; the assembly hook then bottoms when the
        -- render forces it -- an invariant break escaping the handler pre-commit.
        -- The perimeter must answer it: the session survives (an unanswered
        -- escape would abort it), the status is the neutral 500, and the body is
        -- the mount's own error shape carrying no fault detail.
        privateUp <- failingUpstream
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing withBottomingAssemble $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 500
            (decodeUtf8 (simpleBody resp) :: Text) `shouldSatisfy` T.isInfixOf "internal server error"
            (decodeUtf8 (simpleBody resp) :: Text) `shouldSatisfy` (not . T.isInfixOf "simulated render invariant break")

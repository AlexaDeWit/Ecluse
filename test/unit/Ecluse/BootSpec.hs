-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Process exits, configuration refusal, and resource cleanup at the boot boundary.
module Ecluse.BootSpec (spec) where

import Prelude hiding (get)

import Data.Text qualified as T
import System.Environment (setEnv, unsetEnv, withArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (bracket_, throwIO, timeout, try)

import Ecluse (run)
import Ecluse.Boot (BootAborted (..), BootEnv (beLogEnv), applySecretFileIndirection, applyServerSettings, logBootInfo, orExit, probeServerConfig, readConfigDocument, withBootEnv)
import Ecluse.Composition.BootError (
    BootError (AwsEndpointMalformed, FirstPartyWithoutPrivateUpstream, MirrorRoleWithoutMirroring, PrivateUpstreamOnPublicUpstream, SplitRoleNeedsDurableQueue),
    renderBootError,
 )
import Ecluse.Composition.Support (collapsedMirrorRefusal, collapsingMirrorTarget, expectAppConfig, malformedAwsEndpoint, noMaintenanceBackend, overrideEnv, privateInventoryRefusal, privateUpstreamUrl, withoutMirrorTargetToken, withoutMirrorTargetUrl, withoutQueueUrl)
import Ecluse.Composition.Types (BootRole (BootWithoutPipeline))
import Ecluse.Config (AppConfig (cfgServer), Config (configApp), ServerSettings (srvAuthToken), loadConfig)
import Ecluse.Core.Credential (Secret, mkSecret, unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Server.Readiness (Readiness (Latched), alwaysReady)
import Ecluse.Core.Worker (Liveness (Liveness), alwaysLive)
import Ecluse.Dredger (dredgerServerConfig)
import Ecluse.Mirror (mirrorServerConfig)
import Ecluse.Runtime.Server (
    ServerConfig (scCheckLive, scCheckReady, scDrain, scDrainTimeout, scMounts, scOnException, scPort),
    ShutdownDrainTimeout (ShutdownDrainTimeout),
    beginDrain,
    isDraining,
    mkServerConfig,
    newDrainSignal,
 )
import Ecluse.Test.Log (captureStderr, captureStdout)

runEnv :: [(String, String)]
runEnv =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", privateUpstreamUrl)
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "mirror-write-token")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("ECLUSE_SERVER__PORT", "0")
    ]

codeArtifactRepository :: String
codeArtifactRepository = "https://d-111122223333.d.codeartifact.us-east-1.amazonaws.com/npm/r/"

isRegistryMirrorKey :: String -> Bool
isRegistryMirrorKey name = "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__" `isPrefixOf` name

awsRunEnv :: [(String, String)]
awsRunEnv =
    [ ("AWS_REGION", "us-east-1")
    ]
        <> runEnv

spec :: Spec
spec = do
    describe "shared listener settings" $ do
        forM_ [("default", [], 30), ("override", [("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "7")], 7)] $ \(label, timeoutEnv, expected) ->
            it ("uses the " <> label <> " timeout and configured port for every listener") $ do
                appConfig <- expectAppConfig (("ECLUSE_SERVER__PORT", "9231") : timeoutEnv) Nothing
                let configs =
                        [ applyServerSettings (cfgServer appConfig) (mkServerConfig [])
                        , probeServerConfig appConfig
                        , mirrorServerConfig appConfig (pure alwaysReady) (pure alwaysLive)
                        , dredgerServerConfig appConfig (pure alwaysReady)
                        ]
                forM_ configs $ \cfg -> do
                    scDrainTimeout cfg `shouldBe` ShutdownDrainTimeout expected
                    scPort cfg `shouldBe` 9231
                    null (scMounts cfg) `shouldBe` True

        it "preserves the role probes, exception observer, and live drain signal" $ do
            appConfig <- expectAppConfig [("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "7")] Nothing
            drain <- newDrainSignal
            observed <- newIORef False
            let live = Liveness False Nothing
                cfg =
                    applyServerSettings (cfgServer appConfig) $
                        (mkServerConfig [])
                            { scCheckReady = pure Latched
                            , scCheckLive = pure live
                            , scDrain = drain
                            , scOnException = \_ _ -> writeIORef observed True
                            }
            scCheckReady cfg `shouldReturn` Latched
            scCheckLive cfg `shouldReturn` live
            scOnException cfg Nothing (toException (BootAborted "test observer"))
            readIORef observed `shouldReturn` True
            isDraining (scDrain cfg) `shouldReturn` False
            beginDrain drain
            isDraining (scDrain cfg) `shouldReturn` True

    describe "process log cleanup" $
        forM_ [("normal return", Right ()), ("exceptional exit", Left (SimulatedServiceFault "role failed"))] $ \(label, expected) ->
            it ("drains queued final audit lines on " <> label) $
                withEnvVars runEnv $ do
                    output <- captureStdout $ do
                        result <- try $ withBootEnv BootWithoutPipeline $ \boot -> do
                            replicateM_ 100 (logBootInfo (beLogEnv boot) "final queued audit marker")
                            either throwIO pure expected
                        result `shouldBe` expected
                    length (filter (T.isInfixOf "final queued audit marker") (lines output)) `shouldBe` 100

    describe "run" $ do
        it "boots from the environment layer alone (no document, no AWS_REGION) and serves" $
            -- The queue URL's own host carries the region, so a real SQS
            -- deployment needs no AWS_REGION.
            serves ["proxy"] runEnv

        it "boots the serve-only pure public gate on ENABLED alone (no queue or AWS variables)" $
            serves
                ["proxy"]
                [ ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
                , ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
                , ("ECLUSE_SERVER__PORT", "0")
                ]

        it "boots with a config document at the ECLUSE_CONFIG override path and serves" $
            withDocument "server:\n  helpMessage: booted from the override document\n" $ \path ->
                serves ["proxy"] (readingDocument path awsRunEnv)

        it "aborts fast when the ECLUSE_CONFIG document carries an unknown key (the override is read and validated)" $
            withDocument "bogusKey: 1\n" $ \path ->
                abortsBoot ["proxy"] (readingDocument path awsRunEnv)

        it "aborts fast when ECLUSE_CONFIG points at a missing file (never a silent documentless boot)" $
            abortsBoot ["proxy"] (readingDocument "/nonexistent/ecluse/config.yaml" awsRunEnv)

        it "aborts fast when ECLUSE_CONFIG points at an unreadable path (a typed refusal, not a raw exception)" $
            -- A directory gives an unreadable file path without relying on permission bits.
            withSystemTempDirectory "ecluse-bootspec" $ \dir ->
                abortsBoot ["proxy"] (readingDocument dir awsRunEnv)

        it "names the path and the error for an unreadable document, never its contents" $
            withSystemTempDirectory "ecluse-bootspec" $ \dir -> do
                outcome <- readConfigDocument [("ECLUSE_CONFIG", dir)]
                case outcome of
                    Right r -> expectationFailure ("expected a typed refusal, got " <> show r)
                    Left message -> do
                        message `shouldSatisfy` T.isInfixOf (T.pack dir)
                        message `shouldSatisfy` T.isInfixOf "cannot be read"

        it "aborts fast at boot when the queue URL names the unbuilt pubsub backend" $
            abortsBoot ["proxy"] (overrideEnv "ECLUSE_QUEUE__URL" "projects/acme/topics/mirror" runEnv)

        it "aborts fast at boot when the queue URL's shape names no backend" $
            abortsBoot ["proxy"] (overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" runEnv)

        it "boots on the in-memory mirror queue when no ECLUSE_QUEUE__URL is set (graceful rollover) and serves" $
            serves ["proxy"] (withoutQueueUrl runEnv)

        it "refuses ecluse mirror over the in-memory queue, naming that command" $
            -- The proxy accepts this configuration, so the refusal identifies the dispatched role.
            splitRoleRefusal ["mirror"] `shouldReturn` refusalNaming "ecluse mirror"

        it "refuses ecluse proxy --no-worker over the in-memory queue, naming that command" $
            splitRoleRefusal ["proxy", "--no-worker"] `shouldReturn` refusalNaming "ecluse proxy --no-worker"

        it "refuses ecluse dredger where the mirror target is also the private upstream" $
            bootRefusal ["dredger"] collapsedMirrorEnv
                `shouldReturn` (Left (ExitFailure 2), map renderBootError [collapsedMirrorRefusal, noMaintenanceBackend, privateInventoryRefusal])

        it "aborts fast at boot when the SQS endpoint override is set with no AWS_REGION" $
            -- The override forces the SQS interpretation, and an emulator or VPC
            -- endpoint carries no region in its host, so AWS_REGION must scope it.
            abortsBoot ["proxy"] (overrideEnv "AWS_ENDPOINT_URL_SQS" "http://localhost:4566" runEnv)

        it "aborts fast at boot when a mirror target declares its write token and no url" $
            abortsBoot ["proxy"] (withoutMirrorTargetUrl awsRunEnv)

        it "aborts fast at boot when a registry mirror target has no write token" $
            abortsBoot ["proxy"] (withoutMirrorTargetToken awsRunEnv)

        it "aborts fast at boot when a second tag lands on the declared mirror target" $
            -- Overrides fill keys under a tag without removing another layer's tag.
            abortsBoot ["proxy"] (overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL" codeArtifactRepository awsRunEnv)

    describe "the *_FILE secret indirection" $ do
        it "resolves a secret through *_FILE and serves" $
            withSecretFile $ \secretPath ->
                serves ["proxy"] (readingTokenFile secretPath (withoutMirrorTargetToken runEnv))

        it "refuses a secret supplied both directly and through *_FILE (no silent precedence)" $
            withSecretFile $ \secretPath ->
                abortsBoot ["proxy"] (readingTokenFile secretPath runEnv)

        it "passes JSON-looking *_FILE contents through to the exact secret string" $
            withSystemTempDirectory "ecluse-bootspec" $ \dir -> do
                let secretPath = dir </> "token"
                for_ ["12345", "true", "null"] $ \(payload :: Text) -> do
                    writeFileText secretPath (payload <> "\n")
                    resolved <-
                        applySecretFileIndirection
                            [ ("ECLUSE_SERVER__AUTH_TOKEN_FILE", secretPath)
                            , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN_FILE", secretPath)
                            , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__CODE_ARTIFACT__TOKEN_FILE", secretPath)
                            ]
                    case resolved of
                        Left e -> expectationFailure (toString e)
                        Right env -> do
                            map fst env
                                `shouldMatchList` [ "ECLUSE_SERVER__AUTH_TOKEN"
                                                  , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN"
                                                  , "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__CODE_ARTIFACT__TOKEN"
                                                  ]
                            -- JSON-looking secrets must retain their exact string value through loading.
                            case loadConfig (filter ((== "ECLUSE_SERVER__AUTH_TOKEN") . fst) env) Nothing of
                                Left e -> expectationFailure ("unexpected decode error for " <> toString payload <> ": " <> show e)
                                Right cfg ->
                                    (unSecret <$> srvAuthToken (cfgServer (configApp cfg)))
                                        `shouldBe` Just payload

        it "refuses a *_FILE secret whose file cannot be read" $
            abortsBoot ["proxy"] (readingTokenFile "/nonexistent/ecluse/secret" (withoutMirrorTargetToken runEnv))

    describe "private/public repository collisions"
        $ forM_
            [ ["proxy"]
            , ["proxy", "--no-worker"]
            , ["mirror"]
            , ["dredger"]
            , ["pilot"]
            , ["pilot", "compile", "--out", "scratchpad/refused-compile"]
            , ["check-config"]
            ]
        $ \args -> it ("refuses " <> toString (unwords (map toText args)) <> " before starting services") $ do
            let env = overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" "https://registry.npmjs.org:443/" runEnv
                refusal = renderBootError (PrivateUpstreamOnPublicUpstream Npm "https://registry.npmjs.org:443/")
                expected = case args of
                    ["dredger"] -> [refusal, renderBootError noMaintenanceBackend, renderBootError privateInventoryRefusal]
                    ["check-config"] -> [refusal, "configuration: refused"]
                    _ -> [refusal]
            bootRefusal args env `shouldReturn` (Left (ExitFailure 2), expected)

    describe "first-party names without a private authority"
        $ forM_
            [ ["proxy"]
            , ["proxy", "--no-worker"]
            , ["mirror"]
            , ["dredger"]
            , ["pilot"]
            , ["pilot", "compile", "--out", "scratchpad/refused-compile"]
            , ["check-config"]
            ]
        $ \args -> it ("refuses " <> toString (unwords (map toText args)) <> " before starting services") $ do
            let envVars =
                    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
                    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
                    , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", "@acme")
                    ]
                refusal = renderBootError (FirstPartyWithoutPrivateUpstream Npm)
                expected = case args of
                    ["mirror"] -> [refusal, renderBootError MirrorRoleWithoutMirroring]
                    ["check-config"] -> [refusal, "configuration: refused"]
                    _ -> [refusal]
            bootRefusal args envVars `shouldReturn` (Left (ExitFailure 2), expected)

    describe "check-config (validate and print, boot nothing)" $ do
        it "validates a bootable configuration and exits 0" $
            checkConfig runEnv `shouldReturn` Left ExitSuccess

        it "refuses an invalid configuration with exit 2" $
            checkConfig (filter ((/= "ECLUSE_SERVER__PUBLIC_URL") . fst) runEnv) `shouldReturn` refusedCheck

        it "refuses an unrecognised queue URL with exit 2 (the queue plan is checked too)" $
            checkConfig (overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" runEnv)
                `shouldReturn` refusedCheck

        it "refuses a publication target without first-party namespaces with exit 2 (the boot's own refusal)" $
            checkConfig (publishingTo runEnv) `shouldReturn` refusedCheck

        it "refuses a static publication token without an inbound edge with exit 2" $
            checkConfig
                ( overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__TOKEN" "publish-write-token" $
                    overrideEnv "ECLUSE_MOUNTS__NPM__FIRST_PARTY" "@acme" (publishingTo runEnv)
                )
                `shouldReturn` refusedCheck

        it "refuses an enabled ecosystem with no adapter with exit 2" $
            checkConfig (overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" runEnv) `shouldReturn` refusedCheck

        forM_
            [ ("CodeArtifact", [("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL", codeArtifactRepository)], True)
            ,
                ( "Verdaccio"
                ,
                    [ ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL", "https://mirror.example.test")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN", "write-token")
                    ]
                , True
                )
            , ("registry", filter (isRegistryMirrorKey . fst) runEnv, False)
            , ("serve-only", [], False)
            ]
            $ \(label, mirrorEnv, hasControlPlane) ->
                it ("prints maintenance client notices only for control planes (" <> label <> ")") $ do
                    let envVars = mirrorEnv <> filter (not . isRegistryMirrorKey . fst) runEnv
                        notice = "mount \"npm\": the store maintenance client is built at boot against the live environment. check-config does not attempt this build."
                    output <- checkConfigOutput envVars
                    -- The mount prefix carries the posture and the push-age limit too, so
                    -- this reads the notice itself rather than counting every mount line.
                    filter (T.isInfixOf "the store maintenance client") (lines output)
                        `shouldBe` [notice | hasControlPlane]

        it "refuses an advisory deny with no advisory store with exit 2, naming the rule" $ do
            (outcome, report) <- checkConfigRefusal (overrideEnv "ECLUSE_RULES" cveDenyRule runEnv)
            outcome `shouldBe` refusedCheck
            report `shouldSatisfy` any (T.isInfixOf "enables the advisory deny rules DenyIfCve")

        it "prints the mirror-collapse advisory a writing role boots on" $ do
            -- The typed advisory reaches an operator as this line or as nothing at all, so this
            -- is what pins the render to the print path rather than to the pass that logged it.
            output <- checkConfigOutput collapsedMirrorEnv
            lines output `shouldContain` [collapsedMirrorAdvisory]

    describe "the ambient AWS_ENDPOINT_URL refusal (one verdict for both entry points)" $ do
        it "refuses one malformed override in the boot and in check-config alike" $ do
            let envVars = overrideEnv "AWS_ENDPOINT_URL" malformedAwsEndpoint runEnv
            (bootOutcome, bootReport) <- bootRefusal ["proxy"] envVars
            (checkOutcome, checkReport) <- checkConfigRefusal envVars
            bootOutcome `shouldBe` Left (ExitFailure 2)
            checkOutcome `shouldBe` refusedCheck
            bootReport `shouldBe` [endpointRefusal]
            checkReport `shouldBe` [endpointRefusal, "configuration: refused"]
            -- The override can carry a credential, so no report may echo it.
            checkReport `shouldNotSatisfy` any (T.isInfixOf "s3cr3t")

    describe "orExit (boot fail-fast)" $ do
        it "yields the value on a Right (a passing boot phase)" $
            orExit (const "unused") (Right 7 :: Either () Int) `shouldReturn` 7

        it "reports the failure and aborts the boot on a Left" $ do
            outcome <- try (orExit (const "boot rejected") (Left ()) :: IO ()) :: IO (Either BootAborted ())
            case outcome of
                Left (BootAborted rendered) -> rendered `shouldBe` "boot rejected"
                Right () -> expectationFailure "expected the boot to abort"

{- | Run one case with every key any case here sets cleared, then its own entries alone.
Another spec can leave one behind, so the clearing is what scopes the case.
-}
withEnvVars :: [(String, String)] -> IO a -> IO a
withEnvVars envVars = bracket_ enter (traverse_ unsetEnv caseKeys)
  where
    enter = traverse_ unsetEnv caseKeys >> traverse_ (uncurry setEnv) envVars

-- | Every environment key a case in this module sets.
caseKeys :: [String]
caseKeys =
    ordNub $
        map fst awsRunEnv
            <> [ "ECLUSE_CONFIG"
               , "AWS_ENDPOINT_URL"
               , "AWS_ENDPOINT_URL_SQS"
               , "ECLUSE_RULES"
               , "ECLUSE_MOUNTS__NPM__ENABLED"
               , "ECLUSE_MOUNTS__NPM__FIRST_PARTY"
               , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL"
               , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL"
               , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN"
               , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN_FILE"
               , "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL"
               , "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__TOKEN"
               , "ECLUSE_MOUNTS__RUBYGEMS__ENABLED"
               ]

{- | Start a role under these entries and hold it for 100 ms. 'Nothing' is the bound expiring
with the role still serving, which is what a boot that reached its listeners answers.
-}
serves :: [String] -> [(String, String)] -> Expectation
serves args envVars = withEnvVars envVars (timeout 100000 (withArgs args run)) `shouldReturn` Nothing

-- | 'serves' for a boot that must abort before its listeners, reporting the status it took.
abortsBoot :: [String] -> [(String, String)] -> Expectation
abortsBoot args envVars =
    withEnvVars envVars (try (timeout 100000 (withArgs args run)) :: IO (Either ExitCode (Maybe ())))
        `shouldReturn` Left (ExitFailure 2)

-- | Run the checker under these entries, keeping the status it exited with.
checkConfig :: [(String, String)] -> IO (Either ExitCode ())
checkConfig envVars = withEnvVars envVars (try (withArgs ["check-config"] run))

-- | What the checker exits with on a configuration it refuses.
refusedCheck :: Either ExitCode ()
refusedCheck = Left (ExitFailure 2)

-- | The checker's own standard output over a configuration it must clear.
checkConfigOutput :: [(String, String)] -> IO Text
checkConfigOutput envVars =
    withEnvVars envVars . captureStdout $
        (try (withArgs ["check-config"] run) :: IO (Either ExitCode ())) `shouldReturn` Left ExitSuccess

-- | The checker's status and the lines it reported on standard error.
checkConfigRefusal :: [(String, String)] -> IO (Either ExitCode (), [Text])
checkConfigRefusal envVars = withEnvVars envVars $ do
    outcome <- newIORef (Nothing :: Maybe (Either ExitCode ()))
    report <- captureStderr (try (withArgs ["check-config"] run) >>= writeIORef outcome . Just)
    readIORef outcome >>= \case
        Nothing -> fail "the checker left no outcome behind"
        Just result -> pure (result, reportLines report)

-- | The status a boot took and the lines it reported on standard error.
bootRefusal :: [String] -> [(String, String)] -> IO (Either ExitCode (Maybe ()), [Text])
bootRefusal args envVars = withEnvVars envVars $ do
    outcome <- newIORef (Nothing :: Maybe (Either ExitCode (Maybe ())))
    report <- captureStderr $ do
        -- Guard against a hung boot, without requiring refusal within a boot-speed deadline.
        result <- try (timeout 5_000_000 (withArgs args run))
        writeIORef outcome (Just result)
    readIORef outcome >>= \case
        Nothing -> fail "the boot left no outcome behind"
        Just result -> pure (result, reportLines report)

-- | Write a configuration document to a temporary path and hand the path to the case.
withDocument :: Text -> (FilePath -> IO a) -> IO a
withDocument body use =
    withSystemTempDirectory "ecluse-bootspec" $ \dir -> do
        let path = dir </> "config.yaml"
        writeFileText path body
        use path

-- | Write the mirror write token to a temporary file and hand the path to the case.
withSecretFile :: (FilePath -> IO a) -> IO a
withSecretFile use =
    withSystemTempDirectory "ecluse-bootspec" $ \dir -> do
        let path = dir </> "mirror-token"
        writeFileText path "mirror-write-token\n"
        use path

-- | Point ECLUSE_CONFIG at a document path.
readingDocument :: FilePath -> [(String, String)] -> [(String, String)]
readingDocument = overrideEnv "ECLUSE_CONFIG"

-- | Supply the mirror write token through its @*_FILE@ indirection.
readingTokenFile :: FilePath -> [(String, String)] -> [(String, String)]
readingTokenFile = overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN_FILE"

-- | Declare a publication target on the npm mount, with no first-party namespaces of its own.
publishingTo :: [(String, String)] -> [(String, String)]
publishingTo = overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL" "https://publish.example.test"

splitRoleRefusal :: [String] -> IO (Either ExitCode (Maybe ()), [Text])
splitRoleRefusal args = bootRefusal args (withoutQueueUrl runEnv)

-- | A shared policy carrying one advisory deny, which needs a store no fixture here configures.
cveDenyRule :: String
cveDenyRule = "{\"gate\":{\"type\":\"DenyIfCve\",\"minCvss\":8}}"

collapsedMirrorEnv :: [(String, String)]
collapsedMirrorEnv = collapsingMirrorTarget runEnv

-- The advisory 'collapsedMirrorEnv' earns, as check-config prints it: its 'warn' prefix included.
collapsedMirrorAdvisory :: Text
collapsedMirrorAdvisory =
    "warning: mount \"npm\": mirrorTarget and privateUpstream resolve to the same registry (https://private.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"

refusalNaming :: Text -> (Either ExitCode (Maybe ()), [Text])
refusalNaming invocation =
    (Left (ExitFailure 2), [renderBootError (SplitRoleNeedsDurableQueue invocation)])

malformedSecret :: Secret
malformedSecret = mkSecret (toText malformedAwsEndpoint)

endpointRefusal :: Text
endpointRefusal = renderBootError (AwsEndpointMalformed malformedSecret)

reportLines :: Text -> [Text]
reportLines = filter (not . T.null) . lines

newtype SimulatedServiceFault = SimulatedServiceFault Text
    deriving stock (Eq, Show)

instance Exception SimulatedServiceFault

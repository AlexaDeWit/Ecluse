-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared process-boot bracket for Écluse service roles.

'withBootEnv' applies @*_FILE@ secret indirection, locates the configuration
document under the @ECLUSE_CONFIG@ semantics, and validates it. It then applies the
runtime posture, builds the process logger, and brackets the telemetry substrate. It
hands the resulting 'BootEnv' to role-specific composition roots such as
"Ecluse.Proxy", which build their own service resources only after boot succeeds.
-}
module Ecluse.Boot (
    BootEnv (..),
    applySecretFileIndirection,
    readConfigDocument,
    withBootEnv,
    BootAborted (..),
    orExit,
    logBootWarning,
    logBootInfo,
    logRuleBootOrder,
    buildMirrorQueue,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Katip (Environment (Environment), LogEnv, Severity (InfoS, WarningS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import System.Environment (getEnvironment)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import UnliftIO (throwIO, tryIO)

import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    deadLetterTerminusWarning,
    memoryQueueDropWarning,
    mirrorQueuePlanWarning,
 )
import Ecluse.Config (
    AppConfig (cfgObservability, cfgRuntime),
    Config (configApp),
    ObservabilitySettings (obsLogFormat, obsLogLevel, obsTelemetry),
    RuntimeSettings (rtCores, rtMaxHeapBytes),
    loadConfig,
    mountCollisionWarnings,
    renderConfigError,
    resolvedKeyProvenance,
 )
import Ecluse.Config.Ambient (AmbientAws, ambientAwsFromEnv)
import Ecluse.Config.Resolve (secretEnvSpellings)
import Ecluse.Core.Queue (MirrorQueue (deadLetterTerminus, deliveryBudget))
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Rules (renderBootOrder)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (pdRules))
import Ecluse.Rts (EffectiveRuntimePlan, applyRuntimePosture)
import Ecluse.Runtime.Log (moduleField, newLogEnv)
import Ecluse.Runtime.Queue.Sqs (newSqsQueue)
import Ecluse.Runtime.Server (MountBinding (bindingPackumentDeps, bindingPrefix))
import Ecluse.Runtime.Telemetry (Telemetry, TelemetrySwitch (TelemetryOff, TelemetryOn), withTelemetry)
import Ecluse.Runtime.Telemetry.Correlation (ddIdentityFromEnvironment)
import Ecluse.Runtime.Telemetry.Resolve (prepareTelemetry)

{- | The boot context 'withBootEnv' assembles once at start-up and hands to each
subcommand. Each subcommand builds the heavier serve- and worker-side handles later: the
HTTP managers, the mirror queue, and the metadata cache (see "Ecluse.Proxy").
-}
data BootEnv = BootEnv
    { beConfig :: AppConfig
    -- ^ The application-level configuration slice the subcommands read.
    , beAmbient :: AmbientAws
    {- ^ The ambient AWS SDK environment (region, endpoint overrides), read from
    the process environment beside the config, never through the config AST.
    -}
    , beLogEnv :: LogEnv
    -- ^ The process structured-logging environment.
    , beTelemetry :: Telemetry
    -- ^ The telemetry handle, inert unless @ECLUSE_OBSERVABILITY__TELEMETRY@ enabled it.
    , beConfigFull :: Config
    {- ^ The whole loaded configuration document, for subcommands that need more than
    'beConfig' (the serve path's mount and rule wiring, for one).
    -}
    , beRuntimePlan :: EffectiveRuntimePlan
    {- ^ The resolved runtime posture (capabilities and heap ceiling, each with its provenance).
    The downstream sizings and the memory plan compute from it.
    -}
    }

{- | Apply the @*_FILE@ secret indirection: a recognised secret variable supplied as
@\<VAR\>_FILE@ names a file whose contents become its value, one trailing newline
stripped. A token then never has to enter the environment itself.

Only secret-typed keys are eligible, so any other @*_FILE@ spelling fails the strict
parser as an unknown key. Setting both a base variable and its @_FILE@ form is a
fail-loud conflict, and an unreadable file fails the same way. Failures aggregate into
one report.
-}
applySecretFileIndirection :: [(String, String)] -> IO (Either Text [(String, String)])
applySecretFileIndirection envVars = do
    reads' <- traverse readOne fileVars
    let (readErrs, resolved) = partitionEithers reads'
    pure $ case conflicts <> readErrs of
        [] -> Right (filter (not . isSecretFileVar . fst) envVars <> resolved)
        errs -> Left (T.unlines errs)
  where
    fileVars = filter (isSecretFileVar . fst) envVars

    conflicts =
        [ T.pack base <> " and " <> T.pack name <> " are both set: supply the secret through exactly one of them"
        | (name, _) <- fileVars
        , let base = baseVarOf name
        , isJust (lookup base envVars)
        ]

    readOne (name, path) = do
        outcome <- tryIO (readFileBS path)
        pure $ case outcome of
            Left err ->
                Left (T.pack name <> " points at " <> T.pack path <> ", which cannot be read: " <> T.pack (displayException err))
            Right bytes ->
                Right (baseVarOf name, T.unpack (T.dropWhileEnd (== '\n') (decodeUtf8 bytes)))

    isSecretFileVar name =
        let spelling = T.pack name
         in "ECLUSE_" `T.isPrefixOf` spelling && any (`T.isSuffixOf` spelling) secretFileSuffixes

    -- Total even though the callers only pass matched names: an unmatched name
    -- passes through rather than inventing a partial strip.
    baseVarOf name = maybe name T.unpack (T.stripSuffix "_FILE" (T.pack name))

    -- The secret-typed keys, by their env-spelling tails. Anything else keeps the
    -- strict no-secrets-in-config posture, with no file-shaped side door.
    secretFileSuffixes :: [Text]
    secretFileSuffixes = map (<> "_FILE") secretEnvSpellings

{- | Locate and read the config document per the @ECLUSE_CONFIG@ semantics: the bytes and
the path when a document exists, no bytes at an absent default path, and a refusal when an
explicit @ECLUSE_CONFIG@ resolves to nothing. A misconfiguration must never silently boot
without the document the operator pointed at. Any other read failure refuses too, naming
the path and the error but never the file contents.
-}
readConfigDocument :: [(String, String)] -> IO (Either Text (Maybe ByteString, FilePath))
readConfigDocument envVars = do
    let explicitPath = nonBlankPath =<< lookup "ECLUSE_CONFIG" envVars
        docPath = fromMaybe defaultConfigPath explicitPath
    mDocBlob <- tryIO (BS.readFile docPath)
    pure $ case mDocBlob of
        Right bytes -> Right (Just bytes, docPath)
        Left err
            | isDoesNotExistError err ->
                case explicitPath of
                    Nothing -> Right (Nothing, docPath)
                    Just path ->
                        Left
                            ( "ECLUSE_CONFIG points at "
                                <> T.pack path
                                <> ", but no config document exists there; fix the path, or unset ECLUSE_CONFIG to use "
                                <> T.pack defaultConfigPath
                            )
            | otherwise ->
                Left
                    ( "config document at "
                        <> T.pack docPath
                        <> " cannot be read: "
                        <> T.pack (ioeGetErrorString err)
                    )

-- The shipped default. A non-blank ECLUSE_CONFIG relocates it.
defaultConfigPath :: FilePath
defaultConfigPath = "/etc/ecluse/config.yaml"

nonBlankPath :: FilePath -> Maybe FilePath
nonBlankPath p = if T.null (T.strip (T.pack p)) then Nothing else Just p

{- | Assemble the 'BootEnv' and run @action@ within it. The boot fails fast on any
configuration error, before it applies the runtime posture.
-}
withBootEnv :: (BootEnv -> IO ()) -> IO ()
withBootEnv action = do
    rawEnvVars <- getEnvironment
    envVars <- applySecretFileIndirection rawEnvVars >>= orExit id
    let ambient = ambientAwsFromEnv envVars
    (docBlob, docPath) <- readConfigDocument envVars >>= orExit id
    config <- orExit (T.unlines . map renderConfigError) (loadConfig envVars docBlob)
    let env = configApp config
        observability = cfgObservability env
        runtimeSettings = cfgRuntime env
    -- Resolve the log identity from the same table the SDK reads
    -- ("Ecluse.Runtime.Telemetry.Resolve"), before any OTEL_* projection applies. A boot
    -- line then carries the same identity as a served request.
    ddIdentity <- ddIdentityFromEnvironment
    logEnv <- newLogEnv (obsLogFormat observability) (obsLogLevel observability) ddIdentity (Environment "production")
    -- Resolve and apply the runtime posture before anything else spins up. This may
    -- exec the binary in place to enforce a heap ceiling (same PID, see Ecluse.Rts).
    -- Nothing stateful must precede it beyond config and the logger.
    runtimePlan <-
        applyRuntimePosture (logBootInfo logEnv) (logBootWarning logEnv) (rtCores runtimeSettings) (rtMaxHeapBytes runtimeSettings)
    logBootInfo logEnv $ case docBlob of
        Just _ -> "Config document: " <> T.pack docPath
        Nothing -> "Config document: none at " <> T.pack docPath <> " (defaults and environment only)"
    -- One provenance line per resolved config key (secrets redacted), so an operator reads the
    -- effective posture and its origin straight from the boot log.
    traverse_ (logBootInfo logEnv) (resolvedKeyProvenance envVars docBlob)
    traverse_ (logBootWarning logEnv) (mountCollisionWarnings config)
    prepareTelemetryBoot (obsTelemetry observability) logEnv
    withTelemetry (obsTelemetry observability) logEnv $ \telemetry ->
        action
            BootEnv
                { beConfig = env
                , beAmbient = ambient
                , beLogEnv = logEnv
                , beTelemetry = telemetry
                , beConfigFull = config
                , beRuntimePlan = runtimePlan
                }

{- Build the config-selected mirror queue: the durable AWS SQS backend, or the bounded
in-memory backend. Only the memory arm spends @memoryDepth@, so the memory plan allocates
that tenant after the backend selection. 'mirrorQueuePlanWarning' and
'deadLetterTerminusWarning' decide the warnings, and this is only the call site. -}
buildMirrorQueue :: LogEnv -> Int -> MirrorQueuePlan -> IO MirrorQueue
buildMirrorQueue logEnv memoryDepth plan = do
    whenJust (mirrorQueuePlanWarning plan) (logBootWarning logEnv)
    queue <- case plan of
        SqsBackend sqsConfig -> newSqsQueue logEnv mkRegistryUrl sqsConfig
        MemoryBackend ->
            newBoundedInMemoryQueue (defaultMemoryQueueConfig memoryDepth) (logBootWarning logEnv . memoryQueueDropWarning)
    whenJust (deadLetterTerminusWarning plan (deliveryBudget queue) (deadLetterTerminus queue)) (logBootWarning logEnv)
    pure queue

{- Log one line at 'WarningS' through the composition-root 'LogEnv'. The boot phase holds
no @Handler@ reader, so it uses this plain-'IO' katip path. -}
logBootWarning :: LogEnv -> Text -> IO ()
logBootWarning logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM WarningS (ls message))

{- Log one line at 'InfoS' through the composition-root 'LogEnv', the same plain-'IO'
katip path 'logBootWarning' uses, for non-warning boot diagnostics. -}
logBootInfo :: LogEnv -> Text -> IO ()
logBootInfo logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM InfoS (ls message))

{- Log every wired mount's resolved rule boot order, the same total order evaluation walks,
so an operator reads the effective policy resolution from the start-up log. A mount with
no packument deps (the unserved stub) contributes nothing. -}
logRuleBootOrder :: LogEnv -> [MountBinding] -> IO ()
logRuleBootOrder logEnv = traverse_ logMount
  where
    logMount binding = do
        let deps = bindingPackumentDeps binding
        let label = T.intercalate "/" (toList (bindingPrefix binding))
        logBootInfo logEnv ("rule boot order for mount " <> label <> ":")
        traverse_ (logBootInfo logEnv) (renderBootOrder (pdRules deps))

{- | Raised to abort start-up after a boot phase reported its failure to stderr. A distinct
type rather than a bare 'exitFailure' lets a test observe the abort without the process
exiting. Uncaught, it reaches 'main' and the runtime exits non-zero.
-}
data BootAborted = BootAborted
    deriving stock (Eq, Show)

instance Exception BootAborted

{- Report the rendered failure to stderr and throw 'BootAborted' when a phase fails,
otherwise yield its value. It writes the whole aggregated block, so an operator sees every
problem from one failed launch. -}
orExit :: (e -> Text) -> Either e a -> IO a
orExit render = \case
    Right a -> pure a
    Left err -> TIO.hPutStrLn stderr (render err) >> throwIO BootAborted

{- Prepare the telemetry substrate before the SDK initialises, through
"Ecluse.Runtime.Telemetry.Resolve.prepareTelemetry". With @ECLUSE_OBSERVABILITY__TELEMETRY@
off it is a no-op, reading no process environment. -}
prepareTelemetryBoot :: TelemetrySwitch -> LogEnv -> IO ()
prepareTelemetryBoot switch logEnv = case switch of
    TelemetryOff -> pass
    TelemetryOn -> do
        environment <- getEnvironment
        prepareTelemetry logEnv environment

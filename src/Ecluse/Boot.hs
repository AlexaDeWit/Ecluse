-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared process-boot bracket for Écluse service roles.

'withBootEnv' applies @*_FILE@ secret indirection, locates the configuration
document under the @ECLUSE_CONFIG@ semantics, validates it, applies the runtime
posture, builds the process logger, and brackets the telemetry substrate. It
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
    memoryQueueDropWarning,
    mirrorQueuePlanWarning,
 )
import Ecluse.Config (
    AppConfig (cfgObservability, cfgRuntime),
    Config (configApp),
    ObservabilitySettings (obsLogFormat, obsTelemetry),
    RuntimeSettings (rtCores, rtMaxHeapBytes),
    loadConfig,
    mountCollisionWarnings,
    renderConfigError,
    resolvedKeyProvenance,
 )
import Ecluse.Config.Ambient (AmbientAws, ambientAwsFromEnv)
import Ecluse.Config.Resolve (secretEnvSpellings)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Rules (renderBootOrder)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (pdRules))
import Ecluse.Rts (EffectiveRuntimePlan, applyRuntimePosture)
import Ecluse.Runtime.Log (moduleField, newLogEnv)
import Ecluse.Runtime.Queue.Sqs (newSqsQueue)
import Ecluse.Runtime.Server (MountBinding (bindingPackumentDeps, bindingPrefix))
import Ecluse.Runtime.Telemetry (Telemetry, TelemetrySwitch (TelemetryOff, TelemetryOn), withTelemetry)
import Ecluse.Runtime.Telemetry.Resolve (prepareTelemetry)

{- | The boot context assembled once at start-up and handed to each subcommand: the
validated configuration, the process logger, and the telemetry handle. 'withBootEnv'
builds it, and the @ecluse@ entry point (see "Ecluse") dispatches the selected
subcommand over it. The heavier serve- and worker-side handles (the HTTP managers,
the mirror queue, the metadata cache) are built later, per subcommand (see
"Ecluse.Proxy").
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
    {- ^ The resolved runtime posture (capabilities and heap ceiling, each with its
    provenance), the datapoint the downstream sizings and the memory plan
    compute from.
    -}
    }

{- | Apply the @*_FILE@ secret indirection: a recognised secret variable may be
supplied as @\<VAR\>_FILE@ naming a file whose contents (one trailing newline
stripped) become the variable's value -- the standard container-secret mount
pattern, so a token never has to enter the environment itself. Only the
secret-typed keys are eligible; any other @*_FILE@ spelling transliterates to an
unknown document key and is rejected by the strict parser as usual. Setting both
a base variable and its @_FILE@ form is a fail-loud conflict (never a silent
precedence choice), and an unreadable file fails the same way; failures
aggregate so one run reports them all. Shared by the boot and @check-config@.
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

    -- The secret-typed keys, by their env-spelling tails; anything else keeps the
    -- strict no-secrets-in-config posture with no file-shaped side door.
    secretFileSuffixes :: [Text]
    secretFileSuffixes = map (<> "_FILE") secretEnvSpellings

{- | Locate and read the config document per the @ECLUSE_CONFIG@ semantics: the
bytes when a document exists (plus the path consulted), no bytes at an absent
default path (env + defaults alone boot a proxy), and a fail-loud message for an
explicit @ECLUSE_CONFIG@ that resolves to nothing -- a misconfiguration must never
silently boot without the document the operator pointed at. Any other read
failure (a permission error, a directory path) is a typed refusal too, naming the
path and the error but never the file contents. Shared by the boot
('withBootEnv') and @check-config@ ("Ecluse.CheckConfig"), so the two cannot
drift on the override semantics.
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

-- The shipped default; ECLUSE_CONFIG (non-blank) relocates it.
defaultConfigPath :: FilePath
defaultConfigPath = "/etc/ecluse/config.yaml"

nonBlankPath :: FilePath -> Maybe FilePath
nonBlankPath p = if T.null (T.strip (T.pack p)) then Nothing else Just p

{- | Assemble the 'BootEnv' and run @action@ within it: load and validate the
configuration (failing fast on any error), apply the runtime posture, build the
logger, and bracket the telemetry substrate for the action's lifetime.
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
    logEnv <- newLogEnv (obsLogFormat observability) (Environment "production")
    -- Resolve and apply the runtime posture before anything else spins up: this may
    -- exec the binary in place (same PID; see Ecluse.Rts) to enforce a heap
    -- ceiling, so nothing stateful must precede it beyond config and the logger.
    runtimePlan <-
        applyRuntimePosture (logBootInfo logEnv) (logBootWarning logEnv) (rtCores runtimeSettings) (rtMaxHeapBytes runtimeSettings)
    logBootInfo logEnv $ case docBlob of
        Just _ -> "Config document: " <> T.pack docPath
        Nothing -> "Config document: none at " <> T.pack docPath <> " (defaults and environment only)"
    -- The resolved configuration, one provenance line per key (secrets redacted),
    -- so the effective posture and where each value came from read straight from
    -- the boot log.
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

{- Build the config-selected mirror queue from its plan and the memory plan's queue
depth: the durable AWS SQS backend, or the bounded in-memory backend. The depth is a
memory tenant, so it is allocated after the backend selection and parametrises only
this build (the SQS arm never spends it). The in-memory arm first emits the loud boot
warning ('mirrorQueuePlanWarning' -- it is non-durable / best-effort) through the
composition-root logger, then constructs the bounded queue with a drop callback that
logs each rate-limited cap-overflow drop at a warning. (A drop /metric/ hooks in
alongside the log once the @ecluse.mirror.*@ catalogue lands.) -}
buildMirrorQueue :: LogEnv -> Int -> MirrorQueuePlan -> IO MirrorQueue
buildMirrorQueue logEnv memoryDepth plan = do
    whenJust (mirrorQueuePlanWarning plan) (logBootWarning logEnv)
    case plan of
        SqsBackend sqsConfig -> newSqsQueue logEnv mkRegistryUrl sqsConfig
        MemoryBackend ->
            newBoundedInMemoryQueue (defaultMemoryQueueConfig memoryDepth) (logBootWarning logEnv . memoryQueueDropWarning)

{- Log one line at 'WarningS' through the composition-root 'LogEnv', tagged with this
module -- the plain-'IO' katip path the boot phase uses (it holds no @Handler@ reader),
the same shape "Ecluse.Runtime.Telemetry.Resolve" and "Ecluse.Core.Server.Pipeline.Internal" use. -}
logBootWarning :: LogEnv -> Text -> IO ()
logBootWarning logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM WarningS (ls message))

{- Log one line at 'InfoS' through the composition-root 'LogEnv', the same plain-'IO'
katip path 'logBootWarning' uses, for non-warning boot diagnostics. -}
logBootInfo :: LogEnv -> Text -> IO ()
logBootInfo logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM InfoS (ls message))

{- Log every wired mount's resolved rule boot order ('renderBootOrder' -- the single
total order evaluation walks), one line per rule, so an operator can read the
effective policy resolution straight from the start-up log. A mount with no packument
deps (the unserved stub) contributes nothing. -}
logRuleBootOrder :: LogEnv -> [MountBinding] -> IO ()
logRuleBootOrder logEnv = traverse_ logMount
  where
    logMount binding = do
        let deps = bindingPackumentDeps binding
        let label = T.intercalate "/" (toList (bindingPrefix binding))
        logBootInfo logEnv ("rule boot order for mount " <> label <> ":")
        traverse_ (logBootInfo logEnv) (renderBootOrder (pdRules deps))

{- | Raised to abort start-up after a boot phase has reported its aggregated
failure to stderr. A distinct type -- rather than a bare 'exitFailure' -- so the
abort is observable in a test without the process actually exiting; uncaught, it
propagates to 'main' and the runtime exits non-zero, the operator-facing fail-fast.
-}
data BootAborted = BootAborted
    deriving stock (Eq, Show)

instance Exception BootAborted

{- Report the rendered failure to stderr and abort the boot when a phase fails,
otherwise yield its value. The aggregated failure block is written so an operator
sees every problem from a single failed launch, then 'BootAborted' unwinds to
'main'. -}
orExit :: (e -> Text) -> Either e a -> IO a
orExit render = \case
    Right a -> pure a
    Left err -> TIO.hPutStrLn stderr (render err) >> throwIO BootAborted

{- Prepare the telemetry substrate before the SDK initialises: when enabled, resolve
the identity, normalise the @OTEL_*@ environment the SDK reads, and install the
throttled export-error handler ("Ecluse.Runtime.Telemetry.Resolve.prepareTelemetry"). A no-op
when telemetry is off, so an unset @ECLUSE_OBSERVABILITY__TELEMETRY@ reads no process environment and
configures nothing. -}
prepareTelemetryBoot :: TelemetrySwitch -> LogEnv -> IO ()
prepareTelemetryBoot switch logEnv = case switch of
    TelemetryOff -> pass
    TelemetryOn -> do
        environment <- getEnvironment
        prepareTelemetry logEnv environment

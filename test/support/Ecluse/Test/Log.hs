-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared log environments and capture helpers for every test tier.
module Ecluse.Test.Log (
    newTestLogEnv,
    runQuietKatip,
    jsonLogEnv,
    memoryLogEnv,
    captureStdout,
    captureStderr,
    captureStderrWith,
    captureJsonLog,
    captureJsonLogAt,
    runJsonLog,
    runJsonLogAt,
    lineMessage,
) where

import Data.Aeson (Object, eitherDecodeStrict, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Text.Lazy.Builder (toLazyText)
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (
    ColorStrategy (ColorLog),
    Environment (Environment),
    Item (_itemMessage),
    KatipContextT,
    LogEnv,
    LogStr (unLogStr),
    Namespace (Namespace),
    Scribe (Scribe, liPush, scribeFinalizer, scribePermitItem),
    Severity (DebugS),
    SimpleLogPayload,
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    permitItem,
    registerScribe,
    runKatipContextT,
 )
import Katip.Scribes.Handle (jsonFormat, mkHandleScribeWithFormatter)
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempFile)

-- | A scribe-free 'LogEnv', so nothing a spec logs reaches the run's output.
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

-- | Run a @katip@-constrained action against 'newTestLogEnv', at the empty context and namespace.
runQuietKatip :: KatipContextT IO a -> IO a
runQuietKatip action = do
    logEnv <- newTestLogEnv
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

-- | Capture all severities with katip's JSON shape. Assert on payloads when testing application logs.
jsonLogEnv :: IO LogEnv
jsonLogEnv = jsonLogEnvAt DebugS

jsonLogEnvAt :: Severity -> IO LogEnv
jsonLogEnvAt severity = do
    scribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem severity) V2
    base <- newTestLogEnv
    registerScribe "stdout" scribe defaultScribeSettings base

-- | Capture messages in order. The scribe runs asynchronously, so a spec polls the reader.
memoryLogEnv :: IO (LogEnv, IO [Text])
memoryLogEnv = do
    held <- newIORef []
    let keep item = atomicModifyIORef' held (\messages -> (toStrict (toLazyText (unLogStr (_itemMessage item))) : messages, ()))
        scribe = Scribe{liPush = keep, scribeFinalizer = pass, scribePermitItem = permitItem DebugS}
    logEnv <- registerScribe "memory" scribe defaultScribeSettings =<< newTestLogEnv
    pure (logEnv, reverse <$> readIORef held)

-- | Capture stdout, restoring the original handle on every exit path.
captureStdout :: IO () -> IO Text
captureStdout = fmap snd . captureHandle stdout

-- | 'captureStdout' over 'stderr', the stream a boot refusal reports on.
captureStderr :: IO () -> IO Text
captureStderr = fmap snd . captureHandle stderr

-- | Capture stderr alongside the action's result.
captureStderrWith :: IO a -> IO (a, Text)
captureStderrWith = captureHandle stderr

-- The stream is restored on every exit path, so output never leaks into the run.
captureHandle :: Handle -> IO a -> IO (a, Text)
captureHandle stream act =
    withSystemTempFile "ecluse-log-capture.txt" $ \path tmpHandle ->
        bracket (hDuplicate stream) restore $ \_saved -> do
            hFlush stream
            hDuplicateTo tmpHandle stream
            result <- act
            hFlush stream
            hClose tmpHandle
            (result,) . decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stream
        hDuplicateTo saved stream
        hClose saved

-- | Capture all severities alongside the action's result, closing scribes before reading.
captureJsonLog :: (LogEnv -> IO a) -> IO (a, Text)
captureJsonLog = captureJsonLogAt DebugS

-- | Capture JSON logs at the selected severity floor, closing scribes before reading.
captureJsonLogAt :: Severity -> (LogEnv -> IO a) -> IO (a, Text)
captureJsonLogAt severity body = captureHandle stdout (bracket (jsonLogEnvAt severity) (void . closeScribes) body)

-- | 'captureJsonLog' over a @katip@-constrained action, at the empty context and namespace.
runJsonLog :: KatipContextT IO () -> IO Text
runJsonLog = runJsonLogAt DebugS

-- | Capture a logging action at the selected severity floor.
runJsonLogAt :: Severity -> KatipContextT IO () -> IO Text
runJsonLogAt severity action =
    snd <$> captureJsonLogAt severity (\logEnv -> runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action)

-- | Read a JSONL message from the application or native Katip formatter.
lineMessage :: Text -> Maybe Text
lineMessage line = case eitherDecodeStrict (encodeUtf8 line) of
    Right o -> parseMaybe (\fields -> fields .: "message" <|> fields .: "msg") (o :: Object)
    Left _ -> Nothing

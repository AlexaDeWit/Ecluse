-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated package-manager clients and assertions for end-to-end scenarios.
module Ecluse.E2E.Harness.Client (
    withClientDir,
    runClient,

    -- * Assertions
    shouldSucceed,
    shouldFail,
    shouldWarn,
    clientReport,
) where

import Data.ByteString.Lazy qualified as LBS
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import Test.Hspec (expectationFailure)
import UnliftIO (bracket, handleAny)

import Ecluse.E2E.Harness.Docker (uniqueSuffix)
import Ecluse.E2E.Harness.Types

{- | A throwaway project directory for @label@'s client, removed after the action. It sits
outside the repository tree, so no committed client configuration reaches it.
-}
withClientDir :: String -> (FilePath -> IO a) -> IO a
withClientDir label use = do
    sfx <- uniqueSuffix
    tmpRoot <- getTemporaryDirectory
    let projectDir = tmpRoot </> ("ecluse-e2e-" <> label <> "-" <> sfx)
    bracket
        (createDirectoryIfMissing True projectDir >> pure projectDir)
        (handleAny (const pass) . removePathForcibly)
        use

-- | Run a client in a project directory under a prepared environment, capturing its output.
runClient :: FilePath -> [(String, String)] -> String -> [String] -> IO ClientResult
runClient dir env command args = do
    (code, out, err) <- readProcess (setWorkingDir dir (setEnv env (proc command args)))
    pure
        ClientResult
            { crCommand = toText command
            , crExit = code
            , crStdout = decodeUtf8 (LBS.toStrict out)
            , crStderr = decodeUtf8 (LBS.toStrict err)
            }

-- | Fail the assertion with the client's output when the command failed.
shouldSucceed :: (MonadIO m) => ClientResult -> m ClientResult
shouldSucceed res = liftIO $ case crExit res of
    ExitSuccess -> pure res
    _ -> expectationFailure (toString (clientReport res "failed")) >> pure res

-- | Fail the assertion with the client's output when the command unexpectedly succeeded.
shouldFail :: (MonadIO m) => ClientResult -> m ClientResult
shouldFail res = liftIO $ case crExit res of
    ExitSuccess -> expectationFailure (toString (clientReport res "incorrectly succeeded")) >> pure res
    _ -> pure res

-- | Fail the assertion with the client's output unless one line of its stderr is exactly this one.
shouldWarn :: (MonadIO m) => Text -> ClientResult -> m ClientResult
shouldWarn line res = liftIO $ do
    unless (line `elem` lines (crStderr res)) $
        expectationFailure (toString (clientReport res ("wrote no stderr line " <> show line)))
    pure res

-- | The command, outcome, and captured output for a failed client assertion.
clientReport :: ClientResult -> Text -> Text
clientReport res outcome =
    crCommand res
        <> " "
        <> outcome
        <> "!\nSTDOUT:\n"
        <> crStdout res
        <> "\nSTDERR:\n"
        <> crStderr res

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Driving a package-manager client in end-to-end scenarios.
The npm and pip harnesses share the throwaway project directory, the process capture, and
the two outcome assertions.
-}
module Ecluse.E2E.Harness.Client (
    withClientDir,
    runClient,

    -- * Assertions
    shouldSucceed,
    shouldFail,
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
    _ -> expectationFailure (report res "failed") >> pure res

-- | Fail the assertion with the client's output when the command unexpectedly succeeded.
shouldFail :: (MonadIO m) => ClientResult -> m ClientResult
shouldFail res = liftIO $ case crExit res of
    ExitSuccess -> expectationFailure (report res "incorrectly succeeded") >> pure res
    _ -> pure res

report :: ClientResult -> String -> String
report res outcome =
    toString (crCommand res)
        <> " "
        <> outcome
        <> "!\nSTDOUT:\n"
        <> toString (crStdout res)
        <> "\nSTDERR:\n"
        <> toString (crStderr res)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated package-manager clients and assertions for end-to-end scenarios.
module Ecluse.E2E.Harness.Client (
    withClientDir,
    runClient,
    compilerEnvironmentKeys,

    -- * Assertions
    shouldSucceed,
    shouldFail,
    shouldWarn,
    clientReport,
) where

import Data.ByteString.Lazy qualified as LBS
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Install (clientReport, shouldFail, shouldSucceed, shouldWarn)

{- | A throwaway project directory for @label@'s client, removed after the action. It sits
outside the repository tree, so no committed client configuration reaches it.
-}
withClientDir :: String -> (FilePath -> IO a) -> IO a
withClientDir label = withSystemTempDirectory ("ecluse-e2e-" <> label)

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

-- | Pinned compiler wrappers need these Nix variables without inheriting user or service credentials.
compilerEnvironmentKeys :: [String]
compilerEnvironmentKeys =
    [ "NIX_CC"
    , "NIX_BINTOOLS"
    , "NIX_CFLAGS_COMPILE"
    , "NIX_LDFLAGS"
    , "NIX_STORE"
    , "NIX_DONT_SET_RPATH"
    , "NIX_ENFORCE_PURITY"
    , "NIX_HARDENING_ENABLE"
    ]

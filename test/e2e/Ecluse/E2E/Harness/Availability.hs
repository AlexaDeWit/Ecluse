-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Required infrastructure checks run after pure catalogue discovery.
module Ecluse.E2E.Harness.Availability (e2ePrerequisites) where

import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Typed (proc, readProcess)
import UnliftIO (handleAny)

import Ecluse.E2E.Harness.Docker (e2eUnavailable)

-- | Check the selected profile's tools. @pip@ means the module used by @python3 -m pip@.
e2ePrerequisites :: [String] -> IO (Maybe String)
e2ePrerequisites requested = do
    infrastructure <- e2eUnavailable
    case infrastructure of
        Just reason -> pure (Just reason)
        Nothing -> firstMissing (filter (`notElem` ["image", "docker"]) requested)
  where
    firstMissing [] = pure Nothing
    firstMissing (tool : rest) = do
        missing <- if tool == "pip" then pipMissing else executableMissing tool
        maybe (firstMissing rest) (pure . Just) missing
    executableMissing tool = do
        found <- findExecutable tool
        pure (if isNothing found then Just (tool <> " is not available on PATH") else Nothing)
    pipMissing = handleAny (\err -> pure (Just ("python3 -m pip --version failed: " <> show err))) $ do
        (status, _, _) <- readProcess (proc "python3" ["-m", "pip", "--version"])
        pure (if status == ExitSuccess then Nothing else Just "python3 -m pip is unavailable")

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Temporary comparison files use explicit UTF-8 rather than the host locale.
module Ecluse.BenchReport.RefactorFiles (readComparisonText, writeComparisonText, captureProblems) where

import Data.Text.Encoding qualified as TE
import UnliftIO.Exception (throwIO, tryAny)

-- | Reject invalid UTF-8 in evidence before parsing its contents.
readComparisonText :: FilePath -> IO Text
readComparisonText path = readFileBS path >>= either throwIO pure . TE.decodeUtf8'

-- | Keep the bytes independent of the runner's locale.
writeComparisonText :: FilePath -> Text -> IO ()
writeComparisonText path = writeFileBS path . TE.encodeUtf8

-- | Keep a missing or malformed artifact as an audit fault while checking the remaining evidence.
captureProblems :: IO [Text] -> IO [Text]
captureProblems action = either (pure . show) id <$> tryAny action

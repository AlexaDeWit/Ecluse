-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Private marking and setup checks for the temporary full-history overlay.
module Test.Tasty.Bench.FullHistory (ReactMark (..), isMarked, reactPath, preflight, withMarkedRecorder) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.CPUTime (cpuTimePrecision)
import System.Environment (lookupEnv)
import Test.Tasty.Options (IsOption (..))

import qualified Test.Tasty.Bench.Diagnostic as Diagnostic

-- | Only the fixed React leaf receives this private option.
data ReactMark = Unmarked | Marked deriving (Eq)

instance IsOption ReactMark where
    defaultValue = Unmarked
    parseValue _ = Nothing
    optionName = pure "private-full-history-react"
    optionHelp = pure "Private diagnostic metadata"

-- | The provider checks this option before any mode lookup.
isMarked :: ReactMark -> Bool
isMarked = (== Marked)

-- | The pinned mapper supplies names from leaf to root.
reactPath :: [String]
reactPath = ["selective decode", "react (large)", "single-version metadata (per package)", "ecosystem: npm", "ecluse-core (work-per-request)", "All"]

-- | Both modes perform the same setup without reading their mode.
preflight :: [String] -> [String] -> Int -> IO ()
preflight before after count = do
    directory <- outputDirectory
    let row = intercalate "." (reverse reactPath)
    writeFile (directory ++ "/" ++ "tree-before.txt") (unlines before)
    writeFile (directory ++ "/" ++ "tree-after.txt") (unlines after)
    writeFile (directory ++ "/" ++ "precision.txt") (show (max cpuTimePrecision 1000000000) ++ "\n")
    writeFile (directory ++ "/" ++ "mark.txt") (unlines [row, show count])
    unless (before == after && count == 1 && length (filter (== row) before) == 1) (fail "full-history tree or mark differs")

-- | Only the marked provider reads the mode. The recorder and native estimator stay unchanged.
withMarkedRecorder :: (Maybe Diagnostic.Recorder -> IO a) -> IO a
withMarkedRecorder action = do
    mode <- lookupEnv "ECLUSE_BATCH_MODE"
    directory <- outputDirectory
    let finish = writeFile (directory ++ "/" ++ "row-binding.txt") (unlines [intercalate "." (reverse reactPath), fromMaybe "missing" mode])
    ( case mode of
            Just "marker-only" -> action Nothing
            Just "record" -> Diagnostic.withRecorder (\recorder _ -> action recorder)
            _ -> fail "invalid full-history mode"
        )
        `finally` finish

outputDirectory :: IO FilePath
outputDirectory = lookupEnv "ECLUSE_FULL_OUTPUT" >>= maybe (fail "missing full-history output") pure

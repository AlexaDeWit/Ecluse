-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Runner options select whole groups rather than individual Hspec assertions.
module Ecluse.Test.Contract.Options (Command (..), RunOptions (..), parseCommand) where

import Ecluse.Test.Contract.Types

-- | Discovery and audit never evaluate a registered specification.
data Command = ListCatalogue | PrintPlan | SelectCatalogue Selection | Audit FilePath [FilePath] | AuditFull FilePath FilePath | Execute RunOptions
    deriving stock (Eq, Show)

-- | A timeout bounds a complete lifecycle, including acquisition and teardown.
data RunOptions = RunOptions
    { runSelection :: Selection
    , runReportPath :: Maybe FilePath
    , runSeed :: Integer
    , runTimeoutSeconds :: Maybe Int
    , runRepeat :: Bool
    }
    deriving stock (Eq, Show)

-- | Unknown options fail rather than falling through to Hspec's case filters.
parseCommand :: [String] -> Either Text Command
parseCommand ["--catalogue-list"] = Right ListCatalogue
parseCommand ["--catalogue-plan"] = Right PrintPlan
parseCommand ("--catalogue-select" : arguments) = SelectCatalogue . runSelection <$> parseRun (RunOptions emptySelection Nothing 1 Nothing False) arguments
parseCommand ["--audit-full", plan, report] = Right (AuditFull plan report)
parseCommand ("--audit" : plan : reports) = Right (Audit plan reports)
parseCommand arguments = Execute <$> parseRun (RunOptions emptySelection Nothing 1 Nothing False) arguments

parseRun :: RunOptions -> [String] -> Either Text RunOptions
parseRun options = \case
    [] -> Right options
    "--group" : value : rest -> withSelection (\selection -> selection{selectionGroup = Just $ toText value}) rest
    "--shard" : value : rest -> withSelection (\selection -> selection{selectionPartition = Just $ toText value}) rest
    "--ecosystem" : value : rest -> withSelection (\selection -> selection{selectionEcosystem = Just $ toText value}) rest
    "--backend" : value : rest -> withSelection (\selection -> selection{selectionBackend = Just $ toText value}) rest
    "--capability" : value : rest -> withSelection (\selection -> selection{selectionCapability = Just $ toText value}) rest
    "--report" : path : rest -> parseRun options{runReportPath = Just path} rest
    "--seed" : value : rest -> case readMaybe value of
        Nothing -> Left "--seed requires an integer"
        Just seed -> parseRun options{runSeed = seed} rest
    "--timeout" : value : rest -> case readMaybe value of
        Just seconds | seconds > 0 && seconds <= maxBound `div` 1000000 -> parseRun options{runTimeoutSeconds = Just seconds} rest
        _ -> Left "--timeout requires positive whole seconds within the timer range"
    "--repeat" : rest -> parseRun options{runRepeat = True} rest
    flag : _ -> Left $ "unknown or incomplete runner option: " <> toText flag
  where
    withSelection update = parseRun options{runSelection = update $ runSelection options}

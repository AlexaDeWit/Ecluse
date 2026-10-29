-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Convert native benchmark rows to evidence without running measured operations.
module Ecluse.BenchReport.Performance (
    PerformanceRequest (..),
    benchmarkObservations,
    benchmarkRequiredRows,
    benchmarkConsoleObservations,
    allocationRequiredRows,
    subjectObservations,
    makePerformanceReceipt,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Report.BenchLabels (ecosystemPrefix)

import Ecluse.BenchReport (BenchRow (..), parseCsv, splitName, stripAnsi)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity)

-- | The caller supplies reviewed obligations and records the native process result.
data PerformanceRequest = PerformanceRequest
    { requestIdentity :: ExecutionIdentity
    , requestAttempt :: Text
    , requestObligation :: PerformanceObligation
    , requestArtifacts :: [FilePath]
    , requestOutcome :: NativeOutcome
    , requestRecordedConditions :: Map Text Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON PerformanceRequest
instance FromJSON PerformanceRequest

-- | Zero standard deviation means unavailable precision, not an absent measurement.
benchmarkObservations :: NativeOutcome -> Map Text Text -> Text -> Either Text NativePerformance
benchmarkObservations outcome conditions csv = do
    rows <- parseCsv csv
    pure $ NativePerformance outcome (map observation rows) conditions
  where
    observation row = PerformanceObservation (rowGroup row <> "." <> rowBench row) (if valid row then NativePassed else NativeFailed)
    valid row =
        rowMeanPs row >= 0
            && rowStdev2Ps row >= 0
            && all (>= 0) (catMaybes [rowAllocatedBytes row, rowCopiedBytes row, rowPeakBytes row])

-- | The eight common benchmark families require each declared corpus package.
benchmarkRequiredRows :: Text -> [Text] -> [Text]
benchmarkRequiredRows ecosystem packages =
    guards
        <> map
            (prefix <>)
            ( ["route.match.mixed requests", "security guards.boundedRead (8 MiB body, 64 KiB chunks)"]
                <> [ groupName <> "." <> package <> suffix
                   | package <- packages
                   , (groupName, suffix) <-
                        [ ("wire+project (per package)", ".version identifiers")
                        , ("wire+project (per package)", ".full metadata projection")
                        , ("single-version metadata (per package)", ".full decode + select")
                        , ("single-version metadata (per package)", ".selective decode")
                        , ("version (parse + order + latest, per package)", "")
                        , ("rules.evalRules", "")
                        , ("package.mergePackuments", "")
                        , ("serve (filter + merge-assemble)", "")
                        , ("security guards", ".checkNestingDepth")
                        , ("security guards", ".checkVersionCount")
                        , ("security guards", ".artifact locations")
                        ]
                   ]
                <> ["security guards.checkNestingDepth (synthetic / 100000)", "security guards.checkVersionCount (synthetic / 2000)"]
                <> [ "route.match." <> native
                   | ecosystem == "rubygems"
                   , native <- ["gem action", "quick specification action", "native info action", "request building metadata request", "request building artifact request", "request building compact specification request"]
                   ]
                <> ["managed mirror read: 1000 versions, three files per version." <> row | ecosystem == "pypi", row <- ["cold record and listing", "warm conditional 304 and listing", "cold listing then changed revision adding a file"]]
            )
  where
    prefix = ecosystemPrefix <> ecosystem <> "."
    servedLaw = if ecosystem == "rubygems" then "preserves native served records and proxy artifact routes" else "rewrites every artifact onto the proxy origin"
    guards =
        ["native/" <> prefix <> "route.match.malformed native coordinate" | ecosystem == "rubygems"]
            <> [ "native/" <> prefix <> family <> ".scales linearly in version count"
               | family <- ["rules.evalRules", "package.mergePackuments", "serve (filter + merge-assemble)"]
               ]
            <> [ "native/synthetic generators." <> ecosystem <> "." <> law
               | law <- ["decodes every generated release", "projects every measured synthetic size", "selective projection agrees with the full release", servedLaw, "prepares the large wire-guard input"]
               ]

-- | Four business operations are required even if criteria or registrations omit one.
allocationRequiredRows :: Text -> [Text] -> [Text]
allocationRequiredRows ecosystem packages =
    [ ecosystem <> "/" <> package <> "/" <> leg
    | package <- packages
    , leg <- ["full", "singleVersion", "fullShippedAdvisories", "fullAllAdvisoryRules"]
    ]

-- | Select a subject's actual observations without filtering by its expected row list.
subjectObservations :: PerformanceObligation -> NativePerformance -> NativePerformance
subjectObservations obligation native = native{npObservations = filter belongs (npObservations native)}
  where
    ecosystem = T.stripPrefix "ecosystem-" (poSubject obligation)
    belongs observation = case ecosystem of
        Nothing -> True
        Just name ->
            any (`T.isPrefixOf` observationId observation) $
                if poKind obligation == "benchmark"
                    then [ecosystemPrefix <> name <> ".", "native/" <> ecosystemPrefix <> name <> ".", "native/synthetic generators." <> name <> "."]
                    else [name <> "/"]

-- | Keep a native failure even when every observed row passed before the process failed.
makePerformanceReceipt :: PerformanceRequest -> Map FilePath Text -> NativePerformance -> PerformanceReceipt
makePerformanceReceipt request artifacts native =
    PerformanceReceipt 1 (requestIdentity request) (requestAttempt request) (poKind obligation) (poSubject obligation) selected artifacts
  where
    obligation = requestObligation request
    selected = subjectObservations obligation native{npOutcome = combined, npConditions = Map.union (npConditions native) wrapperConditions}
    wrapperConditions = foldr Map.delete (requestRecordedConditions request) ["floorEnforcement", "floorEnforcementReasons"]
    combined
        | requestOutcome request == NativeFailed || npOutcome native == NativeFailed = NativeFailed
        | requestOutcome request == NativeUnavailable || npOutcome native == NativeUnavailable = NativeUnavailable
        | otherwise = NativePassed

-- | Read actual Tasty console outcomes, including the guards omitted from its benchmark CSV.
benchmarkConsoleObservations :: Text -> [PerformanceObservation]
benchmarkConsoleObservations = snd . foldl' step ([], []) . T.lines . stripAnsi
  where
    step parsed@(stack, observations) line
        | T.null (T.strip line) = parsed
        | otherwise =
            let indent = T.length (T.takeWhile (== ' ') line)
                body = T.strip line
                parents = filter ((< indent) . fst) stack
                (label, result) = T.breakOnEnd ": " body
                outcome = case T.words result of
                    "OK" : _ -> Just NativePassed
                    "FAIL" : _ -> Just NativeFailed
                    _ -> Nothing
             in case outcome of
                    Just status
                        | not (T.null label) ->
                            let name = T.intercalate "." (map snd parents <> [T.dropEnd 2 label])
                                (groupName, leaf) = splitName name
                                identifier = "native/" <> groupName <> "." <> leaf
                             in (parents, observations <> [PerformanceObservation identifier status | "All." `T.isPrefixOf` name])
                    _ -> (parents <> [(indent, body) | not (T.null body)], observations)

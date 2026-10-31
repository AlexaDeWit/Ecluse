-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The successes, refusals, and latency of a load run under one pod shape beside those of
the latest run on @main@ under the same shape, in a section for each ecosystem. It reads
both from the report the harness prints (@renderReports@ in the @bench-load@ executable).
One runner measures a whole pod shape, so each section shows how its scenarios moved together and
which operating point rows differ.
-}
module Ecluse.BenchReport.AgainstMain.Load (
    againstMain,

    -- * Reading a load report
    Reading (..),
    Scenario (..),
    readReport,
) where

import Data.Char (isDigit)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.ReportLabels (memoryPeakColumn)

import Ecluse.BenchReport.AgainstMain (
    Baseline,
    Paired (pairedBoth),
    Report (..),
    groupedBy,
    pairBy,
    percentChange,
    renderAgainstMain,
    sharedBaselineNote,
    signedPercent,
    spreadLine,
    unpairedLines,
 )
import Ecluse.BenchReport.Markdown (Table (..), cells, fixed, hasColumns, linkText, records, tables)

-- | Render the comparison from the two @bench-load-results.md@ bodies: the one on @main@, then this run's.
againstMain :: Baseline Text -> Either Text Text -> Text
againstMain =
    renderAgainstMain
        Report
            { reportTitle = "Successes and latency against main: load test"
            , reportRead = readReport
            , reportBody = body
            , reportNotes = notes
            }

-- | One scenario's row of an at-a-glance table, and the rows of the scenario's own table.
data Scenario = Scenario
    { scName :: Text
    , scSuccesses :: Maybe Int
    , scRefusals :: Maybe Int
    , scP50Ms :: Maybe Double
    , scP99Ms :: Maybe Double
    , scEnding :: Maybe Text
    , scMemoryPeakMib :: Maybe Double
    , scFigures :: Map Text Text
    }

-- | What one report holds: each ecosystem's operating point, and every scenario in report order.
data Reading = Reading
    { readingKnobs :: Map Text [(Text, Text)]
    , readingScenarios :: [Scenario]
    }

-- | Read a report's tables. An operating point belongs to the at-a-glance table that follows it.
readReport :: Text -> Either Text Reading
readReport raw = case catMaybes (snd (mapAccumL step [] (tables raw))) of
    [] -> Left "It holds no scenario table."
    sections ->
        Right
            Reading
                { readingKnobs = Map.fromList [(ecosystemOf opening, knobs) | (knobs, opening : _) <- sections]
                , readingScenarios = map withFigures (concatMap snd sections)
                }
  where
    -- Each scenario's own table follows a heading that names it.
    figures = Map.fromList [(heading, Map.fromList [(metric, value) | [metric, value] <- tableRows table]) | table <- tables raw, map T.toLower (tableHeader table) == ["metric", "value"], Just heading <- [tableHeading table]]
    withFigures scenario = scenario{scFigures = Map.findWithDefault mempty (scName scenario) figures}
    step knobs table
        | map T.toLower (tableHeader table) == ["knob", "value"] = ([(knob, value) | [knob, value] <- tableRows table], Nothing)
        | hasColumns ["scenario", "successes", "refusals"] table = ([], Just (knobs, mapMaybe scenarioOf (records table)))
        | otherwise = (knobs, Nothing)

scenarioOf :: Map Text Text -> Maybe Scenario
scenarioOf record = do
    name <- linkText <$> Map.lookup "scenario" record
    pure
        Scenario
            { scName = name
            , scSuccesses = cell "successes" >>= leadingCount
            , scRefusals = cell "refusals" >>= leadingCount
            , scP50Ms = cell "success p50" >>= milliseconds
            , scP99Ms = cell "success p99" >>= milliseconds
            , scEnding = cell "ending"
            , scMemoryPeakMib = cell memoryPeakColumn >>= mebibytes . fst . T.breakOn " / "
            , scFigures = mempty
            }
  where
    cell column = Map.lookup column record
    -- A ramp prints its count with a note after it.
    leadingCount = readMaybe . toString . T.takeWhile isDigit
    milliseconds = T.stripSuffix " ms" >=> readMaybe . toString

-- A size the report prints in MiB.
mebibytes :: Text -> Maybe Double
mebibytes = T.stripSuffix " MiB" . T.strip >=> readMaybe . toString

-- A scenario key opens with its ecosystem.
ecosystemOf :: Scenario -> Text
ecosystemOf scenario = case T.breakOn "/" (scName scenario) of
    (ecosystem, rest) | not (T.null rest) -> ecosystem
    _ -> "no ecosystem"

body :: Reading -> Reading -> [Text]
body onMain here =
    ["**Whole job.** " <> spreadLine "Scenarios" "Successes" (mapMaybe successChange pairs), ""]
        <> concatMap ecosystemSection (groupedBy ecosystemOf pairs)
        <> unpairedLines id paired
  where
    paired = pairBy scName (readingScenarios onMain) (readingScenarios here)
    pairs = pairedBoth paired
    ecosystemSection (ecosystem, scenarios) =
        section ecosystem (differingKnobs (knobsOf ecosystem onMain) (knobsOf ecosystem here)) scenarios
    knobsOf ecosystem = Map.findWithDefault [] ecosystem . readingKnobs

section :: Text -> [Text] -> [(Scenario, Scenario)] -> [Text]
section ecosystem knobRows pairs =
    ["### " <> ecosystem, "", spreadLine "Scenarios" "Successes" (mapMaybe successChange pairs), ""]
        <> knobTable
        <> [ "| scenario | successes on main | this run | change | refusals on main | this run | success p50 on main | this run | change | success p99 on main | this run | change | ending |"
           , "| --- | --: | --: | --: | --: | --: | --: | --: | --: | --: | --: | --: | --- |"
           ]
        <> map scenarioRow pairs
        <> [""]
  where
    knobTable
        | null knobRows = []
        | otherwise = ["Operating point rows that differ:", "", "| knob | main | this run |", "| --- | --- | --- |"] <> knobRows <> [""]

-- The knobs whose values differ, in this run's order, then the knobs only the run on main printed.
differingKnobs :: [(Text, Text)] -> [(Text, Text)] -> [Text]
differingKnobs onMain here =
    [cells [knob, fromMaybe absent (lookup knob onMain), value] | (knob, value) <- here, lookup knob onMain /= Just value]
        <> [cells [knob, value, absent] | (knob, value) <- onMain, isNothing (lookup knob here)]
  where
    absent = "not printed"

scenarioRow :: (Scenario, Scenario) -> Text
scenarioRow pair@(onMain, here) =
    cells
        [ scName here
        , count (scSuccesses onMain)
        , count (scSuccesses here)
        , change (successChange pair)
        , count (scRefusals onMain)
        , count (scRefusals here)
        , milliseconds (scP50Ms onMain)
        , milliseconds (scP50Ms here)
        , change (changeIn scP50Ms pair)
        , milliseconds (scP99Ms onMain)
        , milliseconds (scP99Ms here)
        , change (changeIn scP99Ms pair)
        , ending (scEnding onMain) (scEnding here)
        ]
  where
    count = maybe "n/a" show
    milliseconds = maybe "n/a" (\ms -> fixed 2 ms <> " ms")
    change = maybe "n/a" signedPercent

-- This run's ending, with the ending on main beside it when the two differ.
ending :: Maybe Text -> Maybe Text -> Text
ending onMain here = case (onMain, here) of
    (Just before, Just now) | before /= now -> now <> " (on main: " <> before <> ")"
    (_, Just now) -> now
    _ -> "n/a"

successChange :: (Scenario, Scenario) -> Maybe Double
successChange = changeIn (fmap fromIntegral . scSuccesses)

changeIn :: (Scenario -> Maybe Double) -> (Scenario, Scenario) -> Maybe Double
changeIn figure (onMain, here) = join (percentChange <$> figure onMain <*> figure here)

notes :: [Text]
notes =
    [ "### Reading the comparison"
    , ""
    , "- **One run stands on each side.** One runner measures a whole pod shape, and the run on `main` had another runner. A whole shape can move together, so read a scenario beside the scenarios the change does not touch."
    , "- **Injected latency.** Every stub upstream injects one fixed latency, 20 ms unless a dispatch sets another. When the two runs differ, the operating point rows show both, and the latency-bound scenarios move with that difference whatever the code does."
    , "- **Finite replays.** A pattern scenario replays a fixed trace, so its successes hold and only its latency can move. A replay sends few requests, as its successes show, and its percentiles differ widely between two runs of the same code."
    , "- **Refusals** are `429` and `503` responses. A client retries a refusal at once, so their count follows retry speed."
    , sharedBaselineNote
    , "- **No verdict.** No row is marked, and nothing here fails the job."
    ]

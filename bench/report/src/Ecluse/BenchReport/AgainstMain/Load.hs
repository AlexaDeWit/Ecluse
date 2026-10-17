-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The successes, refusals, and latency of a load run beside other load runs, read from the
reports the harness prints (@renderReports@ in the @bench-load@ executable). 'againstMain' sets a
pod shape beside the latest run on @main@, in a section for each ecosystem. One runner measures a
whole pod shape, and each run injects the upstream latency it probed, so each section shows how its
scenarios moved together. 'againstAnchors' sets each arm of one sweep job beside the job's anchors.
-}
module Ecluse.BenchReport.AgainstMain.Load (
    againstMain,
    againstAnchors,
) where

import Data.Char (isDigit)
import Data.List (lookup, zipWith3)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Float (log)

import Ecluse.BenchLoad.Floors (FloorKey, Pass (Loaded))
import Ecluse.BenchLoad.Pod (PodShape, renderPodShape)
import Ecluse.BenchLoad.ReportLabels (admissionRow, generatorHeadroomRow, memoryEventsRow, memoryPeakColumn, postureRow)
import Ecluse.BenchLoad.Sweep (Arm (..), BootValues (..), Sweep (..), SweepJob (..))

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

-- One scenario's row of an at-a-glance table, and the rows of the scenario's own table.
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

-- What one report holds: each ecosystem's operating point, and every scenario in report order.
data Reading = Reading
    { readingKnobs :: Map Text [(Text, Text)]
    , readingScenarios :: [Scenario]
    }

-- An operating point belongs to the at-a-glance table that follows it.
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
    , "- **Injected latency.** Each run injects the upstream latency it probed. When the two differ, the operating point rows show both, and the latency-bound scenarios move with that difference whatever the code does."
    , "- **Finite replays.** A pattern scenario replays a fixed trace, so its successes hold and only its latency can move. A replay sends few requests, as its successes show, and its percentiles differ widely between two runs of the same code."
    , "- **Refusals** are `429` and `503` responses. A client retries a refusal at once, so their count follows retry speed."
    , sharedBaselineNote
    , "- **No verdict.** No row is marked, and nothing here fails the job."
    ]

-- | Set each arm of one sweep job beside the mean of the job's anchors, from the arms' reports in slot order.
againstAnchors :: Map PodShape (Map FloorKey Int) -> Sweep -> SweepJob -> [Either Text Text] -> Text
againstAnchors floorsByShape sweep job reports =
    T.unlines $
        [ "## Sweep `" <> swName sweep <> "`, job `" <> jobName job <> "`"
        , ""
        , "Each arm's successes divide by the mean of the anchor's successes in slots " <> inWords (map (show . slotNumber) anchors) <> ", scenario by scenario. The score is the geometric mean of those ratios over the scored scenarios the job ran. A boot value the arm did not show, or a safety check it failed, voids its repeat."
        , ""
        , "| slot | arm | boot values | safety | load generator headroom | score |"
        , "| --: | --- | --- | --- | --- | --: |"
        ]
            <> map summaryRow slots
            <> [""]
            <> driftTable anchors scenarioNames
            <> concatMap armSection (filter ((/= swAnchor sweep) . slotArm) slots)
  where
    slots = zipWith3 Slot [1 ..] (toList (jobArms job)) (map (>>= readReport) reports <> repeat (Left "The runner passed no report for this slot."))
    anchors = filter ((== swAnchor sweep) . slotArm) slots
    anchorMean = meanOf . successesIn anchors
    ratio scenario = do
        successes <- scSuccesses scenario
        mean <- mfilter (> 0) (anchorMean (scName scenario))
        pure (fromIntegral successes / mean)
    summaryRow slot = cells [show (slotNumber slot), "`" <> slotArm slot <> "`", bootCell slot, safetyCell slot, headroomCell slot, scoreCell slot]
    bootCell slot = either (const "n/a") (\(arm, reading) -> listed "as expected" (ordNub (concatMap (bootProblems (armExpected arm) . scFigures) (readingScenarios reading)))) (armReading slot)
    safetyCell slot = either ("**void**: " <>) (\(arm, reading) -> listed "clean" (concatMap (safetyProblems arm) (readingScenarios reading))) (armReading slot)
    headroomCell slot = either (const "n/a") (headroom . snd) (armReading slot)
    scoreCell slot
        | slotArm slot == swAnchor sweep = "anchor"
        | otherwise = either (const "n/a") (maybe "n/a" (fixed 3) . score . snd) (armReading slot)
    armReading slot = do
        arm <- maybeToRight ("the sweep has no arm " <> slotArm slot) (Map.lookup (slotArm slot) (swArms sweep))
        reading <- first ("the report could not be read. " <>) (slotReading slot)
        pure (arm, reading)
    score reading = geometricMean (mapMaybe ratio (filter ((`elem` swScored sweep) . scName) (readingScenarios reading)))
    safetyProblems arm scenario = map ((scName scenario <> ": ") <>) (scenarioSafety (referenceFloor arm (scName scenario)) (armPodMemoryBytes arm) scenario)
    referenceFloor arm name = Map.lookup (name, Loaded) =<< Map.lookup (armReferenceShape arm) floorsByShape
    scenarioNames = ordNub [scName scenario | Slot{slotReading = Right reading} <- slots, scenario <- readingScenarios reading]
    armSection slot =
        ["### Slot " <> show (slotNumber slot) <> ": `" <> slotArm slot <> "`", ""] <> case armReading slot of
            Left problem -> [problem, ""]
            Right (arm, reading) ->
                [ "| scenario | successes | anchor mean | ratio | floor under " <> renderPodShape (armReferenceShape arm) <> " | success p50 | success p99 | memory peak | ending |"
                , "| --- | --: | --: | --: | --: | --: | --: | --: | --- |"
                ]
                    <> [ cells
                            [ scName scenario
                            , count (scSuccesses scenario)
                            , maybe "n/a" (fixed 1) (anchorMean (scName scenario))
                            , maybe "n/a" (fixed 3) (ratio scenario)
                            , count (referenceFloor arm (scName scenario))
                            , milliseconds (scP50Ms scenario)
                            , milliseconds (scP99Ms scenario)
                            , maybe "n/a" (\mib -> fixed 1 mib <> " MiB") (scMemoryPeakMib scenario)
                            , fromMaybe "n/a" (scEnding scenario)
                            ]
                       | scenario <- readingScenarios reading
                       ]
                    <> [""]
    count = maybe "n/a" show
    milliseconds = maybe "n/a" (\ms -> fixed 2 ms <> " ms")
    listed none = \case
        [] -> none
        problems -> "**void**: " <> T.intercalate "; " problems

-- How far the closing anchor's successes drifted from the opening one's, scenario by scenario.
driftTable :: [Slot] -> [Text] -> [Text]
driftTable anchors names = case (anchors, reverse anchors) of
    (opener : _ : _, closer : _) ->
        let opening = slotSuccesses opener
            closing = slotSuccesses closer
            count = maybe "n/a" show
            drift name = do
                before <- lookup name opening
                after <- lookup name closing
                percentChange (fromIntegral before) (fromIntegral after)
         in [ "Anchor drift, slot " <> show (slotNumber closer) <> " against slot " <> show (slotNumber opener) <> ":"
            , ""
            , "| scenario | slot " <> show (slotNumber opener) <> " | slot " <> show (slotNumber closer) <> " | drift |"
            , "| --- | --: | --: | --: |"
            ]
                <> [cells [name, count (lookup name opening), count (lookup name closing), maybe "n/a" signedPercent (drift name)] | name <- names]
                <> [""]
    _ -> []

-- Items joined with commas and a final "and".
inWords :: [Text] -> Text
inWords items = case reverse items of
    [] -> "none"
    [single] -> single
    final : rest -> T.intercalate ", " (reverse rest) <> " and " <> final

-- One slot of a sweep job: its position, its arm, and its report.
data Slot = Slot
    { slotNumber :: Int
    , slotArm :: Text
    , slotReading :: Either Text Reading
    }

-- Each scenario's successes in the slots, in slot order.
successesIn :: [Slot] -> Text -> [Int]
successesIn slots name = [count | Slot{slotReading = Right reading} <- slots, scenario <- readingScenarios reading, scName scenario == name, Just count <- [scSuccesses scenario]]

slotSuccesses :: Slot -> [(Text, Int)]
slotSuccesses slot = [(scName scenario, count) | Right reading <- [slotReading slot], scenario <- readingScenarios reading, Just count <- [scSuccesses scenario]]

-- The posture each of the arm's proxies reported against the posture the arm must show.
bootProblems :: BootValues -> Map Text Text -> [Text]
bootProblems expected figures = case (T.splitOn " / " <$> Map.lookup postureRow figures, Map.lookup admissionRow figures) of
    (Just [heap, capabilities, area], Just admission) ->
        differs "capabilities" (show (bvCapabilities expected)) (T.strip capabilities)
            <> differs "allocation area" (mib (Just (bvAllocAreaMib expected))) (T.strip area)
            <> differs "heap ceiling" (mib (bvMaxHeapMib expected)) (T.strip heap)
            <> differs "serve admission" (show (bvAdmission expected)) (T.takeWhile isDigit admission)
    _ -> ["the report shows no posture"]
  where
    differs what wanted shown = [what <> " " <> shown <> ", expected " <> wanted | shown /= wanted]
    mib = maybe "none" (\n -> fixed 1 (fromIntegral n) <> " MiB")

-- A scenario's ending, its kernel memory events, its peak against the pod's memory, and its count against its floor.
scenarioSafety :: Maybe Int -> Int -> Scenario -> [Text]
scenarioSafety floor' podBytes scenario =
    ["ending " <> plain | Just shown <- [scEnding scenario], let plain = T.filter (/= '*') shown, plain /= "clean shutdown"]
        <> case map (readMaybe . toString . T.strip) . T.splitOn " / " <$> Map.lookup memoryEventsRow (scFigures scenario) of
            Just [Just oomKills, _, _, Just high] -> ["oom_kill " <> show (oomKills :: Int) | oomKills > 0] <> ["memory.high events " <> show (high :: Int) | high > 0]
            _ -> ["no memory.events reading"]
        <> ["peak " <> fixed 1 peak <> " MiB above the pod's " <> show (podBytes `div` (1024 * 1024)) <> " MiB" | Just peak <- [scMemoryPeakMib scenario], peak * 1024 * 1024 > fromIntegral podBytes]
        <> [show count <> " successes below the floor of " <> show least | Just least <- [floor'], Just count <- [scSuccesses scenario], count < least]

-- Whether the load generator kept its headroom in every scenario of a report.
headroom :: Reading -> Text
headroom reading = case [(scName scenario, shown) | scenario <- readingScenarios reading, Just shown <- [Map.lookup generatorHeadroomRow (scFigures scenario)]] of
    [] -> "n/a"
    readings -> case [name <> " (" <> T.filter (/= '*') shown <> ")" | (name, shown) <- readings, shown /= "held"] of
        [] -> "held"
        missed -> "**not held** in " <> T.intercalate ", " missed

meanOf :: [Int] -> Maybe Double
meanOf = \case
    [] -> Nothing
    counts -> Just (fromIntegral (sum counts) / fromIntegral (length counts))

geometricMean :: [Double] -> Maybe Double
geometricMean = \case
    [] -> Nothing
    ratios -> Just (exp (sum (map log ratios) / fromIntegral (length ratios)))

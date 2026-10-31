-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One sweep job's arms beside the job's anchors, read from the reports the harness prints. Each
arm's successes divide by the mean of the anchors' successes, scenario by scenario, and a boot value
or a safety check the arm missed voids its repeat. The comparison renders as Markdown for the run
summary and as JSON, so the noise band and the decision rule can be computed across jobs.
-}
module Ecluse.BenchReport.Sweep (
    -- * The comparison
    JobComparison (..),
    SlotResult (..),
    ArmResult (..),
    ScenarioResult (..),
    Failure (..),
    Score (..),
    Drift (..),
    ReferenceFloor (..),
    ReferenceFloors (..),
    referenceFloors,
    compareJob,
    slotVoid,

    -- * Rendering
    renderComparison,
    writeComparison,
) where

import Data.Aeson (ToJSON (toJSON), encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isDigit)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Float (log)
import System.FilePath ((</>))

import Ecluse.BenchLoad.Floors (Calibration (calOperatingPoint), Cell (Floored), FloorKey, Floors (floorsByShape, floorsCalibration), OperatingPoint (opConcurrency, opDurationSeconds, opUpstreamLatencyMs), Pass (Loaded))
import Ecluse.BenchLoad.Pod (PodShape, mib, renderPodShape)
import Ecluse.BenchLoad.ReportLabels (admissionRow, generatorHeadroomRow, headroomHeld, headroomSaturated, latencyRow, loadRow, memoryEventsRow, parseLatencyCell, parseLoadCell, postureRow)
import Ecluse.BenchLoad.Sweep (Arm (..), BootValues (..), Sweep (..), SweepJob (..))
import Ecluse.BenchReport.AgainstMain (percentChange, signedPercent)
import Ecluse.BenchReport.AgainstMain.Load (Reading (..), Scenario (..), readReport)
import Ecluse.BenchReport.Markdown (cells, fixed)

-- | A sweep job's arms in slot order, and how far the closing anchor drifted from the opening one.
data JobComparison = JobComparison
    { cmSweep :: Text
    , cmJob :: Text
    , cmCommit :: Maybe Text
    -- ^ The commit the job measured, as the run names it.
    , cmWindow :: Maybe (Int, Int)
    -- ^ The connections and the window length in seconds the first readable report states.
    , cmAnchor :: Text
    , cmSlots :: [SlotResult]
    , cmDrift :: Maybe Drift
    -- ^ 'Nothing' when the job ran fewer than two anchor slots.
    }
    deriving stock (Eq, Show)

-- | One slot: its arm, and the arm's result, or why there is none.
data SlotResult = SlotResult
    { slNumber :: Int
    , slArm :: Text
    , slResult :: Either Text ArmResult
    }
    deriving stock (Eq, Show)

-- | What one arm's proxies showed, against what the arm must show.
data ArmResult = ArmResult
    { arReferenceShape :: PodShape
    -- ^ The scheduled shape whose loaded floors the arm's counts sit beside.
    , arBoot :: [Failure]
    -- ^ Each boot value a proxy reported other than the arm's, once.
    , arScenarios :: [ScenarioResult]
    , arScore :: Score
    }
    deriving stock (Eq, Show)

-- | One scenario of one arm, against the job's anchors and the reference shape's floor.
data ScenarioResult = ScenarioResult
    { scrScenario :: Text
    , scrSuccesses :: Maybe Int
    , scrAnchorMean :: Maybe Double
    , scrRatio :: Maybe Double
    -- ^ Successes over the anchors' mean, 'Nothing' without a count or a mean above zero.
    , scrFloor :: ReferenceFloor
    , scrP50Ms :: Maybe Double
    , scrP99Ms :: Maybe Double
    , scrPeakMib :: Maybe Double
    , scrEnding :: Maybe Text
    , scrGeneratorSaturated :: Maybe Bool
    , scrFailures :: [Failure]
    }
    deriving stock (Eq, Show)

-- | A reason an arm's repeat is void.
data Failure
    = -- | A boot value: which one, the value shown, and the value expected.
      BootValue Text Text Text
    | NoPosture
    | UncleanEnding Text
    | OomKills Int
    | MemoryHighEvents Int
    | NoMemoryEvents
    | -- | The peak in MiB, above the pod's memory in MiB.
      PeakAbovePod Double Int
    | -- | The successes, below the reference floor.
      BelowFloor Int Int
    deriving stock (Eq, Ord, Show)

-- | An arm's score: the geometric mean of its anchor ratios over the scored scenarios.
data Score
    = AnchorSlot
    | Scored Double
    | -- | No score: the scored scenarios the arm ran without an anchor ratio, none when it ran no scored scenario.
      Unscored [Text]
    deriving stock (Eq, Show)

-- | Each scenario's successes in the first and the last anchor slot.
data Drift = Drift
    { drOpeningSlot :: Int
    , drClosingSlot :: Int
    , drScenarios :: [(Text, Maybe Int, Maybe Int)]
    }
    deriving stock (Eq, Show)

-- | The reference shape's loaded floor for one scenario, and whether its window can be held to it.
data ReferenceFloor
    = NoReferenceFloor
    | HeldTo Int
    | -- | The floor, which a window of other connections, seconds, or injected latency than the floors' calibration cannot be held to.
      NotApplicable Int
    deriving stock (Eq, Show)

-- | The floors in force by pod shape, and the connections, window length in seconds, and injected latency in ms they were calibrated at.
data ReferenceFloors = ReferenceFloors
    { rfConcurrency :: Int
    , rfDurationSeconds :: Int
    , rfUpstreamLatencyMs :: Int
    , rfByShape :: Map PodShape (Map FloorKey Int)
    }
    deriving stock (Eq, Show)

-- | The committed floors as a sweep comparison reads them: each cell that holds a floor.
referenceFloors :: Floors -> ReferenceFloors
referenceFloors floors =
    ReferenceFloors
        (opConcurrency calibrated)
        (opDurationSeconds calibrated)
        (opUpstreamLatencyMs calibrated)
        (Map.map (Map.mapMaybe floorOf) (floorsByShape floors))
  where
    calibrated = calOperatingPoint (floorsCalibration floors)
    floorOf = \case
        Floored least _ _ -> Just least
        _ -> Nothing

-- | Compare one job's reports, in slot order, with the job's anchors and the reference floors, for the named commit.
compareJob :: Maybe Text -> ReferenceFloors -> Sweep -> SweepJob -> [Either Text Text] -> JobComparison
compareJob commit floors sweep job reports =
    JobComparison
        { cmSweep = swName sweep
        , cmJob = jobName job
        , cmCommit = commit
        , cmWindow = listToMaybe (mapMaybe windowOf (rights [reading | (_, _, reading) <- slots]))
        , cmAnchor = swAnchor sweep
        , cmSlots = map (slotResult floors sweep (anchorMeans anchors)) slots
        , cmDrift = driftOf anchors (ordNub [scName scenario | (_, _, Right reading) <- slots, scenario <- readingScenarios reading])
        }
  where
    slots = zip3 [1 ..] (toList (jobArms job)) (map (>>= readReport) reports <> repeat (Left "The runner passed no report for this slot."))
    anchors = [(number, reading) | (number, name, reading) <- slots, name == swAnchor sweep]

-- | Whether a slot's repeat is void: no result, a boot value it did not show, or a failed safety check.
slotVoid :: SlotResult -> Bool
slotVoid slot = either (const True) (\arm -> not (null (arBoot arm) && all (null . scrFailures) (arScenarios arm))) (slResult slot)

-- The connections and the window length in seconds a report's load row states.
windowOf :: Reading -> Maybe (Int, Int)
windowOf reading = listToMaybe [window | knobs <- Map.elems (readingKnobs reading), Just cell <- [lookup loadRow knobs], Just window <- [parseLoadCell cell]]

-- The injected latency in ms a report's latency row states.
latencyOf :: Reading -> Maybe Int
latencyOf reading = listToMaybe [latency | knobs <- Map.elems (readingKnobs reading), Just cell <- [lookup latencyRow knobs], Just latency <- [parseLatencyCell cell]]

slotResult :: ReferenceFloors -> Sweep -> Map Text Double -> (Int, Text, Either Text Reading) -> SlotResult
slotResult floors sweep means (number, name, reading) = SlotResult number name $ do
    arm <- maybeToRight ("the sweep has no arm " <> name) (Map.lookup name (swArms sweep))
    found <- first ("the report could not be read. " <>) reading
    let calibrated = windowOf found == Just (rfConcurrency floors, rfDurationSeconds floors) && latencyOf found == Just (rfUpstreamLatencyMs floors)
        scenarios = map (scenarioResult floors calibrated means arm) (readingScenarios found)
    pure
        ArmResult
            { arReferenceShape = armReferenceShape arm
            , arBoot = ordNub (concatMap (bootFailures (armExpected arm) . scFigures) (readingScenarios found))
            , arScenarios = scenarios
            , arScore = if name == swAnchor sweep then AnchorSlot else scoreOf (filter ((`elem` swScored sweep) . scrScenario) scenarios)
            }

scenarioResult :: ReferenceFloors -> Bool -> Map Text Double -> Arm -> Scenario -> ScenarioResult
scenarioResult floors calibrated means arm scenario =
    ScenarioResult
        { scrScenario = scName scenario
        , scrSuccesses = scSuccesses scenario
        , scrAnchorMean = mean
        , scrRatio = (\successes anchors -> fromIntegral successes / anchors) <$> scSuccesses scenario <*> mfilter (> 0) mean
        , scrFloor = floor'
        , scrP50Ms = scP50Ms scenario
        , scrP99Ms = scP99Ms scenario
        , scrPeakMib = scMemoryPeakMib scenario
        , scrEnding = T.filter (/= '*') <$> scEnding scenario
        , scrGeneratorSaturated = saturation =<< Map.lookup generatorHeadroomRow (scFigures scenario)
        , scrFailures = scenarioFailures floor' (armPodMemoryBytes arm) scenario
        }
  where
    mean = Map.lookup (scName scenario) means
    floor' = case Map.lookup (scName scenario, Loaded) =<< Map.lookup (armReferenceShape arm) (rfByShape floors) of
        Nothing -> NoReferenceFloor
        Just least -> bool (NotApplicable least) (HeldTo least) calibrated
    saturation shown
        | shown == headroomHeld = Just False
        | T.filter (/= '*') shown == headroomSaturated = Just True
        | otherwise = Nothing

-- Each scenario's mean successes over the readable anchor slots.
anchorMeans :: [(Int, Either Text Reading)] -> Map Text Double
anchorMeans anchors =
    Map.map (\counts -> fromIntegral (sum counts) / fromIntegral (length counts)) $
        Map.fromListWith (<>) [(scName scenario, [count]) | (_, Right reading) <- anchors, scenario <- readingScenarios reading, Just count <- [scSuccesses scenario]]

-- The first and last anchor slots' successes for every scenario the job ran.
driftOf :: [(Int, Either Text Reading)] -> [Text] -> Maybe Drift
driftOf anchors names = case (anchors, reverse anchors) of
    ((opening, first') : _ : _, (closing, final) : _) -> Just (Drift opening closing [(name, successesIn first' name, successesIn final name) | name <- names])
    _ -> Nothing
  where
    successesIn reading name = do
        found <- rightToMaybe reading
        scSuccesses =<< find ((== name) . scName) (readingScenarios found)

-- No score when a scored scenario has no anchor ratio, so a missing anchor never shifts the mean.
scoreOf :: [ScenarioResult] -> Score
scoreOf scored = case [scrScenario result | result <- scored, isNothing (scrRatio result)] of
    [] -> maybe (Unscored []) Scored (geometricMean (mapMaybe scrRatio scored))
    missing -> Unscored missing

geometricMean :: [Double] -> Maybe Double
geometricMean = \case
    [] -> Nothing
    ratios -> Just (exp (sum (map log ratios) / fromIntegral (length ratios)))

-- The posture one proxy reported against the posture the arm must show.
bootFailures :: BootValues -> Map Text Text -> [Failure]
bootFailures expected figures = case (T.splitOn " / " <$> Map.lookup postureRow figures, Map.lookup admissionRow figures) of
    (Just [heap, capabilities, area], Just admission) ->
        differs "capabilities" (show (bvCapabilities expected)) (T.strip capabilities)
            <> differs "allocation area" (mibCell (Just (bvAllocAreaMib expected))) (T.strip area)
            <> differs "heap ceiling" (mibCell (bvMaxHeapMib expected)) (T.strip heap)
            <> differs "serve admission" (show (bvAdmission expected)) (T.takeWhile isDigit admission)
    _ -> [NoPosture]
  where
    differs what wanted shown = [BootValue what shown wanted | shown /= wanted]
    mibCell = maybe "none" (\n -> fixed 1 (fromIntegral n) <> " MiB")

-- A scenario's ending, its kernel memory events, its peak against the pod's memory, and its count against its floor.
scenarioFailures :: ReferenceFloor -> Int -> Scenario -> [Failure]
scenarioFailures floor' podBytes scenario =
    [UncleanEnding plain | Just shown <- [scEnding scenario], let plain = T.filter (/= '*') shown, plain /= "clean shutdown"]
        <> case map (readMaybe . toString . T.strip) . T.splitOn " / " <$> Map.lookup memoryEventsRow (scFigures scenario) of
            Just [Just oomKills, _, _, Just high] -> [OomKills oomKills | oomKills > 0] <> [MemoryHighEvents high | high > 0]
            _ -> [NoMemoryEvents]
        <> [PeakAbovePod peak (podBytes `div` mib) | Just peak <- [scMemoryPeakMib scenario], peak * fromIntegral mib > fromIntegral podBytes]
        <> [BelowFloor count least | HeldTo least <- [floor'], Just count <- [scSuccesses scenario], count < least]

-- | The comparison as Markdown: a row per slot, the anchor drift, and a table per test arm.
renderComparison :: JobComparison -> Text
renderComparison comparison =
    T.unlines $
        [ "## Sweep `" <> cmSweep comparison <> "`, job `" <> cmJob comparison <> "`"
        , ""
        , "Commit " <> maybe "unnamed" (\commit -> "`" <> T.take 9 commit <> "`") (cmCommit comparison) <> ", " <> maybe "with no load row" (\(connections, seconds) -> show connections <> " connections x " <> show seconds <> " s windows") (cmWindow comparison) <> "."
        , ""
        , "Each arm's successes divide by the mean of the anchor's successes in slots " <> inWords (map (show . slNumber) anchorSlots) <> ", scenario by scenario. The score is the geometric mean of those ratios over the scored scenarios the job ran, and a scored scenario without a ratio leaves the arm unscored. A boot value the arm did not show, or a safety check it failed, voids its repeat. A floor applies only to a window of the connections, length, and injected latency the floors were calibrated at."
        , ""
        , "| slot | arm | boot values | safety | load generator headroom | score |"
        , "| --: | --- | --- | --- | --- | --: |"
        ]
            <> map summaryRow (cmSlots comparison)
            <> [""]
            <> maybe [] driftLines (cmDrift comparison)
            <> concatMap armLines (filter ((/= cmAnchor comparison) . slArm) (cmSlots comparison))
  where
    anchorSlots = filter ((== cmAnchor comparison) . slArm) (cmSlots comparison)

summaryRow :: SlotResult -> Text
summaryRow slot = cells [show (slNumber slot), "`" <> slArm slot <> "`", boot, safety, headroom, score]
  where
    boot = either (const "n/a") (voidWhen "as expected" . map renderFailure . arBoot) (slResult slot)
    safety = either ("**void**: " <>) (voidWhen "clean" . concatMap scenarioLines . arScenarios) (slResult slot)
    scenarioLines result = map (((scrScenario result <> ": ") <>) . renderFailure) (scrFailures result)
    headroom = either (const "n/a") (headroomCell . arScenarios) (slResult slot)
    score = either (const "n/a") (scoreCell . arScore) (slResult slot)
    voidWhen none = \case
        [] -> none
        problems -> "**void**: " <> T.intercalate "; " problems

headroomCell :: [ScenarioResult] -> Text
headroomCell results
    | all (isNothing . scrGeneratorSaturated) results = "n/a"
    | otherwise = case [scrScenario result <> " (" <> maybe "n/a" (bool headroomHeld headroomSaturated) (scrGeneratorSaturated result) <> ")" | result <- results, scrGeneratorSaturated result /= Just False] of
        [] -> headroomHeld
        missed -> "**not held** in " <> T.intercalate ", " missed

scoreCell :: Score -> Text
scoreCell = \case
    AnchorSlot -> "anchor"
    Scored score -> fixed 3 score
    Unscored [] -> "n/a"
    Unscored missing -> "n/a (no anchor ratio for " <> T.intercalate ", " missing <> ")"

driftLines :: Drift -> [Text]
driftLines drift =
    [ "Anchor drift, slot " <> show (drClosingSlot drift) <> " against slot " <> show (drOpeningSlot drift) <> ":"
    , ""
    , "| scenario | slot " <> show (drOpeningSlot drift) <> " | slot " <> show (drClosingSlot drift) <> " | drift |"
    , "| --- | --: | --: | --: |"
    ]
        <> [cells [name, countCell opening, countCell closing, maybe "n/a" signedPercent (driftPercent opening closing)] | (name, opening, closing) <- drScenarios drift]
        <> [""]

armLines :: SlotResult -> [Text]
armLines slot =
    ["### Slot " <> show (slNumber slot) <> ": `" <> slArm slot <> "`", ""] <> case slResult slot of
        Left problem -> [problem, ""]
        Right arm ->
            [ "| scenario | successes | anchor mean | ratio | floor under " <> renderPodShape (arReferenceShape arm) <> " | success p50 | success p99 | memory peak | ending |"
            , "| --- | --: | --: | --: | --: | --: | --: | --: | --- |"
            ]
                <> map scenarioRow (arScenarios arm)
                <> [""]
  where
    scenarioRow result =
        cells
            [ scrScenario result
            , countCell (scrSuccesses result)
            , maybe "n/a" (fixed 1) (scrAnchorMean result)
            , maybe "n/a" (fixed 3) (scrRatio result)
            , floorCell (scrFloor result)
            , maybe "n/a" (\ms -> fixed 2 ms <> " ms") (scrP50Ms result)
            , maybe "n/a" (\ms -> fixed 2 ms <> " ms") (scrP99Ms result)
            , maybe "n/a" (\peak -> fixed 1 peak <> " MiB") (scrPeakMib result)
            , fromMaybe "n/a" (scrEnding result)
            ]

renderFailure :: Failure -> Text
renderFailure = \case
    BootValue what shown expected -> what <> " " <> shown <> ", expected " <> expected
    NoPosture -> "the report shows no posture"
    UncleanEnding ending -> "ending " <> ending
    OomKills kills -> "oom_kill " <> show kills
    MemoryHighEvents events -> "memory.high events " <> show events
    NoMemoryEvents -> "no memory.events reading"
    PeakAbovePod peak pod -> "peak " <> fixed 1 peak <> " MiB above the pod's " <> show pod <> " MiB"
    BelowFloor successes least -> show successes <> " successes below the floor of " <> show least

driftPercent :: Maybe Int -> Maybe Int -> Maybe Double
driftPercent opening closing = do
    before <- opening
    after <- closing
    percentChange (fromIntegral before) (fromIntegral after)

floorCell :: ReferenceFloor -> Text
floorCell = \case
    NoReferenceFloor -> "n/a"
    HeldTo least -> show least
    NotApplicable least -> show least <> ", not applicable to this window or latency"

countCell :: Maybe Int -> Text
countCell = maybe "n/a" show

-- Items joined with commas and a final "and".
inWords :: [Text] -> Text
inWords items = case reverse items of
    [] -> "none"
    [single] -> single
    final : rest -> T.intercalate ", " (reverse rest) <> " and " <> final

instance ToJSON JobComparison where
    toJSON comparison =
        object
            [ "sweep" .= cmSweep comparison
            , "job" .= cmJob comparison
            , "commit" .= cmCommit comparison
            , "concurrency" .= fmap fst (cmWindow comparison)
            , "durationSeconds" .= fmap snd (cmWindow comparison)
            , "anchor" .= cmAnchor comparison
            , "drift" .= fmap driftValue (cmDrift comparison)
            , "slots" .= cmSlots comparison
            ]
      where
        driftValue drift =
            object
                [ "openingSlot" .= drOpeningSlot drift
                , "closingSlot" .= drClosingSlot drift
                , "scenarios" .= [object ["scenario" .= name, "opening" .= opening, "closing" .= closing, "changePercent" .= driftPercent opening closing] | (name, opening, closing) <- drScenarios drift]
                ]

instance ToJSON SlotResult where
    toJSON slot =
        object $
            [ "slot" .= slNumber slot
            , "arm" .= slArm slot
            , "void" .= slotVoid slot
            , "unread" .= leftToMaybe (slResult slot)
            ]
                <> either (const []) armPairs (slResult slot)
      where
        armPairs arm =
            [ "referenceShape" .= renderPodShape (arReferenceShape arm)
            , "bootFailures" .= arBoot arm
            , "anchorSlot" .= (arScore arm == AnchorSlot)
            , "score" .= case arScore arm of
                Scored score -> Just score
                _ -> Nothing
            , "unscoredScenarios" .= case arScore arm of
                Unscored missing -> missing
                _ -> []
            , "scenarios" .= arScenarios arm
            ]

instance ToJSON ScenarioResult where
    toJSON result =
        object
            [ "scenario" .= scrScenario result
            , "successes" .= scrSuccesses result
            , "anchorMean" .= scrAnchorMean result
            , "ratio" .= scrRatio result
            , "floor" .= case scrFloor result of
                NoReferenceFloor -> Nothing
                HeldTo least -> Just least
                NotApplicable least -> Just least
            , "floorApplies" .= case scrFloor result of
                HeldTo _ -> True
                _ -> False
            , "p50Ms" .= scrP50Ms result
            , "p99Ms" .= scrP99Ms result
            , "peakMib" .= scrPeakMib result
            , "ending" .= scrEnding result
            , "generatorSaturated" .= scrGeneratorSaturated result
            , "failures" .= scrFailures result
            ]

instance ToJSON Failure where
    toJSON failure = object (("kind" .= kind) : ("text" .= renderFailure failure) : facts)
      where
        (kind, facts) = failureFacts failure

failureFacts :: Failure -> (Text, [Pair])
failureFacts = \case
    BootValue what shown expected -> ("bootValue", ["value" .= what, "shown" .= shown, "expected" .= expected])
    NoPosture -> ("noPosture", [])
    UncleanEnding ending -> ("uncleanEnding", ["ending" .= ending])
    OomKills kills -> ("oomKills", ["count" .= kills])
    MemoryHighEvents events -> ("memoryHighEvents", ["count" .= events])
    NoMemoryEvents -> ("noMemoryEvents", [])
    PeakAbovePod peak pod -> ("peakAbovePod", ["peakMib" .= peak, "podMib" .= pod])
    BelowFloor successes least -> ("belowFloor", ["successes" .= successes, "floor" .= least])

-- | Write @comparison.md@ and @comparison.json@ into a job's directory.
writeComparison :: FilePath -> JobComparison -> IO ()
writeComparison directory comparison = do
    writeFileText (directory </> "comparison.md") (renderComparison comparison)
    writeFileLBS (directory </> "comparison.json") (encode comparison <> "\n")

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reviewed success floors by pod shape, scenario, and pass, and the calibration runs they came from.
A cell holds a floor of the calibration's floor fraction of the median count of the runs it names,
is uncalibrated, or is flagged report-only. The reader refuses a floor the median does not give. A
held run fails on a missing cell or a cell that names no count.
-}
module Ecluse.BenchLoad.Floors (
    -- * The committed floors
    Pass (..),
    passKey,
    passNamed,
    PassSelection (..),
    parsePassSelection,
    FloorKey,
    OperatingPoint (..),
    RunnerKind (..),
    RunnerReading (..),
    CalibrationRun (..),
    Calibration (..),
    Cell (..),
    floorFromMedian,
    medianCount,
    Floors (..),
    floorsPath,
    decodeFloors,
    loadFloors,
    renderFloorsFile,

    -- * What a run brings
    Trigger (..),
    triggerIn,
    runnerIn,
    patternOverridesIn,
    RunFacts (..),

    -- * Holding a run to the floors
    Unheld (..),
    Enforcement (..),
    enforce,
    FloorCheck (..),
    floorCheck,
    checkedFloor,
    staleFloorViolations,
    unheldViolations,
    describeEnforcement,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`. The Generic-derived one is used here.
import Prelude hiding (universe)

import Data.Aeson (FromJSON (parseJSON), Value, eitherDecode, encode, withObject, withScientific, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Universe.Class (Universe (universe))
import Data.Universe.Generic (universeGeneric)

import Ecluse.BenchLoad.Pod (PodShape, ProxyTuning (..), noTuning, parsePodShape, renderPodShape)

-- | Which of a scenario's runs a count of successes comes from.
data Pass
    = -- | The scenario at its own connections.
      Loaded
    | -- | The scenario again on a fresh proxy, with the base concurrency set to one.
      ConcurrencyOne
    deriving stock (Eq, Ord, Show, Generic)

instance Universe Pass where universe = universeGeneric

-- | The pass's key in the floors file.
passKey :: Pass -> Text
passKey = \case
    Loaded -> "loaded"
    ConcurrencyOne -> "concurrencyOne"

-- | The pass a key in the floors file names.
passNamed :: Text -> Maybe Pass
passNamed key = find ((== key) . passKey) universe

-- | A count the floors cover: a scenario's ecosystem-qualified key, and the pass.
type FloorKey = (Text, Pass)

-- | The passes a run makes: both, or the loaded pass alone.
data PassSelection
    = EveryPass
    | LoadedPassOnly
    deriving stock (Eq, Show)

-- | Read @every@ or @loaded@.
parsePassSelection :: Text -> Either Text PassSelection
parsePassSelection = \case
    "every" -> Right EveryPass
    "loaded" -> Right LoadedPassOnly
    raw -> Left ("expected every or loaded, not " <> show raw)

-- | Compare knobs, selected scenarios, and pattern overrides. 'Nothing' keeps computed bounds or selects all scenarios.
data OperatingPoint = OperatingPoint
    { opDurationSeconds :: Int
    , opConcurrency :: Int
    , opPayloadBytes :: Int
    , opUpstreamLatencyMs :: Int
    -- ^ The latency every stub upstream injects, except a scenario that fixes its own.
    , opCacheMaxEntries :: Int
    , opWorkingSet :: Int
    , opServeMaxInFlight :: Maybe Int
    , opPublicConnectionsPerHost :: Maybe Int
    , opPrivateConnectionsPerHost :: Maybe Int
    , opScenarios :: Maybe [Text]
    , opPatternOverrides :: [Text]
    -- ^ The @BENCH_PATTERN_*@ variables the run sets, by name.
    , opPasses :: PassSelection
    , opProxyTuning :: ProxyTuning
    }
    deriving stock (Eq, Show)

instance FromJSON OperatingPoint where
    parseJSON = withObject "OperatingPoint" $ \o ->
        OperatingPoint
            <$> o .: "durationSeconds"
            <*> o .: "concurrency"
            <*> o .: "payloadBytes"
            <*> o .: "upstreamLatencyMs"
            <*> o .: "cacheMaxEntries"
            <*> o .: "workingSet"
            <*> o .: "serveMaxInFlight"
            <*> o .: "publicConnectionsPerHost"
            <*> o .: "privateConnectionsPerHost"
            <*> o .: "scenarios"
            <*> o .: "patternOverrides"
            -- The floors are calibrated on both passes of an untuned proxy, so the file records neither.
            <*> pure EveryPass
            <*> pure noTuning

-- The settings in which two operating points differ, by name.
differingSettings :: OperatingPoint -> OperatingPoint -> [Text]
differingSettings calibrated ran =
    [ key
    | (key, agrees) <-
        [ ("durationSeconds", same opDurationSeconds)
        , ("concurrency", same opConcurrency)
        , ("payloadBytes", same opPayloadBytes)
        , ("upstreamLatencyMs", same opUpstreamLatencyMs)
        , ("cacheMaxEntries", same opCacheMaxEntries)
        , ("workingSet", same opWorkingSet)
        , ("serveMaxInFlight", same opServeMaxInFlight)
        , ("publicConnectionsPerHost", same opPublicConnectionsPerHost)
        , ("privateConnectionsPerHost", same opPrivateConnectionsPerHost)
        , ("scenarios", same opScenarios)
        , ("patternOverrides", same opPatternOverrides)
        , ("passes", same opPasses)
        , ("proxyCpus", same (ptCpus . opProxyTuning))
        , ("proxyCores", same (ptCores . opProxyTuning))
        , ("proxyMaxHeapBytes", same (ptMaxHeapBytes . opProxyTuning))
        , ("proxyAllocAreaMib", same (ptAllocAreaMib . opProxyTuning))
        ]
    , not agrees
    ]
  where
    same :: (Eq a) => (OperatingPoint -> a) -> Bool
    same setting = setting calibrated == setting ran

-- | A GitHub Actions runner, as @RUNNER_OS@ and @RUNNER_ARCH@ name it.
data RunnerKind = RunnerKind
    { rkOs :: Text
    , rkArch :: Text
    }
    deriving stock (Eq, Show)

-- | What a run's environment says of its runner: each variable as read, 'Nothing' when it is unset.
data RunnerReading = RunnerReading
    { rrGithubActions :: Maybe Text
    , rrOs :: Maybe Text
    , rrArch :: Maybe Text
    }
    deriving stock (Eq, Show)

-- What a run on a GitHub Actions runner of this kind reads.
readingOn :: RunnerKind -> RunnerReading
readingOn kind = RunnerReading (Just "true") (Just (rkOs kind)) (Just (rkArch kind))

-- | A calibration run a floor came from: its GitHub run ID and URL, the commit it measured, and its event.
data CalibrationRun = CalibrationRun
    { runId :: Integer
    , runUrl :: Text
    , runCommit :: Text
    , runEvent :: Text
    }
    deriving stock (Eq, Show)

instance FromJSON CalibrationRun where
    parseJSON = withObject "CalibrationRun" $ \o -> do
        run <- CalibrationRun <$> o .: "id" <*> o .: "url" <*> o .: "commit" <*> o .: "event"
        when (runId run < 1 || any T.null [runUrl run, runCommit run, runEvent run]) $
            fail "a calibration run must state its ID, its URL, its commit, and its event"
        pure run

-- | Where the floors were measured, how a floor follows from a median, how many re-runs confirm a breach, and the runs the floors came from.
data Calibration = Calibration
    { calConditions :: Text
    -- ^ The harness's measurement conditions the runs were measured under.
    , calRunner :: Text
    -- ^ The runner's label in the workflow, which a run cannot read back.
    , calRunnerKind :: RunnerKind
    , calOperatingPoint :: OperatingPoint
    , calFloorFraction :: Rational
    -- ^ The fraction of a cell's median count its floor is.
    , calConfirmationReruns :: Int
    -- ^ Re-runs a scenario pass below its floor gets before its breach fails the run.
    , calRuns :: [CalibrationRun]
    }
    deriving stock (Eq, Show)

instance FromJSON Calibration where
    parseJSON = withObject "Calibration" $ \o -> do
        calibration <-
            Calibration
                <$> o .: "measurementConditions"
                <*> o .: "runner"
                <*> (RunnerKind <$> o .: "runnerOs" <*> o .: "runnerArch")
                <*> o .: "operatingPoint"
                <*> (withScientific "a fraction" (pure . toRational) =<< o .: "floorFraction")
                <*> o .: "confirmationReruns"
                <*> o .: "runs"
        when (any T.null [calConditions calibration, calRunner calibration, rkOs (calRunnerKind calibration), rkArch (calRunnerKind calibration)]) $
            fail "the calibration must state its measurement conditions and its runner's label, operating system, and architecture"
        when (calConfirmationReruns calibration < 0) $
            fail "the calibration's confirmation re-runs cannot be negative"
        unless (calFloorFraction calibration > 0 && calFloorFraction calibration <= 1) $
            fail "the calibration's floor fraction lies above 0 and at most 1"
        when (length (ordNub (map runId (calRuns calibration))) /= length (calRuns calibration)) $
            fail "the calibration lists a run twice"
        pure calibration

-- | One scenario pass under one pod shape.
data Cell
    = -- | No floor yet. The pass keeps the zero-success and process checks.
      Uncalibrated
    | -- | This floor, the floor fraction of this median count of the runs with these IDs.
      Floored Int Int (NonEmpty Integer)
    | -- | Flagged to keep the zero-success and process checks alone, for this reason. A calibration leaves it without a floor.
      ReportOnlyCell Text
    deriving stock (Eq, Show)

-- | The floor a median count gives at a floor fraction: that fraction of it, rounded down, and at least 1.
floorFromMedian :: Rational -> Int -> Int
floorFromMedian fraction median = max 1 (floor (fromIntegral median * fraction))

-- | The middle count, the lower middle of an even number.
medianCount :: NonEmpty Int -> Int
medianCount counts@(fallback :| _) = case drop ((length counts - 1) `div` 2) (sort (toList counts)) of
    middle : _ -> middle
    [] -> fallback

-- The cell's JSON form, refusing a floor its median does not give at the fraction or that names an unlisted run.
parseCell :: Rational -> Set Integer -> Value -> Parser Cell
parseCell fraction listed = withObject "Cell" $ \o -> do
    least <- o .: "floor"
    median <- o .:? "median"
    from <- o .:? "from" .!= []
    reason <- o .:? "reportOnly"
    case (least, median, nonEmpty from, reason) of
        (Nothing, Nothing, Nothing, Nothing) -> pure Uncalibrated
        (Nothing, Nothing, Nothing, Just why)
            | T.null why -> fail "a report-only cell names its reason"
            | otherwise -> pure (ReportOnlyCell why)
        (Just f, Just m, Just runs, Nothing)
            | m < 1 -> fail "a floor's median is at least 1"
            | f /= floorFromMedian fraction m -> fail ("a floor is the floor fraction of its median, rounded down and at least 1: " <> show (floorFromMedian fraction m) <> " here")
            | length (ordNub (toList runs)) /= length runs -> fail "a floor names a run twice"
            | not (all (`Set.member` listed) runs) -> fail "a floor names a run the calibration does not list"
            | otherwise -> pure (Floored f m runs)
        _ -> fail "a cell holds a floor with its median and runs, a report-only reason, or a null floor alone"

-- | The calibration, and each count's cell by pod shape.
data Floors = Floors
    { floorsNote :: Text
    , floorsCalibration :: Calibration
    , floorsByShape :: Map PodShape (Map FloorKey Cell)
    }
    deriving stock (Eq, Show)

instance FromJSON Floors where
    parseJSON = withObject "Floors" $ \o -> do
        note <- o .:? "_note" .!= ""
        calibration <- o .: "calibration"
        shapes <- o .: "floors"
        let listed = Set.fromList (map runId (calRuns calibration))
            fraction = calFloorFraction calibration
        byShape <- Map.fromList <$> traverse (parseShape fraction listed) (Map.toList shapes)
        let named = Set.fromList [run | cells <- Map.elems byShape, Floored _ _ runs <- Map.elems cells, run <- toList runs]
        unless (named == listed) $
            fail "the calibration lists exactly the runs the floors name"
        pure (Floors note calibration byShape)
      where
        parseShape :: Rational -> Set Integer -> (Text, Map Text (Map Text Value)) -> Parser (PodShape, Map FloorKey Cell)
        parseShape fraction listed (name, scenarios) = (,) <$> shapeNamed name <*> (Map.fromList . concat <$> traverse (parseScenario fraction listed) (Map.toList scenarios))
        parseScenario fraction listed (scenario, passes) = traverse (parsePass fraction listed scenario) (Map.toList passes)
        parsePass fraction listed scenario (key, value) = case passNamed key of
            Nothing -> fail ("unknown pass in the floors: " <> toString key)
            Just whichPass -> ((scenario, whichPass),) <$> parseCell fraction listed value

-- Only the rendered form names a pod shape, so two keys never mean the same one.
shapeNamed :: Text -> Parser PodShape
shapeNamed name = case parsePodShape name of
    Right shape | renderPodShape shape == name -> pure shape
    _ -> fail ("the floors name a pod shape that is not in its rendered form: " <> toString name)

-- | The committed floors' path, relative to the package root the harness runs from.
floorsPath :: FilePath
floorsPath = "bench/load/floors.json"

-- | Decode 'Floors' from raw JSON bytes, refusing a cell its median does not give.
decodeFloors :: LByteString -> Either Text Floors
decodeFloors = first toText . eitherDecode

-- | Read and decode the committed floors from 'floorsPath'.
loadFloors :: IO (Either Text Floors)
loadFloors = first ((toText floorsPath <> " did not decode: ") <>) . decodeFloors <$> readFileLBS floorsPath

-- | The floors file in its committed layout: one line per cell, so a recalibration diffs by cell.
renderFloorsFile :: Floors -> Text
renderFloorsFile floors =
    T.unlines $
        ["{", "  \"_note\": " <> string (floorsNote floors) <> ",", "  \"calibration\": {"]
            <> members (map (map ("    " <>)) calibrationMembers)
            <> ["  },", "  \"floors\": {"]
            <> members (map shapeLines (Map.toList (floorsByShape floors)))
            <> ["  }", "}"]
  where
    calibration = floorsCalibration floors
    point = calOperatingPoint calibration
    calibrationMembers =
        map
            pure
            [ "\"measurementConditions\": " <> string (calConditions calibration)
            , "\"runner\": " <> string (calRunner calibration)
            , "\"runnerOs\": " <> string (rkOs (calRunnerKind calibration))
            , "\"runnerArch\": " <> string (rkArch (calRunnerKind calibration))
            , "\"operatingPoint\": "
                <> inline
                    [ ("durationSeconds", show (opDurationSeconds point))
                    , ("concurrency", show (opConcurrency point))
                    , ("payloadBytes", show (opPayloadBytes point))
                    , ("upstreamLatencyMs", show (opUpstreamLatencyMs point))
                    , ("cacheMaxEntries", show (opCacheMaxEntries point))
                    , ("workingSet", show (opWorkingSet point))
                    , ("serveMaxInFlight", maybe "null" show (opServeMaxInFlight point))
                    , ("publicConnectionsPerHost", maybe "null" show (opPublicConnectionsPerHost point))
                    , ("privateConnectionsPerHost", maybe "null" show (opPrivateConnectionsPerHost point))
                    , ("scenarios", maybe "null" strings (opScenarios point))
                    , ("patternOverrides", strings (opPatternOverrides point))
                    ]
            , "\"floorFraction\": " <> decimalText (calFloorFraction calibration)
            , "\"confirmationReruns\": " <> show (calConfirmationReruns calibration)
            ]
            <> [runsLines]
    runsLines = case calRuns calibration of
        [] -> ["\"runs\": []"]
        runs -> ["\"runs\": ["] <> members [["  " <> inline [("id", show (runId r)), ("url", string (runUrl r)), ("commit", string (runCommit r)), ("event", string (runEvent r))]] | r <- runs] <> ["]"]
    shapeLines (shape, cells) =
        ["    " <> string (renderPodShape shape) <> ": {"]
            <> members (map scenarioLines (Map.toList (Map.fromListWith (flip (<>)) [(scenario, [(whichPass, cell)]) | ((scenario, whichPass), cell) <- Map.toList cells])))
            <> ["    }"]
    scenarioLines (scenario, passes) =
        ["      " <> string scenario <> ": {"]
            <> members [["        " <> string (passKey whichPass) <> ": " <> cellObject cell] | (whichPass, cell) <- sortOn fst passes]
            <> ["      }"]
    cellObject = \case
        Uncalibrated -> inline [("floor", "null")]
        Floored least median runs -> inline [("floor", show least), ("median", show median), ("from", "[" <> T.intercalate ", " (map show (toList runs)) <> "]")]
        ReportOnlyCell why -> inline [("floor", "null"), ("reportOnly", string why)]
    inline fields = "{ " <> T.intercalate ", " [string key <> ": " <> value | (key, value) <- fields] <> " }"
    strings items = "[" <> T.intercalate ", " (map string items) <> "]"
    string :: Text -> Text
    string = decodeUtf8 . encode

-- A terminating decimal exactly, with at least two places, as a JSON number gives one.
decimalText :: Rational -> Text
decimalText x = show whole <> "." <> T.justifyRight places '0' (show fraction)
  where
    places = fromMaybe 18 (find (\k -> denominator (x * 10 ^ k) == 1) [2 .. 18])
    (whole, fraction) = (round (x * 10 ^ places) :: Integer) `quotRem` (10 ^ places)

-- JSON members, one per list of lines: every member but the last takes a comma on its last line.
members :: [[Text]] -> [Text]
members groups = concat (zipWith comma [1 :: Int ..] groups)
  where
    comma i ls
        | i == length groups = ls
        | otherwise = withComma ls
    withComma = \case
        [] -> []
        [line] -> [line <> ","]
        line : more -> line : withComma more

-- | What started a run. A scheduled run must be held to the floors, so its configuration may not keep it off them.
data Trigger
    = Scheduled
    | OnDemand
    deriving stock (Eq, Show)

-- | The trigger @GITHUB_EVENT_NAME@ names. Anything but @schedule@ is on demand, a run off GitHub Actions included.
triggerIn :: Map Text Text -> Trigger
triggerIn environment
    | Map.lookup "GITHUB_EVENT_NAME" environment == Just "schedule" = Scheduled
    | otherwise = OnDemand

-- | What @GITHUB_ACTIONS@, @RUNNER_OS@, and @RUNNER_ARCH@ say of the runner in an environment.
runnerIn :: Map Text Text -> RunnerReading
runnerIn environment = RunnerReading (variable "GITHUB_ACTIONS") (variable "RUNNER_OS") (variable "RUNNER_ARCH")
  where
    variable name = Map.lookup name environment

-- | The request-pattern variables an environment sets, by name. The floors were calibrated with none set.
patternOverridesIn :: Map Text Text -> [Text]
patternOverridesIn = filter ("BENCH_PATTERN_" `T.isPrefixOf`) . Map.keys

-- | What decides whether a run is held to the floors. A live probe of the public registries is not part of it.
data RunFacts = RunFacts
    { rfSettings :: OperatingPoint
    , rfRunner :: RunnerReading
    , rfShape :: PodShape
    , rfConditions :: Text
    }
    deriving stock (Eq, Show)

-- | One reason a run is not held to the floors.
data Unheld
    = -- | It differs from the calibrated operating point in these settings.
      SettingsDiffer (NonEmpty Text)
    | -- | It does not run on the calibrated runner, named by its label and its kind. Last is what the run read.
      OtherRunner Text RunnerKind RunnerReading
    | -- | The floors hold none for its pod shape.
      ShapeNotCalibrated PodShape
    | -- | It runs the GC-thrash probe in place of the passes, and the probe is never held.
      ThrashProbe
    | -- | The floors were measured under the first measurement conditions, and the run under the second.
      ConditionsDiffer Text Text
    deriving stock (Eq, Show)

-- | Whether a run is held to the floors.
data Enforcement
    = -- | Held, under this pod shape, with this many confirmation re-runs per breach, to these cells.
      Enforced PodShape Int (Map FloorKey Cell)
    | -- | Not held, for every one of these reasons.
      NotHeld (NonEmpty Unheld)
    deriving stock (Eq, Show)

-- | Decide a run's enforcement from its facts, naming every reason it is not held.
enforce :: Floors -> RunFacts -> Enforcement
enforce floors facts = case (Map.lookup shape (floorsByShape floors), reasons) of
    (Just held, []) -> Enforced shape (calConfirmationReruns calibration) held
    (Just _, reason : more) -> NotHeld (reason :| more)
    (Nothing, more) -> NotHeld (ShapeNotCalibrated shape :| more)
  where
    shape = rfShape facts
    calibration = floorsCalibration floors
    reasons =
        [SettingsDiffer settings | Just settings <- [nonEmpty (differingSettings (calOperatingPoint calibration) (rfSettings facts))]]
            <> [OtherRunner (calRunner calibration) (calRunnerKind calibration) (rfRunner facts) | rfRunner facts /= readingOn (calRunnerKind calibration)]
            <> [ConditionsDiffer (calConditions calibration) (rfConditions facts) | calConditions calibration /= rfConditions facts]

-- | What one scenario's successes are held to in one pass.
data FloorCheck
    = -- | The run is not held to the floors, so no floor applies.
      Unchecked
    | -- | The run is held and the floors have no cell for this pass of the scenario, which fails it.
      NoFloor Pass
    | -- | Every load of the scenario must reach this floor, set from this median count.
      AtLeast Int Int
    | -- | The cell holds no floor: uncalibrated ('Nothing'), or report-only for this reason.
      ReportOnly (Maybe Text)
    deriving stock (Eq, Show)

-- | The check for one count of a run.
floorCheck :: Enforcement -> FloorKey -> FloorCheck
floorCheck enforcement key@(_, whichPass) = case enforcement of
    NotHeld _ -> Unchecked
    Enforced _ _ cells -> case Map.lookup key cells of
        Nothing -> NoFloor whichPass
        Just (Floored least median _) -> AtLeast least median
        Just Uncalibrated -> ReportOnly Nothing
        Just (ReportOnlyCell why) -> ReportOnly (Just why)

-- | The count below which a load breaches, when the cell holds a floor.
checkedFloor :: FloorCheck -> Maybe Int
checkedFloor = \case
    AtLeast least _ -> Just least
    _ -> Nothing

-- | One line per cell a held run has for a count outside the ones it checks. A stale cell fails the run.
staleFloorViolations :: Enforcement -> Set FloorKey -> [Text]
staleFloorViolations enforcement checked = case enforcement of
    NotHeld _ -> []
    Enforced shape _ cells ->
        [ toText floorsPath <> ": the " <> passKey whichPass <> " cell for " <> scenario <> " under " <> renderPodShape shape <> " names no count this run checks"
        | (scenario, whichPass) <- Set.toList (Map.keysSet cells `Set.difference` checked)
        ]

-- | Fail a scheduled run that is not held, with every reason in one line.
unheldViolations :: Trigger -> Enforcement -> [Text]
unheldViolations trigger enforcement = case (trigger, enforcement) of
    (Scheduled, NotHeld reasons) -> ["a scheduled run must be held to the success floors, and this one is not: " <> T.intercalate ", and " (map describeUnheld (toList reasons))]
    _ -> []

describeUnheld :: Unheld -> Text
describeUnheld = \case
    SettingsDiffer settings -> "it differs from the calibrated operating point in " <> T.intercalate ", " (toList settings)
    OtherRunner label kind reading ->
        "it does not run on the calibrated runner ("
            <> label
            <> ": GitHub Actions on "
            <> rkOs kind
            <> " "
            <> rkArch kind
            <> "), and the harness read "
            <> T.intercalate ", " [readAs "GITHUB_ACTIONS" (rrGithubActions reading), readAs "RUNNER_OS" (rrOs reading), readAs "RUNNER_ARCH" (rrArch reading)]
    ShapeNotCalibrated shape -> "the floors hold no entry for the pod shape " <> renderPodShape shape
    ThrashProbe -> "it runs the GC-thrash probe in place of the passes"
    ConditionsDiffer calibrated ran -> "its measurement conditions are " <> ran <> ", but the floors were measured under " <> calibrated
  where
    readAs name = maybe (name <> " unset") ((name <> "=") <>)

-- | The report's one line on the floors: what a held run is held to, or every reason a run is not held.
describeEnforcement :: Calibration -> Enforcement -> Text
describeEnforcement calibration = \case
    Enforced shape _ cells ->
        "This run is held to the success floors in `"
            <> toText floorsPath
            <> "`, calibrated on "
            <> calRunner calibration
            <> " under measurement conditions "
            <> calConditions calibration
            <> provenance
            <> " Under "
            <> renderPodShape shape
            <> ", "
            <> show (length [() | Floored{} <- Map.elems cells])
            <> " cells hold a floor, "
            <> show (length [() | Uncalibrated <- Map.elems cells])
            <> " are uncalibrated, and "
            <> show (length [() | ReportOnlyCell{} <- Map.elems cells])
            <> " are report-only."
    NotHeld reasons ->
        "This run is not held to the success floors in `"
            <> toText floorsPath
            <> "`: "
            <> T.intercalate ", and " (map describeUnheld (toList reasons))
            <> "."
  where
    provenance = case calRuns calibration of
        [] -> ", from no calibration run yet."
        runs -> ", from " <> show (length runs) <> " calibration runs at " <> T.intercalate ", " (ordNub (map runCommit runs)) <> "."

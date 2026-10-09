-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The reviewed success floors a load run is held to. A floor is the fewest successes one
scenario may have in one pass under one pod shape. The committed file also records where and how
the floors were calibrated. A run is held to them only on that runner, at those settings, under a
pod shape the file holds, and while its npm fixture injects no more latency than the file's
ceiling. A held run fails closed: on a scenario without a floor, and on a floor for a count it does
not check.
-}
module Ecluse.BenchLoad.Floors (
    -- * The committed floors
    Pass (..),
    passKey,
    FloorKey,
    OperatingPoint (..),
    RunnerKind (..),
    CalibrationRun (..),
    LatencyCeiling (..),
    latencyCeilingMs,
    Calibration (..),
    Floors (..),
    floorsPath,
    decodeFloors,
    loadFloors,

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
    staleFloorViolations,
    unheldViolations,
    describeEnforcement,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`. The Generic-derived one is used here.
import Prelude hiding (universe)

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Universe.Class (Universe (universe))
import Data.Universe.Generic (universeGeneric)

import Ecluse.BenchLoad.Pod (PodShape, parsePodShape, renderPodShape)

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

-- | A count the floors cover: a scenario's ecosystem-qualified key, and the pass.
type FloorKey = (Text, Pass)

{- | The settings a run is compared in: its load knobs, its scenario selection, and its request-pattern
overrides. 'Nothing' leaves a bound to the proxy's computed default, or selects every scenario.
-}
data OperatingPoint = OperatingPoint
    { opDurationSeconds :: Int
    , opConcurrency :: Int
    , opPayloadBytes :: Int
    , opUpstreamLatencyMs :: Int
    -- ^ The configured latency. The npm fixture injects the round trip it probes instead.
    , opCacheMaxEntries :: Int
    , opWorkingSet :: Int
    , opServeMaxInFlight :: Maybe Int
    , opPublicConnectionsPerHost :: Maybe Int
    , opPrivateConnectionsPerHost :: Maybe Int
    , opScenarios :: Maybe [Text]
    , opPatternOverrides :: [Text]
    -- ^ The @BENCH_PATTERN_*@ variables the run sets, by name.
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

-- The settings in which two operating points differ, by their keys in the floors file.
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

-- | One run the floors were calibrated from, and the commit it measured.
data CalibrationRun = CalibrationRun
    { runUrl :: Text
    , runCommit :: Text
    }
    deriving stock (Eq, Show)

instance FromJSON CalibrationRun where
    parseJSON = withObject "CalibrationRun" $ \o -> do
        run <- CalibrationRun <$> o .: "url" <*> o .: "commit"
        when (T.null (runUrl run) || T.null (runCommit run)) $
            fail "a calibration run must state its URL and its commit"
        pure run

-- | The most latency the npm fixture injected in the calibration runs, and the most a held run may inject.
data LatencyCeiling = LatencyCeiling
    { lcRule :: Text
    , lcHighestMs :: Int
    , lcCeilingMs :: Int
    }
    deriving stock (Eq, Show)

instance FromJSON LatencyCeiling where
    parseJSON = withObject "LatencyCeiling" $ \o -> do
        latency <- LatencyCeiling <$> o .: "rule" <*> o .: "highestMs" <*> o .: "ceilingMs"
        when (T.null (lcRule latency) || lcHighestMs latency < 1) $
            fail "the latency ceiling must state its rule and a positive highest latency"
        when (lcCeilingMs latency /= latencyCeilingMs (lcHighestMs latency)) $
            fail "the latency ceiling must be 1.5 times the highest latency, rounded up to 10 ms"
        pure latency

-- | The ceiling for a highest calibrated latency: one and a half times it, rounded up to 10 ms.
latencyCeilingMs :: Int -> Int
latencyCeilingMs highestMs = (highestMs * 3 + 19) `div` 20 * 10

-- | Where the floors were measured, the rule that set them, and what a run must match to be held to them.
data Calibration = Calibration
    { calRule :: Text
    , calRunner :: Text
    -- ^ The runner's label in the workflow, which a run cannot read back.
    , calRunnerKind :: RunnerKind
    , calRuns :: NonEmpty CalibrationRun
    , calOperatingPoint :: OperatingPoint
    , calNpmLatency :: LatencyCeiling
    }
    deriving stock (Eq, Show)

instance FromJSON Calibration where
    parseJSON = withObject "Calibration" $ \o -> do
        calibration <-
            Calibration
                <$> o .: "rule"
                <*> o .: "runner"
                <*> (RunnerKind <$> o .: "runnerOs" <*> o .: "runnerArch")
                <*> o .: "runs"
                <*> o .: "operatingPoint"
                <*> o .: "npmInjectedLatency"
        when (any T.null [calRule calibration, calRunner calibration, rkOs (calRunnerKind calibration), rkArch (calRunnerKind calibration)]) $
            fail "the calibration must state its rule and its runner's label, operating system, and architecture"
        pure calibration

-- | The calibration, and each count's floor by pod shape.
data Floors = Floors
    { floorsCalibration :: Calibration
    , floorsByShape :: Map PodShape (Map FloorKey Int)
    }
    deriving stock (Eq, Show)

instance FromJSON Floors where
    parseJSON = withObject "Floors" $ \o -> do
        calibration <- o .: "calibration"
        shapes <- o .: "floors"
        Floors calibration . Map.fromList <$> traverse parseShape (Map.toList shapes)
      where
        -- Only the rendered form names a shape, so two keys never mean the same one.
        parseShape :: (Text, Map Text (Map Text Int)) -> Parser (PodShape, Map FloorKey Int)
        parseShape (name, scenarios) = case parsePodShape name of
            Right shape | renderPodShape shape == name -> (shape,) . Map.fromList . concat <$> traverse parseScenario (Map.toList scenarios)
            _ -> fail ("the floors name a pod shape that is not in its rendered form: " <> toString name)
        parseScenario :: (Text, Map Text Int) -> Parser [(FloorKey, Int)]
        parseScenario (scenario, passes) = traverse (parseFloor scenario) (Map.toList passes)
        parseFloor :: Text -> (Text, Int) -> Parser (FloorKey, Int)
        parseFloor scenario (key, least) = case find ((== key) . passKey) universe of
            Nothing -> fail ("unknown pass in the floors: " <> toString key)
            Just whichPass
                | least >= 1 -> pure ((scenario, whichPass), least)
                | otherwise -> fail ("a floor must be at least 1: " <> toString scenario <> " " <> toString key)

-- | The committed floors' path, relative to the package root the harness runs from.
floorsPath :: FilePath
floorsPath = "bench/load/floors.json"

-- | Decode 'Floors' from raw JSON bytes, refusing an unknown pod shape or pass and a floor below 1.
decodeFloors :: LByteString -> Either Text Floors
decodeFloors = first toText . eitherDecode

-- | Read and decode the committed floors from 'floorsPath'.
loadFloors :: IO (Either Text Floors)
loadFloors = first ((toText floorsPath <> " did not decode: ") <>) . decodeFloors <$> readFileLBS floorsPath

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

-- | The runner GitHub's environment describes, 'Nothing' off GitHub Actions.
runnerIn :: Map Text Text -> Maybe RunnerKind
runnerIn environment = do
    guard (Map.lookup "GITHUB_ACTIONS" environment == Just "true")
    RunnerKind <$> Map.lookup "RUNNER_OS" environment <*> Map.lookup "RUNNER_ARCH" environment

-- | The request-pattern variables an environment sets, by name. The floors were calibrated with none set.
patternOverridesIn :: Map Text Text -> [Text]
patternOverridesIn = filter ("BENCH_PATTERN_" `T.isPrefixOf`) . Map.keys

-- | What decides whether a run is held to the floors.
data RunFacts = RunFacts
    { rfSettings :: OperatingPoint
    , rfRunner :: Maybe RunnerKind
    -- ^ 'Nothing' off GitHub Actions.
    , rfShape :: PodShape
    , rfNpmLatencyMs :: Int
    -- ^ What the npm fixture injects: the public round trip the run probed, or the configured latency.
    }
    deriving stock (Eq, Show)

-- | One reason a run is not held to the floors.
data Unheld
    = -- | It differs from the calibrated operating point in these settings.
      SettingsDiffer (NonEmpty Text)
    | -- | It does not run on this runner, the calibrated one, named by its label and its kind.
      OtherRunner Text RunnerKind
    | -- | The floors hold none for its pod shape.
      ShapeNotCalibrated PodShape
    | -- | Its npm fixture injects this many milliseconds, above this ceiling.
      LatencyAboveCeiling Int Int
    deriving stock (Eq, Show)

-- | Whether a run is held to the floors.
data Enforcement
    = -- | Held, under this pod shape, to these floors.
      Enforced PodShape (Map FloorKey Int)
    | -- | Not held, for every one of these reasons.
      NotHeld (NonEmpty Unheld)
    deriving stock (Eq, Show)

-- | Decide a run's enforcement from its facts, naming every reason it is not held.
enforce :: Floors -> RunFacts -> Enforcement
enforce floors facts = case (Map.lookup shape (floorsByShape floors), reasons) of
    (Just held, []) -> Enforced shape held
    (Just _, reason : more) -> NotHeld (reason :| more)
    (Nothing, more) -> NotHeld (ShapeNotCalibrated shape :| more)
  where
    shape = rfShape facts
    calibration = floorsCalibration floors
    most = lcCeilingMs (calNpmLatency calibration)
    reasons =
        [SettingsDiffer settings | Just settings <- [nonEmpty (differingSettings (calOperatingPoint calibration) (rfSettings facts))]]
            <> [OtherRunner (calRunner calibration) (calRunnerKind calibration) | rfRunner facts /= Just (calRunnerKind calibration)]
            <> [LatencyAboveCeiling (rfNpmLatencyMs facts) most | rfNpmLatencyMs facts > most]

-- | What one scenario's successes are held to in one pass.
data FloorCheck
    = -- | The run is not held to the floors, so no floor applies.
      Unchecked
    | -- | The run is held and the floors have none for this pass of the scenario, which fails it.
      NoFloor Pass
    | -- | Every load of the scenario must reach this many successes.
      AtLeast Int
    deriving stock (Eq, Show)

-- | The check for one count of a run.
floorCheck :: Enforcement -> FloorKey -> FloorCheck
floorCheck enforcement key@(_, whichPass) = case enforcement of
    NotHeld _ -> Unchecked
    Enforced _ floors -> maybe (NoFloor whichPass) AtLeast (Map.lookup key floors)

-- | One line per floor a held run has for a count outside the ones it checks. A stale floor fails the run.
staleFloorViolations :: Enforcement -> Set FloorKey -> [Text]
staleFloorViolations enforcement checked = case enforcement of
    NotHeld _ -> []
    Enforced shape floors ->
        [ toText floorsPath <> ": the " <> passKey whichPass <> " floor for " <> scenario <> " under " <> renderPodShape shape <> " names no count this run checks"
        | (scenario, whichPass) <- Set.toList (Map.keysSet floors `Set.difference` checked)
        ]

{- | The one violation of a scheduled run that its own configuration keeps off the floors. Latency
above the ceiling is the network's doing, so it fails no run.
-}
unheldViolations :: Trigger -> Enforcement -> [Text]
unheldViolations trigger enforcement = case (trigger, enforcement) of
    (Scheduled, NotHeld reasons)
        | faults@(_ : _) <- mapMaybe configurationFault (toList reasons) ->
            ["a scheduled run must be held to the success floors, and this one is not: " <> T.intercalate ", and " faults]
    _ -> []

-- A reason as a fault of the run's configuration, which latency above the ceiling is not.
configurationFault :: Unheld -> Maybe Text
configurationFault reason = case reason of
    SettingsDiffer _ -> Just (describeUnheld reason)
    OtherRunner _ _ -> Just (describeUnheld reason)
    ShapeNotCalibrated _ -> Just (describeUnheld reason)
    LatencyAboveCeiling _ _ -> Nothing

describeUnheld :: Unheld -> Text
describeUnheld = \case
    SettingsDiffer settings -> "it differs from the calibrated operating point in " <> T.intercalate ", " (toList settings)
    OtherRunner label kind -> "it does not run on the calibrated runner (" <> label <> ": GitHub Actions on " <> rkOs kind <> " " <> rkArch kind <> ")"
    ShapeNotCalibrated shape -> "the floors hold no entry for the pod shape " <> renderPodShape shape
    LatencyAboveCeiling injected most -> "its npm fixture injects " <> show injected <> " ms of upstream latency, above the ceiling of " <> show most <> " ms"

-- | The report's one line on the floors: what a held run is held to, or every reason a run is not held.
describeEnforcement :: Calibration -> Enforcement -> Text
describeEnforcement calibration = \case
    Enforced _ _ ->
        "This run is held to the success floors in `"
            <> toText floorsPath
            <> "`, calibrated on "
            <> calRunner calibration
            <> " from "
            <> show (length (calRuns calibration))
            <> " runs at "
            <> T.intercalate ", " (ordNub (map runCommit (toList (calRuns calibration))))
            <> ". "
            <> calRule calibration
            <> " A run whose npm fixture injects more than "
            <> show (lcCeilingMs (calNpmLatency calibration))
            <> " ms of upstream latency is not held."
    NotHeld reasons ->
        "This run is not held to the success floors in `"
            <> toText floorsPath
            <> "`: "
            <> T.intercalate ", and " (map describeUnheld (toList reasons))
            <> "."

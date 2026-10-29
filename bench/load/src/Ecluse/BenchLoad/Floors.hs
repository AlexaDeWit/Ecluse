-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reviewed minimum successes by scenario, pass, and pod shape, with calibration provenance.
A held run fails on a missing floor or a floor that names an unreported count.
-}
module Ecluse.BenchLoad.Floors (
    -- * The committed floors
    Pass (..),
    passKey,
    PassSelection (..),
    parsePassSelection,
    FloorKey,
    OperatingPoint (..),
    RunnerKind (..),
    RunnerReading (..),
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
    describeRetainedConditions,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`. The Generic-derived one is used here.
import Prelude hiding (universe)

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Universe.Class (Universe (universe))
import Data.Universe.Generic (universeGeneric)

import Ecluse.BenchLoad.Pod (PodShape, ProxyTuning (..), noTuning, parsePodShape, renderPodShape)
import Ecluse.BenchLoad.Selection (measurementConditionVersion)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)

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
    -- ^ The configured fallback latency. Each fixture normally injects its own measured round trip.
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

-- | The highest native injected latency per pod shape and the ceiling for held runs.
data LatencyCeiling = LatencyCeiling
    { lcRule :: Text
    , lcHighestMs :: Map PodShape Int
    , lcCeilingMs :: Int
    }
    deriving stock (Eq, Show)

instance FromJSON LatencyCeiling where
    parseJSON = withObject "LatencyCeiling" $ \o -> do
        rule <- o .: "rule"
        highest <- Map.fromList <$> (traverse shapeLatency . Map.toList =<< o .: "highestMs")
        most <- o .: "ceilingMs"
        when (T.null rule || any (< 1) highest) $
            fail "the latency ceiling must state its rule, and a positive highest latency for each pod shape"
        when (fmap latencyCeilingMs (nonEmpty (Map.elems highest)) /= Just most) $
            fail "the latency ceiling must be 1.5 times the lowest of the pod shapes' highest latencies, rounded up to 10 ms"
        pure (LatencyCeiling rule highest most)
      where
        shapeLatency :: (Text, Int) -> Parser (PodShape, Int)
        shapeLatency (name, highest) = (,highest) <$> shapeNamed name

-- | Set the ceiling to 1.5 times the lowest per-shape maximum, rounded up to 10 ms.
latencyCeilingMs :: NonEmpty Int -> Int
latencyCeilingMs (highest :| others) = (foldl' min highest others * 3 + 19) `div` 20 * 10

-- Only the rendered form names a pod shape, so two keys never mean the same one.
shapeNamed :: Text -> Parser PodShape
shapeNamed name = case parsePodShape name of
    Right shape | renderPodShape shape == name -> pure shape
    _ -> fail ("the floors name a pod shape that is not in its rendered form: " <> toString name)

-- | Where the floors were measured, the rule that set them, and what a run must match to be held to them.
data Calibration = Calibration
    { calRule :: Text
    , calRunner :: Text
    -- ^ The runner's label in the workflow, which a run cannot read back.
    , calRunnerKind :: RunnerKind
    , calRuns :: NonEmpty CalibrationRun
    , calOperatingPoint :: OperatingPoint
    , calNpmLatency :: LatencyCeiling
    , calCargoLatency :: Maybe LatencyCeiling
    , calRubyGemsLatency :: Maybe LatencyCeiling
    , calPypiLatency :: Maybe LatencyCeiling
    -- ^ Legacy calibration predates the native PyPI probe.
    , calConditions :: Text
    , calRetainedConditions :: Map Text Text
    -- ^ Unaffected scenario rows retain their earlier measurement identity.
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
                <*> o .:? "cargoInjectedLatency"
                <*> o .:? "rubygemsInjectedLatency"
                <*> o .:? "pypiInjectedLatency"
                <*> o .:? "measurementConditions" .!= "legacy-v1"
                <*> o .:? "retainedFloorConditions" .!= mempty
        when (any T.null [calRule calibration, calRunner calibration, rkOs (calRunnerKind calibration), rkArch (calRunnerKind calibration)]) $
            fail "the calibration must state its rule and its runner's label, operating system, and architecture"
        unless (calConditions calibration `elem` ["legacy-v1", measurementConditionVersion]) $
            fail "the calibration names unknown measurement conditions"
        when (calConditions calibration == measurementConditionVersion && isNothing (calPypiLatency calibration)) $
            fail "unified measurement conditions require a native PyPI latency ceiling"
        when (any T.null (Map.keys (calRetainedConditions calibration) <> Map.elems (calRetainedConditions calibration))) $
            fail "retained floor conditions require a scenario and its measurement identity"
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
        byShape <- Map.fromList <$> traverse parseShape (Map.toList shapes)
        when (any ((/= Map.keysSet byShape) . Map.keysSet . lcHighestMs) (mapMaybe (\(_, entry) -> leCeiling entry calibration) latencyEntries)) $
            fail "the latency ceiling must record a highest latency for each pod shape the floors hold, and for no other"
        unless (all (`elem` map fst (concatMap Map.keys (Map.elems byShape))) (Map.keys (calRetainedConditions calibration))) $
            fail "retained measurement conditions name a scenario without floors"
        for_ [ecosystem | (ecosystem, entry) <- latencyEntries, leNative entry, isNothing (leCeiling entry calibration)] $ \ecosystem ->
            when (any (any (T.isPrefixOf (ecosystemName ecosystem <> "/") . fst) . Map.keys) (Map.elems byShape)) $
                fail (show ecosystem <> " floors require an independently measured native latency ceiling")
        pure (Floors calibration byShape)
      where
        parseShape :: (Text, Map Text (Map Text Int)) -> Parser (PodShape, Map FloorKey Int)
        parseShape (name, scenarios) = (,) <$> shapeNamed name <*> (Map.fromList . concat <$> traverse parseScenario (Map.toList scenarios))
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

-- | What @GITHUB_ACTIONS@, @RUNNER_OS@, and @RUNNER_ARCH@ say of the runner in an environment.
runnerIn :: Map Text Text -> RunnerReading
runnerIn environment = RunnerReading (variable "GITHUB_ACTIONS") (variable "RUNNER_OS") (variable "RUNNER_ARCH")
  where
    variable name = Map.lookup name environment

-- | The request-pattern variables an environment sets, by name. The floors were calibrated with none set.
patternOverridesIn :: Map Text Text -> [Text]
patternOverridesIn = filter ("BENCH_PATTERN_" `T.isPrefixOf`) . Map.keys

-- | What decides whether a run is held to the floors.
data RunFacts = RunFacts
    { rfSettings :: OperatingPoint
    , rfRunner :: RunnerReading
    , rfShape :: PodShape
    , rfNpmLatencyMs :: Int
    -- ^ The native measured round trip or declared configured fallback.
    , rfCargoLatencyMs :: Maybe Int
    , rfRubyGemsLatencyMs :: Maybe Int
    , rfPypiLatencyMs :: Int
    , rfConditions :: Text
    }
    deriving stock (Eq, Show)

-- How the floors read one ecosystem's injected latency.
data LatencyEntry = LatencyEntry
    { leReportOrder :: Int
    , leNative :: Bool
    -- ^ Floors for a native fixture need an independent ceiling, and a recorded ceiling needs an observation.
    , leCeiling :: Calibration -> Maybe LatencyCeiling
    , leObserved :: RunFacts -> Maybe Int
    }

-- Map an ecosystem to its latency fields. A total match, so a new ecosystem must state its fields here.
latencyEntry :: Ecosystem -> LatencyEntry
latencyEntry = \case
    Npm -> LatencyEntry 0 False (Just . calNpmLatency) (Just . rfNpmLatencyMs)
    PyPI -> LatencyEntry 1 False calPypiLatency (Just . rfPypiLatencyMs)
    Cargo -> LatencyEntry 2 True calCargoLatency rfCargoLatencyMs
    RubyGems -> LatencyEntry 3 True calRubyGemsLatency rfRubyGemsLatencyMs

-- Every ecosystem's latency entry, in the order reasons and the report name them.
latencyEntries :: [(Ecosystem, LatencyEntry)]
latencyEntries = sortOn (leReportOrder . snd) [(ecosystem, latencyEntry ecosystem) | ecosystem <- universe]

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
    | -- | A native fixture injects this many milliseconds, above its own ceiling.
      LatencyAboveCeiling Ecosystem Int Int
    | -- | The run uses different measurement conditions from the retained floors.
      ConditionsDiffer Text Text
    | -- | Calibration supplies no ceiling for a measured native fixture.
      MissingNativeLatency Ecosystem
    | -- | A calibrated native fixture supplies no latency observation.
      MissingNativeObservation Ecosystem
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
    readings = [(ecosystem, leNative entry, leObserved entry facts, leCeiling entry calibration) | (ecosystem, entry) <- latencyEntries]
    ceilings = [(ecosystem, injected, latencyBound) | (ecosystem, _, Just injected, Just latencyBound) <- readings]
    reasons =
        [ reason
        | (ecosystem, True, injected, latencyBound) <- readings
        , reason <- [MissingNativeLatency ecosystem | isJust injected, isNothing latencyBound] <> [MissingNativeObservation ecosystem | isNothing injected, isJust latencyBound]
        ]
            <> [SettingsDiffer settings | Just settings <- [nonEmpty (differingSettings (calOperatingPoint calibration) (rfSettings facts))]]
            <> [OtherRunner (calRunner calibration) (calRunnerKind calibration) (rfRunner facts) | rfRunner facts /= readingOn (calRunnerKind calibration)]
            <> [ConditionsDiffer (calConditions calibration) (rfConditions facts) | calConditions calibration /= rfConditions facts]
            <> [LatencyAboveCeiling ecosystem injected most | (ecosystem, injected, latencyBound) <- ceilings, let most = lcCeilingMs latencyBound, injected > most]

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

-- | Fail a scheduled configuration mismatch. A network latency breach keeps it unheld without failing it.
unheldViolations :: Trigger -> Enforcement -> [Text]
unheldViolations trigger enforcement = case (trigger, enforcement) of
    (_, NotHeld reasons) | missing@(_ : _) <- mapMaybe missingNative (toList reasons) -> missing
    (Scheduled, NotHeld reasons)
        | faults@(_ : _) <- mapMaybe configurationFault (toList reasons) ->
            ["a scheduled run must be held to the success floors, and this one is not: " <> T.intercalate ", and " faults]
    _ -> []
  where
    missingNative reason = case reason of
        MissingNativeLatency _ -> Just (describeUnheld reason)
        MissingNativeObservation _ -> Just (describeUnheld reason)
        _ -> Nothing

-- A reason as a fault of the run's configuration, which latency above the ceiling is not.
configurationFault :: Unheld -> Maybe Text
configurationFault reason = case reason of
    SettingsDiffer _ -> Just (describeUnheld reason)
    OtherRunner{} -> Just (describeUnheld reason)
    ShapeNotCalibrated _ -> Just (describeUnheld reason)
    ThrashProbe -> Just (describeUnheld reason)
    LatencyAboveCeiling{} -> Nothing
    ConditionsDiffer _ _ -> Just (describeUnheld reason)
    MissingNativeLatency _ -> Just (describeUnheld reason)
    MissingNativeObservation _ -> Just (describeUnheld reason)

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
    MissingNativeObservation ecosystem -> "no native latency observation for " <> ecosystemName ecosystem
    MissingNativeLatency ecosystem -> "no independently measured native latency ceiling for " <> ecosystemName ecosystem
    ConditionsDiffer calibrated ran -> "its measurement conditions are " <> ran <> ", but the floors retain " <> calibrated
    LatencyAboveCeiling ecosystem injected most -> "its " <> ecosystemName ecosystem <> " fixture injects " <> show injected <> " ms of upstream latency, above the ceiling of " <> show most <> " ms"
  where
    readAs name = maybe (name <> " unset") ((name <> "=") <>)

-- | The report's one line on the floors: what a held run is held to, or every reason a run is not held.
describeEnforcement :: Calibration -> Enforcement -> Text
describeEnforcement calibration = \case
    Enforced _ _ ->
        "This run is held to the success floors in `"
            <> toText floorsPath
            <> "`, calibrated on "
            <> calRunner calibration
            <> " with provenance for "
            <> show (length (calRuns calibration))
            <> " runs at "
            <> T.intercalate ", " (ordNub (map runCommit (toList (calRuns calibration))))
            <> ". "
            <> calRule calibration
            <> " Measurement conditions: "
            <> calConditions calibration
            <> ". "
            <> T.intercalate " " [ecosystemName ecosystem <> " latency ceiling: " <> show (lcCeilingMs latencyBound) <> " ms." | (ecosystem, entry) <- latencyEntries, Just latencyBound <- [leCeiling entry calibration]]
            <> describeRetainedConditions calibration
    NotHeld reasons ->
        "This run is not held to the success floors in `"
            <> toText floorsPath
            <> "`: "
            <> T.intercalate ", and " (map describeUnheld (toList reasons))
            <> "."

-- | Identify unaffected rows that retain conditions from an earlier calibration.
describeRetainedConditions :: Calibration -> Text
describeRetainedConditions calibration =
    if Map.null (calRetainedConditions calibration)
        then ""
        else " Retained row conditions: " <> T.intercalate ", " [name <> "=" <> conditionVersion | (name, conditionVersion) <- Map.toList (calRetainedConditions calibration)] <> "."

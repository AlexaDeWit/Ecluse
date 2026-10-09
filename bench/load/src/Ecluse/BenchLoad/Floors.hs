-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The reviewed success floors a load run is held to. A floor is the fewest successes one
scenario may have in one pass under one pod shape. The committed file also records the runs the
floors were calibrated from and the operating point they hold for, and a run at any other
operating point is not held to them. A held run fails closed: on a scenario without a floor, and
on a floor for a count it does not check.
-}
module Ecluse.BenchLoad.Floors (
    -- * The committed floors
    Pass (..),
    passKey,
    FloorKey,
    OperatingPoint (..),
    Calibration (..),
    Floors (..),
    floorsPath,
    decodeFloors,
    loadFloors,

    -- * Holding a run to them
    Enforcement (..),
    enforce,
    FloorCheck (..),
    floorCheck,
    staleFloorViolations,
    describeEnforcement,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.BenchLoad.Pod (PodShape, parsePodShape, renderPodShape)

-- | Which of a scenario's runs a count of successes comes from.
data Pass
    = -- | The scenario at its own connections.
      Loaded
    | -- | The scenario again on a fresh proxy, with the base concurrency set to one.
      ConcurrencyOne
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The pass's key in the floors file.
passKey :: Pass -> Text
passKey = \case
    Loaded -> "loaded"
    ConcurrencyOne -> "concurrencyOne"

-- | A count the floors cover: a scenario's ecosystem-qualified key, and the pass.
type FloorKey = (Text, Pass)

{- | Every setting a run can vary. 'Nothing' leaves a bound to the proxy's computed default, and
for the scenarios it means all of them.
-}
data OperatingPoint = OperatingPoint
    { opDurationSeconds :: Int
    , opConcurrency :: Int
    , opPayloadBytes :: Int
    , opUpstreamLatencyMs :: Int
    -- ^ The configured latency. The npm fixture injects the round-trip time it probes instead.
    , opCacheMaxEntries :: Int
    , opWorkingSet :: Int
    , opServeMaxInFlight :: Maybe Int
    , opPublicConnectionsPerHost :: Maybe Int
    , opPrivateConnectionsPerHost :: Maybe Int
    , opScenarios :: Maybe [Text]
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
        ]
    , not agrees
    ]
  where
    same :: (Eq a) => (OperatingPoint -> a) -> Bool
    same setting = setting calibrated == setting ran

-- | Where the floors were measured, the rule that set them, and the operating point they hold for.
data Calibration = Calibration
    { calRule :: Text
    , calRunner :: Text
    , calCommit :: Text
    , calRuns :: NonEmpty Text
    , calOperatingPoint :: OperatingPoint
    }
    deriving stock (Eq, Show)

instance FromJSON Calibration where
    parseJSON = withObject "Calibration" $ \o -> do
        calibration <- Calibration <$> o .: "rule" <*> o .: "runner" <*> o .: "commit" <*> o .: "runs" <*> o .: "operatingPoint"
        when (any T.null [calRule calibration, calRunner calibration, calCommit calibration]) $
            fail "the calibration must state its rule, runner, and commit"
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

-- | Whether a run is held to the floors. Only a run at the calibrated operating point is.
data Enforcement
    = -- | Held to these floors, the ones for its pod shape.
      Enforced (Map FloorKey Int)
    | -- | Not held: the run differs from the calibrated operating point in these settings.
      OffCalibration (NonEmpty Text)
    deriving stock (Eq, Show)

-- | Decide the enforcement for a run at this operating point under this pod shape.
enforce :: Floors -> OperatingPoint -> PodShape -> Enforcement
enforce floors ran shape =
    maybe (Enforced (Map.findWithDefault Map.empty shape (floorsByShape floors))) OffCalibration $
        nonEmpty (differingSettings (calOperatingPoint (floorsCalibration floors)) ran)

-- | What one scenario's successes are held to in one pass.
data FloorCheck
    = -- | The run is off the calibrated operating point, so no floor applies.
      Unchecked
    | -- | The run is held to the floors and they have none for this count, which fails it.
      NoFloor
    | -- | Every load of the scenario must reach this many successes.
      AtLeast Int
    deriving stock (Eq, Show)

-- | The check for one count of a run.
floorCheck :: Enforcement -> FloorKey -> FloorCheck
floorCheck enforcement key = case enforcement of
    OffCalibration _ -> Unchecked
    Enforced floors -> maybe NoFloor AtLeast (Map.lookup key floors)

-- | One line per floor a held run has for a count outside the ones it checks. A stale floor fails the run.
staleFloorViolations :: PodShape -> Enforcement -> Set FloorKey -> [Text]
staleFloorViolations shape enforcement checked = case enforcement of
    OffCalibration _ -> []
    Enforced floors ->
        [ toText floorsPath <> ": the " <> passKey whichPass <> " floor for " <> scenario <> " under " <> renderPodShape shape <> " names no count this run checks"
        | (scenario, whichPass) <- Set.toList (Map.keysSet floors `Set.difference` checked)
        ]

-- | The report's one line on the floors: what a held run is held to, or why a run is not held.
describeEnforcement :: Calibration -> Enforcement -> Text
describeEnforcement calibration = \case
    Enforced _ ->
        "This run is held to the success floors in `"
            <> toText floorsPath
            <> "`, calibrated on "
            <> calRunner calibration
            <> " at "
            <> calCommit calibration
            <> " from "
            <> show (length (calRuns calibration))
            <> " runs. "
            <> calRule calibration
    OffCalibration settings ->
        "This run is not held to the success floors in `"
            <> toText floorsPath
            <> "`: it differs from the operating point they were calibrated at in "
            <> T.intercalate ", " (toList settings)
            <> "."

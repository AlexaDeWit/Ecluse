-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A committed load sweep under @bench/load/sweeps/@: its arms, each a pod shape with the proxy
settings it pins and the boot values it must show, and its jobs. A job runs the anchor arm, its
test arms, and the anchor again, so one runner measures them all. @scripts/load-sweep.sh@ runs a
job's arms in order, and @bench-report sweep@ sets each arm beside the job's anchors.
-}
module Ecluse.BenchLoad.Sweep (
    Sweep (..),
    Arm (..),
    BootValues (..),
    Phase (..),
    SweepJob (..),
    sweepPath,
    decodeSweep,
    loadSweep,
    sweepJob,
) where

import Data.Aeson (FromJSON (parseJSON), Object, eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum, isAscii)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.Floors (PassSelection, parsePassSelection)
import Ecluse.BenchLoad.Pod (PodShape, ProxyTuning (..), mib, parsePodShape)

-- | One sweep: what every arm shares, the arms by name, and the phases of jobs.
data Sweep = Sweep
    { swName :: Text
    , swUpstreamLatencyMs :: Int
    -- ^ The fixed latency each selected ecosystem injects, with the round-trip probe off.
    , swPasses :: PassSelection
    , swScenarios :: NonEmpty Text
    , swScored :: [Text]
    -- ^ The scenarios whose anchor ratios make an arm's score.
    , swAnchor :: Text
    , swArms :: Map Text Arm
    , swPhases :: NonEmpty Phase
    }
    deriving stock (Eq, Show)

-- | One configuration the sweep measures.
data Arm = Arm
    { armShape :: PodShape
    , armTuning :: ProxyTuning
    , armServeMaxInFlight :: Maybe Int
    , armPodMemoryBytes :: Int
    -- ^ The memory the arm's peak is judged against: its limit, its request, or a notional pod.
    , armReferenceShape :: PodShape
    -- ^ The scheduled shape whose loaded floors the arm's counts are set beside.
    , armExpected :: BootValues
    }
    deriving stock (Eq, Show)

-- | The posture an arm's proxy must report. A different one voids the arm.
data BootValues = BootValues
    { bvCapabilities :: Int
    , bvAllocAreaMib :: Int
    , bvMaxHeapMib :: Maybe Int
    -- ^ 'Nothing' for an unbounded heap.
    , bvAdmission :: Int
    }
    deriving stock (Eq, Show)

-- | A group of jobs that answers one question. A phase run only on request is left out of a default selection.
data Phase = Phase
    { phaseName :: Text
    , phaseOnRequest :: Bool
    , phaseJobs :: NonEmpty SweepJob
    }
    deriving stock (Eq, Show)

-- | The arms one runner measures in order, opening and closing with the anchor.
data SweepJob = SweepJob
    { jobName :: Text
    , jobArms :: NonEmpty Text
    }
    deriving stock (Eq, Show)

instance FromJSON Sweep where
    parseJSON = withObject "Sweep" $ \o -> do
        sweep <-
            Sweep
                <$> o .: "name"
                <*> o .: "upstreamLatencyMs"
                <*> (either (fail . toString) pure . parsePassSelection =<< o .: "passes")
                <*> o .: "scenarios"
                <*> o .: "scored"
                <*> o .: "anchor"
                <*> (traverse armOf =<< o .: "arms")
                <*> (traverse phaseOf =<< o .: "phases")
        either (fail . toString) pure (checked sweep)

armOf :: Object -> Parser Arm
armOf o =
    Arm
        <$> (shapeOf =<< o .: "shape")
        <*> tuningOf
        <*> o .:? "serveMaxInFlight"
        <*> ((* mib) <$> o .: "podMemoryMib")
        <*> (shapeOf =<< o .: "referenceShape")
        <*> (bootValuesOf =<< o .: "expect")
  where
    tuningOf = ProxyTuning <$> o .:? "cpus" <*> o .:? "cores" <*> o .:? "maxHeapBytes" <*> o .:? "allocAreaMib"
    shapeOf = either (fail . toString) pure . parsePodShape
    bootValuesOf expected = BootValues <$> expected .: "capabilities" <*> expected .: "allocAreaMib" <*> expected .:? "maxHeapMib" <*> expected .: "admission"

phaseOf :: Object -> Parser Phase
phaseOf o = Phase <$> o .: "phase" <*> o .:? "onRequest" .!= False <*> (traverse jobOf =<< o .: "jobs")
  where
    jobOf = withObject "SweepJob" $ \job -> SweepJob <$> job .: "job" <*> job .: "arms"

-- Every name a job uses resolves, every job is anchored at both ends, and names are unique and path-safe.
checked :: Sweep -> Either Text Sweep
checked sweep
    | Map.notMember (swAnchor sweep) (swArms sweep) = Left ("the anchor " <> swAnchor sweep <> " names no arm")
    | (name : _) <- filter (not . pathSafe) (Map.keys (swArms sweep) <> map jobName jobs) = Left ("the name " <> show name <> " is not ASCII letters, digits, and hyphens")
    | (unknown : _) <- [arm | job <- jobs, arm <- toList (jobArms job), Map.notMember arm (swArms sweep)] = Left ("a job names no arm " <> unknown)
    | (job : _) <- filter (not . anchored) jobs = Left ("job " <> jobName job <> " must run the anchor first and last, with a test arm between")
    | (name : _) <- duplicates (map phaseName (toList (swPhases sweep)) <> map jobName jobs) = Left ("the name " <> name <> " is used twice")
    | (scenario : _) <- filter (`notElem` swScenarios sweep) (swScored sweep) = Left ("the scored scenario " <> scenario <> " is not one the sweep runs")
    | otherwise = Right sweep
  where
    jobs = concatMap (toList . phaseJobs) (swPhases sweep)
    anchored job = length (jobArms job) >= 3 && NE.head (jobArms job) == swAnchor sweep && NE.last (jobArms job) == swAnchor sweep
    duplicates names = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(name, 1) | name <- names]))
    -- The runner names a directory and an artifact after each.
    pathSafe name = not (T.null name) && T.all (\c -> (isAscii c && isAlphaNum c) || c == '-') name

-- | The committed file of a sweep, relative to the package root.
sweepPath :: Text -> FilePath
sweepPath name = "bench/load/sweeps/" <> toString name <> ".json"

-- | Decode a sweep, refusing an unknown arm, a job not anchored at both ends, and a name used twice.
decodeSweep :: LByteString -> Either Text Sweep
decodeSweep = first toText . eitherDecode

-- | Read the committed sweep of this name, refusing a file whose own name differs.
loadSweep :: Text -> IO (Either Text Sweep)
loadSweep name = do
    decoded <- decodeSweep <$> readFileLBS (sweepPath name)
    pure $ case decoded of
        Right sweep | swName sweep /= name -> Left (toText (sweepPath name) <> " names itself " <> swName sweep)
        other -> first ((toText (sweepPath name) <> " did not decode: ") <>) other

-- | The job of this name in any phase.
sweepJob :: Sweep -> Text -> Maybe SweepJob
sweepJob sweep name = find ((== name) . jobName) (concatMap (toList . phaseJobs) (swPhases sweep))

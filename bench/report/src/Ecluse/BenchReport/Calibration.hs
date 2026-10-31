-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Write the success floors from three calibration runs of the Load test workflow. Each selected cell
that is not flagged report-only takes the floor fraction of the median of its three first-attempt
counts. The runs must be dispatches of one commit, every run must be held to the floors, and every
count must come from a first attempt that ran clean.
-}
module Ecluse.BenchReport.Calibration (
    RunObservations (..),
    FirstAttempt (..),
    calibrationRuns,
    calibrate,
    renderCalibration,
    readRunsDirectory,
) where

import Data.Aeson (Value, eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

import Ecluse.BenchLoad.Floors (
    Calibration (calFloorFraction, calRuns),
    CalibrationRun (..),
    Cell (Floored, ReportOnlyCell, Uncalibrated),
    FloorKey,
    Floors (..),
    floorFromMedian,
    medianCount,
    passKey,
    passNamed,
 )
import Ecluse.BenchLoad.Pod (PodShape, parsePodShape, renderPodShape)
import Ecluse.Report.Markdown (cells)

-- | How many calibration runs a floor comes from.
calibrationRuns :: Int
calibrationRuns = 3

-- | A scenario pass's first attempt: its count of successes, and whether it ran clean.
data FirstAttempt = FirstAttempt
    { faSuccesses :: Maybe Int
    , faClean :: Bool
    }
    deriving stock (Eq, Show)

-- | One calibration run: its record, and for each pod shape whether it was held and its first attempts.
data RunObservations = RunObservations
    { roRun :: CalibrationRun
    , roShapes :: Map PodShape (Bool, Map FloorKey FirstAttempt)
    }
    deriving stock (Eq, Show)

{- | The floors with each selected cell set from the runs, or every reason they cannot be. 'Nothing'
selects every scenario. A cell flagged report-only keeps its flag.
-}
calibrate :: Floors -> [RunObservations] -> Maybe [Text] -> Either [Text] Floors
calibrate floors runs selection = do
    ids <- case nonEmpty (ordNub (map (runId . roRun) runs)) of
        Just distinct | length distinct == calibrationRuns, length runs == calibrationRuns -> Right distinct
        _ -> Left ["a calibration takes " <> show calibrationRuns <> " distinct runs, not " <> show (length runs)]
    unless (null provenance) (Left provenance)
    unless (null unknown) (Left ["no scenario in the floors is named " <> name | name <- unknown])
    counts <- first ordNub (everyProblem [(target,) <$> everyProblem (map (countIn shape key) runs) | target@(shape, key) <- targets])
    let fraction = calFloorFraction (floorsCalibration floors)
        fresh = Map.fromList [(target, Floored (floorFromMedian fraction (medianCount found)) (medianCount found) ids) | (target, Just found) <- map (second nonEmpty) counts]
        byShape = Map.mapWithKey (\shape -> Map.mapWithKey (\key cell -> Map.findWithDefault cell (shape, key) fresh)) (floorsByShape floors)
        named = [run | cellsOf <- Map.elems byShape, Floored _ _ from <- Map.elems cellsOf, run <- toList from]
        listed = Map.fromList [(runId run, run) | run <- calRuns (floorsCalibration floors) <> map roRun runs]
    pure floors{floorsCalibration = (floorsCalibration floors){calRuns = [run | (runNumber, run) <- Map.toList listed, runNumber `elem` named]}, floorsByShape = byShape}
  where
    provenance =
        [runName observed <> " was triggered by " <> event <> ", not workflow_dispatch" | observed <- runs, let event = runEvent (roRun observed), event /= "workflow_dispatch"]
            <> ["the runs measured different commits: " <> T.intercalate ", " [runName observed <> " at " <> runCommit (roRun observed) | observed <- runs] | length (ordNub (map (runCommit . roRun) runs)) > 1]
    known = ordNub [scenario | cellsOf <- Map.elems (floorsByShape floors), (scenario, _) <- Map.keys cellsOf]
    unknown = maybe [] (filter (`notElem` known)) selection
    targets = [(shape, key) | (shape, cellsOf) <- Map.toList (floorsByShape floors), (key@(scenario, _), cell) <- Map.toList cellsOf, maybe True (scenario `elem`) selection, notFlagged cell]
    notFlagged = \case
        ReportOnlyCell _ -> False
        _ -> True
    countIn :: PodShape -> FloorKey -> RunObservations -> Either [Text] Int
    countIn shape key@(scenario, whichPass) observed = case Map.lookup shape (roShapes observed) of
        Nothing -> Left [runName observed <> " has no load observations under " <> renderPodShape shape]
        Just (False, _) -> Left [runName observed <> " under " <> renderPodShape shape <> " was not held to the floors"]
        Just (True, attempts) -> case Map.lookup key attempts of
            Just (FirstAttempt (Just count) True) | count >= 1 -> Right count
            Just _ -> Left [runName observed <> " under " <> renderPodShape shape <> ": the first attempt at " <> scenario <> " " <> passKey whichPass <> " did not run clean"]
            Nothing -> Left [runName observed <> " under " <> renderPodShape shape <> " did not measure " <> scenario <> " " <> passKey whichPass]
    runName observed = "run " <> show (runId (roRun observed))

-- Every result, or every problem of every failure, so one pass of the command names them all.
everyProblem :: [Either [Text] a] -> Either [Text] [a]
everyProblem results = case lefts results of
    [] -> Right (rights results)
    problems -> Left (concat problems)

-- | The cells a calibration changed, before and after, as a Markdown table for the pull request.
renderCalibration :: Floors -> Floors -> Text
renderCalibration before after =
    T.unlines $
        ["## Success floors from calibration runs " <> T.intercalate ", " [show (runId run) | run <- calRuns (floorsCalibration after), runId run `notElem` map runId (calRuns (floorsCalibration before))], ""]
            <> case changed of
                [] -> ["No cell changed."]
                rows -> ["| pod shape | scenario | pass | floor before | floor after | median |", "| --- | --- | --- | --: | --: | --: |"] <> rows
  where
    changed =
        [ cells [renderPodShape shape, scenario, passKey whichPass, floorText old, floorText new, maybe "n/a" show (medianOf new)]
        | (shape, cellsOf) <- Map.toList (floorsByShape after)
        , ((scenario, whichPass), new) <- Map.toList cellsOf
        , let old = Map.findWithDefault Uncalibrated (scenario, whichPass) (Map.findWithDefault mempty shape (floorsByShape before))
        , old /= new
        ]
    floorText = \case
        Floored least _ _ -> show least
        Uncalibrated -> "none"
        ReportOnlyCell _ -> "report-only"
    medianOf = \case
        Floored _ median _ -> Just median
        _ -> Nothing

{- | Read every run under a directory: a subdirectory per run, holding the run's @run.json@ from
@gh run view@ and its downloaded @load-observations.json@ files, one per pod shape.
-}
readRunsDirectory :: FilePath -> IO (Either [Text] [RunObservations])
readRunsDirectory root = do
    entries <- sort <$> listDirectory root
    runDirs <- filterM (doesDirectoryExist . (root </>)) entries
    everyProblem <$> traverse (readRun . (root </>)) runDirs

readRun :: FilePath -> IO (Either [Text] RunObservations)
readRun dir = do
    recordFile <- doesFileExist (dir </> "run.json")
    if not recordFile
        then pure (Left [toText dir <> " has no run.json"])
        else do
            record <- eitherDecodeFileStrict (dir </> "run.json")
            files <- observationFiles dir
            shapes <- traverse readObservations files
            pure $ do
                run <- first (\problem -> [toText dir <> "/run.json did not decode: " <> toText problem]) (runRecord =<< first toString record)
                parsed <- everyProblem shapes
                pure (RunObservations run (Map.fromList parsed))
  where
    runRecord :: Value -> Either String CalibrationRun
    runRecord = parseEither . withObject "run" $ \o ->
        CalibrationRun <$> o .: "databaseId" <*> o .: "url" <*> o .: "headSha" <*> o .: "event"

-- Every load-observations.json below a directory.
observationFiles :: FilePath -> IO [FilePath]
observationFiles dir = do
    entries <- sort <$> listDirectory dir
    fmap concat . forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir then observationFiles path else pure [path | takeFileName path == "load-observations.json"]

-- One job's observations: its pod shape, whether it was held, and each pass's first attempt.
readObservations :: FilePath -> IO (Either [Text] (PodShape, (Bool, Map FloorKey FirstAttempt)))
readObservations path = first (\problem -> [toText path <> " did not decode: " <> toText problem]) . (>>= parseEither observations) <$> eitherDecodeFileStrict path
  where
    observations :: Value -> Parser (PodShape, (Bool, Map FloorKey FirstAttempt))
    observations = withObject "load observations" $ \o -> do
        conditions <- o .: "conditions"
        shapeName <- conditions .: "podShape"
        shape <- either (fail . toString) pure (parsePodShape shapeName)
        held <- (== ("held" :: Text)) <$> conditions .: "floorEnforcement"
        passes <- traverse firstAttempt =<< o .: "observations"
        pure (shape, (held, Map.fromList passes))
    firstAttempt :: Value -> Parser (FloorKey, FirstAttempt)
    firstAttempt = withObject "observation" $ \o -> do
        key <- floorKeyOf =<< o .: "id"
        attempts <- o .: "attempts"
        case attempts of
            attempt : _ -> (key,) <$> parseAttempt attempt
            [] -> fail "an observation without attempts"
    parseAttempt :: Value -> Parser FirstAttempt
    parseAttempt = withObject "attempt" $ \o -> FirstAttempt <$> o .: "successes" <*> o .: "clean"

-- An observation ID, @<ecosystem>/<scenario>/<pass>@, as the floors key it.
floorKeyOf :: Text -> Parser FloorKey
floorKeyOf identifier = case passNamed suffix of
    Just whichPass | not (T.null scenario) -> pure (scenario, whichPass)
    _ -> fail ("an observation ID without a known pass: " <> toString identifier)
  where
    (prefix, suffix) = T.breakOnEnd "/" identifier
    scenario = T.dropEnd 1 prefix

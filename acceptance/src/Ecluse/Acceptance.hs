-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Allocation budgets and reports for performance acceptance.
The captures run holds each leg over the committed captures to its calibrated allocation. The live
run reports the same legs over live registry documents, with no budget.
-}
module Ecluse.Acceptance (
    -- * Legs and criteria
    Leg (..),
    legKey,
    Calibration (..),
    Criteria (..),
    criteriaPath,
    loadCriteria,
    decodeCriteria,
    budgetBytes,
    hostArch,

    -- * Measurements
    Measurement (..),
    Sample (..),
    PackageOutcome (..),
    OperatingPoint (..),

    -- * The captures run
    Verdict (..),
    AssessedLeg (..),
    Row (..),
    CapturesSection (..),
    CapturesReport (..),
    assessCaptures,
    capturesProblems,
    capturesExitCode,
    renderCapturesReport,

    -- * The live run
    liveExitCode,
    renderLiveReport,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`. The Generic-derived one is used here.
import Prelude hiding (universe)

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Universe.Class (Universe (universe))
import Data.Universe.Generic (universeGeneric)
import Numeric (showFFloat)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Info qualified as Info

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)

-- | One measured operation over a package's document.
data Leg
    = -- | Decode, projection, rules, assembly, and serialisation of the whole document.
      FullDocument
    | -- | Selective projection of one version, forcing its artifact digests.
      SingleVersion
    | -- | The full leg under the shipped policy, with the corpus advisories served.
      FullShippedAdvisories
    | -- | The full leg under the shipped policy and both advisory denies, with the corpus advisories served.
      FullAllAdvisoryRules
    deriving stock (Eq, Ord, Show, Generic)

instance Universe Leg where universe = universeGeneric

-- | The leg's key in the criteria file and its name in the reports.
legKey :: Leg -> Text
legKey = \case
    FullDocument -> "full"
    SingleVersion -> "singleVersion"
    FullShippedAdvisories -> "fullShippedAdvisories"
    FullAllAdvisoryRules -> "fullAllAdvisoryRules"

parseLeg :: Text -> Maybe Leg
parseLeg key = find ((== key) . legKey) universe

-- | Where the calibrated figures were measured, and the margin a budget adds to them.
data Calibration = Calibration
    { calArch :: Text
    -- ^ The CPU architecture, as 'Info.arch' names it.
    , calRunner :: Text
    , calCommit :: Text
    , calRuns :: NonEmpty Text
    , calMarginPercent :: Int64
    -- ^ How far past its calibrated figure a leg may allocate, in percent.
    }
    deriving stock (Eq, Show)

instance FromJSON Calibration where
    parseJSON = withObject "Calibration" $ \o -> do
        calibration <- Calibration <$> o .: "arch" <*> o .: "runner" <*> o .: "commit" <*> o .: "runs" <*> o .: "marginPercent"
        when (any T.null [calArch calibration, calRunner calibration, calCommit calibration]) $
            fail "the calibration must name its architecture, runner, and commit"
        when (calMarginPercent calibration < 0) $
            fail "the calibration margin must not be negative"
        pure calibration

-- | The calibration, and each package's calibrated allocation in bytes per leg, by ecosystem.
data Criteria = Criteria
    { critCalibration :: Calibration
    , critAllocatedBytes :: Map Ecosystem (Map Text (Map Leg Int64))
    }
    deriving stock (Eq, Show)

instance FromJSON Criteria where
    parseJSON = withObject "Criteria" $ \o -> do
        calibration <- o .: "calibration"
        sections <- o .: "allocatedBytes"
        Criteria calibration . Map.fromList <$> traverse parseSection (Map.toList sections)
      where
        parseSection :: (Text, Map Text (Map Text Int64)) -> Parser (Ecosystem, Map Text (Map Leg Int64))
        parseSection (name, packages) = case parseEcosystem name of
            Nothing -> fail ("unknown ecosystem in the criteria: " <> toString name)
            Just eco -> (eco,) <$> traverse parseLegs packages
        parseLegs :: Map Text Int64 -> Parser (Map Leg Int64)
        parseLegs legs = Map.fromList <$> traverse parseFigure (Map.toList legs)
        parseFigure :: (Text, Int64) -> Parser (Leg, Int64)
        parseFigure (key, bytes) = case parseLeg key of
            Nothing -> fail ("unknown leg in the criteria: " <> toString key)
            Just leg
                | bytes > 0 -> pure (leg, bytes)
                | otherwise -> fail ("a calibrated allocation must be positive: " <> toString key)

-- | The committed criteria's path, relative to the package root the harness runs from.
criteriaPath :: FilePath
criteriaPath = "acceptance/criteria.json"

-- | Decode 'Criteria' from raw JSON bytes.
decodeCriteria :: LByteString -> Either String Criteria
decodeCriteria = eitherDecode

-- | Read and decode the committed criteria from 'criteriaPath'.
loadCriteria :: IO Criteria
loadCriteria = do
    raw <- readFileLBS criteriaPath
    either (\e -> fail (criteriaPath <> " did not decode: " <> e)) pure (decodeCriteria raw)

-- | The most a leg may allocate: its calibrated figure plus the margin, rounded up to a byte.
budgetBytes :: Calibration -> Int64 -> Int64
budgetBytes calibration calibrated = calibrated + (calibrated * calMarginPercent calibration + 99) `div` 100

-- | The CPU architecture this process runs on, named as 'calArch' names it.
hostArch :: Text
hostArch = toText Info.arch

-- | One leg's figures, each the median over the leg's passes.
data Measurement = Measurement
    { measuredBytes :: Int64
    -- ^ Bytes the measuring thread allocated.
    , measuredMs :: Double
    -- ^ Wall-clock time, which no budget applies to.
    }
    deriving stock (Eq, Show)

-- | One package's measured legs.
data Sample = Sample
    { sampleName :: Text
    , sampleVersions :: Int
    , sampleUpstreamMs :: Maybe Double
    -- ^ The fetch time of a live document. A committed capture has none.
    , sampleLegs :: [(Leg, Measurement)]
    }
    deriving stock (Eq, Show)

-- | A package's result in a run.
data PackageOutcome
    = -- | Every leg ran.
      Measured Sample
    | -- | The proxy's code or limits refused the document: the package name and the reason.
      Failed Text Text
    | -- | The registry did not deliver the document: the package name and the reason.
      Unavailable Text Text
    deriving stock (Eq, Show)

-- | How a run measured: passes per leg, and the RTS options it ran under.
data OperatingPoint = OperatingPoint
    { opPasses :: Int
    , opCapabilities :: Int
    , opAllocationAreaBytes :: Int
    }
    deriving stock (Eq, Show)

-- | A leg's standing against its budget.
data Verdict
    = -- | At or under the budget.
      Within
    | -- | Over the budget by this many bytes.
      Breached Int64
    | -- | The criteria hold no calibrated figure for the leg.
      NoBudget
    | -- | The budgets were calibrated on another architecture.
      Uncalibrated
    deriving stock (Eq, Show)

-- | One measured leg of a captures run, with its calibrated figure and verdict.
data AssessedLeg = AssessedLeg
    { alPackage :: Text
    , alVersions :: Int
    , alLeg :: Leg
    , alMeasurement :: Measurement
    , alCalibrated :: Maybe Int64
    , alVerdict :: Verdict
    }
    deriving stock (Eq, Show)

-- | One row of a captures report: an assessed leg, or a package whose legs did not run.
data Row
    = Assessed AssessedLeg
    | FailedPackage Text Text
    deriving stock (Eq, Show)

-- | One ecosystem's rows, in catalogue order.
data CapturesSection = CapturesSection
    { sectionEcosystem :: Ecosystem
    , sectionRows :: [Row]
    }
    deriving stock (Eq, Show)

-- | A captures run held to the criteria.
data CapturesReport = CapturesReport
    { reportCalibration :: Calibration
    , reportSections :: [CapturesSection]
    , reportUnmeasured :: [(Ecosystem, Text)]
    -- ^ Packages the criteria calibrate that the run did not measure.
    }
    deriving stock (Eq, Show)

-- | Hold each measured leg to its budget. A captures run never fetches, so an unavailable package fails.
assessCaptures :: Criteria -> [(Ecosystem, [PackageOutcome])] -> CapturesReport
assessCaptures criteria runs =
    CapturesReport
        { reportCalibration = calibration
        , reportSections = [CapturesSection eco (concatMap (rows eco) outcomes) | (eco, outcomes) <- runs]
        , reportUnmeasured =
            [ (eco, name)
            | (eco, packages) <- Map.toList (critAllocatedBytes criteria)
            , name <- Map.keys packages
            , name `notElem` [outcomeName outcome | (measured, outcomes) <- runs, measured == eco, outcome <- outcomes]
            ]
        }
  where
    calibration = critCalibration criteria
    rows eco = \case
        Measured sample -> [Assessed (assessLeg eco sample leg measurement) | (leg, measurement) <- sampleLegs sample]
        Failed name reason -> [FailedPackage name reason]
        Unavailable name reason -> [FailedPackage name ("unavailable: " <> reason)]
    assessLeg eco sample leg measurement =
        let calibrated = Map.lookup eco (critAllocatedBytes criteria) >>= Map.lookup (sampleName sample) >>= Map.lookup leg
         in AssessedLeg (sampleName sample) (sampleVersions sample) leg measurement calibrated (verdict calibrated (measuredBytes measurement))
    verdict calibrated bytes
        | calArch calibration /= hostArch = Uncalibrated
        | otherwise = case calibrated of
            Nothing -> NoBudget
            Just figure
                | bytes > budgetBytes calibration figure -> Breached (bytes - budgetBytes calibration figure)
                | otherwise -> Within

outcomeName :: PackageOutcome -> Text
outcomeName = \case
    Measured sample -> sampleName sample
    Failed name _ -> name
    Unavailable name _ -> name

-- | Why a captures run fails. The run passes only when this is empty.
capturesProblems :: CapturesReport -> [Text]
capturesProblems report =
    [ "the budgets were calibrated on " <> calArch calibration <> " and this run is on " <> hostArch <> ", so no leg is assessed"
    | calArch calibration /= hostArch
    ]
        <> ["no package was measured" | null rows]
        <> countOf "leg(s) over budget" [() | Assessed leg <- rows, Breached _ <- [alVerdict leg]]
        <> countOf "leg(s) without a budget" [() | Assessed leg <- rows, NoBudget <- [alVerdict leg]]
        <> countOf "package(s) failed" [() | FailedPackage _ _ <- rows]
        <> countOf "calibrated package(s) not measured" (reportUnmeasured report)
  where
    calibration = reportCalibration report
    rows = concatMap sectionRows (reportSections report)
    countOf label items = [show (length items) <> " " <> label | not (null items)]

-- | Exit 0 only when 'capturesProblems' finds nothing.
capturesExitCode :: CapturesReport -> ExitCode
capturesExitCode report
    | null (capturesProblems report) = ExitSuccess
    | otherwise = ExitFailure 1

-- | The live run fails when the proxy refused a document. An unavailable registry leaves it incomplete.
liveExitCode :: [(Ecosystem, [PackageOutcome])] -> ExitCode
liveExitCode runs
    | any (any isFailed . snd) runs = ExitFailure 1
    | otherwise = ExitSuccess

isFailed :: PackageOutcome -> Bool
isFailed = \case
    Failed _ _ -> True
    _ -> False

-- | Render the captures run: the result, how it measured, and one table per ecosystem.
renderCapturesReport :: OperatingPoint -> CapturesReport -> Text
renderCapturesReport op report =
    T.unlines $
        [ "## Performance acceptance: allocation over the committed captures"
        , ""
        , "Result: " <> result
        , ""
        ]
            <> operatingLines op
            <> [ "- Captures: bench/corpus, each evaluated at its capture time in bench/corpus/pins.json."
               , "- Budgets: acceptance/criteria.json. Each budget is the figure measured on "
                    <> calRunner calibration
                    <> " ("
                    <> calArch calibration
                    <> ") at "
                    <> calCommit calibration
                    <> ", plus "
                    <> show (calMarginPercent calibration)
                    <> "%. Runs: "
                    <> T.intercalate ", " (toList (calRuns calibration))
                    <> "."
               , ""
               ]
            <> concatMap (capturesSection calibration) (reportSections report)
            <> unmeasuredLines
  where
    calibration = reportCalibration report
    result = case capturesProblems report of
        [] -> "PASS: every leg is within its allocation budget"
        problems -> "FAIL: " <> T.intercalate ", " problems
    unmeasuredLines = case reportUnmeasured report of
        [] -> []
        unmeasured -> ["Calibrated packages this run did not measure: " <> T.intercalate ", " [ecosystemName eco <> " " <> name | (eco, name) <- unmeasured] <> ".", ""]

capturesSection :: Calibration -> CapturesSection -> [Text]
capturesSection calibration section =
    [ "### " <> ecosystemName (sectionEcosystem section)
    , ""
    , "| Package | Versions | Leg | Allocated (bytes) | Calibrated (bytes) | Change | Budget (bytes) | Time (ms) | Verdict |"
    , "|---|--:|---|--:|--:|--:|--:|--:|---|"
    ]
        <> map row (sectionRows section)
        <> [""]
  where
    row = \case
        Assessed leg ->
            let bytes = measuredBytes (alMeasurement leg)
             in cells
                    [ alPackage leg
                    , show (alVersions leg)
                    , legKey (alLeg leg)
                    , show bytes
                    , maybe "--" show (alCalibrated leg)
                    , maybe "--" (change bytes) (alCalibrated leg)
                    , maybe "--" (show . budgetBytes calibration) (alCalibrated leg)
                    , fmt 3 (measuredMs (alMeasurement leg))
                    , renderVerdict (alVerdict leg)
                    ]
        FailedPackage name reason -> cells [name, "--", "--", "--", "--", "--", "--", "--", "FAILED: " <> reason]
    change bytes calibrated =
        let percent = (fromIntegral bytes / fromIntegral calibrated - 1) * 100 :: Double
         in (if percent >= 0 then "+" else "") <> fmt 1 percent <> "%"

renderVerdict :: Verdict -> Text
renderVerdict = \case
    Within -> "within"
    Breached over -> "OVER by " <> show over <> " bytes"
    NoBudget -> "NO BUDGET"
    Uncalibrated -> "uncalibrated"

-- | Render the live run: the result, how it measured, and one table per ecosystem.
renderLiveReport :: OperatingPoint -> [(Ecosystem, [PackageOutcome])] -> Text
renderLiveReport op runs =
    T.unlines $
        [ "## Performance acceptance: live registry documents"
        , ""
        , "Result: " <> result
        , ""
        ]
            <> operatingLines op
            <> [ "- Budgets: none. A refusal by the proxy's own code or limits fails the run. An unavailable registry leaves it incomplete."
               , ""
               ]
            <> concatMap liveSection runs
  where
    outcomes = concatMap snd runs
    failed = length (filter isFailed outcomes)
    unavailable = length [() | Unavailable _ _ <- outcomes]
    result
        | failed > 0 = "FAILED: the proxy refused or could not process " <> show failed <> " package(s)"
        | unavailable > 0 = "incomplete: " <> show unavailable <> " package(s) unavailable"
        | otherwise = "complete"

liveSection :: (Ecosystem, [PackageOutcome]) -> [Text]
liveSection (eco, outcomes) =
    [ "### " <> ecosystemName eco
    , ""
    , "| Package | Versions | Upstream (ms) | Leg | Allocated (bytes) | Time (ms) | Result |"
    , "|---|--:|--:|---|--:|--:|---|"
    ]
        <> concatMap rows outcomes
        <> [""]
  where
    rows = \case
        Measured sample ->
            [ cells [sampleName sample, show (sampleVersions sample), maybe "--" (fmt 3) (sampleUpstreamMs sample), legKey leg, show (measuredBytes measurement), fmt 3 (measuredMs measurement), "measured"]
            | (leg, measurement) <- sampleLegs sample
            ]
        Failed name reason -> [cells [name, "--", "--", "--", "--", "--", "FAILED: " <> reason]]
        Unavailable name reason -> [cells [name, "--", "--", "--", "--", "--", "unavailable: " <> reason]]

operatingLines :: OperatingPoint -> [Text]
operatingLines op =
    [ "- Figures: the median of " <> show (opPasses op) <> " passes per leg."
    , "- Allocation: the bytes the measuring thread allocated, read from GHC's per-thread allocation counter."
    , "- Time: wall-clock, for information only."
    , "- RTS: -N" <> show (opCapabilities op) <> " -A" <> show (opAllocationAreaBytes op `div` (1024 * 1024)) <> "m, read from the running RTS."
    , "- Legs: `full` decodes, projects, applies the rules, assembles, and serialises the whole document. `singleVersion` projects one version selectively and forces its artifact digests."
    , "- Advisory legs: `fullShippedAdvisories` and `fullAllAdvisoryRules` repeat `full` with the corpus advisories in bench/corpus/advisories served, under the shipped policy and under the shipped policy with both advisory denies."
    ]

cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

fmt :: Int -> Double -> Text
fmt places x = toText (showFFloat (Just places) x "")

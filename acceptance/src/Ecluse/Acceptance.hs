-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Budgets and reports for live registry performance acceptance.
Each ecosystem keeps its own package budgets, while either processing leg can fail the run.
-}
module Ecluse.Acceptance (
    -- * Acceptance criteria
    Criteria (..),
    CriteriaCatalogue (..),
    criteriaPath,
    loadCriteria,
    decodeCriteria,
    budgetFor,
    singleVersionBudgetFor,

    -- * Measurements and verdicts
    Sample (..),
    Verdict (..),
    Assessment (..),
    PackageOutcome (..),
    Report (..),
    evaluate,
    reportBreached,
    reportExitCode,

    -- * Rendering
    OperatingPoint (..),
    headroom,
    watchFraction,
    renderReport,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName, parseEcosystem)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

-- | Positive overhead budgets in milliseconds, scoped to one ecosystem.
data Criteria = Criteria
    { critDefaultBudgetMs :: Double
    -- ^ The full-document overhead budget applied to any package without an override.
    , critPerPackageBudgetMs :: Map Text Double
    -- ^ Per-package full-document budget overrides, keyed by the package name.
    , critDefaultSingleVersionBudgetMs :: Double
    -- ^ The single-version overhead budget applied to any package without an override.
    , critPerPackageSingleVersionBudgetMs :: Map Text Double
    -- ^ Per-package single-version budget overrides, keyed by the package name.
    }
    deriving stock (Eq, Show)

instance FromJSON Criteria where
    parseJSON = withObject "Criteria" $ \o -> do
        crit <-
            Criteria
                <$> o .: "defaultBudgetMs"
                <*> o .:? "perPackageBudgetMs" .!= mempty
                <*> o .: "defaultSingleVersionBudgetMs"
                <*> o .:? "perPackageSingleVersionBudgetMs" .!= mempty
        let budgets =
                [critDefaultBudgetMs crit, critDefaultSingleVersionBudgetMs crit]
                    <> Map.elems (critPerPackageBudgetMs crit)
                    <> Map.elems (critPerPackageSingleVersionBudgetMs crit)
        unless (all (\n -> n > 0 && not (isInfinite n || isNaN n)) budgets) $
            fail "acceptance budgets must be finite and positive"
        pure crit

-- | Explicit ecosystem budgets. Uncalibrated sections emit measurements and fail the run.
newtype CriteriaCatalogue = CriteriaCatalogue
    { catalogueCriteria :: Map Ecosystem (Maybe Criteria)
    }
    deriving stock (Eq, Show)

instance FromJSON CriteriaCatalogue where
    parseJSON = withObject "CriteriaCatalogue" $ \o -> do
        raw <- o .: "ecosystems"
        entries <- traverse parseEntry (Map.toList raw)
        let sections = Map.fromList entries
        unless (all (`Map.member` sections) [Npm, PyPI]) $
            fail "acceptance criteria require npm and pypi sections"
        pure (CriteriaCatalogue sections)
      where
        parseEntry :: (Text, Maybe Criteria) -> Parser (Ecosystem, Maybe Criteria)
        parseEntry (name, crit) = case parseEcosystem name of
            Nothing -> fail ("unknown acceptance ecosystem: " <> toString name)
            Just eco -> pure (eco, crit)

-- | The committed criteria's path, relative to the package root the harness runs from.
criteriaPath :: FilePath
criteriaPath = "acceptance/criteria.json"

-- | Decode 'Criteria' from raw JSON bytes.
decodeCriteria :: LByteString -> Either String CriteriaCatalogue
decodeCriteria = eitherDecode

-- | Read and decode the committed criteria from 'criteriaPath'.
loadCriteria :: IO CriteriaCatalogue
loadCriteria = do
    raw <- readFileLBS criteriaPath
    either (\e -> fail (criteriaPath <> " did not decode: " <> e)) pure (decodeCriteria raw)

-- | The full-document overhead budget for a package: its override, or the default.
budgetFor :: Criteria -> Text -> Double
budgetFor crit name =
    Map.findWithDefault (critDefaultBudgetMs crit) name (critPerPackageBudgetMs crit)

-- | The single-version overhead budget for a package: its override, or the default.
singleVersionBudgetFor :: Criteria -> Text -> Double
singleVersionBudgetFor crit name =
    Map.findWithDefault (critDefaultSingleVersionBudgetMs crit) name (critPerPackageSingleVersionBudgetMs crit)

-- | One package's live measurements, with each duration in milliseconds.
data Sample = Sample
    { sampleName :: Text
    , sampleVersions :: Int
    -- ^ The number of published versions in the fetched document.
    , sampleUpstreamMs :: Double
    -- ^ Fetch time, separate from both processing legs.
    , sampleFullOverheadMs :: Double
    , sampleSingleVersionOverheadMs :: Double
    }
    deriving stock (Eq, Show)

-- | The verdict for a measured leg: within its budget, or over it by a margin (in milliseconds).
data Verdict
    = Within
    | Breached Double
    deriving stock (Eq, Show)

-- | One measured leg assessed against its budget: the budget it was held to and the verdict.
data Assessment = Assessment
    { assessBudgetMs :: Double
    , assessVerdict :: Verdict
    }
    deriving stock (Eq, Show)

-- | A package's outcome in a run: measured, or not assessable.
data PackageOutcome
    = -- | A measured package: its sample, the full-document assessment, then the single-version assessment.
      Measured Sample Assessment Assessment
    | -- | Measurements awaiting initial calibration, which cannot pass the run.
      Uncalibrated Sample
    | -- | A package that could not be assessed: its name and the reason.
      Unavailable Text Text
    deriving stock (Eq, Show)

-- | One ecosystem's outcomes, in catalogue order.
data Report = Report
    { reportEcosystem :: Ecosystem
    , reportCalibrated :: Bool
    , reportOutcomes :: [PackageOutcome]
    }
    deriving stock (Eq, Show)

-- | Evaluate each package's raw input against the criteria.
evaluate :: Ecosystem -> Maybe Criteria -> [Either (Text, Text) Sample] -> Report
evaluate eco crit = Report eco (isJust crit) . map outcome
  where
    outcome (Left (name, reason)) = Unavailable name reason
    outcome (Right sample) = case crit of
        Nothing -> Uncalibrated sample
        Just budgets ->
            Measured
                sample
                (assess (budgetFor budgets (sampleName sample)) (sampleFullOverheadMs sample))
                (assess (singleVersionBudgetFor budgets (sampleName sample)) (sampleSingleVersionOverheadMs sample))

assess :: Double -> Double -> Assessment
assess budget overheadMs =
    let margin = overheadMs - budget
     in Assessment budget (if margin > 0 then Breached margin else Within)

-- | Whether any measured leg breached its budget: the run's red condition.
reportBreached :: Report -> Bool
reportBreached = any isBreach . reportOutcomes
  where
    isBreach (Measured _ full single) = breached full || breached single
    isBreach _ = False

-- | The process fails for any ecosystem's breach or uncalibrated criteria.
reportExitCode :: [Report] -> ExitCode
reportExitCode reports
    | any reportBreached reports || not (all reportCalibrated reports) = ExitFailure 1
    | otherwise = ExitSuccess

breached :: Assessment -> Bool
breached (Assessment _ (Breached _)) = True
breached _ = False

-- | Timed passes per leg and the total catalogue size.
data OperatingPoint = OperatingPoint
    { opPassesPerLeg :: Int
    -- ^ Timed passes per leg. The reported figure is their median.
    , opCatalogueSize :: Int
    -- ^ Packages in the curated catalogue this run set out to measure.
    }
    deriving stock (Eq, Show)

-- | Budget divided by overhead. Non-positive observations have no meaningful ratio.
headroom :: Double -> Double -> Maybe Double
headroom budget observed
    | observed <= 0 = Nothing
    | otherwise = Just (budget / observed)

-- | Mark a leg for attention when it consumes 70% of its budget.
watchFraction :: Double
watchFraction = 0.7

watching :: Assessment -> Double -> Bool
watching a observed = case assessVerdict a of
    Within -> assessBudgetMs a > 0 && observed / assessBudgetMs a >= watchFraction
    Breached _ -> False

-- | Render one table per ecosystem, separating upstream latency from processing overhead.
renderReport :: OperatingPoint -> [Report] -> Text
renderReport op reports =
    T.unlines
        [ "## Live performance-acceptance (Context B)"
        , ""
        , "Catalogue: " <> show (opCatalogueSize op) <> " packages (bench/corpus/pins.json)."
        , "Full overhead: decode, projection, rules, assembly, and serialisation."
        , "Single-version overhead: selective projection and forcing artifact digests."
        , "Input copies, target selection, and snapshot digests are prepared outside processing timers."
        , ""
        ]
        <> foldMap (renderSection op) reports

renderSection :: OperatingPoint -> Report -> Text
renderSection op report =
    T.unlines (headerLines <> operatingLines <> tableLines <> footerLines)
  where
    outcomes = reportOutcomes report
    breaches = length [() | Measured _ full single <- outcomes, breached full || breached single]
    unavailable = length [() | Unavailable _ _ <- outcomes]
    watched =
        length
            [ ()
            | Measured s full single <- outcomes
            , (a, observed) <- [(full, sampleFullOverheadMs s), (single, sampleSingleVersionOverheadMs s)]
            , watching a observed
            ]

    headerLines =
        [ "### " <> ecosystemName (reportEcosystem report)
        , ""
        , overall
        , ""
        ]
    overall
        | breaches > 0 =
            "Result: BREACH: " <> show breaches <> " package(s) over budget" <> incompleteSuffix
        | not (reportCalibrated report) = "Result: UNCALIBRATED (measurements only, run fails)" <> incompleteSuffix
        | otherwise =
            "Result: within budget" <> incompleteSuffix
    incompleteSuffix
        | unavailable > 0 = " (" <> show unavailable <> " package(s) unavailable, not assessed)"
        | otherwise = ""

    operatingLines =
        [ "**Operating point**"
        , ""
        , "| knob | value |"
        , "| --- | --- |"
        , cells ["catalogue", show (length outcomes) <> " packages (bench/corpus/pins.json)"]
        , cells ["timing", "median of " <> show (opPassesPerLeg op) <> " timed passes per leg"]
        , cells ["budgets", "acceptance/criteria.json (version-controlled. Budget changes require review)"]
        , ""
        ]

    tableLines =
        [ "| Package | Versions | Upstream (ms) | Full overhead (ms) | Single-version (ms) | Budget full/1-ver (ms) | Headroom full/1-ver | Verdict |"
        , "|---|--:|--:|--:|--:|--:|--:|---|"
        ]
            <> map row outcomes

    row (Measured s full single) =
        cells $
            sampleCells s
                <> [ fmt 1 (assessBudgetMs full) <> " / " <> fmt 1 (assessBudgetMs single)
                   , headroomCell full (sampleFullOverheadMs s)
                        <> " / "
                        <> headroomCell single (sampleSingleVersionOverheadMs s)
                   , renderVerdicts s full single
                   ]
    row (Uncalibrated s) =
        cells (sampleCells s <> ["uncalibrated", "n/a", "UNCALIBRATED"])
    row (Unavailable name reason) =
        cells [name, "--", "--", "--", "--", "--", "--", "unavailable: " <> reason]

    headroomCell a observed = maybe "n/a" (\h -> fmt 1 h <> "x") (headroom (assessBudgetMs a) observed)

    footerLines = unavailableNote <> watchNote
    unavailableNote
        | unavailable > 0 =
            ["", "_" <> show unavailable <> " package(s) could not be fetched or decoded. Registry failure is not a breach._"]
        | otherwise = []
    watchNote
        | watched > 0 =
            [ ""
            , "_watch marks a leg at or above "
                <> fmt 0 (watchFraction * 100)
                <> "% of its budget. A watch does not fail the run._"
            ]
        | otherwise = []

sampleCells :: Sample -> [Text]
sampleCells s =
    [ sampleName s
    , show (sampleVersions s)
    , fmt 3 (sampleUpstreamMs s)
    , fmt 3 (sampleFullOverheadMs s)
    , fmt 3 (sampleSingleVersionOverheadMs s)
    ]

renderVerdicts :: Sample -> Assessment -> Assessment -> Text
renderVerdicts s full single =
    case catMaybes [tag "full" full (sampleFullOverheadMs s), tag "1-ver" single (sampleSingleVersionOverheadMs s)] of
        [] -> "within"
        marks -> T.intercalate ", " marks
  where
    tag label a observed = case assessVerdict a of
        Breached margin -> Just ("BREACH " <> label <> " +" <> fmt 1 margin <> " ms")
        Within
            | watching a observed ->
                Just ("watch: " <> label <> " at " <> fmt 0 (observed / assessBudgetMs a * 100) <> "% of budget")
            | otherwise -> Nothing

cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

fmt :: Int -> Double -> Text
fmt places x = toText (showFFloat (Just places) x "")

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Performance-acceptance evaluation: the pure core of the live
performance-acceptance harness.

The harness fetches real packuments from the live registries and times Écluse's
work-per-request over each. It then asks one question: __is the per-request overhead
within the acceptance budget under today's real-world conditions?__ A breach is a
prompt for a human decision, a code regression or reality outgrowing the provisioned
budget, never an automatic block.

The harness measures two overheads per package, each with its own budget:

  * The __full-packument__ transform (decode, project, rule sweep, filter, URL
    rewrite, re-serialise) that backs a metadata read of every version.
  * The __single-version__ selective decode the tarball gate consults to serve one
    package version. This is the cold path's per-package overhead, which a
    whole-document decode dominates on the heavy many-version packuments and a
    selective decode does not. Tracking it separately keeps an improvement to the
    single-version path visible in the report rather than lost behind the
    full-packument figure.

This module is the deterministic part: the version-controlled acceptance 'Criteria',
the per-package 'evaluate', and the 'renderReport' summary. 'evaluate' turns a
measured 'Sample' into a per-leg 'Assessment' against its budget.

The live fetch and timing live in the harness executable. Everything here is pure and
unit-tested, so a test exercises the acceptance decision deterministically rather than
only against the live registries.

The criteria come from a __version-controlled__ JSON file ('criteriaPath'), so moving
the bar is an explicit, reviewed act.
-}
module Ecluse.Acceptance (
    -- * Acceptance criteria
    Criteria (..),
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

    -- * Rendering
    OperatingPoint (..),
    headroom,
    watchFraction,
    renderReport,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

{- | The acceptance budget: the maximum Écluse work-per-request overhead, in milliseconds,
allowed before the run reds. Per-package overrides cover the heavy, many-version packuments.
-}
data Criteria = Criteria
    { critDefaultBudgetMs :: Double
    -- ^ The full-packument overhead budget applied to any package without an override.
    , critPerPackageBudgetMs :: Map Text Double
    -- ^ Per-package full-packument budget overrides, keyed by the package name.
    , critDefaultSingleVersionBudgetMs :: Double
    -- ^ The single-version overhead budget applied to any package without an override.
    , critPerPackageSingleVersionBudgetMs :: Map Text Double
    -- ^ Per-package single-version budget overrides, keyed by the package name.
    }
    deriving stock (Eq, Show)

instance FromJSON Criteria where
    parseJSON = withObject "Criteria" $ \o ->
        Criteria
            <$> o .: "defaultBudgetMs"
            <*> o .:? "perPackageBudgetMs" .!= mempty
            <*> o .: "defaultSingleVersionBudgetMs"
            <*> o .:? "perPackageSingleVersionBudgetMs" .!= mempty

{- | The committed criteria's path, relative to the package root the harness runs
from. Version-controlled so that moving the bar is an explicit, reviewed change.
-}
criteriaPath :: FilePath
criteriaPath = "acceptance/criteria.json"

-- | Decode 'Criteria' from raw JSON bytes.
decodeCriteria :: LByteString -> Either String Criteria
decodeCriteria = eitherDecode

{- | Read and decode the committed criteria from 'criteriaPath'. It fails loudly when the file
is missing or malformed, because that is a committed-config defect.
-}
loadCriteria :: IO Criteria
loadCriteria = do
    raw <- readFileLBS criteriaPath
    either (\e -> fail (criteriaPath <> " did not decode: " <> e)) pure (decodeCriteria raw)

-- | The full-packument overhead budget for a package: its override, or the default.
budgetFor :: Criteria -> Text -> Double
budgetFor crit name =
    Map.findWithDefault (critDefaultBudgetMs crit) name (critPerPackageBudgetMs crit)

-- | The single-version overhead budget for a package: its override, or the default.
singleVersionBudgetFor :: Criteria -> Text -> Double
singleVersionBudgetFor crit name =
    Map.findWithDefault (critDefaultSingleVersionBudgetMs crit) name (critPerPackageSingleVersionBudgetMs crit)

{- | One package's live measurement. The upstream fetch, the full-packument transform, and
the single-version decode stay separate, so no upstream cost reads as an Écluse one.
-}
data Sample = Sample
    { sampleName :: Text
    -- ^ The package name measured.
    , sampleVersions :: Int
    -- ^ The number of published versions in the fetched packument.
    , sampleUpstreamMs :: Double
    -- ^ Wall-clock time to fetch the packument from the live registry, in milliseconds.
    , sampleFullOverheadMs :: Double
    -- ^ Wall-clock time for Écluse's full-packument work-per-request over it, in milliseconds.
    , sampleSingleVersionOverheadMs :: Double
    -- ^ Wall-clock time for the single-version selective decode of its latest version, in milliseconds.
    }
    deriving stock (Eq, Show)

{- | The verdict for a measured leg: within its budget, or over it by a margin
(in milliseconds).
-}
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

{- | A package's outcome in a run: measured, or not assessable. A fetch or decode failure is
__not__ a breach, because only an over-budget measurement reds the run.
-}
data PackageOutcome
    = -- | A measured package: its sample, the full-packument assessment, then the single-version assessment.
      Measured Sample Assessment Assessment
    | -- | A package that could not be assessed: its name and the reason.
      Unavailable Text Text
    deriving stock (Eq, Show)

-- | A whole run's outcomes, in input order.
newtype Report = Report
    { reportOutcomes :: [PackageOutcome]
    }
    deriving stock (Eq, Show)

{- | Evaluate each package's raw input against the criteria. A @Left (name, reason)@ carries
through as an unavailable package and never counts as a breach.
-}
evaluate :: Criteria -> [Either (Text, Text) Sample] -> Report
evaluate crit = Report . map outcome
  where
    outcome (Left (name, reason)) = Unavailable name reason
    outcome (Right sample) =
        Measured
            sample
            (assess (budgetFor crit (sampleName sample)) (sampleFullOverheadMs sample))
            (assess (singleVersionBudgetFor crit (sampleName sample)) (sampleSingleVersionOverheadMs sample))

-- | Assess one overhead leg against its budget.
assess :: Double -> Double -> Assessment
assess budget overheadMs =
    let margin = overheadMs - budget
     in Assessment budget (if margin > 0 then Breached margin else Within)

{- | Whether any measured leg breached its budget: the run's red condition. An
unavailable package never counts, because a flaky registry is not a perf regression.
-}
reportBreached :: Report -> Bool
reportBreached = any isBreach . reportOutcomes
  where
    isBreach (Measured _ full single) = breached full || breached single
    isBreach _ = False

-- | Whether an assessment is over budget.
breached :: Assessment -> Bool
breached (Assessment _ (Breached _)) = True
breached _ = False

{- | The run-shape facts the summary names, so a reader interprets the numbers without opening
the harness.
-}
data OperatingPoint = OperatingPoint
    { opPassesPerLeg :: Int
    -- ^ Timed passes per leg. The reported figure is their median.
    , opCatalogueSize :: Int
    -- ^ Packages in the curated catalogue this run set out to measure.
    }
    deriving stock (Eq, Show)

{- | The budget-to-observed multiple for one leg: how many times its overhead fits inside its
budget. 'Nothing' when the observed figure is not positive, where the multiple is meaningless.
-}
headroom :: Double -> Double -> Maybe Double
headroom budget observed
    | observed <= 0 = Nothing
    | otherwise = Just (budget / observed)

{- | The fraction of its budget a within-budget leg may consume before the report marks it
__watch__, an early warning while the exit code stays green. Budgets sit at roughly 2.2x the
observed CI maxima, so 0.7 stays quiet near a healthy 45% and trips at about 1.55x that maximum.
-}
watchFraction :: Double
watchFraction = 0.7

-- Whether a within-budget leg is on watch. The budget guard keeps the ratio defined.
watching :: Assessment -> Double -> Bool
watching a observed = case assessVerdict a of
    Within -> assessBudgetMs a > 0 && observed / assessBudgetMs a >= watchFraction
    Breached _ -> False

{- | Render a run as a Markdown summary: an overall verdict line, the operating
point, then a per-package table. The table keeps the __upstream__, __full-packument
overhead__, and __single-version overhead__ legs in separate columns, so an
upstream-normalisation view fits later without reshaping the table.

Each measured row names its budgets, its per-leg headroom, and a verdict. The verdict
is @within@, a @watch@ on a leg at or above 'watchFraction' of its budget, or a breach
naming the leg and its margin. The report lists unavailable packages as such, never
as breaches.
-}
renderReport :: OperatingPoint -> Report -> Text
renderReport op report =
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
        [ "## Live performance-acceptance (Context B)"
        , ""
        , overall
        , ""
        ]
    overall
        | breaches > 0 =
            "Result: BREACH -- " <> show breaches <> " package(s) over budget" <> incompleteSuffix
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
        , cells ["catalogue", show (opCatalogueSize op) <> " packages (bench/corpus/pins.json)"]
        , cells ["timing", "median of " <> show (opPassesPerLeg op) <> " timed passes per leg"]
        , cells ["budgets", "acceptance/criteria.json (version-controlled; moving the bar is a reviewed change)"]
        , ""
        ]

    tableLines =
        [ "| Package | Versions | Upstream (ms) | Full overhead (ms) | Single-version (ms) | Budget full/1-ver (ms) | Headroom full/1-ver | Verdict |"
        , "|---|--:|--:|--:|--:|--:|--:|---|"
        ]
            <> map row outcomes

    row (Measured s full single) =
        cells
            [ sampleName s
            , show (sampleVersions s)
            , fmt1 (sampleUpstreamMs s)
            , fmt1 (sampleFullOverheadMs s)
            , fmt1 (sampleSingleVersionOverheadMs s)
            , fmt1 (assessBudgetMs full) <> " / " <> fmt1 (assessBudgetMs single)
            , headroomCell full (sampleFullOverheadMs s)
                <> " / "
                <> headroomCell single (sampleSingleVersionOverheadMs s)
            , renderVerdicts s full single
            ]
    row (Unavailable name reason) =
        cells [name, "--", "--", "--", "--", "--", "--", "unavailable: " <> reason]

    headroomCell a observed = maybe "n/a" (\h -> fmt1 h <> "x") (headroom (assessBudgetMs a) observed)

    footerLines = unavailableNote <> watchNote
    unavailableNote
        | unavailable > 0 =
            ["", "_" <> show unavailable <> " package(s) could not be fetched or decoded; a flaky registry is not a breach._"]
        | otherwise = []
    watchNote
        | watched > 0 =
            [ ""
            , "_watch marks a leg at or above "
                <> fmt0 (watchFraction * 100)
                <> "% of its budget: early warning, not a failure -- only a breach exits non-zero._"
            ]
        | otherwise = []

-- A measured row's verdict cell.
renderVerdicts :: Sample -> Assessment -> Assessment -> Text
renderVerdicts s full single =
    case catMaybes [tag "full" full (sampleFullOverheadMs s), tag "1-ver" single (sampleSingleVersionOverheadMs s)] of
        [] -> "within"
        marks -> T.intercalate ", " marks
  where
    tag label a observed = case assessVerdict a of
        Breached margin -> Just ("BREACH " <> label <> " +" <> fmt1 margin <> " ms")
        Within
            | watching a observed ->
                Just ("watch -- " <> label <> " at " <> fmt0 (observed / assessBudgetMs a * 100) <> "% of budget")
            | otherwise -> Nothing

-- A Markdown table row from its cells.
cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- A double rendered to one decimal place (non-scientific), for the summary table.
fmt1 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")

-- A double rendered with no decimal places (non-scientific), for whole percentages.
fmt0 :: Double -> Text
fmt0 x = toText (showFFloat (Just 0) x "")

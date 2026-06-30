{- | Performance-acceptance evaluation: the pure core of the live
performance-acceptance harness.

The harness fetches real packuments from the live registries and times Écluse's
work-per-request over each, then asks one question: __is the per-request overhead
within the acceptance budget under today's real-world conditions?__ A breach is a
prompt for a human decision -- a code regression, or reality outgrowing the
provisioned budget -- never an automatic block.

Two overheads are measured per package, each with its own budget:

  * the __full-packument__ transform (decode, project, rule sweep, filter, URL
    rewrite, re-serialise) that backs a metadata read of every version; and
  * the __single-version__ selective decode the tarball gate consults to serve one
    package version -- the cold path's per-package overhead, which a whole-document
    decode dominates on the heavy many-version packuments and a selective decode does
    not. Tracking it separately keeps an improvement to the single-version path
    visible in the report rather than lost behind the full-packument figure.

This module is the deterministic part: the version-controlled acceptance 'Criteria',
the per-package 'evaluate' that turns a measured 'Sample' into a per-leg 'Assessment'
against its budget, and the 'renderReport' summary. The live fetch and timing live in
the harness executable; everything here is pure and unit-tested, so the acceptance
decision is exercised deterministically rather than only against the live registries.

The criteria are read from a __version-controlled__ JSON file ('criteriaPath') so
moving the bar is an explicit, reviewed act.
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
    renderReport,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

{- | The acceptance budget: the maximum Écluse work-per-request overhead, in
milliseconds, allowed before the run reds. A separate default applies to the
full-packument transform and to the single-version selective decode, each with
optional per-package overrides for the heavy, many-version packuments whose
processing is legitimately costlier.
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

{- | Read and decode the committed criteria from 'criteriaPath'. Fails loudly if
the file is missing or malformed -- a committed-config defect, not a runtime
condition the harness decides on.
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

{- | One package's live measurement: how long the registry took to serve the
packument (the upstream leg) and how long Écluse took to process it -- split into the
full-packument transform and the single-version selective decode -- so an
upstream-bound cost is never mistaken for an Écluse one, and the single-version path
is tracked on its own.
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

{- | A package's outcome in a run: either it was measured (with the per-leg
assessments -- the full-packument leg, then the single-version leg), or it could not
be assessed (a fetch or decode failure, which is __not__ a breach -- only an
over-budget measurement reds the run).
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

{- | Evaluate each package's raw input against the criteria. A @Left (name, reason)@
is an unavailable package (carried through, never a breach); a @Right sample@ is
measured against its resolved budgets -- each leg over budget yields a 'Breached'
margin, otherwise 'Within'.
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

{- | Whether any measured leg breached its budget -- the run's red condition. An
unavailable package never counts (a flaky registry is not a perf regression).
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

{- | Render a run as a Markdown summary: an overall verdict line, then a per-package
table that keeps the __upstream__, __full-packument overhead__, and __single-version
overhead__ legs in separate columns -- so an upstream-normalisation view can be added
without reshaping the table -- with each measured row naming its budgets and, on a
breach, which leg went over and by how much. Unavailable packages are listed as such,
never as breaches.
-}
renderReport :: Report -> Text
renderReport report =
    T.unlines (headerLines <> tableLines <> footerLines)
  where
    outcomes = reportOutcomes report
    breaches = length [() | Measured _ full single <- outcomes, breached full || breached single]
    unavailable = length [() | Unavailable _ _ <- outcomes]

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

    tableLines =
        [ "| Package | Versions | Upstream (ms) | Full overhead (ms) | Single-version (ms) | Budget full/1-ver (ms) | Verdict |"
        , "|---|--:|--:|--:|--:|--:|---|"
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
            , renderVerdicts full single
            ]
    row (Unavailable name reason) =
        cells [name, "--", "--", "--", "--", "--", "unavailable: " <> reason]

    footerLines
        | unavailable > 0 =
            ["", "_" <> show unavailable <> " package(s) could not be fetched or decoded; a flaky registry is not a breach._"]
        | otherwise = []

-- A measured row's verdict cell: "within" when both legs are in budget, else the
-- breached legs named with their margins.
renderVerdicts :: Assessment -> Assessment -> Text
renderVerdicts full single =
    case catMaybes [tag "full" full, tag "1-ver" single] of
        [] -> "within"
        breaches -> T.intercalate ", " breaches
  where
    tag label a = case assessVerdict a of
        Within -> Nothing
        Breached margin -> Just ("BREACH " <> label <> " +" <> fmt1 margin <> " ms")

-- A Markdown table row from its cells.
cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- A double rendered to one decimal place (non-scientific), for the summary table.
fmt1 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")

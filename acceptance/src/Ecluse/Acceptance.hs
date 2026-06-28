{- | Performance-acceptance evaluation: the pure core of the live
performance-acceptance harness.

The harness fetches real packuments from the live registries and times Écluse's
work-per-request over each, then asks one question: __is the per-request overhead
within the acceptance budget under today's real-world conditions?__ A breach is a
prompt for a human decision — a code regression, or reality outgrowing the
provisioned budget — never an automatic block.

This module is the deterministic part: the version-controlled acceptance
'Criteria', the per-package 'evaluate' that turns a measured 'Sample' into a
'Verdict' against its budget, and the 'renderReport' summary. The live fetch and
timing live in the harness executable; everything here is pure and unit-tested, so
the acceptance decision is exercised deterministically rather than only against the
live registries.

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

    -- * Measurements and verdicts
    Sample (..),
    Verdict (..),
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

-- ── acceptance criteria ────────────────────────────────────────────────────────

{- | The acceptance budget: the maximum Écluse work-per-request overhead, in
milliseconds, allowed for a single real packument before the run reds. A default
budget applies to every package, with optional per-package overrides for the
heavy, many-version packuments whose processing is legitimately costlier.
-}
data Criteria = Criteria
    { critDefaultBudgetMs :: Double
    -- ^ The overhead budget applied to any package without an override.
    , critPerPackageBudgetMs :: Map Text Double
    -- ^ Per-package budget overrides, keyed by the package name.
    }
    deriving stock (Eq, Show)

instance FromJSON Criteria where
    parseJSON = withObject "Criteria" $ \o ->
        Criteria
            <$> o .: "defaultBudgetMs"
            <*> o .:? "perPackageBudgetMs" .!= mempty

{- | The committed criteria's path, relative to the package root the harness runs
from. Version-controlled so that moving the bar is an explicit, reviewed change.
-}
criteriaPath :: FilePath
criteriaPath = "acceptance/criteria.json"

-- | Decode 'Criteria' from raw JSON bytes.
decodeCriteria :: LByteString -> Either String Criteria
decodeCriteria = eitherDecode

{- | Read and decode the committed criteria from 'criteriaPath'. Fails loudly if
the file is missing or malformed — a committed-config defect, not a runtime
condition the harness decides on.
-}
loadCriteria :: IO Criteria
loadCriteria = do
    raw <- readFileLBS criteriaPath
    either (\e -> fail (criteriaPath <> " did not decode: " <> e)) pure (decodeCriteria raw)

-- | The overhead budget for a package: its per-package override, or the default.
budgetFor :: Criteria -> Text -> Double
budgetFor crit name =
    Map.findWithDefault (critDefaultBudgetMs crit) name (critPerPackageBudgetMs crit)

-- ── measurements and verdicts ──────────────────────────────────────────────────

{- | One package's live measurement: how long the registry took to serve the
packument (the upstream leg) and how long Écluse took to process it (the overhead
leg), separated so an upstream-bound cost is never mistaken for an Écluse one.
-}
data Sample = Sample
    { sampleName :: Text
    -- ^ The package name measured.
    , sampleVersions :: Int
    -- ^ The number of published versions in the fetched packument.
    , sampleUpstreamMs :: Double
    -- ^ Wall-clock time to fetch the packument from the live registry, in milliseconds.
    , sampleOverheadMs :: Double
    -- ^ Wall-clock time for Écluse's work-per-request over it, in milliseconds.
    }
    deriving stock (Eq, Show)

{- | The verdict for a measured package: within its budget, or over it by a margin
(in milliseconds).
-}
data Verdict
    = Within
    | Breached Double
    deriving stock (Eq, Show)

{- | A package's outcome in a run: either it was measured (with its resolved budget
and verdict), or it could not be assessed (a fetch or decode failure, which is
__not__ a breach — only an over-budget measurement reds the run).
-}
data PackageOutcome
    = -- | A measured package: its sample, the budget it was held to, and the verdict.
      Measured Sample Double Verdict
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
measured against its resolved budget — over budget yields a 'Breached' margin,
otherwise 'Within'.
-}
evaluate :: Criteria -> [Either (Text, Text) Sample] -> Report
evaluate crit = Report . map outcome
  where
    outcome (Left (name, reason)) = Unavailable name reason
    outcome (Right sample) =
        let budget = budgetFor crit (sampleName sample)
            margin = sampleOverheadMs sample - budget
         in Measured sample budget (if margin > 0 then Breached margin else Within)

{- | Whether any measured package breached its budget — the run's red condition.
An unavailable package never counts (a flaky registry is not a perf regression).
-}
reportBreached :: Report -> Bool
reportBreached = any isBreach . reportOutcomes
  where
    isBreach (Measured _ _ (Breached _)) = True
    isBreach _ = False

-- ── rendering ──────────────────────────────────────────────────────────────────

{- | Render a run as a Markdown summary: an overall verdict line, then a per-package
table separating the __upstream__ and __Écluse overhead__ legs (the breakdown a
later upstream-normalization column slots beside), each measured row naming its
budget and — on a breach — the margin over it. Unavailable packages are listed as
such, never as breaches.
-}
renderReport :: Report -> Text
renderReport report =
    T.unlines (headerLines <> tableLines <> footerLines)
  where
    outcomes = reportOutcomes report
    breaches = length [() | Measured _ _ (Breached _) <- outcomes]
    unavailable = length [() | Unavailable _ _ <- outcomes]

    headerLines =
        [ "## Live performance-acceptance (Context B)"
        , ""
        , overall
        , ""
        ]
    overall
        | breaches > 0 =
            "Result: BREACH — " <> show breaches <> " package(s) over budget" <> incompleteSuffix
        | otherwise =
            "Result: within budget" <> incompleteSuffix
    incompleteSuffix
        | unavailable > 0 = " (" <> show unavailable <> " package(s) unavailable, not assessed)"
        | otherwise = ""

    tableLines =
        [ "| Package | Versions | Upstream (ms) | Écluse overhead (ms) | Budget (ms) | Verdict |"
        , "|---|--:|--:|--:|--:|---|"
        ]
            <> map row outcomes

    row (Measured s budget verdict) =
        cells
            [ sampleName s
            , show (sampleVersions s)
            , fmt1 (sampleUpstreamMs s)
            , fmt1 (sampleOverheadMs s)
            , fmt1 budget
            , renderVerdict verdict
            ]
    row (Unavailable name reason) =
        cells [name, "—", "—", "—", "—", "unavailable: " <> reason]

    footerLines
        | unavailable > 0 =
            ["", "_" <> show unavailable <> " package(s) could not be fetched or decoded; a flaky registry is not a breach._"]
        | otherwise = []

renderVerdict :: Verdict -> Text
renderVerdict = \case
    Within -> "within"
    Breached margin -> "BREACH +" <> fmt1 margin <> " ms"

-- A Markdown table row from its cells.
cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- A double rendered to one decimal place (non-scientific), for the summary table.
fmt1 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")

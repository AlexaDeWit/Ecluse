{- | Work-per-request benches for the rules engine ("Ecluse.Core.Rules"): evaluating a
rule set against every version of a packument, the sweep that decides which versions
survive a metadata response.

The realistic benches run over the curated real-world corpus (each package's real
version set and per-version signals); a synthetic bench scales the version count and
asserts the sweep stays linear, guarding the accidentally quadratic regression a
per-version rule fold can hide. Evaluation is effectful -- the engine 'prepare's
rules, then evaluates each version in 'IO' -- so the per-version sweep is the measured
'IO' work. The synthetic generator is retained __only__ for this complexity-scaling
assertion, not as the realistic case.
-}
module Ecluse.Core.RulesBench (
    benchmarks,
) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (
    LoadedEntry,
    benchEvalContext,
    benchRules,
    entryInfo,
    entryName,
    syntheticPackageInfo,
 )
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (PackageInfo, infoVersions)
import Ecluse.Core.Rules (bootOrder, evalRulesInBootOrder, inertRuleDeps, prepare)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | The rule-sweep benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup "rules.evalRules" $
        [ bench (entryName le) (whnfAppIO rulesDepth (entryInfo le))
        | le <- loaded
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (64, 8192)
                    (syntheticPackageInfo . fromIntegral)
                    rulesDepth
               ]

{- | Evaluate the rule set against every version, forcing each decision. The engine
'prepare's and 'bootOrder's the rules once -- both constant in the version count -- and
then sweeps every version in 'IO' through 'evalRulesInBootOrder', mirroring the serve
path's per-request work exactly (see 'Ecluse.Core.Server.Pipeline.Packument'), so the
measured sweep is the shipped shape rather than a per-version re-sort.
-}
rulesDepth :: PackageInfo -> IO Int
rulesDepth info = do
    prepared <- prepare inertRuleDeps benchRules
    let ordered = bootOrder prepared
    sum <$> traverse (fmap decisionCode . evalRulesInBootOrder benchEvalContext ordered) (Map.elems (infoVersions info))

-- | A distinct code per decision arm, forcing the engine's verdict to a constructor.
decisionCode :: Decision -> Int
decisionCode = \case
    Admitted{} -> 1
    Blocked{} -> 2
    BlockedByDefault{} -> 3
    Undecidable{} -> 4

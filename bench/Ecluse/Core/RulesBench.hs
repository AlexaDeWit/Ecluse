-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure rule evaluation over each ecosystem's projected metadata.
Synthetic release counts check the growth of the same rule sweep.
-}
module Ecluse.Core.RulesBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (benchEvalContext, benchRules, entryInfo, entryName, syntheticPackageInfo)
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (PackageInfo, infoVersions)
import Ecluse.Core.Rules (evalRules, prepare)
import Ecluse.Core.Rules.Types (Decision (Admitted, Blocked, BlockedByDefault, Undecidable), completeEvidence)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Rules (inertRuleDeps)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | Measure captured rule sweeps and their growth across synthetic release counts.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "rules.evalRules" $
        [ bench (entryName entry) (whnfAppIO rulesDepth (entryInfo entry))
        | entry <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (64, 8192)
                    (syntheticPackageInfo ecosystem . fromIntegral)
                    (either (const (pure (-1))) rulesDepth)
               ]

rulesDepth :: PackageInfo -> IO Int
rulesDepth info = do
    prepared <- prepare inertRuleDeps benchRules
    sum <$> traverse (fmap decisionCode . evalRules benchEvalContext prepared . completeEvidence) (Map.elems (infoVersions info))

decisionCode :: Decision -> Int
decisionCode = \case
    Admitted{} -> 1
    Blocked{} -> 2
    BlockedByDefault{} -> 3
    Undecidable{} -> 4

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Complexity assertions for the version-count-scaled benches. @tasty-bench-fit@ fits a bench's
growth and fails it when the growth is worse than linear, which catches a fold that turns
@O(n^2)@ in version count where a single-size timing cannot. A timing depends on the machine, but a
growth class does not, so a failure exits non-zero and reds the benchmark run.
-}
module Ecluse.Bench.Fit (
    notWorseThanLinear,
    notWorseThanLinearIO,
) where

import Test.Tasty (TestTree, Timeout, mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (RelStDev), whnf, whnfAppIO)
import Test.Tasty.Bench.Fit (
    Complexity (cmplVarPower),
    FitConfig (..),
    fit,
    guessComplexity,
 )
import Test.Tasty.HUnit (assertBool, testCase)

{- | The exponent ceiling a fitted complexity must stay under to pass. It sits above the
power @1@ of linear and @n log n@ growth and below quadratic, with slack for fit noise.
-}
linearCeiling :: Double
linearCeiling = 1.5

{- | Assert that a pure operation's running time grows no worse than linearly in the input
size. The size-to-input function runs once per size, so the fit covers the operation alone.
-}
notWorseThanLinear ::
    -- | Test label.
    String ->
    {- | The smallest and largest input sizes to fit between (the largest should be
    at least @100x@ the smallest).
    -}
    (Word, Word) ->
    -- | Build an input of the given size (run once per size, not measured).
    (Word -> input) ->
    -- | The operation under test, summarised to a fully-forced 'Int'.
    (input -> Int) ->
    TestTree
notWorseThanLinear label (low, high) build operation =
    testCase label $ do
        complexity <- fit (fitConfig (low, high) (whnf operation . build))
        assertBool
            ("expected growth no worse than linear, but the fit is " <> show complexity)
            (cmplVarPower complexity < linearCeiling)

{- | Like 'notWorseThanLinear', but for an operation that computes its 'Int' result in 'IO',
as the rule engine's per-request version sweep does.
-}
notWorseThanLinearIO ::
    String ->
    (Word, Word) ->
    (Word -> input) ->
    (input -> IO Int) ->
    TestTree
notWorseThanLinearIO label (low, high) build operation =
    testCase label $ do
        complexity <- fit (fitConfig (low, high) (whnfAppIO operation . build))
        assertBool
            ("expected growth no worse than linear, but the fit is " <> show complexity)
            (cmplVarPower complexity < linearCeiling)

{- | The shared 'FitConfig'. Every iteration at a size reuses that size's input, so input
construction never folds into the fit.
-}
fitConfig :: (Word, Word) -> (Word -> Benchmarkable) -> FitConfig
fitConfig (low, high) toBench =
    FitConfig
        { fitBench = toBench
        , fitLow = low
        , fitHigh = high
        , fitTimeout = measurementCap
        , fitRelStDev = RelStDev 0.04
        , fitOracle = guessComplexity
        }

-- | An upper bound on any single measurement, so a pathological size cannot hang the run.
measurementCap :: Timeout
measurementCap = mkTimeout 100_000_000

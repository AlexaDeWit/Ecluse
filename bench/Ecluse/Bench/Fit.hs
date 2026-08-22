-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The complexity-assertion helpers shared by the version-count-scaled benches.

A scaled bench reports a time. @tasty-bench-fit@ also fits its /growth/ and requires
that growth to be no worse than linear. That is the guard against the accidentally
quadratic class of regression, a fold that becomes @O(n^2)@ in version count. A
single-size timing would never reveal it.

Unlike a perf-regression comparison (machine-dependent, noisy, never gated), an
algorithmic-class assertion is a real correctness signal. A packument merge or rule
sweep going quadratic in version count is a bug, not a slow machine. A failure here is
therefore a genuine benchmark failure (a non-zero exit), the one red state the
benchmark workflow recognises.

Two variants: 'notWorseThanLinear' for a pure operation, and 'notWorseThanLinearIO'
for one that computes its result in 'IO' (the rule engine evaluates effectfully).
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

{- | The exponent ceiling a fitted complexity must stay under to pass. It sits above
linear and linearithmic, whose fitted variable power is @1@, and below quadratic
(@2@). A linear or @n log n@ growth therefore passes, and an accidentally-quadratic one
trips. The slack also absorbs the measurement noise a real fit carries.
-}
linearCeiling :: Double
linearCeiling = 1.5

{- | Assert that a pure operation's running time grows no worse than linearly in the
input size. The size-to-input function runs once per measured size, and its result
serves every iteration at that size. The fit therefore covers the operation alone,
never the input construction. Summarise the operation to a forced 'Int' so the
measurement evaluates the whole result.
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

{- | Like 'notWorseThanLinear', but for an operation that computes its result in 'IO'.
The rule engine prepares rules once, then evaluates each version effectfully, so the
per-request sweep is an 'IO' action. Each measurement runs the action and forces its
'Int' result.
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

{- | The shared 'FitConfig': measure the given size-to-'Benchmarkable' over the size
range. Every iteration at a size reuses that size's input, so construction never folds
into the fit.
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

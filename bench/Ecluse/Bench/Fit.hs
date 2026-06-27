{- | The complexity-assertion helpers shared by the version-count-scaled benches.

A scaled bench is not only timed; its /growth/ is fitted with @tasty-bench-fit@ and
checked to be no worse than linear. This is the guard against the accidentally
quadratic class of regression — a fold that becomes @O(n^2)@ in version count —
which a single-size timing would never reveal.

Unlike a perf-regression comparison (machine-dependent, noisy, never gated), an
algorithmic-class assertion is a real correctness signal: a packument merge or rule
sweep going quadratic in version count is a bug, not a slow machine. A failure here
is therefore a genuine benchmark failure (a non-zero exit), which is the one red
state the benchmark workflow recognises.

Two variants: 'notWorseThanLinear' for a pure operation, 'notWorseThanLinearIO' for
one whose result is computed in 'IO' (the rule engine evaluates effectfully).
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

{- | The exponent ceiling a fitted complexity must stay under to pass. Set above
linear and linearithmic (whose fitted variable power is @1@) and below quadratic
(@2@), so a linear or @n log n@ growth passes while an accidentally-quadratic one
is flagged. The slack also absorbs the measurement noise a real fit carries.
-}
linearCeiling :: Double
linearCeiling = 1.5

{- | Assert that a pure operation's running time grows no worse than linearly in the
input size. The size-to-input function runs once per measured size (its result is
shared across the iterations at that size), so only the operation — not the input
construction — is fitted. The operation is summarised to a forced 'Int' so the whole
result is evaluated.
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

{- | Like 'notWorseThanLinear', but for an operation whose result is computed in
'IO' — the rule engine prepares rules once, then evaluates each version effectfully,
so the per-request sweep is an 'IO' action. The action is run per measurement and its
'Int' result forced.
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
range, sharing each size's input across iterations so construction is not folded into
the fit.
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

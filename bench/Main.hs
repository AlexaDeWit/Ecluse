{- | The Écluse benchmark entry point: the work-per-request (Layer A) micro-benches
over the pure @ecluse-core@ hot paths, the version-count complexity assertions, and
the synthetic-corpus generator's correctness tests, all in one @tasty@ tree.

@tasty-bench@ reports time and — under @+RTS -T@, baked into the component's RTS
options — allocated bytes for each bench. Allocations are the machine-independent
signal the baseline tracks; time is informational. See
@docs\/architecture\/performance.md@.

The generator tests and the complexity assertions are ordinary @tasty@ test cases
mixed into the same tree, so a malformed corpus or an accidentally-quadratic hot
path fails the run (a non-zero exit) — the one red state this harness recognises.
-}
module Main (main) where

import Data.Aeson (Value (Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Bench.Corpus (
    benchPackageName,
    benchPackageText,
    projectInfo,
    syntheticPackumentBytes,
    syntheticPackumentValue,
    syntheticProxyBase,
    versionKeysOf,
 )
import Ecluse.Core.MergeBench qualified as MergeBench
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Registry.Npm.Filter (rewriteTarballUrls)
import Ecluse.Core.Registry.Npm.Wire (Packument (pkmtVersions))
import Ecluse.Core.RouteBench qualified as RouteBench
import Ecluse.Core.RulesBench qualified as RulesBench
import Ecluse.Core.SecurityBench qualified as SecurityBench
import Ecluse.Core.ServeBench qualified as ServeBench
import Ecluse.Core.VersionBench qualified as VersionBench
import Ecluse.Core.WireBench qualified as WireBench
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench (bgroup, defaultMain)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
    defaultMain
        [ bgroup
            "ecluse-core (work-per-request)"
            [ WireBench.benchmarks
            , VersionBench.benchmarks
            , RouteBench.benchmarks
            , RulesBench.benchmarks
            , MergeBench.benchmarks
            , ServeBench.benchmarks
            , SecurityBench.benchmarks
            ]
        , generatorTests
        ]

{- | Correctness tests for the synthetic packument generator, run as part of the
benchmark so a broken corpus stops the run rather than silently benching a degenerate
input. They pin the invariants the scaled benches rely on: the requested version
count is produced, it survives a wire decode and a projection intact, and every
tarball URL is rewritten onto the proxy origin.
-}
generatorTests :: TestTree
generatorTests =
    testGroup
        "synthetic packument generator"
        [ testCase "yields the requested version count" $
            length (versionKeysOf (syntheticPackumentValue sampleCount)) @?= sampleCount
        , testCase "decodes with every version preserved" $
            case eitherDecodeStrict (syntheticPackumentBytes sampleCount) :: Either String Packument of
                Left err -> assertFailure ("synthetic packument did not decode: " <> err)
                Right packument -> Map.size (pkmtVersions packument) @?= sampleCount
        , testCase "projects with every version preserved" $
            Map.size (infoVersions (projectInfo benchPackageName (syntheticPackumentValue sampleCount)))
                @?= sampleCount
        , testCase "rewrites every tarball onto the proxy origin" $ do
            let urls = tarballUrlsOf (rewriteTarballUrls syntheticProxyBase (syntheticPackumentValue sampleCount))
            length urls @?= sampleCount
            assertBool
                "every rewritten tarball should sit under the proxy origin"
                (all (rewrittenPrefix `T.isPrefixOf`) urls)
        ]
  where
    sampleCount :: Int
    sampleCount = 500

    rewrittenPrefix :: Text
    rewrittenPrefix = syntheticProxyBase <> "/" <> benchPackageText <> "/-/"

{- | Every @dist.tarball@ URL in a packument value, in @versions@-object order — used
to confirm the serve-time rewrite reached each version.
-}
tarballUrlsOf :: Value -> [Text]
tarballUrlsOf value =
    [ url
    | Object top <- [value]
    , Just (Object versions) <- [KeyMap.lookup "versions" top]
    , (_, versionValue) <- KeyMap.toList versions
    , Object versionObject <- [versionValue]
    , Just (Object dist) <- [KeyMap.lookup "dist" versionObject]
    , Just (String url) <- [KeyMap.lookup "tarball" dist]
    ]

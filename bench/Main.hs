-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Run work-per-request benchmarks and generator contracts for every registered ecosystem.
Cache and stream measurements share no format-specific inputs and run once.
-}
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Bench.Corpus (benchEvalContext, syntheticInput)
import Ecluse.Core.CacheBench qualified as CacheBench
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.MergeBench qualified as MergeBench
import Ecluse.Core.Package (artUrl, infoVersions, pkgArtifacts)
import Ecluse.Core.RouteBench qualified as RouteBench
import Ecluse.Core.RulesBench qualified as RulesBench
import Ecluse.Core.SecurityBench qualified as SecurityBench
import Ecluse.Core.SelectiveBench qualified as SelectiveBench
import Ecluse.Core.ServeBench qualified as ServeBench
import Ecluse.Core.StreamBench qualified as StreamBench
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.VersionBench qualified as VersionBench
import Ecluse.Core.WireBench qualified as WireBench
import Ecluse.Test.Corpus (syntheticProxyBase)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), ecosystemBenches)
import Ecluse.Test.Server.Transform (serveDocumentBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench (bgroup, defaultMain)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = do
    ecosystems <- ecosystemBenches
    cacheBenchmarks <- CacheBench.benchmarks
    defaultMain
        [ bgroup
            "ecluse-core (work-per-request)"
            (map ecosystemGroup ecosystems <> [StreamBench.benchmarks, cacheBenchmarks])
        , testGroup "synthetic generators" (map generatorTests ecosystems)
        ]

ecosystemGroup :: EcosystemBench -> TestTree
ecosystemGroup ecosystem =
    bgroup
        ("ecosystem: " <> toString (ecosystemName (ebEcosystem ecosystem)))
        [ group ecosystem
        | group <-
            [ RouteBench.benchmarks
            , WireBench.benchmarks
            , SelectiveBench.benchmarks
            , VersionBench.benchmarks
            , RulesBench.benchmarks
            , MergeBench.benchmarks
            , ServeBench.benchmarks
            , SecurityBench.benchmarks
            ]
        ]

generatorTests :: EcosystemBench -> TestTree
generatorTests ecosystem =
    testGroup
        (toString (ecosystemName (ebEcosystem ecosystem)))
        [ testCase "decodes every generated release" $ do
            versions <- expectRight (ebDecode ecosystem name raw)
            length versions @?= sampleCount
        , testCase "projects every measured synthetic size" $
            for_ [1, 32, 500, 2000, 4096, 8192] $ \count -> do
                (info, _) <- expectRight (ebProject ecosystem name (ebSynthetic ecosystem count))
                Map.size (infoVersions info) @?= count
        , testCase "selective projection agrees with the full release" $ do
            (info, _) <- expectRight (ebProject ecosystem name raw)
            for_ (Map.keys (infoVersions info)) $ \key -> do
                selected <- expectRight (ebSelective ecosystem name (mkVersion (ebEcosystem ecosystem) key) raw)
                selected @?= Map.lookup key (infoVersions info)
        , testCase "rewrites every artifact onto the proxy origin" $ do
            input@(_, original) <- expectRight (syntheticInput ecosystem sampleCount)
            bytes <- serveDocumentBytes (ebMetadata ecosystem) benchEvalContext input
            (served, _) <- expectRight (ebProject ecosystem name bytes)
            let artifacts info = concatMap (toList . pkgArtifacts) (Map.elems (infoVersions info))
            Map.size (infoVersions served) @?= sampleCount
            length (artifacts served) @?= length (artifacts original)
            assertBool
                "every artifact URL uses the proxy origin"
                (all (((syntheticProxyBase <> "/") `T.isPrefixOf`) . artUrl) (artifacts served))
        , testCase "prepares the large wire-guard input" $ do
            document <- expectRight (ebReadDocument ecosystem (ebSynthetic ecosystem 100000))
            ebNestingDepth ecosystem document @?= 1
        ]
  where
    sampleCount = 500
    name = ebSyntheticName ecosystem
    raw = ebSynthetic ecosystem sampleCount

expectRight :: (Show err) => Either err value -> IO value
expectRight = either (assertFailure . show) pure

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed project reads include state validation, restoration, and packed listing rendering.
module Ecluse.Core.MirrorReadBench (benchmarks) where

import Data.ByteString qualified as BS
import Ecluse.Test.Mirror.Read
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | Cold reads and changed revisions start with independent retention stores.
benchmarks :: IO Benchmark
benchmarks = do
    fixture <- makeReadFixture 1000
    warm <- newReadSession fixture
    void (readManagedProject warm)
    pure $
        bgroup
            "ecosystem: pypi"
            [ bgroup
                "managed mirror read: 1000 versions, three files per version"
                [ bench "cold record and listing" (whnfIO (newReadSession fixture >>= render))
                , bench "warm conditional 304 and listing" (whnfIO (render warm))
                , bench "cold listing then changed revision adding a file" (whnfIO (changed fixture))
                ]
            ]
  where
    render session = BS.length . readBody <$> readManagedProject session
    changed fixture = do
        session <- newReadSession fixture
        before <- render session
        advanceReadSession session
        after <- render session
        pure (before + after)

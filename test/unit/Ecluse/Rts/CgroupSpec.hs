-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Contracts for cgroup limits and memory-use parsing.
module Ecluse.Rts.CgroupSpec (spec) where

import Test.Hspec

import Ecluse.Rts.Cgroup (parseCpuMax, parseInactiveFile, parseMemoryMax, usePermille)

spec :: Spec
spec = cgroupParsingSpec

mib :: Int
mib = 1024 * 1024

cgroupParsingSpec :: Spec
cgroupParsingSpec = describe "cgroup v2 parsing" $ do
    it "reads memory use less reclaimable page cache in thousandths of the limit" $ do
        usePermille (1000 * mib) (Just (100 * mib)) (600 * mib) `shouldBe` 500
        usePermille (1000 * mib) Nothing (600 * mib) `shouldBe` 600
        usePermille (1000 * mib) (Just (700 * mib)) (600 * mib) `shouldBe` 0

    it "reads cpu.max quota over period as granted cores" $ do
        parseCpuMax "200000 100000\n" `shouldBe` Just 2.0
        parseCpuMax "50000 100000" `shouldBe` Just 0.5

    it "reads the cpu.max unlimited sentinel as no limit" $
        parseCpuMax "max 100000\n" `shouldBe` Nothing

    it "infers no cpu limit from a malformed body" $ do
        parseCpuMax "" `shouldBe` Nothing
        parseCpuMax "banana" `shouldBe` Nothing
        parseCpuMax "-100000 100000" `shouldBe` Nothing

    it "reads inactive_file from a memory.stat body" $ do
        parseInactiveFile "anon 100\nfile 200\nactive_file 50\ninactive_file 150\n" `shouldBe` Just 150
        parseInactiveFile "anon 100\n" `shouldBe` Nothing

    it "reads memory.max bytes and the unlimited sentinel" $ do
        parseMemoryMax "536870912\n" `shouldBe` Just (512 * mib)
        parseMemoryMax "max\n" `shouldBe` Nothing
        parseMemoryMax "much" `shouldBe` Nothing

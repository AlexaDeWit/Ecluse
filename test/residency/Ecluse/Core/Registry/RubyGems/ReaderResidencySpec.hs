-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native selected reads drop other releases and full refusals release their accumulated state.
module Ecluse.Core.Registry.RubyGems.ReaderResidencySpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry.Json.WalkProbe (allowance, heldReading)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (..), readGemInfoCharged)
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Retention (boundedRetention)
import Ecluse.Test.Corpus.RubyGems (benchGem, syntheticInfoBytes)
import Ecluse.Test.Registry.JsonStream (testTable)

spec :: Spec
spec = describe "RubyGems reader live bytes" $ do
    it (caseLabel "retention/rubygems/selected/other-releases" "drops distinct other releases during selected reads") $
        boundedRetention allowance (heldFor (SelectedRead (mkVersion RubyGems "1.0.0")) defaultLimits)
    it (caseLabel "retention/rubygems/full/past-limit" "releases state after the full read reaches its version limit") $
        boundedRetention allowance (heldFor FullRead defaultLimits{maxVersionCount = 2})
    for_ ["1.0.0", "2.0.0"] $ \version ->
        it ("keeps a growing native row within its observed reservation: " <> toString version) $
            for_ [8192, 256 * 1024, 2 * 1024 * 1024] $ \size -> do
                peak <- newIORef (0 :: Int)
                let pay cost = modifyIORef' peak (max cost)
                    input = encodeUtf8 ("---\n" <> version <> " |checksum:" <> T.replicate 64 "0" <> ",ecluse_benchmark:" <> T.replicate size "x" <> "\n")
                held <- heldReading (\bound -> readGemInfoCharged pay defaultLimits benchGem "https://rubygems.org" (SelectedRead (mkVersion RubyGems "1.0.0")) bound (testTable [])) input
                paid <- readIORef peak
                paid `shouldSatisfy` (>= size)
                held `shouldSatisfy` (<= toInteger paid + allowance)

heldFor :: GemRead -> Limits -> Int -> IO Integer
heldFor mode limits count = do
    peak <- newIORef (0 :: Int)
    heldReading (\bound -> readGemInfoCharged (modifyIORef' peak . max) limits benchGem "https://rubygems.org" mode bound (testTable [])) (syntheticInfoBytes count)

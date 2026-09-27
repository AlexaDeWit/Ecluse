-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Rts.SamplerSpec (spec) where

import Test.Hspec

import Ecluse.Rts.Sampler (parseByteCount, parseInactiveFile)

-- | The cgroup file parsing behind the sampler's kernel view.
spec :: Spec
spec = do
    describe "parseByteCount" $ do
        it "reads a memory.current body" $
            parseByteCount "268435456\n" `shouldBe` Just 268435456

        it "refuses a malformed or negative body" $ do
            parseByteCount "max\n" `shouldBe` Nothing
            parseByteCount "-1\n" `shouldBe` Nothing

    describe "parseInactiveFile" $ do
        it "reads the inactive_file line of a memory.stat body" $
            parseInactiveFile "anon 1000\nfile 5000\nactive_file 2000\ninactive_file 3000\n" `shouldBe` Just 3000

        it "reports nothing when the line is absent" $
            parseInactiveFile "anon 1000\nfile 5000\n" `shouldBe` Nothing

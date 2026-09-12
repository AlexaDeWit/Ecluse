-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE MagicHash #-}

module Ecluse.Core.FaultSpec (spec) where

import Data.Array.Byte (ByteArray (..))
import Data.Text qualified as T
import Data.Text.Internal qualified as Text
import GHC.Exts (Int (I#), sizeofByteArray#)
import Test.Hspec

import Ecluse.Core.Fault (boundedDetail)

spec :: Spec
spec = describe "boundedDetail" $ do
    it "owns only the bounded ASCII allocation" $ do
        let backing = T.replicate 65536 "x"
        textAllocationBytes (boundedDetail backing) `shouldSatisfy` (<= 512)

    it "keeps the first 512 ASCII characters" $
        boundedDetail (T.replicate 1024 "a") `shouldBe` T.replicate 512 "a"

    it "keeps the first 512 multibyte characters" $
        boundedDetail (T.replicate 1024 "\x1f600") `shouldBe` T.replicate 512 "\x1f600"

    it "keeps a short detail" $
        boundedDetail "connection refused" `shouldBe` "connection refused"

    it "keeps an empty detail" $
        boundedDetail "" `shouldBe` ""

textAllocationBytes :: Text -> Int
textAllocationBytes (Text.Text (ByteArray array) _ _) = I# (sizeofByteArray# array)

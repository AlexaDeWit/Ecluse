-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Fault.RetryAfterSpec (spec) where

import Data.ByteString qualified as BS
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Fault.RetryAfter (parseRetryAfter)

spec :: Spec
spec = describe "parseRetryAfter" $ do
    it "reads positive delta-seconds and optional HTTP whitespace" $ do
        parse (Just "120") `shouldBe` Just (RetryAfter 120)
        parse (Just " \t000120\t ") `shouldBe` Just (RetryAfter 120)

    it "clamps at the caller's cap" $ do
        parse (Just "599") `shouldBe` Just (RetryAfter 599)
        parse (Just "600") `shouldBe` Just (RetryAfter 600)
        parse (Just "601") `shouldBe` Just (RetryAfter 600)
        parseRetryAfter now (RetryAfter 1) (Just "120") `shouldBe` Just (RetryAfter 1)

    it "bounds numeric conversion for long decimal values" $ do
        parse (Just (BS.replicate 10000 57)) `shouldBe` Just (RetryAfter 600)
        parse (Just (BS.replicate 10000 48 <> "1")) `shouldBe` Just (RetryAfter 1)
        parse (Just (BS.replicate 10000 57 <> "x")) `shouldBe` Nothing
        parse (Just (encodeUtf8 (show (maxBound :: Int)))) `shouldBe` Just (RetryAfter 600)

    it "treats missing, zero, and malformed hints as absent" $
        for_ [Nothing, Just "", Just "0", Just "000", Just "-1", Just "+1", Just "1.5", Just "0x10", Just "1 2", Just "1\n", Just "soon"] $ \raw ->
            parse raw `shouldBe` Nothing

    it "rejects nonpositive caller caps" $
        for_ [0, -1] $ \cap ->
            parseRetryAfter now (RetryAfter cap) (Just "120") `shouldBe` Nothing

    it "reads each date form relative to the supplied clock" $
        for_ ["Sun, 06 Nov 1994 08:49:37 GMT", "Sunday, 06-Nov-94 08:49:37 GMT", "Sun Nov  6 08:49:37 1994"] $ \raw ->
            parse (Just raw) `shouldBe` Just (RetryAfter 37)

    it "rounds a positive fractional remainder up" $ do
        parseRetryAfter (addUTCTime 36.5 now) (RetryAfter 600) (Just "Sun, 06 Nov 1994 08:49:37 GMT") `shouldBe` Just (RetryAfter 1)
        parseRetryAfter (addUTCTime 35.5 now) (RetryAfter 1) (Just "Sun, 06 Nov 1994 08:49:37 GMT") `shouldBe` Just (RetryAfter 1)

    it "ignores dates at or before the supplied clock" $ do
        parse (Just "Sun, 06 Nov 1994 08:49:00 GMT") `shouldBe` Nothing
        parse (Just "Sun, 06 Nov 1994 08:48:59 GMT") `shouldBe` Nothing

    it "caps a date far in the future" $
        parse (Just "Tue, 01 Sep 2026 00:00:00 GMT") `shouldBe` Just (RetryAfter 600)
  where
    parse = parseRetryAfter now (RetryAfter 600)

now :: UTCTime
now = UTCTime (fromGregorian 1994 11 6) (8 * 3600 + 49 * 60)

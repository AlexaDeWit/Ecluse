-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Text.HttpDateSpec (spec) where

import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Osv.Provenance qualified as Provenance
import Ecluse.Core.Text.HttpDate (parseHttpDate, parseHttpDateAt)

spec :: Spec
spec = do
    describe "parseHttpDate" $
        it "retains the OSV re-export and surrounding whitespace behaviour" $ do
            let raw = " \tSun, 06 Nov 1994 08:49:37 GMT\n"
            parseHttpDate raw `shouldBe` Just example
            Provenance.parseHttpDate raw `shouldBe` parseHttpDate raw

    describe "parseHttpDateAt" $ do
        it "accepts the three RFC 9110 examples" $
            for_ ["Sun, 06 Nov 1994 08:49:37 GMT", "Sunday, 06-Nov-94 08:49:37 GMT", "Sun Nov  6 08:49:37 1994"] $ \raw ->
                parseHttpDateAt now raw `shouldBe` Just example

        it "accepts a two-digit asctime day" $
            parseHttpDateAt now "Sun Nov 06 08:49:37 1994" `shouldBe` Just example

        it "keeps a two-digit year exactly fifty years ahead" $ do
            let future = UTCTime (fromGregorian 2076 1 1) 0
            parseHttpDateAt now (obsolete future) `shouldBe` Just future

        it "interprets a date beyond fifty years as the most recent matching past year" $ do
            let past = UTCTime (fromGregorian 1976 1 1) 1
            parseHttpDateAt now (obsolete past) `shouldBe` Just past

        it "resolves a two-digit year across the next century" $ do
            let laterNow = UTCTime (fromGregorian 2090 1 1) 0
                future = UTCTime (fromGregorian 2120 1 1) 0
            parseHttpDateAt laterNow (obsolete future) `shouldBe` Just future

        it "rejects malformed dates and non-HTTP timezone forms" $
            for_
                [ "Sun, 31 Feb 1994 08:49:37 GMT"
                , "Sun, 06 Nov 1994 08:49:37 UTC"
                , "Sun, 06 Nov 1994 08:49:37 +0000"
                , "sun, 06 Nov 1994 08:49:37 GMT"
                , "Sun, 06 Nov 1994 08:49:37 GMT trailing"
                , "Sun, 06 Nov 1994 25:49:37 GMT"
                , "Sunday, 06-Nov-1994 08:49:37 GMT"
                , "Sun Nov  6 08:49:37"
                , "Sat, 01 Jan 10000 00:00:00 GMT"
                , "Sat Jan  1 00:00:00 10000"
                , "whenever"
                ]
                (\raw -> parseHttpDateAt now raw `shouldBe` Nothing)

        it "uses the supplied clock at the fifty-year boundary" $ do
            let past = UTCTime (fromGregorian 1976 1 1) 1
                future = UTCTime (fromGregorian 2076 1 1) 1
            parseHttpDateAt (addUTCTime 1 now) (obsolete future) `shouldBe` Just future
            parseHttpDateAt now (obsolete past) `shouldBe` Just past

now :: UTCTime
now = UTCTime (fromGregorian 2026 1 1) 0

example :: UTCTime
example = UTCTime (fromGregorian 1994 11 6) (8 * 3600 + 49 * 60 + 37)

obsolete :: UTCTime -> Text
obsolete = toText . formatTime defaultTimeLocale "%A, %d-%b-%y %H:%M:%S GMT"

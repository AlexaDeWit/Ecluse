-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Osv.EpssSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Network.HTTP.Types.Status (status200)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)

import Ecluse.Core.Osv.Epss (
    EpssFeedEmpty (..),
    EpssFeedTooLarge (..),
    epssForIds,
    epssScoreCount,
    fetchEpssScores,
    maxEpssFeedBytes,
    mkEpssScores,
    parseEpssLine,
 )
import Ecluse.Test.Osv (runOsvTestM)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

-- The feed's own preamble, in the shape FIRST.org publishes: a metadata comment, then a header.
feedPreamble :: LByteString
feedPreamble = "#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000\ncve,epss,percentile\n"

-- Serve a gzipped feed body and fetch it back through the real HTTP and ungzip path.
fetchServed :: Int -> LByteString -> IO Int
fetchServed cap body =
    withStub status200 (GZip.compress body) $ \stub ->
        runOsvTestM (epssScoreCount <$> fetchEpssScores cap (toString (stubBaseUrl stub) <> "/epss.csv.gz"))

spec :: Spec
spec = do
    describe "parseEpssLine" $ do
        it "reads the cve id and its probability, ignoring the percentile column" $
            parseEpssLine "CVE-2026-10001,0.875,0.99500" `shouldBe` Just ("CVE-2026-10001", 0.875)

        it "tolerates surrounding whitespace, which a hand-edited feed can carry" $
            parseEpssLine " CVE-2026-10001 , 0.875 ,0.99500" `shouldBe` Just ("CVE-2026-10001", 0.875)

        it "drops the feed's comment and header lines" $ do
            parseEpssLine "#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000" `shouldBe` Nothing
            parseEpssLine "cve,epss,percentile" `shouldBe` Nothing

        it "drops a row whose score is not a number" $
            parseEpssLine "CVE-2026-10001,not-a-number,0.5" `shouldBe` Nothing

        it "drops a score outside the probability range, which cannot be an EPSS value" $ do
            parseEpssLine "CVE-2026-10001,1.5,0.5" `shouldBe` Nothing
            parseEpssLine "CVE-2026-10001,-0.5,0.5" `shouldBe` Nothing

        it "drops a truncated row and an empty id" $ do
            parseEpssLine "CVE-2026-10001" `shouldBe` Nothing
            parseEpssLine ",0.5,0.5" `shouldBe` Nothing
            parseEpssLine "" `shouldBe` Nothing

    describe "the score table" $ do
        it "matches an identifier whatever its case, so a case difference cannot miss the join" $
            epssForIds (mkEpssScores [("cve-2026-10001", 0.5)]) ["CVE-2026-10001"] `shouldBe` Just 0.5

        it "keeps the higher score when the feed repeats an id" $
            epssForIds (mkEpssScores [("CVE-2026-10001", 0.25), ("CVE-2026-10001", 0.75)]) ["CVE-2026-10001"]
                `shouldBe` Just 0.75

        it "takes the highest score among the identifiers asked for" $
            epssForIds (mkEpssScores [("CVE-A", 0.25), ("CVE-B", 0.75)]) ["CVE-A", "CVE-B", "CVE-C"]
                `shouldBe` Just 0.75

        it "yields nothing when the table scores none of them" $ do
            epssForIds (mkEpssScores [("CVE-A", 0.25)]) ["GHSA-only", "CVE-B"] `shouldBe` Nothing
            epssForIds (mkEpssScores []) ["CVE-A"] `shouldBe` Nothing

    describe "fetchEpssScores" $ do
        it "fetches, decompresses, and decodes a served feed" $
            fetchServed maxEpssFeedBytes (feedPreamble <> "CVE-2026-10001,0.875,0.995\nCVE-2026-10002,0.5,0.900\n")
                `shouldReturn` 2

        it "keeps the pass going past an unreadable row" $
            fetchServed maxEpssFeedBytes (feedPreamble <> "CVE-2026-BAD01,not-a-number,0.1\nCVE-2026-10002,0.5,0.900\n")
                `shouldReturn` 1

        it "refuses a feed that decompresses past the cap, rather than truncating it" $
            -- A compression bomb: a truncated table would read downstream as unscored, which
            -- denies every affected version under an EPSS rule.
            fetchServed 4096 (toLazy (BS.replicate 65536 0x78))
                `shouldThrow` (\case DecompressedTooLarge cap seen -> cap == 4096 && seen > 4096; _ -> False)

        it "refuses a served stream past the cap before decompressing it" $
            -- The bound upstream of gzip. Without it an endless stream of empty gzip members
            -- never grows the decompressed count, and the pass never terminates.
            fetchServed 32 (feedPreamble <> "CVE-2026-10001,0.875,0.995\n")
                `shouldThrow` (\case CompressedTooLarge cap seen -> cap == 32 && seen > 32; _ -> False)

        it "refuses a feed that decodes to no scores at all" $ do
            -- A 200 carrying an error page, and a feed whose rows the decode no longer reads.
            -- Either would publish an all-unscored artifact, which denies every affected version.
            fetchServed maxEpssFeedBytes "<html><body>service unavailable</body></html>"
                `shouldThrow` (== EpssFeedEmpty)
            fetchServed maxEpssFeedBytes feedPreamble `shouldThrow` (== EpssFeedEmpty)

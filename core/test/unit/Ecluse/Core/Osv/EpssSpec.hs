-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.EpssSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (AsyncException (ThreadKilled))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Streaming.Zlib (ZlibException (ZlibException))
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException), HttpExceptionContent (..), defaultRequest)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hLastModified)
import Network.HTTP.Types.Status (Status, status200, status404)
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

import Ecluse.Core.Fault (TransportCause (TransportProtocol, TransportTimeout, TransportUnreachable))
import Ecluse.Core.Osv.Epss (
    EpssEnrichment (..),
    EpssFeed (..),
    EpssFeedEmpty (..),
    EpssFeedFailure (..),
    EpssFeedTooLarge (..),
    EpssFeedTruncated (..),
    EpssPreamble (..),
    acquireEpssFeed,
    classifyEpssFailure,
    enrichmentStatus,
    epssForIds,
    epssScoreCount,
    fetchEpssScores,
    maxEpssFeedBytes,
    mkEpssScores,
    parseEpssLine,
    parseEpssPreamble,
    resolveEnrichment,
 )
import Ecluse.Core.Osv.Schema (EpssRequirement (..), EpssStatus (..))
import Ecluse.Test.Osv (runOsvTestM)
import Ecluse.Test.Stub (allCaptured, stubBaseUrl, withStub, withStubHeaders)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- The feed's own preamble, in the shape FIRST.org publishes: a metadata comment, then a header.
feedPreamble :: LByteString
feedPreamble = "#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000\ncve,epss,percentile\n"

-- Serve a gzipped feed body and fetch it back through the real HTTP and ungzip path.
fetchServed :: Int -> LByteString -> IO Int
fetchServed cap body = epssScoreCount . efScores <$> fetchFeed cap body

fetchFeed :: Int -> LByteString -> IO EpssFeed
fetchFeed = fetchFeedWith []

fetchFeedWith :: [Header] -> Int -> LByteString -> IO EpssFeed
fetchFeedWith extraHeaders cap body = fetchRaw extraHeaders cap (GZip.compress body)

-- Fetch bytes exactly as served, so a case can damage the gzip stream itself.
fetchRaw :: [Header] -> Int -> LByteString -> IO EpssFeed
fetchRaw extraHeaders cap served =
    withStubHeaders status200 extraHeaders served $ \stub ->
        runOsvTestM (fetchEpssScores cap (toString (stubBaseUrl stub) <> "/epss.csv.gz"))

-- One attempt under the retry policy, with the number of requests the stub saw.
acquireServed :: Int -> Status -> LByteString -> IO (Either EpssFeedFailure EpssFeed, Int)
acquireServed cap status served =
    withStub status served $ \stub -> do
        outcome <- runOsvTestM (acquireEpssFeed cap (toString (stubBaseUrl stub) <> "/epss.csv.gz"))
        requests <- length <$> allCaptured stub
        pure (outcome, requests)

-- Four thousand rows over two CVEs behind the preamble, as one gzip member.
wholeFeed :: LByteString
wholeFeed = GZip.compress (feedPreamble <> mconcat (replicate 2000 "CVE-2026-10001,0.875,0.995\nCVE-2026-10002,0.5,0.900\n"))

-- A request-scoped client failure, as http-client wraps one.
requestFailure :: HttpExceptionContent -> SomeException
requestFailure = toException . HttpExceptionRequest defaultRequest

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
            fetchServed 4096 (toLazy (BS.replicate 65536 0x78))
                `shouldThrow` (\case DecompressedTooLarge cap seen -> cap == 4096 && seen > 4096; _ -> False)

        it "refuses a served stream past the cap before decompressing it" $
            -- The bound upstream of gzip. Without it an endless stream of empty gzip members
            -- never grows the decompressed count, and the pass never terminates.
            fetchServed 32 (feedPreamble <> "CVE-2026-10001,0.875,0.995\n")
                `shouldThrow` (\case CompressedTooLarge cap seen -> cap == 32 && seen > 32; _ -> False)

        it "carries the preamble's score date and model version onto the fetched feed" $ do
            feed <- fetchFeed maxEpssFeedBytes (feedPreamble <> "CVE-2026-10001,0.875,0.995\n")
            efScoreDate feed `shouldBe` Just (UTCTime (fromGregorian 2026 8 29) 0)
            efModelVersion feed `shouldBe` Just "v2026.08.01"

        it "records the Last-Modified of the response that carried the rows" $ do
            feed <- fetchFeedWith [(hLastModified, "Sat, 29 Aug 2026 06:30:00 GMT")] maxEpssFeedBytes (feedPreamble <> "CVE-2026-10001,0.875,0.995\n")
            efLastModified feed `shouldBe` Just (UTCTime (fromGregorian 2026 8 29) (secondsToDiffTime 23400))

        it "records no feed date when the response carries no Last-Modified" $ do
            feed <- fetchFeed maxEpssFeedBytes (feedPreamble <> "CVE-2026-10001,0.875,0.995\n")
            efLastModified feed `shouldBe` Nothing

        it "loads the scores of a feed with no preamble, recording no date and no model" $ do
            feed <- fetchFeed maxEpssFeedBytes "cve,epss,percentile\nCVE-2026-10001,0.875,0.995\n"
            epssScoreCount (efScores feed) `shouldBe` 1
            efScoreDate feed `shouldBe` Nothing
            efModelVersion feed `shouldBe` Nothing

        it "refuses a feed that decodes to no scores at all" $ do
            -- A 200 carrying an error page, and a feed whose rows the decode no longer reads.
            fetchServed maxEpssFeedBytes "<html><body>service unavailable</body></html>"
                `shouldThrow` (== EpssFeedEmpty)
            fetchServed maxEpssFeedBytes feedPreamble `shouldThrow` (== EpssFeedEmpty)

        it "passes a whole gzip stream" $
            epssScoreCount . efScores <$> fetchRaw [] maxEpssFeedBytes wholeFeed `shouldReturn` 2

        it "refuses a gzip stream cut in half, whose rows would read as a short table" $
            fetchRaw [] maxEpssFeedBytes (LBS.take (LBS.length wholeFeed `div` 2) wholeFeed)
                `shouldThrow` (== EpssFeedTruncated)

        it "refuses a gzip stream that lacks its trailer" $
            fetchRaw [] maxEpssFeedBytes (LBS.take (LBS.length wholeFeed - 8) wholeFeed)
                `shouldThrow` (== EpssFeedTruncated)

        it "refuses an empty body, which is no gzip stream at all" $
            fetchRaw [] maxEpssFeedBytes "" `shouldThrow` (== EpssFeedTruncated)

        it "reads every member of a multi-member stream, as gunzip does" $
            epssScoreCount . efScores <$> fetchRaw [] maxEpssFeedBytes (GZip.compress (feedPreamble <> "CVE-2026-10001,0.875,0.995\n") <> GZip.compress "CVE-2026-10002,0.5,0.900\n")
                `shouldReturn` 2

        it "refuses a stream whose second member is cut" $
            fetchRaw [] maxEpssFeedBytes (wholeFeed <> LBS.take (LBS.length wholeFeed `div` 2) wholeFeed)
                `shouldThrow` (== EpssFeedTruncated)

    describe "acquireEpssFeed" $ do
        it "returns the fetched feed after one request" $ do
            (outcome, requests) <- acquireServed maxEpssFeedBytes status200 wholeFeed
            fmap (epssScoreCount . efScores) outcome `shouldBe` Right 2
            requests `shouldBe` 1

        for_
            [ ("a 404", maxEpssFeedBytes, status404, "", EpssFeedStatus 404)
            , ("a scoreless feed", maxEpssFeedBytes, status200, GZip.compress feedPreamble, EpssFeedNoScores)
            , ("a stream that is not gzip", maxEpssFeedBytes, status200, "not gzip", EpssFeedUndecodable)
            , ("a cut stream", maxEpssFeedBytes, status200, LBS.take (LBS.length wholeFeed `div` 2) wholeFeed, EpssFeedUndecodable)
            , ("a stream with its second member cut", maxEpssFeedBytes, status200, wholeFeed <> LBS.take (LBS.length wholeFeed `div` 2) wholeFeed, EpssFeedUndecodable)
            , ("a whole member followed by trailing bytes", maxEpssFeedBytes, status200, wholeFeed <> "trailing bytes", EpssFeedUndecodable)
            , ("a served stream past the ceiling", 32, status200, wholeFeed, EpssFeedOversize (CompressedTooLarge 32 0))
            , ("an expansion past the ceiling", 4096, status200, GZip.compress (toLazy (BS.replicate 65536 0x78)), EpssFeedOversize (DecompressedTooLarge 4096 0))
            ]
            $ \(label, cap, status, served, expected) ->
                it ("returns " <> label <> " as a failure after a single request") $ do
                    (outcome, requests) <- acquireServed cap status served
                    first withoutSeen outcome `shouldBe` Left expected
                    requests `shouldBe` 1

        it "propagates an invalid feed URL, a configuration fault rather than an outage" $
            runOsvTestM (acquireEpssFeed maxEpssFeedBytes "not a url")
                `shouldThrow` (\case InvalidUrlException{} -> True; _ -> False)

    describe "classifyEpssFailure" $ do
        it "reads a transport failure through its shared cause" $ do
            classifyEpssFailure (requestFailure ConnectionTimeout) `shouldBe` Just (EpssFeedTransport TransportTimeout)
            classifyEpssFailure (requestFailure ResponseTimeout) `shouldBe` Just (EpssFeedTransport TransportTimeout)
            classifyEpssFailure (requestFailure (ConnectionFailure (toException (TestContractEscape "refused")))) `shouldBe` Just (EpssFeedTransport TransportUnreachable)
            classifyEpssFailure (requestFailure ConnectionClosed) `shouldBe` Just (EpssFeedTransport TransportUnreachable)

        it "tolerates every other request failure http-client reports" $
            for_ [TooManyRedirects [], OverlongHeaders, InvalidStatusLine "bad", InvalidHeader "bad", InvalidChunkHeaders, IncompleteHeaders, InvalidDestinationHost "bad", TlsNotSupported] $ \content ->
                classifyEpssFailure (requestFailure content) `shouldBe` Just (EpssFeedTransport TransportProtocol)

        it "names the feed's own failures" $ do
            classifyEpssFailure (toException (CompressedTooLarge 1 2)) `shouldBe` Just (EpssFeedOversize (CompressedTooLarge 1 2))
            classifyEpssFailure (toException (DecompressedTooLarge 1 2)) `shouldBe` Just (EpssFeedOversize (DecompressedTooLarge 1 2))
            classifyEpssFailure (toException EpssFeedEmpty) `shouldBe` Just EpssFeedNoScores
            classifyEpssFailure (toException EpssFeedTruncated) `shouldBe` Just EpssFeedUndecodable

        it "reads every gzip error as an undecodable feed" $
            for_ [-2, -3, -4, -5] $ \code ->
                classifyEpssFailure (toException (ZlibException code)) `shouldBe` Just EpssFeedUndecodable

        it "leaves an invalid URL, a cancellation, and an unnamed fault to propagate" $ do
            classifyEpssFailure (toException (InvalidUrlException "bad source" "invalid")) `shouldBe` Nothing
            classifyEpssFailure (toException ThreadKilled) `shouldBe` Nothing
            classifyEpssFailure (toException (TestContractEscape "a decode bug")) `shouldBe` Nothing
            classifyEpssFailure (toException (mkIOError doesNotExistErrorType "a local file" Nothing Nothing)) `shouldBe` Nothing

    describe "resolveEnrichment" $ do
        let feed = EpssFeed (mkEpssScores [("CVE-A", 0.5)]) Nothing Nothing Nothing
        it "joins a fetched feed whatever the requirement" $
            for_ [EpssRequired, EpssOptional] $ \requirement ->
                resolveEnrichment requirement (Right feed) `shouldBe` Right (EpssEnriched feed)

        it "keeps a failure fatal where the ecosystem requires enrichment" $
            resolveEnrichment EpssRequired (Left EpssFeedNoScores) `shouldBe` Left EpssFeedNoScores

        it "records a failure as unavailable enrichment where it is optional" $ do
            let resolved = resolveEnrichment EpssOptional (Left EpssFeedNoScores)
            resolved `shouldBe` Right (EpssUnavailable EpssFeedNoScores)
            fmap enrichmentStatus resolved `shouldSatisfy` (== Right EnrichmentUnavailable)

    describe "parseEpssPreamble" $ do
        it "reads the score date and the model version FIRST.org writes" $ do
            let preamble = parseEpssPreamble "#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000"
            epScoreDate preamble `shouldBe` Just (UTCTime (fromGregorian 2026 8 29) 0)
            epModelVersion preamble `shouldBe` Just "v2026.08.01"

        it "reads a date-only score date as that day's UTC start" $
            epScoreDate (parseEpssPreamble "#model_version:v1,score_date:2026-08-29")
                `shouldBe` Just (UTCTime (fromGregorian 2026 8 29) 0)

        it "keeps the time of day a full timestamp declares" $
            epScoreDate (parseEpssPreamble "#score_date:2026-08-29T06:30:00Z")
                `shouldBe` Just (UTCTime (fromGregorian 2026 8 29) (secondsToDiffTime 23400))

        it "records nothing for a malformed date, rather than a substitute" $
            epScoreDate (parseEpssPreamble "#model_version:v1,score_date:not-a-date") `shouldBe` Nothing

        it "records nothing for a line that is not the feed's comment" $ do
            let preamble = parseEpssPreamble "cve,epss,percentile"
            epScoreDate preamble `shouldBe` Nothing
            epModelVersion preamble `shouldBe` Nothing

-- The bytes a ceiling saw when it tripped depend on chunking, so a case compares the ceiling alone.
withoutSeen :: EpssFeedFailure -> EpssFeedFailure
withoutSeen = \case
    EpssFeedOversize (CompressedTooLarge cap _) -> EpssFeedOversize (CompressedTooLarge cap 0)
    EpssFeedOversize (DecompressedTooLarge cap _) -> EpssFeedOversize (DecompressedTooLarge cap 0)
    other -> other

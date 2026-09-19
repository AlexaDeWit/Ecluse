-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | The bounded OSV ingest: what reaches the rows, what is dropped, and what the tally says.
module Ecluse.Core.Osv.StreamSpec (spec) where

import Conduit
import Data.Aeson (Value (String))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types.Status (status200)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Osv.Advisory (ExtractedOsv (..))
import Ecluse.Core.Osv.Ecosystem (
    OsvEcosystem (osvExportDirectory, osvMaxAdvisoryFanOut, osvWireName),
    osvEcosystemFor,
    osvEcosystemNamed,
 )
import Ecluse.Core.Osv.Stream (
    IngestLimits (..),
    IngestStats (..),
    defaultIngestLimits,
    newOsvIngest,
    parseOsvStream,
    readIngestStats,
    streamOsvUrl,
    systemicDrop,
 )
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Test.Osv (OsvTestM, noScores, npmFeed, osvZipOf, runOsvJsonLog, runOsvTestM)
import Ecluse.Test.Osv.Withdrawal (withdrawalZip)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

-- The run clock every ingest here is built with. Nothing in this module reads the record
-- dates it judges, so its only job is to be a clock.
ingestClock :: UTCTime
ingestClock = UTCTime (fromGregorian 2026 9 1) 0

fanOutFlag :: Text
fanOutFlag = "exceeding the sanity threshold"

-- One advisory of @feed@ naming @n@ exact versions, which the ingest expands into @n@ rows.
fanOutAdvisory :: OsvEcosystem -> Int -> LByteString
fanOutAdvisory feed n = encodeUtf8 (opening <> T.intercalate "," (map versionLiteral [1 .. n]) <> "]}]}")
  where
    opening =
        "{\"id\":\"GHSA-fan\",\"affected\":[{\"package\":{\"name\":\"fan\",\"ecosystem\":\""
            <> osvExportDirectory feed
            <> "\"},\"versions\":["
    versionLiteral i = "\"1.0." <> show i <> "\""

{- | Ingest a whole in-memory archive, answering the rows it emitted beside the run's tally.
Every archive case here drives the stream this way, so the wiring lives in one place.
-}
ingestArchive :: IngestLimits -> OsvEcosystem -> LByteString -> OsvTestM ([ExtractedOsv], IngestStats)
ingestArchive limits feed archive = do
    ingest <- newOsvIngest limits feed noScores ingestClock
    rows <- runConduit $ yieldMany (LBS.toChunks archive) .| parseOsvStream Nothing ingest .| sinkList
    stats <- readIngestStats ingest
    pure (rows, stats)

-- | 'ingestArchive' at the shipped limits, run against a scribe-free log environment.
ingestedRows :: OsvEcosystem -> LByteString -> IO ([ExtractedOsv], IngestStats)
ingestedRows feed archive = runOsvTestM (ingestArchive defaultIngestLimits feed archive)

-- | Stream an OSV archive off disk at the shipped limits, answering the rows it emitted.
ingestedFile :: FilePath -> IO [ExtractedOsv]
ingestedFile path =
    runOsvTestM $ do
        ingest <- newOsvIngest defaultIngestLimits npmFeed noScores ingestClock
        runConduit (sourceFile path .| parseOsvStream Nothing ingest .| sinkList)

-- | Fetch and stream an OSV archive over HTTP, answering the rows it emitted.
ingestedUrl :: String -> IO [ExtractedOsv]
ingestedUrl url =
    runOsvTestM $ do
        ingest <- newOsvIngest defaultIngestLimits npmFeed noScores ingestClock
        runConduit (streamOsvUrl Nothing ingest url .| sinkList)

fanOutRows :: OsvEcosystem -> Int -> IO ([ExtractedOsv], IngestStats)
fanOutRows feed n = osvZipOf [("fan.json", fanOutAdvisory feed n)] >>= ingestedRows feed

fanOutLog :: OsvEcosystem -> Int -> IO Text
fanOutLog feed n = do
    zipData <- osvZipOf [("fan.json", fanOutAdvisory feed n)]
    runOsvJsonLog (void (ingestArchive defaultIngestLimits feed zipData))

-- The one row the sample archive carries, asserted wherever that archive is streamed.
theSampleRow :: [ExtractedOsv] -> IO ()
theSampleRow = \case
    [ext] -> do
        extPackage ext `shouldBe` "hono"
        extEcosystem ext `shouldBe` "npm"
        extCveId ext `shouldBe` "GHSA-2234-fmw7-43wr"
        extUpperBound ext `shouldBe` FixedBefore "4.6.5"
    other -> fail ("expected exactly one extracted row, got " <> show (length other))

spec :: Spec
spec = describe "the OSV ingest stream" $ do
    it "streams an OSV zip archive and emits ExtractedOsv elements" $
        ingestedFile "test/unit/fixtures/osv/sample.zip" >>= theSampleRow

    it "handles an empty zip archive gracefully without emitting anything" $
        ingestedFile "test/unit/fixtures/osv/empty.zip" `shouldReturn` []

    it "skips malformed JSON files inside a zip archive and logs a warning" $
        ingestedFile "test/unit/fixtures/osv/malformed-json.zip" `shouldReturn` []

    it "throws an exception when streaming a non-zip file" $
        ingestedFile "test/unit/fixtures/osv/not-a-zip.zip" `shouldThrow` anyException

    it "fetches and streams an OSV zip archive over HTTP" $ do
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        results <- withStub status200 zipData $ \stub ->
            ingestedUrl (unpack (stubBaseUrl stub) <> "/sample.zip")
        theSampleRow results

    it "throws an exception if the URL is invalid" $
        ingestedUrl "not-a-valid-url" `shouldThrow` anyException

    for_ [(String "2024-05-14T20:15:44Z", 0), (String "invalid", 1)] $ \(withdrawn, malformed) ->
        it ("preserves the drop accounting for withdrawal " <> show withdrawn) $ do
            archive <- withdrawalZip (Just withdrawn)
            (rows, stats) <- ingestedRows npmFeed archive
            sort (map extCveId rows) `shouldBe` ["GHSA-corpus-0001", "GHSA-independent"]
            stats `shouldBe` IngestStats (3 - malformed) 0 malformed 0 0

    describe "ingest bounds" $ do
        it "drops an over-large advisory and keeps ingesting the entries after it" $ do
            -- Reaching the good entry proves the oversized entry drained to its boundary.
            zipData <-
                osvZipOf
                    [ ("big.json", LBS.replicate 3000 120)
                    , ("good.json", "{\"id\":\"GHSA-good\",\"affected\":[{\"package\":{\"name\":\"good-pkg\",\"ecosystem\":\"npm\"},\"versions\":[\"1.0.0\"]}]}")
                    ]
            (results, stats) <-
                runOsvTestM (ingestArchive defaultIngestLimits{ilMaxAdvisoryBytes = 2000} npmFeed zipData)
            map extCveId results `shouldBe` ["GHSA-good"]
            statAccepted stats `shouldBe` 1
            statDroppedOversize stats `shouldBe` 1
            statDroppedMalformed stats `shouldBe` 0

        it "keeps every range of a flagged advisory, so the flag refuses nothing" $ do
            let over = osvMaxAdvisoryFanOut npmFeed + 1
            (results, stats) <- fanOutRows npmFeed over
            length results `shouldBe` over
            statAccepted stats `shouldBe` 1

        -- Each bound is measured against its own export, so a fan-out that is ordinary on
        -- PyPI is still anomalous on npm.
        forM_ [npmFeed, osvEcosystemFor PyPI] $ \feed ->
            describe (toString (osvWireName feed)) $ do
                it "flags an advisory one range past the ecosystem's threshold" $ do
                    logged <- fanOutLog feed (osvMaxAdvisoryFanOut feed + 1)
                    logged `shouldSatisfy` T.isInfixOf fanOutFlag

                it "leaves an advisory at the ecosystem's threshold unflagged" $ do
                    logged <- fanOutLog feed (osvMaxAdvisoryFanOut feed)
                    logged `shouldSatisfy` (not . T.isInfixOf fanOutFlag)

        -- 'systemicDrop' is what escalates a feed whose drops stop being isolated.
        it "keeps every per-entry drop below the level an operator pages on" $ do
            zipData <-
                osvZipOf
                    [ ("big.json", LBS.replicate 6000 120)
                    , ("bad.json", "not json at all")
                    , ("fan.json", fanOutAdvisory npmFeed (osvMaxAdvisoryFanOut npmFeed + 1))
                    ]
            let limits = defaultIngestLimits{ilMaxAdvisoryBytes = 5000}
            logged <-
                runOsvJsonLog (void (ingestArchive limits npmFeed zipData))
            logged `shouldSatisfy` T.isInfixOf "Dropping oversized OSV entry"
            logged `shouldSatisfy` T.isInfixOf "Failed to parse OSV advisory JSON"
            logged `shouldSatisfy` T.isInfixOf fanOutFlag
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")

        it "counts a row the grammar cannot order, and still emits every row" $ do
            -- The tally is an alarm, not a filter: both versions reach the artifact.
            zipData <-
                osvZipOf
                    [("mixed.json", "{\"id\":\"GHSA-mixed\",\"affected\":[{\"package\":{\"name\":\"mixed\",\"ecosystem\":\"npm\"},\"versions\":[\"1.0.0\",\"2026.05.1\"]}]}")]
            (results, stats) <- ingestedRows npmFeed zipData
            map extIntroduced results `shouldBe` [Just "1.0.0", Just "2026.05.1"]
            statAccepted stats `shouldBe` 1
            statUnorderable stats `shouldBe` 1

        it "counts nothing unorderable for a name this build does not serve" $ do
            -- A one-shot compile of an unserved ecosystem carries no grammar to judge by.
            zipData <-
                osvZipOf
                    [("other.json", "{\"id\":\"GHSA-other\",\"affected\":[{\"package\":{\"name\":\"other\",\"ecosystem\":\"npm\"},\"versions\":[\"v1.2\"]}]}")]
            (results, stats) <- ingestedRows (osvEcosystemNamed "Go") zipData
            map extIntroduced results `shouldBe` [Just "v1.2"]
            statUnorderable stats `shouldBe` 0

    describe "systemicDrop" $ do
        it "does not trip on a healthy feed with a few bad entries" $
            systemicDrop (IngestStats 40000 3 2 0 0) `shouldBe` False
        it "does not trip below the absolute floor even at a high fraction" $
            systemicDrop (IngestStats 10 5 5 0 0) `shouldBe` False
        it "trips when drops are both non-trivial and a large fraction of entries" $
            systemicDrop (IngestStats 50 30 20 0 0) `shouldBe` True
        it "does not trip when non-trivial drops are only a small fraction" $
            systemicDrop (IngestStats 10000 30 20 0 0) `shouldBe` False
        it "ignores the unorderable tally, which counts rows rather than entries" $
            systemicDrop (IngestStats 40000 3 2 30000 0) `shouldBe` False

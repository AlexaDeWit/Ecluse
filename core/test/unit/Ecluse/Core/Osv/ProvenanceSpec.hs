-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.ProvenanceSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, secondsToDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Osv.Provenance (
    AdvisoryProvenance (..),
    ProvenanceSource (EpssFeed, OsvExport),
    QuietTime (..),
    SourceAge (..),
    decodeProvenance,
    noProvenance,
    parseHttpDate,
    parseSourceTime,
    provenanceRows,
    renderSourceAge,
    sourceAges,
    sourceQuiet,
 )

day :: UTCTime
day = UTCTime (fromGregorian 2026 9 1) 0

recorded :: AdvisoryProvenance
recorded =
    AdvisoryProvenance
        { apOsvSource = Just "https://osv.example.test/npm/all.zip"
        , apOsvLastModified = Just day
        , apOsvNewestModified = Just day
        , apEpssSource = Just "https://epss.example.test/scores.csv.gz"
        , apEpssLastModified = Just day
        , apEpssScoreDate = Just day
        , apEpssModelVersion = Just "v2026.08.01"
        }

quietTime :: QuietTime
quietTime = QuietTime{qtOsv = 604800, qtEpss = 604800}

spec :: Spec
spec = do
    describe "provenanceRows" $ do
        it "writes one row per recorded value, timestamps as RFC 3339 UTC" $
            provenanceRows recorded
                `shouldBe` [ ("osv_source", "https://osv.example.test/npm/all.zip")
                           , ("osv_last_modified", "2026-09-01T00:00:00Z")
                           , ("osv_newest_modified", "2026-09-01T00:00:00Z")
                           , ("epss_source", "https://epss.example.test/scores.csv.gz")
                           , ("epss_last_modified", "2026-09-01T00:00:00Z")
                           , ("epss_score_date", "2026-09-01T00:00:00Z")
                           , ("epss_model_version", "v2026.08.01")
                           ]

        it "writes no row for a value the source did not supply" $
            provenanceRows noProvenance `shouldBe` []

    describe "decodeProvenance" $ do
        it "round-trips what the writer recorded" $
            decodeProvenance (provenanceRows recorded) `shouldBe` recorded

        it "reads an artifact carrying none of the keys as explicit absence" $
            decodeProvenance [("ecosystem", "npm"), ("row_count", "12")] `shouldBe` noProvenance

        it "reads an unparseable timestamp as absence rather than failing the artifact" $
            apOsvNewestModified (decodeProvenance [("osv_newest_modified", "the day before")]) `shouldBe` Nothing

        it "reads an over-long value as absence, so an artifact cannot hand out an unbounded string" $
            apOsvSource (decodeProvenance [("osv_source", toText (replicate 4096 'u'))]) `shouldBe` Nothing

    describe "parseSourceTime" $ do
        it "reads a bare date as that day's UTC start" $
            parseSourceTime "2026-09-01" `shouldBe` Just day

        it "reads an offset written without its colon, as the EPSS feed writes it" $
            parseSourceTime "2026-09-01T00:00:00+0000" `shouldBe` Just day

        it "reads a Zulu timestamp" $
            parseSourceTime "2026-09-01T06:30:00Z"
                `shouldBe` Just (UTCTime (fromGregorian 2026 9 1) (secondsToDiffTime 23400))

        it "reads nothing from a value that is not a timestamp" $
            parseSourceTime "yesterday" `shouldBe` Nothing

    describe "parseHttpDate" $ do
        it "reads the RFC 1123 date an HTTP Last-Modified carries" $
            parseHttpDate "Tue, 01 Sep 2026 00:00:00 GMT" `shouldBe` Just day

        it "reads nothing from a header value it cannot parse" $
            parseHttpDate "whenever" `shouldBe` Nothing

    describe "sourceAges" $ do
        it "reads one age per source that recorded a timestamp" $
            map saSource (sourceAges day quietTime recorded) `shouldBe` [OsvExport, EpssFeed]

        it "reads no age for a source that recorded no timestamp" $
            sourceAges day quietTime noProvenance `shouldBe` []

        it "calls a source quiet once it passes its threshold, and not before" $ do
            let at offset = sourceAges (addUTCTime offset day) quietTime recorded
            map sourceQuiet (at 604800) `shouldBe` [False, False]
            map sourceQuiet (at 604801) `shouldBe` [True, True]

        it "names the source, its age, and its threshold in the line an operator reads" $
            map renderSourceAge (sourceAges (addUTCTime 604801 day) quietTime recorded)
                `shouldSatisfy` all (\line -> "last changed 604801s ago, quiet-time threshold 604800s" `T.isInfixOf` line)

        it "names a source with no recorded URL rather than inventing one" $
            map saUrl (sourceAges day quietTime recorded{apOsvSource = Nothing})
                `shouldBe` ["<unrecorded>", "https://epss.example.test/scores.csv.gz"]

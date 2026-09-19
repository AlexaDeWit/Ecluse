-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.OsvSpec (spec) where

import Database.SQLite.Simple (FromRow, Only (..), Query, close, open, query_)
import Database.SQLite.Simple.FromField (FromField)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)
import UnliftIO (bracket)

import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Test.Osv (CorpusVersion (..), mkDbWithViewShadowingRanges, mkDbWithWrongEpoch)
import Ecluse.Test.OsvDb (withFixtureOsvDb)

-- (package, cve, introduced, fixed, severity, epss). Severity is the numeric CVSS score
-- the writer stores, and epss the score the fixture feed carries for the advisory's alias.
type RangeRow = (Text, Text, Maybe Text, Maybe Text, Maybe Double, Maybe Double)

-- The pins are literal: editing the corpus or the EPSS fixture updates them in the same change.
-- LOW->3.9, MODERATE->6.9, HIGH->8.9, CRITICAL->10.0. A NULL is an unscored alias or a "0" bound.
corpusV1Rows :: [RangeRow]
corpusV1Rows =
    [ ("@corpus/scoped", "GHSA-corpus-0005", Nothing, Just "3.0.0", Just 3.9, Just 0.25)
    , ("corpus-mixed", "GHSA-corpus-0006", Nothing, Just "1.0.0", Just 6.9, Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Nothing, Just "1.0.0", Nothing, Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Just "1.5.0", Just "2.0.0", Nothing, Nothing)
    , ("corpus-unfixed", "GHSA-corpus-0002", Just "1.0.0", Nothing, Just 10.0, Just 0.5)
    , ("corpus-vuln", "GHSA-corpus-0001", Nothing, Just "1.2.0", Just 8.9, Just 0.875)
    , ("corpus-vuln", "GHSA-corpus-0004", Just "2.0.0", Just "2.5.0", Just 6.9, Just 0.0625)
    ]

corpusV2Rows :: [RangeRow]
corpusV2Rows =
    [ ("@corpus/scoped", "GHSA-corpus-0005", Nothing, Just "3.0.0", Just 3.9, Just 0.25)
    , ("corpus-clean", "GHSA-corpus-1001", Nothing, Nothing, Just 8.9, Just 0.375)
    , ("corpus-mixed", "GHSA-corpus-0006", Nothing, Just "1.0.0", Just 6.9, Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Nothing, Just "1.0.0", Nothing, Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Just "1.5.0", Just "2.0.0", Nothing, Nothing)
    , ("corpus-revoked", "GHSA-corpus-1002", Nothing, Just "1.2.0", Just 8.9, Nothing)
    , ("corpus-unfixed", "GHSA-corpus-0002", Just "1.0.0", Nothing, Just 10.0, Just 0.5)
    , ("corpus-vuln", "GHSA-corpus-0001", Nothing, Just "1.2.0", Just 8.9, Just 0.875)
    , ("corpus-vuln", "GHSA-corpus-0004", Just "2.0.0", Just "2.5.0", Just 6.9, Just 0.0625)
    ]

-- | Read one query off a generated artifact, closing the connection before the assertion.
readRows :: (FromRow r) => FilePath -> Query -> IO [r]
readRows db sql = bracket (open db) close (`query_` sql)

-- | The one column of a single-column query.
readColumn :: (FromField a) => FilePath -> Query -> IO [a]
readColumn db sql = map fromOnly <$> readRows db sql

rangeRows :: FilePath -> IO [RangeRow]
rangeRows db = readRows db "SELECT package_name, cve_id, introduced_version, fixed_version, severity, epss_score FROM package_vulnerability_ranges ORDER BY package_name, cve_id, introduced_version"

spec :: Spec
spec = do
    describe "the OSV fixture corpus" $ do
        -- Full-table equality: the malformed corpus entry contributes zero rows, so
        -- its omission is the assertion.
        it "compiles CorpusV1 to exactly the pinned advisory ranges" $
            withFixtureOsvDb CorpusV1 (\db -> rangeRows db `shouldReturn` corpusV1Rows)

        it "compiles CorpusV2 to CorpusV1 plus the two V2 advisories (the swap flip)" $
            withFixtureOsvDb CorpusV2 (\db -> rangeRows db `shouldReturn` corpusV2Rows)

        it "omits foreign affected packages from mixed-ecosystem advisories" $
            withFixtureOsvDb CorpusV1 $ \db ->
                (readColumn db "SELECT package_name FROM package_vulnerability_ranges WHERE package_name = 'redis'" :: IO [Text])
                    `shouldReturn` []

        it "stamps the generated artifact with the current schema epoch" $
            withFixtureOsvDb CorpusV1 $ \db ->
                (readColumn db "PRAGMA user_version" :: IO [Int]) `shouldReturn` [osvSchemaEpoch]

    describe "hostile artifacts" $ do
        it "the wrong-epoch artifact carries a mismatched user_version" $
            withSystemTempDirectory "ecluse-osv-hostile" $ \dir -> do
                let path = dir </> "wrong-epoch.db"
                mkDbWithWrongEpoch path
                (readColumn path "PRAGMA user_version" :: IO [Int]) `shouldReturn` [osvSchemaEpoch + 1]

        it "the view-shadowed artifact defines the ranges relation as a view, not a table" $
            withSystemTempDirectory "ecluse-osv-hostile" $ \dir -> do
                let path = dir </> "view-shadow.db"
                mkDbWithViewShadowingRanges path
                (readColumn path "SELECT type FROM sqlite_master WHERE name = 'package_vulnerability_ranges'" :: IO [Text])
                    `shouldReturn` ["view"]

module Ecluse.Test.OsvSpec (spec) where

import Database.SQLite.Simple (Only (..), close, open, query_)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Test.Osv (CorpusVersion (..), mkDbWithViewShadowingRanges, mkDbWithWrongEpoch)
import Ecluse.Test.OsvDb (withFixtureOsvDb)

type RangeRow = (Text, Text, Maybe Text, Maybe Text, Maybe Text)

-- The pins are literal on purpose: editing the corpus means updating them,
-- deliberately, in the same PR.
corpusV1Rows :: [RangeRow]
corpusV1Rows =
    [ ("@corpus/scoped", "GHSA-corpus-0005", Just "0", Just "3.0.0", Just "LOW")
    , ("corpus-multi", "GHSA-corpus-0003", Just "0", Just "1.0.0", Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Just "1.5.0", Just "2.0.0", Nothing)
    , ("corpus-unfixed", "GHSA-corpus-0002", Just "1.0.0", Nothing, Just "CRITICAL")
    , ("corpus-vuln", "GHSA-corpus-0001", Just "0", Just "1.2.0", Just "HIGH")
    , ("corpus-vuln", "GHSA-corpus-0004", Just "2.0.0", Just "2.5.0", Just "MODERATE")
    ]

corpusV2Rows :: [RangeRow]
corpusV2Rows =
    [ ("@corpus/scoped", "GHSA-corpus-0005", Just "0", Just "3.0.0", Just "LOW")
    , ("corpus-clean", "GHSA-corpus-1001", Just "0", Nothing, Just "HIGH")
    , ("corpus-multi", "GHSA-corpus-0003", Just "0", Just "1.0.0", Nothing)
    , ("corpus-multi", "GHSA-corpus-0003", Just "1.5.0", Just "2.0.0", Nothing)
    , ("corpus-unfixed", "GHSA-corpus-0002", Just "1.0.0", Nothing, Just "CRITICAL")
    , ("corpus-vuln", "GHSA-corpus-0001", Just "0", Just "1.2.0", Just "HIGH")
    , ("corpus-vuln", "GHSA-corpus-0004", Just "2.0.0", Just "2.5.0", Just "MODERATE")
    ]

rangeRows :: FilePath -> IO [RangeRow]
rangeRows db = do
    conn <- open db
    rows <- query_ conn "SELECT package_name, cve_id, introduced_version, fixed_version, severity FROM package_vulnerability_ranges ORDER BY package_name, cve_id, introduced_version"
    close conn
    pure rows

spec :: Spec
spec = do
    describe "the OSV fixture corpus" $ do
        -- Full-table equality: the malformed corpus entry contributing zero
        -- rows is asserted by omission.
        it "compiles CorpusV1 to exactly the pinned advisory ranges" $
            withFixtureOsvDb CorpusV1 (\db -> rangeRows db `shouldReturn` corpusV1Rows)

        it "compiles CorpusV2 to CorpusV1 plus the corpus-clean advisory (the swap flip)" $
            withFixtureOsvDb CorpusV2 (\db -> rangeRows db `shouldReturn` corpusV2Rows)

        it "stamps the generated artifact with the current schema epoch" $
            withFixtureOsvDb CorpusV1 $ \db -> do
                conn <- open db
                stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
                close conn
                map fromOnly stamped `shouldBe` [osvSchemaEpoch]

    describe "hostile artifacts" $ do
        it "the wrong-epoch artifact carries a mismatched user_version" $
            withSystemTempDirectory "ecluse-osv-hostile" $ \dir -> do
                let path = dir </> "wrong-epoch.db"
                mkDbWithWrongEpoch path
                conn <- open path
                stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
                close conn
                map fromOnly stamped `shouldBe` [osvSchemaEpoch + 1]

        it "the view-shadowed artifact defines the ranges relation as a view, not a table" $
            withSystemTempDirectory "ecluse-osv-hostile" $ \dir -> do
                let path = dir </> "view-shadow.db"
                mkDbWithViewShadowingRanges path
                conn <- open path
                kinds <- query_ conn "SELECT type FROM sqlite_master WHERE name = 'package_vulnerability_ranges'" :: IO [Only Text]
                close conn
                map fromOnly kinds `shouldBe` ["view"]

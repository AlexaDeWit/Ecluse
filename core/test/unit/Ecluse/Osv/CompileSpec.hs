{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Osv.CompileSpec (spec) where

import Conduit
import Control.Monad.Catch (MonadCatch, MonadMask)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Version (showVersion)
import Database.SQLite.Simple
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Paths_ecluse (version)
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import System.IO.Error (catchIOError)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldThrow)

import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Core.Osv.Stream (PilotIngestAborted (..))
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Network.HTTP.Types.Status (status200)

newtype TestM a = TestM {runTestM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, MonadCatch, MonadMask, PrimMonad, MonadUnliftIO)

instance Katip TestM where
    getLogEnv = TestM ask
    localLogEnv f (TestM m) = TestM (local f m)

instance KatipContext TestM where
    getKatipContext = pure mempty
    localKatipContext _ m = m
    getKatipNamespace = pure mempty
    localKatipNamespace _ m = m

spec :: Spec
spec = describe "SQLite OSV Compilation" $ do
    it "fetches an OSV zip and compiles it into a named, stamped SQLite artifact" $ do
        le <- initLogEnv "test" (Environment "test")
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        dbFile <- withStub status200 zipData $ \stub -> do
            runResourceT $
                runReaderT
                    ( runTestM $
                        compileOsvToSqlite Nothing "/tmp" "npm" (unpack (stubBaseUrl stub) <> "/sample.zip")
                    )
                    le

        -- Verify the sqlite db
        conn <- open dbFile
        rows <- query_ conn "SELECT package_name, cve_id, fixed_version, severity FROM package_vulnerability_ranges" :: IO [(Text, Text, Maybe Text, Maybe Double)]
        stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
        metaRows <- query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)]
        indexes <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'package_vulnerability_ranges' AND name LIKE 'idx_%' ORDER BY name" :: IO [Only Text]
        strictTables <- query_ conn "SELECT name FROM pragma_table_list WHERE name IN ('package_vulnerability_ranges', 'meta') AND strict = 1 ORDER BY name" :: IO [Only Text]
        dedupIndexes <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'package_vulnerability_ranges' AND name LIKE 'uq_%'" :: IO [Only Text]
        close conn
        catchIOError (removeFile dbFile) (const $ pure ())

        -- The file-name literal and the meta keys below pin the artifact's wire
        -- contract, the forms a reader depends on, not the constants that
        -- produced them.
        takeFileName dbFile `shouldBe` "npm-osv-schema3.db"
        -- The sample carries a CVSS 3.1 vector (5.9); the computed base score is
        -- stored, in preference to the "MODERATE" label.
        rows `shouldBe` [("hono", "GHSA-2234-fmw7-43wr", Just "4.6.5", Just 5.9)]
        map fromOnly stamped `shouldBe` [osvSchemaEpoch]
        -- The reader's lookups ride these: by-package fetch and the exact
        -- (name, fixed) remediation probe.
        map fromOnly indexes `shouldBe` ["idx_package_fixed", "idx_package_name"]
        -- The reader accepts an artifact only if both tables are STRICT; a
        -- freshly compiled artifact must satisfy its own contract.
        map fromOnly strictTables `shouldBe` ["meta", "package_vulnerability_ranges"]
        -- The dedup guard behind INSERT OR IGNORE (the former composite PK).
        map fromOnly dedupIndexes `shouldBe` ["uq_ranges_segment"]

        let meta = Map.fromList metaRows
        Map.keys meta `shouldBe` ["built_at", "ecosystem", "pilot_version", "row_count", "source_url"]
        Map.lookup "ecosystem" meta `shouldBe` Just "npm"
        Map.lookup "row_count" meta `shouldBe` Just "1"
        Map.lookup "pilot_version" meta `shouldBe` Just (toText (showVersion version))
        Map.lookup "source_url" meta `shouldSatisfy` maybe False (T.isSuffixOf "/sample.zip")
        Map.lookup "built_at" meta `shouldSatisfy` maybe False (not . T.null)

    it "aborts the compile without publishing when the drop rate is systemic" $ do
        le <- initLogEnv "test" (Environment "test")
        -- A feed that is almost entirely unusable: 20 malformed entries to one good
        -- one trips the systemic-drop breaker, which must abandon the run rather than
        -- finalise a fresh-looking artifact that silently omits most advisories.
        zipData <-
            osvZipOf
                ( [("mal-" <> show i <> ".json", "this is not valid json") | i <- [1 .. 20 :: Int]]
                    <> [("good.json", "{\"id\":\"GHSA-ok\",\"affected\":[{\"package\":{\"name\":\"ok\",\"ecosystem\":\"npm\"},\"versions\":[\"1.0.0\"]}]}")]
                )
        let action =
                withStub status200 zipData $ \stub ->
                    runResourceT $
                        runReaderT
                            ( runTestM $
                                compileOsvToSqlite Nothing "/tmp" "npm" (unpack (stubBaseUrl stub) <> "/all.zip")
                            )
                            le
        action `shouldThrow` (\(PilotIngestAborted _) -> True)

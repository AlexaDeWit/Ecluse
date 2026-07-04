{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Pilot.Osv.CompileSpec (spec) where

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
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Osv.Schema (osvSchemaEpoch)
import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Telemetry (telemetryDisabled)
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
                        compileOsvToSqlite telemetryDisabled "/tmp" "npm" (unpack (stubBaseUrl stub) <> "/sample.zip")
                    )
                    le

        -- Verify the sqlite db
        conn <- open dbFile
        rows <- query_ conn "SELECT package_name, cve_id, fixed_version, severity FROM package_vulnerability_ranges" :: IO [(Text, Text, Maybe Text, Maybe Text)]
        stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
        metaRows <- query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)]
        close conn
        catchIOError (removeFile dbFile) (const $ pure ())

        -- The file-name literal and the meta keys below pin the artifact's wire
        -- contract, the forms a reader depends on, not the constants that
        -- produced them.
        takeFileName dbFile `shouldBe` "npm-osv-schema1.db"
        rows `shouldBe` [("hono", "GHSA-2234-fmw7-43wr", Just "4.6.5", Just "MODERATE")]
        map fromOnly stamped `shouldBe` [osvSchemaEpoch]

        let meta = Map.fromList metaRows
        Map.keys meta `shouldBe` ["built_at", "ecosystem", "pilot_version", "row_count", "source_url"]
        Map.lookup "ecosystem" meta `shouldBe` Just "npm"
        Map.lookup "row_count" meta `shouldBe` Just "1"
        Map.lookup "pilot_version" meta `shouldBe` Just (toText (showVersion version))
        Map.lookup "source_url" meta `shouldSatisfy` maybe False (T.isSuffixOf "/sample.zip")
        Map.lookup "built_at" meta `shouldSatisfy` maybe False (not . T.null)

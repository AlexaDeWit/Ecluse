{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Pilot.Osv.CompileSpec (spec) where

import Conduit
import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Database.SQLite.Simple
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Network.HTTP.Types.Status (status200)

newtype TestM a = TestM {runTestM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, PrimMonad, MonadUnliftIO)

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
    it "fetches an OSV zip and compiles it into an SQLite database" $ do
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
        rows <- query_ conn "SELECT package_name, cve_id, fixed_version FROM package_vulnerability_ranges" :: IO [(Text, Text, Maybe Text)]
        close conn
        catchIOError (removeFile dbFile) (const $ pure ())

        rows `shouldBe` [("hono", "GHSA-2234-fmw7-43wr", Just "4.6.5")]

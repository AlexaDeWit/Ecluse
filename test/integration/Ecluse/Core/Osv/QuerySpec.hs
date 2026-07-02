{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Core.Osv.QuerySpec (spec) where

import Conduit
import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Database.SQLite.Simple
import Ecluse.Core.Osv.Query
import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Network.HTTP.Types.Status (status200)
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO

newtype TestM a = TestM {runTestM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, MonadUnliftIO, PrimMonad)

instance Katip TestM where
    getLogEnv = TestM ask
    localLogEnv f (TestM m) = TestM (local f m)

instance KatipContext TestM where
    getKatipContext = pure mempty
    localKatipContext _ m = m
    getKatipNamespace = pure mempty
    localKatipNamespace _ m = m

spec :: Spec
spec = describe "Osv.Query integration" $ do
    it "compiles an OSV zip archive into SQLite and successfully queries the ranges" $ do
        le <- initLogEnv "test" (Environment "test")
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        withStub status200 zipData $ \stub -> do
            withSystemTempDirectory "osv-test" $ \tmpDir -> do
                dbPath <-
                    runResourceT $
                        runReaderT
                            ( runTestM $
                                compileOsvToSqlite telemetryDisabled tmpDir "npm" (unpack (stubBaseUrl stub) <> "/sample.zip")
                            )
                            le

                bracket (open dbPath) close $ \conn -> do
                    res <- queryPackageVulnerabilities conn "hono"
                    length res `shouldBe` 1
                    case res of
                        [r] -> do
                            osvPackage r `shouldBe` "hono"
                            osvCveId r `shouldBe` "GHSA-2234-fmw7-43wr"
                            osvIntroduced r `shouldBe` Just "0"
                            osvFixed r `shouldBe` Just "4.6.5"
                        _ -> fail "Expected exactly 1 result"

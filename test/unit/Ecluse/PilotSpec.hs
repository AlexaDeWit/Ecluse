module Ecluse.PilotSpec (spec) where

import Prelude hiding (get)

import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Database.SQLite.Simple (Only (..), close, open, query_)
import Katip (Environment (..), initLogEnv)
import Network.HTTP.Types.Status (status200)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Config (AppConfig, Config (configApp), loadConfig)
import Ecluse.Pilot (PilotCompileOptions (..), PilotUploadUnconfigured (..), pilotApplication, runPilotCompile)
import Ecluse.Server (mkServerConfig)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

spec :: Spec
spec = do
    describe "Pilot worker mode" $ do
        let app = pilotApplication (mkServerConfig [])
        with app $ do
            it "starts up and answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

    describe "runPilotCompile (one-shot compile mode)" $ do
        it "compiles a served OSV zip into the requested directory and returns the artifact's path" $ do
            le <- initLogEnv "test" (Environment "test")
            appCfg <- defaultAppConfig
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            withSystemTempDirectory "ecluse-pilot-compile" $ \outDir -> do
                dbFile <- withStub status200 zipData $ \stub ->
                    runPilotCompile
                        le
                        telemetryDisabled
                        appCfg
                        (compileOptions (stubBaseUrl stub) outDir)
                takeDirectory dbFile `shouldBe` outDir
                exists <- doesFileExist dbFile
                exists `shouldBe` True
                conn <- open dbFile
                rows <- query_ conn "SELECT package_name FROM package_vulnerability_ranges" :: IO [Only Text]
                close conn
                map fromOnly rows `shouldBe` ["hono"]

        it "fails loudly when an upload is requested without a configured bucket" $ do
            le <- initLogEnv "test" (Environment "test")
            appCfg <- defaultAppConfig
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            withSystemTempDirectory "ecluse-pilot-compile" $ \outDir -> do
                let action = withStub status200 zipData $ \stub ->
                        runPilotCompile
                            le
                            telemetryDisabled
                            appCfg
                            (compileOptions (stubBaseUrl stub) outDir){pcoUpload = True}
                action `shouldThrow` (== PilotUploadUnconfigured)

compileOptions :: Text -> FilePath -> PilotCompileOptions
compileOptions baseUrl outDir =
    PilotCompileOptions
        { pcoEcosystem = "npm"
        , pcoSource = Just (unpack baseUrl <> "/all.zip")
        , pcoOutDir = outDir
        , pcoUpload = False
        }

defaultAppConfig :: IO AppConfig
defaultAppConfig = case loadConfig [] Nothing of
    Right c -> pure (configApp c)
    Left e -> fail ("Config error: " <> show e)

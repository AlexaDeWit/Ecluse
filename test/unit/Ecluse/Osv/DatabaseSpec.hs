{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Osv.DatabaseSpec (spec) where

import Conduit
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQLite
import Katip (ColorStrategy (ColorIfTerminal), KatipContextT, LogContexts, LogEnv, Severity (..), Verbosity (V3), defaultLogEnv, logFM, ls, registerSeverity, runKatipContextT)
import System.IO (stdout)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Osv (ExtractedOsv (..))
import Ecluse.Osv.Database (compileToSqlite, lookupAdvisories, withAdvisoryDb)

instance (PrimMonad m) => PrimMonad (KatipContextT m) where
    type PrimState (KatipContextT m) = PrimState m
    primitive = lift . primitive

spec :: Spec
spec = describe "Ecluse.Pilot.Osv.Database" $ do
    it "successfully creates schema and inserts data" $ do
        withSystemTempFile "osv-test.db" $ \dbPath _ -> do
            let osvs =
                    [ ExtractedOsv "leftpad" "npm" ["1.0.0", "1.0.1"]
                    , ExtractedOsv "express" "npm" ["4.17.2"]
                    ]

            runTestLog $
                runConduitRes $
                    yieldMany osvs .| compileToSqlite dbPath

            withAdvisoryDb dbPath $ \conn -> do
                rows <- SQLite.query conn "SELECT package, ecosystem, fixed_version FROM advisories ORDER BY package, fixed_version" () :: IO [(Text, Text, Text)]
                rows
                    `shouldBe` [ ("express", "npm", "4.17.2")
                               , ("leftpad", "npm", "1.0.0")
                               , ("leftpad", "npm", "1.0.1")
                               ]

    it "supports safe lookups after compilation" $ do
        withSystemTempFile "osv-test-lookup.db" $ \dbPath _ -> do
            let osvs = [ExtractedOsv "express" "npm" ["4.17.2"]]
            runTestLog $ runConduitRes $ yieldMany osvs .| compileToSqlite dbPath

            withAdvisoryDb dbPath $ \conn -> do
                lookupAdvisories conn "express" "npm" `shouldReturn` ["4.17.2"]
                lookupAdvisories conn "unknown" "npm" `shouldReturn` []

    it "overwrites existing data on re-compilation" $ do
        withSystemTempFile "osv-test-overwrite.db" $ \dbPath _ -> do
            let osvs1 = [ExtractedOsv "old" "npm" ["1.0.0"]]
                osvs2 = [ExtractedOsv "new" "npm" ["2.0.0"]]

            runTestLog $
                runConduitRes $
                    yieldMany osvs1 .| compileToSqlite dbPath

            runTestLog $
                runConduitRes $
                    yieldMany osvs2 .| compileToSqlite dbPath

            conn <- SQLite.open dbPath
            rows <- SQLite.query conn "SELECT package FROM advisories" () :: IO [SQLite.Only Text]
            SQLite.close conn

            rows `shouldBe` [SQLite.Only "new"]

runTestLog :: KatipContextT IO a -> IO a
runTestLog action = do
    logEnv <- defaultLogEnv "ecluse-test" "test"
    runKatipContextT logEnv (mempty :: LogContexts) mempty action

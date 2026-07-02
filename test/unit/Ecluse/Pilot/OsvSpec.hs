{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Pilot.OsvSpec (spec) where

import Conduit
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Ecluse.Pilot.Osv
import Ecluse.Pilot.Osv.Stream (parseOsvStream, streamOsvUrl)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Network.HTTP.Types.Status (status200)

newtype TestM a = TestM {runTestM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, PrimMonad)

instance Katip TestM where
    getLogEnv = TestM ask
    localLogEnv f (TestM m) = TestM (local f m)

instance KatipContext TestM where
    getKatipContext = pure mempty
    localKatipContext _ m = m
    getKatipNamespace = pure mempty
    localKatipNamespace _ m = m

spec :: Spec
spec = describe "Osv parsing and streaming" $ do
    it "decodes a sample OSV advisory and extracts remediation boundaries" $ do
        fileBytes <- BS.readFile "test/unit/fixtures/osv/sample.json"
        let res = eitherDecodeStrict fileBytes :: Either String OsvAdvisory
        case res of
            Left err -> fail ("Failed to decode: " <> err)
            Right adv -> do
                osvId adv `shouldBe` "GHSA-2234-fmw7-43wr"
                let extracted = extractFromAdvisory adv
                extracted
                    `shouldBe` [ ExtractedOsv
                                    { extPackage = "hono"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-2234-fmw7-43wr"
                                    , extIntroduced = Just "0"
                                    , extFixed = Just "4.6.5"
                                    }
                               ]

    it "extracts multiple packages and ranges from a complex OSV advisory" $ do
        fileBytes <- BS.readFile "test/unit/fixtures/osv/complex.json"
        let res = eitherDecodeStrict fileBytes :: Either String OsvAdvisory
        case res of
            Left err -> fail ("Failed to decode: " <> err)
            Right adv -> do
                osvId adv `shouldBe` "GHSA-multi"
                let extracted = extractFromAdvisory adv
                extracted
                    `shouldBe` [ ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "0"
                                    , extFixed = Just "1.0.0"
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "1.1.0"
                                    , extFixed = Just "1.2.0"
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "2.0.0"
                                    , extFixed = Just "2.1.0"
                                    }
                               , ExtractedOsv
                                    { extPackage = "other-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "0"
                                    , extFixed = Just "3.0.0"
                                    }
                               ]

    it "streams an OSV zip archive and emits ExtractedOsv elements" $ do
        le <- initLogEnv "test" (Environment "test")
        results <-
            runResourceT $
                runReaderT
                    ( runTestM $
                        runConduit $
                            sourceFile "test/unit/fixtures/osv/sample.zip"
                                .| parseOsvStream telemetryDisabled
                                .| sinkList
                    )
                    le

        length results `shouldBe` 1
        case results of
            [ext] -> do
                extPackage ext `shouldBe` "hono"
                extEcosystem ext `shouldBe` "npm"
                extCveId ext `shouldBe` "GHSA-2234-fmw7-43wr"
                extFixed ext `shouldBe` Just "4.6.5"
            _ -> fail "Expected exactly 1 result"

    it "handles an empty zip archive gracefully without emitting anything" $ do
        le <- initLogEnv "test" (Environment "test")
        results <-
            runResourceT $
                runReaderT
                    ( runTestM $
                        runConduit $
                            sourceFile "test/unit/fixtures/osv/empty.zip"
                                .| parseOsvStream telemetryDisabled
                                .| sinkList
                    )
                    le
        results `shouldBe` []

    it "skips malformed JSON files inside a zip archive and logs a warning" $ do
        le <- initLogEnv "test" (Environment "test")
        results <-
            runResourceT $
                runReaderT
                    ( runTestM $
                        runConduit $
                            sourceFile "test/unit/fixtures/osv/malformed-json.zip"
                                .| parseOsvStream telemetryDisabled
                                .| sinkList
                    )
                    le
        results `shouldBe` []

    it "throws an exception when streaming a non-zip file" $ do
        le <- initLogEnv "test" (Environment "test")
        let action =
                runResourceT $
                    runReaderT
                        ( runTestM $
                            runConduit $
                                sourceFile "test/unit/fixtures/osv/not-a-zip.zip"
                                    .| parseOsvStream telemetryDisabled
                                    .| sinkList
                        )
                        le
        action `shouldThrow` anyException

    it "fetches and streams an OSV zip archive over HTTP" $ do
        le <- initLogEnv "test" (Environment "test")
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        results <- withStub status200 zipData $ \stub -> do
            runResourceT $
                runReaderT
                    ( runTestM $
                        runConduit $
                            streamOsvUrl telemetryDisabled (unpack (stubBaseUrl stub) <> "/sample.zip")
                                .| sinkList
                    )
                    le
        length results `shouldBe` 1
        case results of
            [ext] -> do
                extPackage ext `shouldBe` "hono"
                extEcosystem ext `shouldBe` "npm"
                extCveId ext `shouldBe` "GHSA-2234-fmw7-43wr"
                extFixed ext `shouldBe` Just "4.6.5"
            _ -> fail "Expected exactly 1 result"

    it "throws an exception if the URL is invalid" $ do
        le <- initLogEnv "test" (Environment "test")
        let action =
                runResourceT $
                    runReaderT
                        ( runTestM $
                            runConduit $
                                streamOsvUrl telemetryDisabled "not-a-valid-url"
                                    .| sinkList
                        )
                        le
        action `shouldThrow` anyException

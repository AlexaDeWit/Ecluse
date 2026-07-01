{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Pilot.OsvSpec (spec) where

import Conduit
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Pilot.Osv
import Ecluse.Pilot.Osv.Stream (parseOsvStream)
import Ecluse.Telemetry (telemetryDisabled)

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
                                    , extFixedVersions = ["4.6.5"]
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
                extFixedVersions ext `shouldBe` ["4.6.5"]
            _ -> fail "Expected exactly 1 result"

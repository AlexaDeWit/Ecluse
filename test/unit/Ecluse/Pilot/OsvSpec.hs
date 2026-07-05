{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.Pilot.OsvSpec (spec) where

import Conduit
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldSatisfy, shouldThrow)

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

-- | An advisory carrying only the severity evidence under test.
advisory :: [OsvSeverityEntry] -> Maybe Text -> OsvAdvisory
advisory entries label =
    OsvAdvisory
        { osvId = "GHSA-test-severity"
        , osvAffected = Nothing
        , osvSeverity = if null entries then Nothing else Just entries
        , osvDatabaseSpecific = OsvDatabaseSpecific . Just <$> label
        }

spec :: Spec
spec = describe "Osv parsing and streaming" $ do
    describe "osvExportUrl" $ do
        it "derives the per-ecosystem export under the base URL" $
            osvExportUrl "https://osv-vulnerabilities.storage.googleapis.com" "npm"
                `shouldBe` "https://osv-vulnerabilities.storage.googleapis.com/npm/all.zip"

        it "tolerates a trailing slash on the base URL" $
            osvExportUrl "https://mirror.example.com/osv/" "npm"
                `shouldBe` "https://mirror.example.com/osv/npm/all.zip"

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
                                    , extLastAffected = Nothing
                                    , -- The fixture carries both a CVSS 3.1 vector and the
                                      -- "MODERATE" label; the computed base score wins.
                                      extSeverity = Just 5.9
                                    }
                               ]

    describe "advisorySeverity" $ do
        it "computes the base score from a CVSS vector and prefers it over the label" $
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"] (Just "HIGH"))
                `shouldBe` Just 9.8

        it "takes the highest score when several vectors parse" $
            advisorySeverity
                ( advisory
                    [ OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:H/PR:N/UI:R/S:U/C:L/I:H/A:N" -- 5.9
                    , OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" -- 9.8
                    ]
                    Nothing
                )
                `shouldBe` Just 9.8

        it "parses a CVSS v4 vector (needs cvss >= 0.3) rather than dropping it" $
            -- A critical v4 vector, no label: it can only score above 8 if the v4
            -- parser is present. On cvss 0.2 it would have been unscored (Nothing).
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V4" "CVSS:4.0/AV:N/AC:L/AT:N/PR:N/UI:N/VC:H/VI:H/VA:H/SC:N/SI:N/SA:N"] Nothing)
                `shouldSatisfy` maybe False (>= 8.0)

        it "falls back to the qualitative label when no vector parses" $
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V3" "not-a-real-vector"] (Just "CRITICAL"))
                `shouldBe` Just 10.0

        it "yields Nothing for an advisory with no severity evidence at all" $
            advisorySeverity (advisory [] Nothing) `shouldBe` Nothing

    describe "extractFromAdvisory (affected-set shapes)" $ do
        it "records an exact enumerated version as a point segment (no ranges)" $ do
            -- The npm malware feed names the single bad version in versions[] with
            -- no ranges; the old parser dropped these entirely.
            let adv = OsvAdvisory "MAL-test" (Just [OsvAffected (OsvPackage "bad-pkg" "npm") Nothing (Just ["1.0.0"])]) Nothing Nothing
            extractFromAdvisory adv
                `shouldBe` [ExtractedOsv "bad-pkg" "npm" "MAL-test" (Just "1.0.0") Nothing (Just "1.0.0") Nothing]

        it "carries an inclusive last_affected bound distinct from a fix" $ do
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing Nothing (Just "3.8.8")]
                adv = OsvAdvisory "GHSA-la" (Just [OsvAffected (OsvPackage "electerm" "npm") (Just [OsvRange "SEMVER" events]) Nothing]) Nothing Nothing
            extractFromAdvisory adv
                `shouldBe` [ExtractedOsv "electerm" "npm" "GHSA-la" (Just "0") Nothing (Just "3.8.8") Nothing]

    it "extracts multiple packages and ranges from a complex OSV advisory" $ do
        fileBytes <- BS.readFile "test/unit/fixtures/osv/complex.json"
        let res = eitherDecodeStrict fileBytes :: Either String OsvAdvisory
        case res of
            Left err -> fail ("Failed to decode: " <> err)
            Right adv -> do
                osvId adv `shouldBe` "GHSA-multi"
                let extracted = extractFromAdvisory adv
                -- The complex fixture has "database_specific": null, so every
                -- extracted range carries no severity label.
                extracted
                    `shouldBe` [ ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "0"
                                    , extFixed = Just "1.0.0"
                                    , extLastAffected = Nothing
                                    , extSeverity = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "1.1.0"
                                    , extFixed = Just "1.2.0"
                                    , extLastAffected = Nothing
                                    , extSeverity = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "2.0.0"
                                    , extFixed = Just "2.1.0"
                                    , extLastAffected = Nothing
                                    , extSeverity = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "other-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "0"
                                    , extFixed = Just "3.0.0"
                                    , extLastAffected = Nothing
                                    , extSeverity = Nothing
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

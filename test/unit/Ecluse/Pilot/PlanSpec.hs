-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

{- | Pilot compilation and publication planning regressions.
Object keys retain their configured prefix and schema epoch.
-}
module Ecluse.Pilot.PlanSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.Support (expectAppConfig, expectConfig)
import Ecluse.Config (AdvisoriesSettings, AdvisoryStoreUrl, AppConfig (cfgAdvisories), Config (configApp, configMounts))
import Ecluse.Config.AdvisoryStore (mkAdvisoryStoreUrl)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Compile (CompileSources (..))
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor)
import Ecluse.Core.Osv.Provenance (QuietTime (..), defaultQuietTime)
import Ecluse.Core.Osv.Publication (ObjectRevision (ObjectRevision), Rebaseline (Rebaseline), ShrinkPolicy (..), mkPercent)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired))
import Ecluse.Pilot.Plan (
    ExportLoopPlan (ExportIdle, ExportTo),
    ExportTarget (ExportTarget, etEcosystem, etEpss),
    PilotCompileOptions (..),
    PilotUploadUnconfigured (PilotUploadUnconfigured),
    UploadPlan (UploadSkipped, UploadTo),
    UploadRequest (..),
    compileEpssRequirement,
    configuredSources,
    epssAttemptLine,
    exportCadenceMicros,
    exportLoopPlan,
    idleCadenceMicros,
    pilotPostureLines,
    quietTimeFor,
    shrinkPolicyOf,
    unmountedCompileWarning,
    uploadPlan,
    uploadTarget,
 )

-- The advisory settings the shipped defaults resolve to, with the given overlay applied.
advisoriesWith :: [(String, String)] -> IO AdvisoriesSettings
advisoriesWith env = cfgAdvisories <$> expectAppConfig env Nothing

-- A store the config layer would have built, so the plan sees what a boot hands it.
storeAt :: Text -> IO AdvisoryStoreUrl
storeAt raw = either (fail . toString) pure (mkAdvisoryStoreUrl "advisories.url" raw)

-- The advisory store a source-configured deployment declares.
sourceEnv :: (String, String)
sourceEnv = ("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")

advisoriesOf :: Config -> AdvisoriesSettings
advisoriesOf = cfgAdvisories . configApp

-- A one-shot run that compiles npm and does not upload.
bareOptions :: PilotCompileOptions
bareOptions = PilotCompileOptions{pcoEcosystem = "npm", pcoOutDir = "/tmp/ecluse-pilot", pcoUpload = NoUpload}

-- A scheduled target under the requirement a mount without EPSS rules resolves to.
optionalTarget :: Ecosystem -> ExportTarget
optionalTarget eco = ExportTarget{etEcosystem = eco, etEpss = EpssOptional}

{- A global EPSS rule npm inherits and PyPI switches off, with PyPI's remediation abstaining, beside
a RubyGems mount that adds its own EPSS rule under the skip alignment and the maximum threshold. -}
mixedPolicyDoc :: ByteString
mixedPolicyDoc =
    "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false},\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}},\"rubygems\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false},\"local-risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":1,\"onUnavailable\":\"skip\"}}}}}"

spec :: Spec
spec = do
    describe "compileEpssRequirement -- what a failed EPSS feed means for a one-shot compile" $ do
        it "takes each source-configured mount's resolved policy, inherited, removed, or added" $ do
            config <- expectConfig [("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test"), sourceEnv] (Just mixedPolicyDoc)
            let mounts = configMounts config
            map (\name -> compileEpssRequirement (advisoriesOf config) mounts bareOptions{pcoEcosystem = name}) ["npm", "pypi", "rubygems"]
                `shouldBe` [EpssRequired, EpssOptional, EpssRequired]
            map (\name -> unmountedCompileWarning mounts bareOptions{pcoEcosystem = name}) ["npm", "pypi", "rubygems"]
                `shouldBe` [Nothing, Nothing, Nothing]

        it "requires the feed for every mount without an advisory store, whose artifact may serve a policy that reads EPSS" $ do
            config <- expectConfig [("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")] (Just mixedPolicyDoc)
            map (\name -> compileEpssRequirement (advisoriesOf config) (configMounts config) bareOptions{pcoEcosystem = name}) ["npm", "pypi", "rubygems"]
                `shouldBe` [EpssRequired, EpssRequired, EpssRequired]

        it "requires the feed, with a warning, for an ecosystem the configuration does not mount" $
            for_ [[], [sourceEnv]] $ \env -> do
                config <- expectConfig env Nothing
                for_ ["npm", "go"] $ \name -> do
                    let opts = bareOptions{pcoEcosystem = name}
                    compileEpssRequirement (advisoriesOf config) (configMounts config) opts `shouldBe` EpssRequired
                    unmountedCompileWarning (configMounts config) opts `shouldSatisfy` maybe False (T.isInfixOf ("mounts no " <> name <> " ecosystem"))

    describe "epssAttemptLine -- the egress every compile attempts" $ do
        it "names the configured feed by host and port, never its path" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__EPSS_FEED_URL", "https://epss.example.test/private/scores.csv.gz")]
            epssAttemptLine advisories `shouldSatisfy` T.isInfixOf "the EPSS feed at epss.example.test:443,"
            epssAttemptLine advisories `shouldSatisfy` (not . T.isInfixOf "private")

        it "names the port a plain-http feed dials" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__EPSS_FEED_URL", "http://mirror.example.test/epss.csv.gz")]
            epssAttemptLine advisories `shouldSatisfy` T.isInfixOf "the EPSS feed at mirror.example.test:80,"

    describe "quietTimeFor -- the thresholds one compile is judged against" $ do
        it "takes the shipped seven days for a mounted ecosystem and for EPSS" $ do
            advisories <- advisoriesWith []
            quietTimeFor advisories (Just Npm) `shouldBe` QuietTime{qtOsv = 604800, qtEpss = 604800}

        it "takes an operator's threshold for the ecosystem it names" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__QUIET_TIME__NPM", "86400")]
            qtOsv (quietTimeFor advisories (Just Npm)) `shouldBe` 86400
            qtOsv (quietTimeFor advisories (Just PyPI)) `shouldBe` 604800

        it "falls back to the default for a one-shot compile of a name this build does not serve" $ do
            advisories <- advisoriesWith []
            qtOsv (quietTimeFor advisories Nothing) `shouldBe` defaultQuietTime

    describe "exportLoopPlan -- whether the scheduled loop exports at all, and for what" $ do
        it "idles on the shipped defaults, which configure no store" $ do
            advisories <- advisoriesWith []
            exportLoopPlan advisories [optionalTarget Npm] `shouldBe` Just ExportIdle

        it "idles with no mount either, because the store is what turns exporting on" $ do
            advisories <- advisoriesWith []
            exportLoopPlan advisories [] `shouldBe` Just ExportIdle

        it "exports to the configured store, the one thing that turns it on" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")]
            store <- storeAt "s3://advisories/ecluse"
            exportLoopPlan advisories [optionalTarget Npm] `shouldBe` Just (ExportTo store (optionalTarget Npm :| []))

        it "carries every mounted ecosystem, so a second mount earns its own artifact" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")]
            store <- storeAt "s3://advisories/ecluse"
            let targets = [ExportTarget Npm EpssRequired, optionalTarget PyPI]
            exportLoopPlan advisories targets `shouldBe` Just (ExportTo store (ExportTarget Npm EpssRequired :| [optionalTarget PyPI]))

        it "plans nothing for a configured store with no mount, which the boot refuses" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")]
            exportLoopPlan advisories [] `shouldBe` Nothing

    describe "exportCadenceMicros -- the delay between cycles" $ do
        it "converts the shipped hourly interval to microseconds" $ do
            advisories <- advisoriesWith []
            exportCadenceMicros advisories `shouldBe` 3600 * 1000000

        it "converts a configured interval" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "90")]
            exportCadenceMicros advisories `shouldBe` 90 * 1000000

        it "stays positive at the largest interval the decoder accepts" $ do
            -- The decoder caps compileInterval at maxBound `div` 1000000 seconds. At that
            -- cap the conversion must not wrap, or the loop would spin instead of waiting.
            let cap = (maxBound :: Int) `div` 1000000
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__COMPILE_INTERVAL", show cap)]
            exportCadenceMicros advisories `shouldSatisfy` (> 0)

    describe "idleCadenceMicros -- how long the disabled loop sleeps" $
        it "sleeps a day, because only a reboot picks up an added store" $
            idleCadenceMicros `shouldBe` 24 * 60 * 60 * 1000000

    describe "configuredSources -- the upstreams a compile reads" $ do
        it "builds the ecosystem's export URL under the configured base" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test")]
            csOsvExportUrl (configuredSources advisories (osvEcosystemFor Npm))
                `shouldBe` "https://osv.example.test/npm/all.zip"

        it "reads the EPSS feed from its own key, not the export base" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__EPSS_FEED_URL", "https://epss.example.test/scores.csv.gz")]
            csEpssFeedUrl (configuredSources advisories (osvEcosystemFor Npm))
                `shouldBe` "https://epss.example.test/scores.csv.gz"

        it "spells the export path as osv.dev does, not as the mount key does" $ do
            -- osv.dev files the PyPI export under "PyPI". Reaching for the mount key would
            -- fetch a directory that does not exist.
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test")]
            csOsvExportUrl (configuredSources advisories (osvEcosystemFor PyPI))
                `shouldBe` "https://osv.example.test/PyPI/all.zip"

    describe "uploadPlan -- whether a one-shot run publishes" $ do
        it "skips the upload the run did not ask for, store or no store" $ do
            store <- storeAt "s3://advisories"
            uploadPlan bareOptions Nothing `shouldBe` Right UploadSkipped
            uploadPlan bareOptions (Just store) `shouldBe` Right UploadSkipped

        it "uploads to the configured store when the run asks" $ do
            store <- storeAt "s3://advisories"
            uploadPlan bareOptions{pcoUpload = Upload} (Just store) `shouldBe` Right (UploadTo store Nothing)

        it "carries an operator's rebaseline to the store's upload, and to no other" $ do
            store <- storeAt "s3://advisories"
            approval <- Rebaseline (ObjectRevision "\"abc\"") <$> maybe (fail "fixture percentage") pure (mkPercent 10)
            uploadPlan bareOptions{pcoUpload = UploadRebaselined approval} (Just store) `shouldBe` Right (UploadTo store (Just approval))
            uploadPlan bareOptions{pcoUpload = UploadRebaselined approval} Nothing `shouldBe` Left PilotUploadUnconfigured

        it "refuses an asked-for upload with no store, rather than exiting 0 having published nothing" $
            uploadPlan bareOptions{pcoUpload = Upload} Nothing `shouldBe` Left PilotUploadUnconfigured

    describe "the publication guard -- what every publication is held to" $ do
        it "takes the shipped 2% shrink and 5% EPSS credit cap, and an operator's own" $ do
            shipped <- shrinkPolicyOf <$> advisoriesWith []
            expected <- maybe (fail "fixture percentage") pure (ShrinkPolicy <$> mkPercent 2 <*> mkPercent 5)
            shipped `shouldBe` expected
            tuned <- shrinkPolicyOf <$> advisoriesWith [("ECLUSE_ADVISORIES__MAX_SHRINK_PERCENT", "0.5"), ("ECLUSE_ADVISORIES__EPSS_LOSS_CAP_PERCENT", "0")]
            Just tuned `shouldBe` (ShrinkPolicy <$> mkPercent 0.5 <*> mkPercent 0)

        it "reports only the EPSS egress line while no store is configured" $ do
            advisories <- advisoriesWith []
            pilotPostureLines advisories `shouldBe` [epssAttemptLine advisories]

        it "reports the guard and the store's durable baseline beside the EPSS egress line" $ do
            advisories <- advisoriesWith [("ECLUSE_ADVISORIES__URL", "s3://advisories")]
            case pilotPostureLines advisories of
                [egress, guardLine] -> do
                    egress `shouldBe` epssAttemptLine advisories
                    for_ ["older than the stored artifact", "2% under its durable baseline", "up to 5% more after an optional EPSS outage"] $ \fragment ->
                        guardLine `shouldSatisfy` T.isInfixOf fragment
                other -> expectationFailure ("expected two posture lines, got " <> show other)

    describe "uploadTarget -- where one artifact lands in the store" $ do
        it "keys the artifact on its own file name, dropping the local directory" $ do
            store <- storeAt "s3://advisories"
            uploadTarget store "/var/lib/ecluse/advisories/npm-osv-schema5.db"
                `shouldBe` ("advisories", "npm-osv-schema5.db")

        it "writes the store's prefix ahead of that name, which is where the sync reads" $ do
            store <- storeAt "s3://advisories/ecluse"
            uploadTarget store "/var/lib/ecluse/advisories/npm-osv-schema5.db"
                `shouldBe` ("advisories", "ecluse/npm-osv-schema5.db")

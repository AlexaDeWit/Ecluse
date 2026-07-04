module Ecluse.Core.CveSpec (spec) where

import Database.SQLite.Simple (SQLError, close, execute_)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDbRejected (..), CveLookup (..), openCveDb)
import Ecluse.Core.Cve.Internal (openHardenedConnection)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), mkDbWithMaliciousTrigger, mkDbWithViewShadowingRanges, mkDbWithWrongEpoch)
import Ecluse.Test.OsvDb (withFixtureOsvDb)

-- CorpusV1's rows, in the fake's vocabulary. Kept in lockstep with the corpus
-- pins in Ecluse.Test.OsvSpec: the conformance cases below run against both
-- this fake and the real artifact compiled from the same corpus.
corpusRows :: [(Text, AdvisoryRange)]
corpusRows =
    [ ("@corpus/scoped", AdvisoryRange "GHSA-corpus-0005" (Just "LOW") (Just "0") (Just "3.0.0"))
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing (Just "0") (Just "1.0.0"))
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing (Just "1.5.0") (Just "2.0.0"))
    , ("corpus-unfixed", AdvisoryRange "GHSA-corpus-0002" (Just "CRITICAL") (Just "1.0.0") Nothing)
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0001" (Just "HIGH") (Just "0") (Just "1.2.0"))
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0004" (Just "MODERATE") (Just "2.0.0") (Just "2.5.0"))
    ]

-- The behavioural contract, written once and run against every 'CveLookup'
-- implementation, so the fake the core suite trusts cannot drift from the
-- real handle.
lookupContract :: ((CveLookup -> IO ()) -> IO ()) -> Spec
lookupContract withLookup = do
    it "probes True for a version an advisory names as its fixed bound" $
        withLookup $ \l -> do
            cveRemediationProbe l "corpus-vuln" "1.2.0" `shouldReturn` True
            cveRemediationProbe l "corpus-vuln" "2.5.0" `shouldReturn` True
            cveRemediationProbe l "@corpus/scoped" "3.0.0" `shouldReturn` True

    it "probes False for versions no advisory names as a fix" $
        withLookup $ \l -> do
            cveRemediationProbe l "corpus-vuln" "1.2.1" `shouldReturn` False
            cveRemediationProbe l "corpus-unfixed" "1.0.0" `shouldReturn` False
            cveRemediationProbe l "no-such-package" "1.0.0" `shouldReturn` False

    it "returns every advisory range recorded against a package" $
        withLookup $ \l -> do
            ranges <- cveAdvisoriesFor l "corpus-vuln"
            sortOn arCveId ranges
                `shouldBe` [ AdvisoryRange "GHSA-corpus-0001" (Just "HIGH") (Just "0") (Just "1.2.0")
                           , AdvisoryRange "GHSA-corpus-0004" (Just "MODERATE") (Just "2.0.0") (Just "2.5.0")
                           ]

    it "returns nothing for a package with no advisories" $
        withLookup (\l -> cveAdvisoriesFor l "no-such-package" `shouldReturn` [])

withFakeLookup :: (CveLookup -> IO ()) -> IO ()
withFakeLookup use = use (fakeCveLookup corpusRows)

withRealLookup :: (CveLookup -> IO ()) -> IO ()
withRealLookup use =
    withFixtureOsvDb CorpusV1 $ \dbFile ->
        bracket (openOrFail dbFile) cveClose use
  where
    openOrFail dbFile =
        openCveDb Npm dbFile >>= \case
            Right l -> pure l
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)

spec :: Spec
spec = do
    describe "CveLookup conformance (the fake and the real handle agree)" $ do
        describe "in-memory fake" (lookupContract withFakeLookup)
        describe "SQLite handle over the compiled corpus" (lookupContract withRealLookup)

    describe "openCveDb acceptance" $ do
        it "rejects an artifact stamped with the wrong schema epoch" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "wrong-epoch.db"
                mkDbWithWrongEpoch path
                openCveDb Npm path >>= rejectionShouldBe (CveDbWrongEpoch (osvSchemaEpoch + 1))

        it "rejects an artifact whose ranges relation is a view" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "view-shadow.db"
                mkDbWithViewShadowingRanges path
                openCveDb Npm path >>= rejectionShouldBe CveDbRangesNotATable

        it "rejects an artifact compiled for a different ecosystem" $
            withFixtureOsvDb CorpusV1 (openCveDb PyPI >=> rejectionShouldBe (CveDbEcosystemMismatch (Just "npm")))

        it "ignores a malicious trigger: reads behave as on a clean artifact" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "trigger.db"
                mkDbWithMaliciousTrigger path
                openCveDb Npm path >>= \case
                    Left rejection -> fail ("trigger artifact unexpectedly rejected: " <> show rejection)
                    Right l -> do
                        cveRemediationProbe l "trigger-pkg" "1.0.0" `shouldReturn` True
                        cveClose l

    describe "the hardened connection" $ do
        it "refuses writes outright, so no trigger can ever fire through it" $
            withFixtureOsvDb CorpusV1 $ \dbFile -> do
                opened <- openHardenedConnection Npm dbFile
                case opened of
                    Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
                    Right conn -> do
                        let write = execute_ conn "INSERT INTO meta (key, value) VALUES ('tampered', '1')"
                        write `shouldThrow` \(_ :: SQLError) -> True
                        close conn

-- A rejection assertion that also releases the handle if acceptance
-- unexpectedly succeeded, so a failing test never leaks the connection.
rejectionShouldBe :: CveDbRejected -> Either CveDbRejected CveLookup -> IO ()
rejectionShouldBe expected = \case
    Left rejection -> rejection `shouldBe` expected
    Right l -> do
        cveClose l
        fail ("expected rejection " <> show expected <> " but the artifact was accepted")

module Ecluse.Core.CveSpec (spec) where

import Data.List (isSuffixOf)
import Database.SQLite.Simple (SQLError, close, execute_)
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Exception (catchAny)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveDbRejected (..), CveLookup (..), openCveDb, withCveDb)
import Ecluse.Core.Cve.Internal (openHardenedConnection)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), mkDbWithMalformedProvenance, mkDbWithMaliciousTrigger, mkDbWithViewShadowingRanges, mkDbWithWrongEpoch)
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
        withCveDb Npm dbFile use >>= \case
            Right () -> pass
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

        it "withCveDb short-circuits a rejection without running the action" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "wrong-epoch.db"
                mkDbWithWrongEpoch path
                ran <- newIORef False
                result <- withCveDb Npm path (\_ -> writeIORef ran True)
                result `shouldBe` Left (CveDbWrongEpoch (osvSchemaEpoch + 1))
                readIORef ran `shouldReturn` False

        it "closes the connection when a malformed provenance row fails handle construction" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "malformed-meta.db"
                mkDbWithMalformedProvenance path
                openCveDb Npm path `shouldThrow` anyException
                -- The failed construction must not leak its accepted
                -- connection: no descriptor may still reference the artifact.
                held <- openFdTargets
                held `shouldSatisfy` not . any (path `isSuffixOf`)

        it "ignores a malicious trigger: reads behave as on a clean artifact" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "trigger.db"
                mkDbWithMaliciousTrigger path
                withCveDb Npm path (\l -> cveRemediationProbe l "trigger-pkg" "1.0.0" `shouldReturn` True) >>= \case
                    Left rejection -> fail ("trigger artifact unexpectedly rejected: " <> show rejection)
                    Right () -> pass

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

-- Every path this process holds an open descriptor to (Linux's /proc table;
-- empty elsewhere, degrading the leak assertion to the throw alone).
openFdTargets :: IO [FilePath]
openFdTargets =
    ( do
        fds <- listDirectory "/proc/self/fd"
        catMaybes
            <$> forM
                fds
                (\fd -> (Just <$> getSymbolicLinkTarget ("/proc/self/fd" </> fd)) `catchAny` const (pure Nothing))
    )
        `catchAny` const (pure [])

-- A rejection assertion that also releases the resource if acceptance
-- unexpectedly succeeded, so a failing test never leaks the connection.
rejectionShouldBe :: CveDbRejected -> Either CveDbRejected CveDb -> IO ()
rejectionShouldBe expected = \case
    Left rejection -> rejection `shouldBe` expected
    Right db -> do
        cveDbClose db
        fail ("expected rejection " <> show expected <> " but the artifact was accepted")

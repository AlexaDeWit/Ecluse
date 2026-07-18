-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Cve.AcceptanceSpec (spec) where

import Data.List (isSuffixOf)
import Database.SQLite.Simple (Only, Query (Query), SQLError, close, execute_, fromOnly, open, query_)
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Exception (bracket, catchAny, finally, try)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveDbRejected (..), CveLookup (..), CveQueryFault (cqfQuery), openCveDb)
import Ecluse.Core.Cve.Internal (openHardenedConnection)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (metaTableDdl, osvSchemaEpoch, rangesTableDdl)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), mkDbWithCorruptPage, mkDbWithLaxSchema, mkDbWithMalformedProvenance, mkDbWithMaliciousTrigger, mkDbWithViewShadowingRanges, mkDbWithWrongEpoch)
import Ecluse.Test.OsvDb (withFixtureOsvDb)

-- CorpusV1's rows, in the fake's vocabulary. Kept in lockstep with the corpus
-- pins in Ecluse.Test.OsvSpec: the conformance cases below run against both
-- this fake and the real artifact compiled from the same corpus.
-- Severities are the CVSS band ceilings the writer maps the fixtures' GHSA labels
-- to (LOW 3.9, MODERATE 6.9, HIGH 8.9, CRITICAL 10.0); these fixtures carry no
-- last_affected bound, so it is Nothing throughout.
corpusRows :: [(Text, AdvisoryRange)]
corpusRows =
    [ ("@corpus/scoped", AdvisoryRange "GHSA-corpus-0005" (Just 3.9) (Just "0") (Just "3.0.0") Nothing)
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing (Just "0") (Just "1.0.0") Nothing)
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing (Just "1.5.0") (Just "2.0.0") Nothing)
    , ("corpus-unfixed", AdvisoryRange "GHSA-corpus-0002" (Just 10.0) (Just "1.0.0") Nothing Nothing)
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0001" (Just 8.9) (Just "0") (Just "1.2.0") Nothing)
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0004" (Just 6.9) (Just "2.0.0") (Just "2.5.0") Nothing)
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
                `shouldBe` [ AdvisoryRange "GHSA-corpus-0001" (Just 8.9) (Just "0") (Just "1.2.0") Nothing
                           , AdvisoryRange "GHSA-corpus-0004" (Just 6.9) (Just "2.0.0") (Just "2.5.0") Nothing
                           ]

    it "returns nothing for a package with no advisories" $
        withLookup (\l -> cveAdvisoriesFor l "no-such-package" `shouldReturn` [])

withFakeLookup :: (CveLookup -> IO ()) -> IO ()
withFakeLookup use = use (fakeCveLookup corpusRows)

-- Hand the body the fixture artifact's path and its accepted owning handle; a
-- rejection of the fixture is a loud test failure. The body owns the close.
withAcceptedDb :: (FilePath -> CveDb -> IO ()) -> IO ()
withAcceptedDb body =
    withFixtureOsvDb CorpusV1 $ \dbFile ->
        openCveDb Npm dbFile >>= \case
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
            Right db -> body dbFile db

withRealLookup :: (CveLookup -> IO ()) -> IO ()
withRealLookup use =
    withFixtureOsvDb CorpusV1 $
        openCveDb Npm >=> \case
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
            Right db -> use (cveDbLookup db) `finally` cveDbClose db

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
                openCveDb Npm path >>= rejectionShouldBe (CveDbSchemaNonConformant "package_vulnerability_ranges")

        it "rejects an artifact whose tables are not STRICT" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "lax-schema.db"
                -- The right names and columns under affinity-hinted (non-STRICT)
                -- declarations: the reader cannot trust its decodes, so schema
                -- conformance must refuse it as a value.
                mkDbWithLaxSchema path
                openCveDb Npm path >>= rejectionShouldBe (CveDbSchemaNonConformant "package_vulnerability_ranges")

        it "rejects an artifact compiled for a different ecosystem" $
            withFixtureOsvDb CorpusV1 (openCveDb PyPI >=> rejectionShouldBe (CveDbEcosystemMismatch (Just "npm")))

        it "rejects an artifact with no meta table as a value, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "no-meta.db"
                -- A structurally-sound artifact with the canonical ranges table and
                -- the right epoch stamp but no @meta@ table: schema conformance must
                -- refuse the missing relation as a rejection value rather than an
                -- uncaught throw that would re-download the artifact every poll and
                -- leak the just-opened connection.
                bracket (open path) close $ \conn -> do
                    execute_ conn ("PRAGMA user_version = " <> show osvSchemaEpoch)
                    execute_ conn (Query rangesTableDdl)
                openCveDb Npm path >>= rejectionShouldBe (CveDbSchemaNonConformant "meta")
                -- The rejected artifact's connection must not leak.
                held <- openFdTargets
                held `shouldSatisfy` not . any (path `isSuffixOf`)

        it "rejects an artifact whose meta lacks the ecosystem row" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "no-ecosystem-row.db"
                -- Conformant tables, but @meta@ never names an ecosystem: the
                -- ecosystem cannot be confirmed, and the refusal is a value.
                bracket (open path) close $ \conn -> do
                    execute_ conn ("PRAGMA user_version = " <> show osvSchemaEpoch)
                    execute_ conn (Query rangesTableDdl)
                    execute_ conn (Query metaTableDdl)
                openCveDb Npm path >>= rejectionShouldBe (CveDbEcosystemMismatch Nothing)

        it "rejects an artifact whose stored meta values violate the strict declaration, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "malformed-meta.db"
                -- A BLOB smuggled under a forged STRICT declaration: the integrity
                -- walk verifies stored values against the declared column types and
                -- must refuse the artifact as a rejection value (so the sync task
                -- remembers its ETag), never a thrown decode error.
                mkDbWithMalformedProvenance path
                openCveDb Npm path >>= \case
                    Left (CveDbIntegrityFailed problems) -> problems `shouldSatisfy` not . null
                    Left other -> fail ("expected CveDbIntegrityFailed, got " <> show other)
                    Right db -> do
                        cveDbClose db
                        fail "expected the forged artifact to be rejected, but it was accepted"
                -- The rejected artifact's connection must not leak: no descriptor
                -- may still reference the artifact.
                held <- openFdTargets
                held `shouldSatisfy` not . any (path `isSuffixOf`)

        it "ignores a malicious trigger: reads behave as on a clean artifact" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "trigger.db"
                mkDbWithMaliciousTrigger path
                openCveDb Npm path >>= \case
                    Left rejection -> fail ("trigger artifact unexpectedly rejected: " <> show rejection)
                    Right db ->
                        (cveRemediationProbe (cveDbLookup db) "trigger-pkg" "1.0.0" `shouldReturn` True)
                            `finally` cveDbClose db

        it "rejects an artifact whose b-tree pages are structurally corrupt" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "corrupt.db"
                mkDbWithCorruptPage path
                openCveDb Npm path >>= \case
                    Left (CveDbIntegrityFailed problems) -> problems `shouldSatisfy` not . null
                    Left other -> fail ("expected CveDbIntegrityFailed, got " <> show other)
                    Right db -> do
                        cveDbClose db
                        fail "expected a corrupt artifact to be rejected, but it was accepted"

        it "rejects a non-SQLite artifact as a value, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "not-a-database.db"
                -- Arbitrary non-SQLite bytes: the header magic is absent, so the
                -- first file-touching statement (the epoch-stamp PRAGMA) makes
                -- SQLite raise SQLITE_NOTADB. That must surface as a rejection
                -- value -- so the sync task remembers the ETag rather than
                -- re-downloading the same hostile object every poll -- never an
                -- uncaught exception that leaks the just-opened connection.
                writeFileBS path "this is not an SQLite database, not even close"
                openCveDb Npm path >>= \case
                    Left (CveDbIntegrityFailed problems) -> problems `shouldSatisfy` not . null
                    Left other -> fail ("expected CveDbIntegrityFailed, got " <> show other)
                    Right db -> do
                        cveDbClose db
                        fail "expected a non-SQLite artifact to be rejected, but it was accepted"
                -- The rejected artifact's connection must not leak: no descriptor
                -- may still reference the file.
                held <- openFdTargets
                held `shouldSatisfy` not . any (path `isSuffixOf`)

    describe "the confined query-fault channel" $ do
        it "re-raises a mid-query SQLite fault as CveQueryFault, tagged with the field asked" $
            withAcceptedDb $ \dbFile db -> do
                -- Break the accepted schema out from under the open handle
                -- through a second (unhardened) connection: the next query
                -- through the view is the infrastructural fault the confined
                -- channel carries -- unreachable from artifact content, which
                -- acceptance made total.
                saboteur <- open dbFile
                execute_ saboteur "DROP TABLE package_vulnerability_ranges"
                close saboteur
                probed <- try (cveRemediationProbe (cveDbLookup db) "corpus-vuln" "1.2.0")
                first cqfQuery probed `shouldBe` Left "remediation-probe"
                listed <- try (cveAdvisoriesFor (cveDbLookup db) "corpus-vuln")
                bimap cqfQuery (map arCveId) listed `shouldBe` Left "advisories-for"
                cveDbClose db

        it "cveDbClose never throws, a second close of the same handle included" $
            withAcceptedDb $ \_dbFile db -> do
                cveDbClose db
                -- The close fault (the connection is already released) is
                -- absorbed inside the handle: total by construction.
                cveDbClose db

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

        it "validates cell sizes and reads through the pager, not a memory map" $
            withFixtureOsvDb CorpusV1 $ \dbFile -> do
                opened <- openHardenedConnection Npm dbFile
                case opened of
                    Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
                    Right conn -> do
                        cellCheck <- query_ conn "PRAGMA cell_size_check" :: IO [Only Int]
                        mmap <- query_ conn "PRAGMA mmap_size" :: IO [Only Int]
                        map fromOnly cellCheck `shouldBe` [1]
                        map fromOnly mmap `shouldBe` [0]
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

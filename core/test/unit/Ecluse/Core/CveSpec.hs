-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Opening an advisory artifact, the lookups it serves, and the pure range matching.
module Ecluse.Core.CveSpec (spec) where

import Data.List (isSuffixOf)
import Database.SQLite.Simple (Only (..), Query (Query), close, execute, execute_, open)
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Exception (bracket, catchAny, finally, try)

import Ecluse.Core.Cve (
    AdvisoryRange (..),
    CveDb (..),
    CveDbRejected (..),
    CveLookup (..),
    CveQueryFault (cqfQuery),
    MissingScorePolicy (..),
    insideAffectedRange,
    openCveDb,
    scoreAtLeast,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (EpssRequirement (..), metaTableDdl, osvSchemaEpoch, rangesTableDdl)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Osv (
    CorpusVersion (CorpusV1),
    mkDbWithCorruptPage,
    mkDbWithLaxSchema,
    mkDbWithMalformedProvenance,
    mkDbWithMaliciousTrigger,
    mkDbWithViewShadowingRanges,
    mkDbWithWrongEpoch,
    mkDbWithoutEpssColumn,
 )
import Ecluse.Test.OsvDb (withFixtureOsvDb)

-- Keep these fake rows aligned with the committed corpus pins in Ecluse.Test.OsvSpec.
corpusRows :: [(Text, AdvisoryRange)]
corpusRows =
    [ ("@corpus/scoped", AdvisoryRange "GHSA-corpus-0005" (Just 3.9) Nothing (FixedBefore "3.0.0") (Just 0.25))
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing Nothing (FixedBefore "1.0.0") Nothing)
    , ("corpus-multi", AdvisoryRange "GHSA-corpus-0003" Nothing (Just "1.5.0") (FixedBefore "2.0.0") Nothing)
    , ("corpus-unfixed", AdvisoryRange "GHSA-corpus-0002" (Just 10.0) (Just "1.0.0") Unbounded (Just 0.5))
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0001" (Just 8.9) Nothing (FixedBefore "1.2.0") (Just 0.875))
    , ("corpus-vuln", AdvisoryRange "GHSA-corpus-0004" (Just 6.9) (Just "2.0.0") (FixedBefore "2.5.0") (Just 0.0625))
    ]

-- The behavioural contract, written once and run against every 'CveLookup'
-- implementation, so the fake the core suite trusts cannot drift from the real handle.
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
                `shouldBe` [ AdvisoryRange "GHSA-corpus-0001" (Just 8.9) Nothing (FixedBefore "1.2.0") (Just 0.875)
                           , AdvisoryRange "GHSA-corpus-0004" (Just 6.9) (Just "2.0.0") (FixedBefore "2.5.0") (Just 0.0625)
                           ]

    it "returns nothing for a package with no advisories" $
        withLookup (\l -> cveAdvisoriesFor l "no-such-package" `shouldReturn` [])

    it "enumerates every name it holds an advisory against" $
        withLookup $ \l -> do
            covered <- cveCoveredNames l
            filter (`notElem` covered) (ordNub (map fst corpusRows)) `shouldBe` []

    it "enumerates a name carrying several advisories once, so a sweep reads it once" $
        -- corpus-multi carries two ranges, so a per-row enumeration would name it twice and make
        -- the store sweep read the same package's metadata twice in one cycle.
        withLookup $ \l -> do
            covered <- cveCoveredNames l
            length covered `shouldBe` length (ordNub covered)

    it "names nothing it holds no advisory against" $
        withLookup $ \l -> do
            covered <- cveCoveredNames l
            ranges <- traverse (cveAdvisoriesFor l) covered
            filter null ranges `shouldBe` []

withFakeLookup :: (CveLookup -> IO ()) -> IO ()
withFakeLookup use = use (fakeCveLookup corpusRows)

-- Hand the body the fixture artifact's path and its accepted owning handle. A
-- rejection of the fixture is a loud test failure. The body owns the close.
withAcceptedDb :: (FilePath -> CveDb -> IO ()) -> IO ()
withAcceptedDb body =
    withFixtureOsvDb CorpusV1 $ \dbFile ->
        openCveDb Npm EpssOptional dbFile >>= \case
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
            Right db -> body dbFile db

withRealLookup :: (CveLookup -> IO ()) -> IO ()
withRealLookup use =
    withFixtureOsvDb CorpusV1 $
        openCveDb Npm EpssOptional >=> \case
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
            Right db -> use (cveDbLookup db) `finally` cveDbClose db

-- A builder for an advisory segment, exposing only its bounds.
range :: Maybe Text -> UpperBound -> AdvisoryRange
range intro upper =
    AdvisoryRange
        { arCveId = "GHSA-test"
        , arSeverity = Nothing
        , arIntroduced = intro
        , arUpperBound = upper
        , arEpss = Nothing
        }

-- A builder for an interval closed by an inclusive @last_affected@ bound.
through :: Maybe Text -> Text -> AdvisoryRange
through intro lastAffected = range intro (LastAffected lastAffected)

-- A builder for an exact affected point (introduced == last_affected).
point :: Text -> AdvisoryRange
point v = through (Just v) v

inside :: Text -> AdvisoryRange -> Bool
inside = insideAffectedRange Npm

spec :: Spec
spec = do
    describe "CveLookup conformance (the fake and the real handle agree)" $ do
        describe "in-memory fake" (lookupContract withFakeLookup)
        describe "SQLite handle over the compiled corpus" (lookupContract withRealLookup)

    describe "openCveDb acceptance" $ do
        for_ [EpssRequired, EpssOptional] $ \requirement ->
            for_ [Nothing, Just "available", Just "unavailable", Just "unknown"] $ \marker ->
                it ("qualifies " <> show marker <> " under " <> show requirement) $
                    withFixtureOsvDb CorpusV1 $ \path -> do
                        bracket (open path) close $ \conn -> do
                            execute_ conn "DELETE FROM meta WHERE key = 'epss_status'"
                            for_ marker $ \value -> execute conn "INSERT INTO meta (key, value) VALUES ('epss_status', ?)" (Only (value :: Text))
                        result <- openCveDb Npm requirement path
                        if requirement == EpssRequired && marker /= Just "available"
                            then do
                                rejectionShouldBe CveDbEpssNotEstablished result
                                held <- openFdTargets
                                held `shouldSatisfy` not . any (path `isSuffixOf`)
                            else case result of
                                Left rejection -> fail ("unexpected qualification rejection: " <> show rejection)
                                Right db -> cveDbClose db

        it "accepts established enrichment without dates or individual scores" $
            withFixtureOsvDb CorpusV1 $ \path -> do
                bracket (open path) close $ \conn -> do
                    execute_ conn "INSERT OR REPLACE INTO meta (key, value) VALUES ('epss_status', 'available')"
                    execute_ conn "DELETE FROM meta WHERE key IN ('epss_score_date', 'epss_last_modified', 'epss_model_version')"
                    execute_ conn "UPDATE package_vulnerability_ranges SET epss_score = NULL"
                openCveDb Npm EpssRequired path >>= \case
                    Left rejection -> fail ("unscored artifact rejected: " <> show rejection)
                    Right db -> flip finally (cveDbClose db) $ do
                        ranges <- cveAdvisoriesFor (cveDbLookup db) "corpus-vuln"
                        map arEpss ranges `shouldBe` [Nothing, Nothing]
                        cveRemediationProbe (cveDbLookup db) "corpus-vuln" "1.2.0" `shouldReturn` True

        it "rejects epoch 3 even when its tables conform to the current shape" $
            withFixtureOsvDb CorpusV1 $ \path -> do
                bracket (open path) close $ \conn -> execute_ conn "PRAGMA user_version = 3"
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbWrongEpoch 3)

        it "rejects an artifact stamped with the wrong schema epoch" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "wrong-epoch.db"
                mkDbWithWrongEpoch path
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbWrongEpoch (osvSchemaEpoch + 1))

        it "rejects an artifact whose ranges relation is a view" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "view-shadow.db"
                mkDbWithViewShadowingRanges path
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbSchemaNonConformant "package_vulnerability_ranges")

        it "rejects an artifact whose tables are not STRICT" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "lax-schema.db"
                -- The reader cannot trust decodes under affinity-hinted (non-STRICT) declarations,
                -- so schema conformance must refuse the artifact as a value.
                mkDbWithLaxSchema path
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbSchemaNonConformant "package_vulnerability_ranges")

        it "rejects an artifact whose ranges table lacks a column the reader decodes" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "no-epss-column.db"
                -- Missing columns violate the artifact shape regardless of the configured rules.
                mkDbWithoutEpssColumn path
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbSchemaNonConformant "package_vulnerability_ranges")

        it "rejects an artifact compiled for a different ecosystem" $
            withFixtureOsvDb CorpusV1 (openCveDb PyPI EpssOptional >=> rejectionShouldBe (CveDbEcosystemMismatch (Just "npm")))

        it "rejects an artifact with no meta table as a value, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "no-meta.db"
                -- Missing metadata must reject the artifact without leaking a connection.
                bracket (open path) close $ \conn -> do
                    execute_ conn ("PRAGMA user_version = " <> show osvSchemaEpoch)
                    execute_ conn (Query rangesTableDdl)
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbSchemaNonConformant "meta")
                -- The rejected artifact's connection must not leak.
                held <- openFdTargets
                held `shouldSatisfy` not . any (path `isSuffixOf`)

        it "rejects an artifact whose meta lacks the ecosystem row" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "no-ecosystem-row.db"
                -- Conformant tables, but @meta@ never names an ecosystem: acceptance
                -- cannot confirm the ecosystem, and the refusal is a value.
                bracket (open path) close $ \conn -> do
                    execute_ conn ("PRAGMA user_version = " <> show osvSchemaEpoch)
                    execute_ conn (Query rangesTableDdl)
                    execute_ conn (Query metaTableDdl)
                openCveDb Npm EpssOptional path >>= rejectionShouldBe (CveDbEcosystemMismatch Nothing)

        it "rejects an artifact whose stored meta values violate the strict declaration, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "malformed-meta.db"
                -- A BLOB smuggled under a forged STRICT declaration. Refusal must be a rejection
                -- value, never a thrown decode error, so the sync task remembers its ETag.
                mkDbWithMalformedProvenance path
                openCveDb Npm EpssOptional path >>= \case
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
                openCveDb Npm EpssOptional path >>= \case
                    Left rejection -> fail ("trigger artifact unexpectedly rejected: " <> show rejection)
                    Right db ->
                        (cveRemediationProbe (cveDbLookup db) "trigger-pkg" "1.0.0" `shouldReturn` True)
                            `finally` cveDbClose db

        it "rejects an artifact whose b-tree pages are structurally corrupt" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "corrupt.db"
                mkDbWithCorruptPage path
                openCveDb Npm EpssOptional path >>= \case
                    Left (CveDbIntegrityFailed problems) -> problems `shouldSatisfy` not . null
                    Left other -> fail ("expected CveDbIntegrityFailed, got " <> show other)
                    Right db -> do
                        cveDbClose db
                        fail "expected a corrupt artifact to be rejected, but it was accepted"

        it "rejects a non-SQLite artifact as a value, without leaking the connection" $
            withSystemTempDirectory "ecluse-cve-hostile" $ \dir -> do
                let path = dir </> "not-a-database.db"
                -- SQLITE_NOTADB must become a rejection value without leaking a connection.
                writeFileBS path "this is not an SQLite database, not even close"
                openCveDb Npm EpssOptional path >>= \case
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
                -- Break the schema after acceptance to force a query fault through the confined channel.
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
                -- The handle absorbs the close fault (the connection is already
                -- released): total by construction.
                cveDbClose db

    describe "scoreAtLeast" $ do
        it "clears the threshold at or above it, and not below" $ do
            scoreAtLeast DenyMissingScore 8.0 (Just 9.8) `shouldBe` True
            scoreAtLeast DenyMissingScore 8.0 (Just 8.0) `shouldBe` True
            scoreAtLeast DenyMissingScore 8.0 (Just 7.9) `shouldBe` False

        it "counts an absent score as clearing every threshold, the fail-closed direction" $ do
            scoreAtLeast DenyMissingScore 10.0 Nothing `shouldBe` True
            scoreAtLeast DenyMissingScore 0.0 Nothing `shouldBe` True

        it "abstains on an absent EPSS score even at zero threshold" $ do
            scoreAtLeast AbstainMissingScore 0.0 Nothing `shouldBe` False
            scoreAtLeast AbstainMissingScore 1.0 Nothing `shouldBe` False

        it "compares known EPSS scores at the threshold" $ do
            scoreAtLeast AbstainMissingScore 0.5 (Just 0.49) `shouldBe` False
            scoreAtLeast AbstainMissingScore 0.5 (Just 0.5) `shouldBe` True
            scoreAtLeast AbstainMissingScore 0.5 (Just 0.75) `shouldBe` True

    describe "insideAffectedRange" $ do
        describe "the half-open interval [introduced, fixed)" $ do
            it "contains a version strictly between the bounds" $
                inside "1.5.0" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` True

            it "contains the introduced bound itself" $
                inside "1.0.0" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` True

            it "excludes a version below the introduced bound" $
                inside "0.9.0" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` False

            it "excludes the fixed bound itself (the fix is not affected)" $
                inside "2.0.0" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` False

            it "excludes a version above the fixed bound" $
                inside "2.1.0" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` False

        describe "open ends" $ do
            it "a missing introduced bound starts the range at the beginning" $
                inside "0.0.1" (range Nothing (FixedBefore "2.0.0")) `shouldBe` True

            it "an unbounded segment never ends the range" $
                inside "99.0.0" (range (Just "1.0.0") Unbounded) `shouldBe` True

        describe "the inclusive last_affected bound [introduced, last_affected]" $ do
            it "contains the last_affected bound itself (unlike a fix)" $
                inside "3.8.8" (through (Just "0") "3.8.8") `shouldBe` True

            it "excludes a version above the last_affected bound" $
                inside "3.9.0" (through (Just "0") "3.8.8") `shouldBe` False

        describe "an exact affected point (introduced == last_affected)" $ do
            it "is affected only at that exact version" $
                inside "1.0.0" (point "1.0.0") `shouldBe` True

            it "excludes any other version, above or below" $ do
                inside "1.0.1" (point "1.0.0") `shouldBe` False
                inside "0.9.9" (point "1.0.0") `shouldBe` False

        describe "fail-closed on unprovable comparisons" $ do
            it "an unparseable introduced bound counts as inside" $
                inside "0.0.1" (range (Just "not-a-version") (FixedBefore "2.0.0")) `shouldBe` True

            it "an unparseable fixed bound counts as inside" $
                inside "99.0.0" (range (Just "1.0.0") (FixedBefore "not-a-version")) `shouldBe` True

            it "an unparseable subject version counts as inside" $
                inside "definitely not semver" (range (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` True

            it "answers a decoded \"0\" lower bound exactly as it answers the raw one" $ do
                -- Pilot decodes OSV's "0" lower bound to no lower bound at all. Semver cannot
                -- order "0", so on npm the two spellings agree at every version.
                let agrees upper v = inside v (range Nothing upper) `shouldBe` inside v (range (Just "0") upper)
                    versions = ["0.0.1", "1.0.0", "99.0.0", "definitely not semver"]
                traverse_ (agrees (FixedBefore "2.0.0")) versions
                traverse_ (agrees (LastAffected "3.8.8")) versions
                traverse_ (agrees Unbounded) versions

            it "an unorderable range endpoint denies every version of the package" $ do
                -- The deliberate fail-closed direction: nothing can place the fix, so no
                -- version can be shown to sit at or above it.
                inside "0.0.1" (range Nothing (FixedBefore "2026.05.1")) `shouldBe` True
                inside "99.0.0" (range Nothing (FixedBefore "2026.05.1")) `shouldBe` True
                inside "1.0.0" (range (Just "6.0") Unbounded) `shouldBe` True

        describe "the segment a decoded \"0\" lower bound leaves" $ do
            it "denies below the fix and admits at and above it, on npm" $ do
                inside "1.9.9" (range Nothing (FixedBefore "2.0.0")) `shouldBe` True
                inside "2.0.0" (range Nothing (FixedBefore "2.0.0")) `shouldBe` False
                inside "2.0.1" (range Nothing (FixedBefore "2.0.0")) `shouldBe` False

            it "denies below the fix and admits at and above it, on PyPI" $ do
                insideAffectedRange PyPI "1.9.9" (range Nothing (FixedBefore "2.0")) `shouldBe` True
                insideAffectedRange PyPI "2.0" (range Nothing (FixedBefore "2.0")) `shouldBe` False
                insideAffectedRange PyPI "2.0.post1" (range Nothing (FixedBefore "2.0")) `shouldBe` False

            it "covers a PyPI pre-release of the zero version, which a \"0\" bound excluded" $ do
                -- PEP 440 orders 0rc1 below 0, so the raw bound left it outside the range it
                -- belongs to. With no lower bound it is inside.
                insideAffectedRange PyPI "0rc1" (range (Just "0") (FixedBefore "2.0")) `shouldBe` False
                insideAffectedRange PyPI "0rc1" (range Nothing (FixedBefore "2.0")) `shouldBe` True

        describe "a point segment naming a version no grammar can order" $ do
            it "is affected at exactly its own string" $
                inside "0.1-bulbasaur" (point "0.1-bulbasaur") `shouldBe` True

            it "admits every other version, including a parseable neighbour" $ do
                inside "1.0.0" (point "0.1-bulbasaur") `shouldBe` False
                inside "0.1-charmander" (point "0.1-bulbasaur") `shouldBe` False

            it "leaves an orderable point on the ordinary comparison" $ do
                inside "1.0.0" (point "1.0.0") `shouldBe` True
                inside "1.0.1" (point "1.0.0") `shouldBe` False
                -- Ordered equality, not string equality. Both hold only while a bound the
                -- grammar parses keeps the point arm out of the way.
                insideAffectedRange PyPI "1.0.0" (point "1.0") `shouldBe` True
                inside "1.0.0+build" (point "1.0.0") `shouldBe` True

            it "does not read a range with two different unorderable bounds as a point" $
                -- Only a segment whose bounds are the same text names one version. Anything
                -- else keeps the fail-closed reading.
                inside "9.9.9" (through (Just "0.9-stable") "1.3") `shouldBe` True

-- Every path this process holds an open descriptor to, read from Linux's /proc
-- table. It is empty elsewhere, which degrades the leak assertion to the throw alone.
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

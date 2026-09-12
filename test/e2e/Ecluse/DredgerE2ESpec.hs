-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Dredger lifecycle evidence from a real Verdaccio store.
Complete version snapshots connect store contents with each cycle's audit records. One group
decides by operator identity alone, the other by the advisory generation Pilot compiles and every
role reads back from object storage.
-}
module Ecluse.DredgerE2ESpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

import Ecluse.E2E.Fixtures.Npm (
    PkgSpec,
    corpusRevokedPkg,
    dredgerDryRunPkg,
    dredgerKeepPkg,
    dredgerPkg,
    psName,
    psVersion,
    psVersions,
 )
import Ecluse.E2E.Harness
import Ecluse.Test.Log (lineMessage)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2))

-- | Verify store contents and audit records under operator identity denies and advisory denials.
spec :: Spec
spec = do
    unavailable <- runIO e2eUnavailable
    case unavailable of
        Just reason -> it "Dredger end-to-end environment is available" (pendingWith reason)
        Nothing -> do
            aroundAll withGlobalDataPlane (aroundAllWith withSeededStore identityScenarios)
            aroundAll withGlobalDataPlane revocationScenario

identityScenarios :: SpecWith (GlobalDataPlane, E2E)
identityScenarios = describe "identity denies with no advisory database" $ do
    it "walks the seeded store through listPackagesIn, including scoped base-name buckets" $ \(_, e2e) -> do
        verdaccioSnapshot e2e `shouldReturn` seededVersions
        names <- verdaccioListing e2e
        names `shouldMatchList` Map.keys seededVersions
        verdaccioNamesUnder e2e "" `shouldReturn` sort names
        verdaccioNamesUnder e2e "e" `shouldReturn` sort names
        verdaccioNamesUnder e2e "z" `shouldReturn` []

    it "deletes every denied version and preserves all other versions, including first-party versions" $ \(plane, e2e) -> do
        initial <- verdaccioSnapshot e2e
        run <- runDredgerOnce plane ["--once"] (sweepEnv dredgerPkg)
        assertFullSweep "deleting " dredgerPkg initial run
        verdaccioVersions e2e (psName dredgerPkg) `shouldReturn` []
        finalStore <- verdaccioSnapshot e2e
        finalStore `shouldBe` Map.delete (psName dredgerPkg) initial
        verdaccioNamesUnder e2e "" `shouldReturn` Map.keys finalStore

    it "reports a preview as partial without an advisory generation, and preserves the store snapshot" $ \(plane, e2e) -> do
        initial <- verdaccioSnapshot e2e
        run <- runDredgerOnce plane ["--once", "--dry-run"] (sweepEnv dredgerDryRunPkg)
        -- The shipped rule set reads advisories and no generation is loaded here, so the preview
        -- counted from part of the store. Its status says so, and its counts still report the reach.
        roleExit run `shouldSatisfy` (/= ExitSuccess)
        assertSweepLines "dry run, would delete " dredgerDryRunPkg initial run
        roleOutput run `shouldSatisfy` T.isInfixOf "previewing only: this run holds nothing that could delete"
        roleOutput run `shouldSatisfy` T.isInfixOf "This preview deleted nothing, so it proves no authority to delete"
        verdaccioSnapshot e2e `shouldReturn` initial

    it "refuses missing consent, names the key, and leaves every version intact" $ \(plane, e2e) -> do
        initial <- verdaccioSnapshot e2e
        run <- runDredgerOnce plane ["--once"] (sweepEnv dredgerPkg <> [(consentKey, "false")])
        (roleExit run, roleOutput run) `shouldSatisfy` ((/= ExitSuccess) . fst)
        roleOutput run `shouldSatisfy` T.isInfixOf (consentKey <> " is not set")
        sweepMessages run `shouldBe` []
        verdaccioSnapshot e2e `shouldReturn` initial

    it "protects every first-party version even when an identity deny names one" $ \(plane, e2e) -> do
        initial <- verdaccioSnapshot e2e
        let rules = identityRule (publishDredgerName <> "@" <> publishVersion)
            guardCount = length (Map.findWithDefault [] publishDredgerName initial)
        run <- runDredgerOnce plane ["--once"] [("ECLUSE_RULES", rules), ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)]
        (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        sweepMessages run `shouldBe` [cycleLine ["examined 0", "deleted 0", "kept 0", "guard-skipped " <> show guardCount]]
        verdaccioVersions e2e publishDredgerName `shouldReturn` firstPartyVersions
        verdaccioSnapshot e2e `shouldReturn` initial

withSeededStore :: ((GlobalDataPlane, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withSeededStore action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = publishTargetEnv} seed plane
  where
    seed e2e = do
        for_ mirroredPackages $ \pkg -> do
            verdaccioVersions e2e (psName pkg) `shouldReturn` []
            void $ npmInstall e2e (psName pkg) >>= shouldSucceed
            verdaccioHasVersion e2e (psName pkg) (psVersion pkg) `shouldReturn` True
        for_ firstPartyVersions $ \version -> do
            void $ withPublishProject e2e publishDredgerName version npmPublishIn >>= shouldSucceed
            verdaccioHasVersion e2e publishDredgerName version `shouldReturn` True
        awaitListed e2e (Map.keys seededVersions)
        action (plane, e2e)

mirroredPackages :: [PkgSpec]
mirroredPackages = [dredgerPkg, dredgerKeepPkg, dredgerDryRunPkg]

seededVersions :: Map Text [Text]
seededVersions = Map.fromList ((publishDredgerName, firstPartyVersions) : [(psName pkg, [psVersion pkg]) | pkg <- mirroredPackages])

firstPartyVersions :: [Text]
firstPartyVersions = [publishVersion, "2.0.0"]

consentKey :: Text
consentKey = "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"

sweepEnv :: PkgSpec -> [(Text, Text)]
sweepEnv pkg =
    [ ("ECLUSE_RULES", identityRule (psName pkg))
    , ("ECLUSE_DREDGER__FULL_WALK", "true")
    , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)
    ]

identityRule :: Text -> Text
identityRule name = renderRules ["revoke-swept" .= object ["type" .= ("DenyByIdentity" :: Text), "identity" .= name]]

assertFullSweep :: Text -> PkgSpec -> Map Text [Text] -> RoleRun -> Expectation
assertFullSweep opening pkg initial run = do
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    assertSweepLines opening pkg initial run

-- | The lines a full walk over the seeded store wrote, for a run whose own status the case asserts.
assertSweepLines :: Text -> PkgSpec -> Map Text [Text] -> RoleRun -> Expectation
assertSweepLines opening pkg initial run = do
    let versions = Map.findWithDefault [] (psName pkg) initial
        guardCount = length (Map.findWithDefault [] publishDredgerName initial)
        examined = sum (map length (Map.elems initial)) - guardCount
        fields =
            [ "examined " <> show examined
            , "deleted " <> show (length versions)
            , "kept " <> show (examined - length versions)
            , "guard-skipped " <> show guardCount
            ]
    versions `shouldBe` [psVersion pkg]
    sweepMessages run `shouldMatchList` (map auditLine versions <> [cycleLine fields])
  where
    auditLine version =
        opening
            <> psName pkg
            <> "@"
            <> version
            <> ": blocked by DenyByIdentity (identity "
            <> psName pkg
            <> " is revoked by operator); advisory generation none"

{- The cycle's closing line. The shipped rule set reads advisories, so a cycle with no generation
loaded reports the gap it decided across beside its counts. -}
cycleLine :: [Text] -> Text
cycleLine fields =
    "mirror sweep cycle complete: "
        <> T.intercalate ", " fields
        <> "; counted from partial evidence: 1 mount decided without an advisory generation"

revocationScenario :: SpecWith GlobalDataPlane
revocationScenario =
    describe "advisory compilation, mirror revocation, and the next install" $
        aroundAllWith withAdvisoryStore $
            it "revokes the version a new advisory generation condemns and keeps its fix installable" $ \(plane, e2e) -> do
                seeded <- seedRevocationStore e2e
                loadSecondGeneration plane e2e
                run <- runDredgerOnce plane ["--once"] (advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules)])
                assertRevoked e2e seeded run
                assertVulnerableRefused e2e
                assertFixInstallable e2e

{- The first generation reaches the store before the proxy boots, because the proxy's readiness gate
waits for a successful advisory sync. -}
withAdvisoryStore :: ((GlobalDataPlane, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withAdvisoryStore action plane = do
    createAdvisoryBucket plane
    compileGeneration plane CorpusV1
    withE2EWith defaultE2EConfig{ecExtraEnv = revocationProxyEnv} (\e2e -> action (plane, e2e)) plane

{- The proxy reads the advisory store the Pilot wrote, under the same policy the Dredger sweeps by.
Its packument cache turns over in a second, so a swapped-in generation reaches the next install. -}
revocationProxyEnv :: [(Text, Text)]
revocationProxyEnv = advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules), ("ECLUSE_CACHE__TTL", "1")]

{- Both roles decide under one policy: the quarantine every fixture version predates, and the
advisory deny a new generation turns on. -}
advisoryRules :: Text
advisoryRules =
    renderRules
        [ "min-age" .= object ["type" .= ("AllowIfOlderThan" :: Text), "ageSeconds" .= (0 :: Int)]
        , "deny-cve" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]
        ]

-- Run Pilot to completion, reporting its own output when the compile or the upload did not land.
compileGeneration :: GlobalDataPlane -> CorpusVersion -> IO ()
compileGeneration plane generation = do
    run <- publishAdvisoryGeneration plane generation
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)

{- Install every fixture version through the proxy so the worker mirrors it, then snapshot the store
the sweep decides over. -}
seedRevocationStore :: E2E -> IO (Map Text [Text])
seedRevocationStore e2e = do
    for_ revokedVersions $ \version ->
        void $ npmInstall e2e (revokedName <> "@" <> version) >>= shouldSucceed
    void $ npmInstall e2e (psName dredgerKeepPkg) >>= shouldSucceed
    verdaccioAwaitVersions e2e revokedName revokedVersions `shouldReturn` revokedVersions
    verdaccioAwaitVersions e2e (psName dredgerKeepPkg) [psVersion dredgerKeepPkg]
        `shouldReturn` [psVersion dredgerKeepPkg]
    awaitListed e2e [revokedName, psName dredgerKeepPkg]
    verdaccioSnapshot e2e

{- Publish the generation that names the target and wait for the running proxy to swap it in. The
second swap line is the cue that the sweep and the next install both decide under it. -}
loadSecondGeneration :: GlobalDataPlane -> E2E -> IO ()
loadSecondGeneration plane e2e = do
    compileGeneration plane CorpusV2
    swapped <- awaitProxyLog e2e ((> 1) . length . T.breakOnAll swapMessage) 240
    unless swapped $ do
        logs <- proxyContainerLogs e2e
        expectationFailure (toString ("the proxy never swapped in the second generation. Its last lines:\n" <> T.takeEnd 4000 logs))

-- The sync's own line for a generation taking effect, so a second one means G2 replaced G1.
swapMessage :: Text
swapMessage = "advisory database swapped in"

{- The cycle condemns the affected version by name and leaves the fix and every version no advisory
covers. The whole message list is the subject, so a mismatch prints the generation each line named. -}
assertRevoked :: E2E -> Map Text [Text] -> RoleRun -> Expectation
assertRevoked e2e seeded run = do
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    let messages = sweepMessages run
    unless (reportsRevocation messages) (expectationFailure (toString (revocationReport messages run)))
    verdaccioSnapshot e2e `shouldReturn` Map.adjust (filter (/= vulnerableVersion)) revokedName seeded

-- What the cycle reported, with the generation each line named, then the run's whole output.
revocationReport :: [Text] -> RoleRun -> Text
revocationReport messages run =
    "the cycle reported these sweep lines:\n"
        <> T.unlines messages
        <> "\nexpected the deletion "
        <> revocationAuditLine
        <> "\nand the tally "
        <> revocationTally
        <> "\nwhole cycle output:\n"
        <> roleOutput run

{- One deletion for the affected version, and one closing tally. The generation a deletion names is
the Dredger's own report: it prints on failure and is never pinned here. -}
reportsRevocation :: [Text] -> Bool
reportsRevocation messages =
    map (fst . T.breakOn generationMarker) (filter (T.isPrefixOf "deleting ") messages) == [revocationAuditLine]
        && filter (T.isPrefixOf "mirror sweep cycle ") messages == [revocationTally]

generationMarker :: Text
generationMarker = "; advisory generation "

revocationAuditLine :: Text
revocationAuditLine =
    "deleting "
        <> revokedName
        <> "@"
        <> vulnerableVersion
        <> ": blocked by DenyIfCve (affected by GHSA-corpus-1002 (CVSS >= 7.0))"

revocationTally :: Text
revocationTally = "mirror sweep cycle complete: examined 2, deleted 1, kept 1, guard-skipped 0"

{- The next request for the revoked version is refused by policy on the public leg, and a refusal
enqueues no mirror, so the store entry stays gone. -}
assertVulnerableRefused :: E2E -> IO ()
assertVulnerableRefused e2e = do
    void $ npmInstall e2e (revokedName <> "@" <> vulnerableVersion) >>= shouldFail
    (status, body) <- proxyGet e2e (npmTarballPath revokedName vulnerableVersion)
    (status, decodeUtf8 body :: Text) `shouldSatisfy` ((== 403) . fst)
    -- Give the worker a 1.5s window to erroneously re-mirror the refused version, then assert absence.
    threadDelay 1500000
    verdaccioVersions e2e revokedName `shouldReturn` [fixedVersion]

-- The fix survives the sweep in the store, and its metadata and artifact both still serve.
assertFixInstallable :: E2E -> IO ()
assertFixInstallable e2e = do
    void $ npmInstall e2e (revokedName <> "@" <> fixedVersion) >>= shouldSucceed
    verdaccioVersions e2e revokedName `shouldReturn` [fixedVersion]
    -- The proxy falls back to the public leg for a name that is not first-party, so only a direct
    -- store read tells a surviving artifact from that fallback.
    stored <- verdaccioArtifact e2e revokedName fixedVersion
    stored `shouldSatisfy` (\(code, size) -> code == 200 && size > 0)
    (status, body) <- proxyGet e2e (npmTarballPath revokedName fixedVersion)
    (status, LBS.length body) `shouldSatisfy` (\(code, size) -> code == 200 && size > 0)

revokedName :: Text
revokedName = psName corpusRevokedPkg

revokedVersions :: [Text]
revokedVersions = sort (psVersions corpusRevokedPkg)

{- The fixture's two versions as the corpus advisory's range decides them: everything below the
stated fix is affected. Editing the fixture or that advisory means editing these too. -}
vulnerableVersion, fixedVersion :: Text
vulnerableVersion = "1.0.0"
fixedVersion = "1.2.0"

-- Fail with the store's own diagnostic for any package its listing never reported.
awaitListed :: E2E -> [Text] -> IO ()
awaitListed e2e names =
    for_ names $ \name -> do
        unlisted <- verdaccioAwaitListed e2e name
        whenJust unlisted (expectationFailure . toString)

-- The rule policy as ECLUSE_RULES carries it: one JSON object of named rule entries.
renderRules :: [Pair] -> Text
renderRules = decodeUtf8 . toStrict . encode . object

sweepMessages :: RoleRun -> [Text]
sweepMessages run = filter isSweepMessage (mapMaybe lineMessage (T.lines (roleOutput run)))

isSweepMessage :: Text -> Bool
isSweepMessage message = any (`T.isPrefixOf` message) ["deleting ", "dry run, would delete ", "mirror sweep cycle "]

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Dredger lifecycle evidence from a real Verdaccio store.
Complete version snapshots connect store contents with each cycle's audit records. One group decides
by operator identity alone, one by the advisory generation Pilot compiles, one follows what the next
private read sees once a cleanup has run, and one rolls policy out across roles in the wrong order.
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
    mirrorPkg,
    psName,
    psVersion,
    psVersions,
    recoveryFaultPkg,
    recoveryLatePkg,
    recoveryLostPkg,
    recoveryPkg,
    recoveryReadmitPkg,
 )
import Ecluse.E2E.Harness
import Ecluse.Test.Log (lineMessage)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2))
import Ecluse.Test.Package (sriSha512Of)

-- | Verify store contents and audit records under operator identity denies and advisory denials.
spec :: Spec
spec = do
    unavailable <- runIO e2eUnavailable
    case unavailable of
        Just reason -> it "Dredger end-to-end environment is available" (pendingWith reason)
        Nothing -> do
            aroundAll withGlobalDataPlane (aroundAllWith withSeededStore identityScenarios)
            aroundAll withGlobalDataPlane revocationScenario
            aroundAll withGlobalDataPlane (aroundAllWith withRecoveryStores recoveryScenarios)
            aroundAll withGlobalDataPlane (aroundAllWith withRolloutStores rolloutScenarios)

identityScenarios :: SpecWith (GlobalDataPlane, E2E, E2E)
identityScenarios = describe "identity denies with no advisory database" $ do
    it "walks the seeded store through listPackagesIn, including scoped base-name buckets" $ \(_, e2e, _) -> do
        verdaccioSnapshot e2e `shouldReturn` seededVersions
        names <- verdaccioListing e2e
        names `shouldMatchList` Map.keys seededVersions
        verdaccioNamesUnder e2e "" `shouldReturn` sort names
        verdaccioNamesUnder e2e "e" `shouldReturn` sort names
        verdaccioNamesUnder e2e "z" `shouldReturn` []

    it "deletes every denied version and preserves all other versions, including first-party versions" $ \(plane, e2e, cache) -> do
        initial <- verdaccioSnapshot e2e
        privateInitial <- verdaccioSnapshot cache
        run <- runDredgerOnce plane ["--once"] (sweepEnv dredgerPkg)
        assertFullSweep "deleting " dredgerPkg initial run
        verdaccioVersions e2e (psName dredgerPkg) `shouldReturn` []
        finalStore <- verdaccioSnapshot e2e
        finalStore `shouldBe` Map.delete (psName dredgerPkg) initial
        verdaccioNamesUnder e2e "" `shouldReturn` Map.keys finalStore
        verdaccioSnapshot cache `shouldReturn` privateInitial

    it "previews distinct real inventories without writes and preserves first-party versions" $ \(plane, e2e, cache) -> do
        for_ [psVersion dredgerDryRunPkg, "2.0.0"] $ \version ->
            void $ withPublishProject cache (psName dredgerDryRunPkg) version npmPublishIn >>= shouldSucceed
        void $ withPublishProject cache publishDredgerName publishVersion npmPublishIn >>= shouldSucceed
        awaitListed cache [psName dredgerDryRunPkg, publishDredgerName]
        initial <- verdaccioSnapshot e2e
        privateInitial <- verdaccioSnapshot cache
        run <- runDredgerOnce plane ["--once", "--dry-run"] (sweepEnv dredgerDryRunPkg)
        roleExit run `shouldSatisfy` (/= ExitSuccess)
        let output = roleOutput run
        output `shouldSatisfy` T.isInfixOf "npm mirror store on mirrorTarget https://mirror/"
        output `shouldSatisfy` T.isInfixOf "npm private cache on privateUpstream https://private-cache/"
        output `shouldSatisfy` T.isInfixOf ("dry run, would delete " <> psName dredgerDryRunPkg <> "@2.0.0")
        output `shouldSatisfy` T.isInfixOf "deleted 2"
        output `shouldSatisfy` T.isInfixOf "counted from partial evidence"
        output `shouldSatisfy` T.isInfixOf "previewing only: this run holds nothing that could delete"
        output `shouldSatisfy` T.isInfixOf "This preview deleted nothing, so it proves no authority to delete"
        output `shouldSatisfy` (not . T.isInfixOf ("would delete " <> publishDredgerName))
        verdaccioSnapshot e2e `shouldReturn` initial
        verdaccioSnapshot cache `shouldReturn` privateInitial

    it "refuses missing consent, names the key, and leaves every version intact" $ \(plane, e2e, cache) -> do
        initial <- verdaccioSnapshot e2e
        privateInitial <- verdaccioSnapshot cache
        run <- runDredgerOnce plane ["--once"] (sweepEnv dredgerPkg <> [(consentKey, "false")])
        (roleExit run, roleOutput run) `shouldSatisfy` ((/= ExitSuccess) . fst)
        roleOutput run `shouldSatisfy` T.isInfixOf (consentKey <> " is not set")
        sweepMessages run `shouldBe` []
        verdaccioSnapshot cache `shouldReturn` privateInitial
        verdaccioSnapshot e2e `shouldReturn` initial

    it "protects every first-party version even when an identity deny names one" $ \(plane, e2e, cache) -> do
        initial <- verdaccioSnapshot e2e
        privateInitial <- verdaccioSnapshot cache
        let rules = identityRule (publishDredgerName <> "@" <> publishVersion)
            guardCount = length (Map.findWithDefault [] publishDredgerName initial) + length (Map.findWithDefault [] publishDredgerName privateInitial)
        run <- runDredgerOnce plane ["--once"] [("ECLUSE_RULES", rules), ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)]
        (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        sweepMessages run `shouldBe` [cycleLine ["examined 0", "deleted 0", "kept 0", "guard-skipped " <> show guardCount]]
        verdaccioVersions e2e publishDredgerName `shouldReturn` firstPartyVersions
        verdaccioSnapshot e2e `shouldReturn` initial

    it "deletes shared and cache-only versions while preserving each store's other contents" $ \(plane, mirror, cache) -> do
        -- The mirror never serves this version, so the case holds its own cache-only copy.
        void $ withPublishProject cache (psName dredgerDryRunPkg) cacheOnlyVersion npmPublishIn >>= shouldSucceed
        verdaccioHasVersion cache (psName dredgerDryRunPkg) cacheOnlyVersion `shouldReturn` True
        verdaccioHasVersion mirror (psName dredgerDryRunPkg) cacheOnlyVersion `shouldReturn` False
        mirrorBefore <- verdaccioSnapshot mirror
        cacheBefore <- verdaccioSnapshot cache
        run <- runDredgerOnce plane ["--once"] (sweepEnv dredgerDryRunPkg)
        roleExit run `shouldBe` ExitSuccess
        verdaccioSnapshot mirror `shouldReturn` Map.delete (psName dredgerDryRunPkg) mirrorBefore
        verdaccioSnapshot cache `shouldReturn` Map.delete (psName dredgerDryRunPkg) cacheBefore
        verdaccioVersions mirror (psName dredgerDryRunPkg) `shouldReturn` []
        verdaccioVersions cache (psName dredgerDryRunPkg) `shouldReturn` []

withSeededStore :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
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
        withDredgerPrivateCache plane e2e $ \cache -> action (plane, e2e, cache)

mirroredPackages :: [PkgSpec]
mirroredPackages = [dredgerPkg, dredgerKeepPkg, dredgerDryRunPkg]

seededVersions :: Map Text [Text]
seededVersions = Map.fromList ((publishDredgerName, firstPartyVersions) : [(psName pkg, [psVersion pkg]) | pkg <- mirroredPackages])

firstPartyVersions :: [Text]
firstPartyVersions = [publishVersion, "2.0.0"]

-- | A version the seeded mirror never holds, published into the private cache alone.
cacheOnlyVersion :: Text
cacheOnlyVersion = "3.0.0"

consentKey :: Text
consentKey = "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"

sweepEnv :: PkgSpec -> [(Text, Text)]
sweepEnv pkg =
    [ ("ECLUSE_RULES", identityRule (psName pkg))
    , ("ECLUSE_DREDGER__FULL_WALK", "true")
    , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)
    ]

identityRule :: Text -> Text
identityRule name = identityRules [name]

-- | A Dredger policy of named identity denies and nothing else, one entry per condemned identity.
identityRules :: [Text] -> Text
identityRules = renderRules . zipWith identityEntry [1 :: Int ..]

identityEntry :: Int -> Text -> Pair
identityEntry position revoked =
    fromString ("revoke-" <> show position) .= object ["type" .= ("DenyByIdentity" :: Text), "identity" .= revoked]

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
    withE2EWith defaultE2EConfig{ecExtraEnv = revocationProxyEnv} (\e2e -> withDredgerPrivateCache plane e2e (\_ -> action (plane, e2e))) plane

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
    unless swapped (failWithLog (e2eProxyContainer e2e) "the proxy never swapped in the second generation")

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

recoveryScenarios :: SpecWith (GlobalDataPlane, E2E, E2E)
recoveryScenarios = describe "next private reads and recovery after a grouped cleanup" $ do
    it "serves the sibling and refuses the denied version once both stores lose it" $ \(plane, proxy, cache) -> do
        let name = psName recoveryPkg
            sibling = psVersion recoveryPkg
        verdaccioVersions cache name `shouldReturn` sort (psVersions recoveryPkg)
        verdaccioArtifact cache name deniedRecoveryVersion >>= (`shouldSatisfy` servedBytes)
        run <- runDredgerOnce plane ["--once"] (recoverySweepEnv (name <> "@" <> deniedRecoveryVersion))
        roleExit run `shouldBe` ExitSuccess
        verdaccioVersions proxy name `shouldReturn` [sibling]
        verdaccioVersions cache name `shouldReturn` [sibling]
        (fst <$> verdaccioArtifact cache name deniedRecoveryVersion) `shouldReturn` 404
        (fst <$> proxyGet proxy ("/npm/" <> name)) `shouldReturn` 200
        (fst <$> proxyGet proxy (npmTarballPath name sibling)) `shouldReturn` 200
        assertRefusedNext proxy name deniedRecoveryVersion
        withNpmProject proxy $ \project -> do
            void $ npmInstallIn project (name <> "@" <> sibling) >>= shouldSucceed
            installedVersion project name `shouldReturn` Just sibling
        verdaccioVersions proxy name `shouldReturn` [sibling]
        verdaccioVersions cache name `shouldReturn` [sibling]

    it "keeps the cache copy its backend refused and removes it on a later run" $ \(plane, proxy, cache) -> do
        let name = psName recoveryFaultPkg
            version = psVersion recoveryFaultPkg
        refused <- withPrivateCacheDeletesRefused plane (runDredgerOnce plane ["--once"] (recoverySweepEnv name))
        auditMessages refused
            `shouldSatisfy` any (T.isInfixOf (privateTarget <> ": " <> name <> "@" <> version <> ": the backend refused the delete, HTTP 503"))
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` [version]
        verdaccioArtifact cache name version >>= (`shouldSatisfy` servedBytes)
        residual <- runDredgerOnce plane ["--once"] (recoverySweepEnv name)
        roleExit residual `shouldBe` ExitSuccess
        verdaccioVersions cache name `shouldReturn` []
        assertRefusedNext proxy name version
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` []

    it "mirrors the version again when the policy lifts and the public source still holds bytes" $ \(plane, proxy, cache) -> do
        let name = psName recoveryReadmitPkg
            version = psVersion recoveryReadmitPkg
        run <- runDredgerOnce plane ["--once"] (recoverySweepEnv name)
        roleExit run `shouldBe` ExitSuccess
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` []
        withRelaxedProxy plane $ \relaxed -> do
            withNpmProject relaxed $ \project -> do
                void $ npmInstallIn project name >>= shouldSucceed
                installedVersion project name `shouldReturn` Just version
            verdaccioAwaitVersions relaxed name [version] `shouldReturn` [version]
            verdaccioArtifact relaxed name version >>= (`shouldSatisfy` servedBytes)

    it "leaves both stores empty when the policy lifts and no source bytes remain" $ \(plane, proxy, cache) -> do
        let name = psName recoveryLostPkg
            version = psVersion recoveryLostPkg
        run <- runDredgerOnce plane ["--once"] (recoverySweepEnv name)
        roleExit run `shouldBe` ExitSuccess
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` []
        withPublicArtifactWithheld plane name version $
            withRelaxedProxy plane $ \relaxed -> do
                (fst <$> proxyGet relaxed (npmTarballPath name version)) `shouldReturn` 404
                withNpmProject relaxed (\project -> void (npmInstallIn project name >>= shouldFail))
                -- Give the worker a 1.5s window to mirror bytes it never obtained, then read both stores.
                threadDelay 1500000
                verdaccioVersions relaxed name `shouldReturn` []
                verdaccioVersions cache name `shouldReturn` []

    it "removes the copies each store received after the cycle that reported them gone" $ \(plane, proxy, cache) -> do
        let name = psName recoveryLatePkg
            version = psVersion recoveryLatePkg
        removed <- runDredgerOnce plane ["--once"] (recoverySweepEnv name)
        roleExit removed `shouldBe` ExitSuccess
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` []
        for_ [publishingDirectly proxy, cache] $ \store ->
            void $ withPublishProject store name version npmPublishIn >>= shouldSucceed
        for_ [proxy, cache] $ \store -> do
            verdaccioAwaitVersions store name [version] `shouldReturn` [version]
            awaitListed store [name]
        rediscovered <- runDredgerOnce plane ["--once"] (recoverySweepEnv name)
        roleExit rediscovered `shouldBe` ExitSuccess
        verdaccioVersions proxy name `shouldReturn` []
        verdaccioVersions cache name `shouldReturn` []

{- The recovery group's stores: a mirror the real worker fills, and a private cache the fixture
publisher seeds with the same identities. The group's own proxy reads that cache. -}
withRecoveryStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withRecoveryStores action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = recoveryProxyEnv} boot plane
  where
    boot proxy = withDredgerPrivateCache plane proxy $ \cache -> do
        seedMirrorCopies plane
        seedCacheCopies cache
        action (plane, proxy, cache)

{- Seed the mirror through a proxy that still permits these versions, because the group's own proxy
denies every one of them from boot. -}
seedMirrorCopies :: GlobalDataPlane -> IO ()
seedMirrorCopies = withE2EWith defaultE2EConfig install
  where
    install seeder = do
        for_ recoveryCopies $ \(name, version) ->
            void $ npmInstall seeder (name <> "@" <> version) >>= shouldSucceed
        for_ recoveryPackages $ \pkg ->
            verdaccioAwaitVersions seeder (psName pkg) (sort (psVersions pkg)) `shouldReturn` sort (psVersions pkg)
        awaitListed seeder (map psName recoveryPackages)

-- The cache's copies come from the fixture publisher, which writes to the store and not the proxy.
seedCacheCopies :: E2E -> IO ()
seedCacheCopies cache = do
    for_ recoveryCopies $ \(name, version) ->
        void $ withPublishProject cache name version npmPublishIn >>= shouldSucceed
    awaitListed cache (map psName recoveryPackages)

-- Every recovery copy, oldest version first, so a mirrored older version never retags the store.
recoveryCopies :: [(Text, Text)]
recoveryCopies = [(psName pkg, version) | pkg <- recoveryPackages, version <- sort (psVersions pkg)]

recoveryPackages :: [PkgSpec]
recoveryPackages = [recoveryPkg, recoveryFaultPkg, recoveryReadmitPkg, recoveryLostPkg, recoveryLatePkg]

-- The version of 'recoveryPkg' the recovery policy names, leaving 'psVersion' as its sibling.
deniedRecoveryVersion :: Text
deniedRecoveryVersion = "1.0.0"

{- The group's proxy reads the cache the Dredger cleans and denies every version these cases
delete. Its packument cache turns over in a second, so a read after a cycle sees current state. -}
recoveryProxyEnv :: [(Text, Text)]
recoveryProxyEnv = privateCacheEnv <> [("ECLUSE_RULES", recoveryRules)]

-- The same topology once the deny is lifted, which is what a re-admission is decided under.
relaxedProxyEnv :: [(Text, Text)]
relaxedProxyEnv = privateCacheEnv <> [("ECLUSE_RULES", permissiveRules)]

privateCacheEnv :: [(Text, Text)]
privateCacheEnv =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", "https://private-cache/")
    , ("ECLUSE_CACHE__TTL", "1")
    ]

-- Run a second proxy over the same stores, for a case whose next read decides under a new policy.
withRelaxedProxy :: GlobalDataPlane -> (E2E -> IO ()) -> IO ()
withRelaxedProxy plane action = withE2EWith defaultE2EConfig{ecExtraEnv = relaxedProxyEnv} action plane

recoveryRules :: Text
recoveryRules = denyingRules recoveryDenials

-- Every fixture version predates the shipped quarantine, but a copy the publisher wrote does not.
minAgeRule :: Pair
minAgeRule = "min-age" .= object ["type" .= ("AllowIfOlderThan" :: Text), "ageSeconds" .= (0 :: Int)]

recoveryDenials :: [Text]
recoveryDenials =
    (psName recoveryPkg <> "@" <> deniedRecoveryVersion)
        : map psName [recoveryFaultPkg, recoveryReadmitPkg, recoveryLostPkg, recoveryLatePkg]

recoverySweepEnv :: Text -> [(Text, Text)]
recoverySweepEnv revoked = [("ECLUSE_RULES", identityRule revoked)]

-- The label the Dredger's audit lines carry for the private cache it swept.
privateTarget :: Text
privateTarget = "privateUpstream https://private-cache/"

-- npm publishes into a store directly when the store's own URL is the project's registry.
publishingDirectly :: E2E -> E2E
publishingDirectly e2e = e2e{e2eRegistry = e2eVerdaccio e2e}

-- A denied version reaches no client, whatever either store still holds.
assertRefusedNext :: E2E -> Text -> Text -> IO ()
assertRefusedNext proxy name version = do
    (status, _) <- proxyGet proxy (npmTarballPath name version)
    status `shouldBe` 403
    void $ withNpmProject proxy (\project -> npmInstallIn project (name <> "@" <> version)) >>= shouldFail

servedBytes :: (Int, Int64) -> Bool
servedBytes (status, size) = status == 200 && size > 0

-- Fail with the store's own diagnostic for any package its listing never reported.
awaitListed :: E2E -> [Text] -> IO ()
awaitListed e2e names =
    for_ names $ \name -> do
        unlisted <- verdaccioAwaitListed e2e name
        whenJust unlisted (expectationFailure . toString)

rolloutScenarios :: SpecWith (GlobalDataPlane, E2E, E2E)
rolloutScenarios = describe "eventual cleanup after out-of-order role updates" $ do
    it "removes the copies an old worker mirrored after a stricter proxy started" $ \(plane, _, cache) -> do
        queueUrl <- sharedMirrorQueue plane
        let target = psName dredgerPkg
            version = psVersion dredgerPkg
            unaffected = psName dredgerKeepPkg
        withServeOnlyProxy plane queueUrl (permissiveEnv <> publishTargetEnv) $ \old -> do
            for_ [target, unaffected] $ \name -> void (npmInstall old name >>= shouldSucceed)
            void $ withPublishProject old publishDredgerName publishVersion npmPublishIn >>= shouldSucceed
            -- No worker has run yet, so the admitted versions wait on the durable queue.
            verdaccioVersions old target `shouldReturn` []
        withServeOnlyProxy plane queueUrl (strictEnv target) $ \strict -> do
            assertRefusedNext strict target version
            withMirrorRole plane (mirrorRoleEnv queueUrl permissiveRules) $ \worker ->
                for_ [target <> "@" <> version, unaffected <> "@" <> psVersion dredgerKeepPkg] (awaitPublication worker)
            -- The worker container is gone, so the rollout has no outstanding old write left.
            verdaccioAwaitVersions strict target [version] `shouldReturn` [version]
            verdaccioVersions cache target `shouldReturn` [version]
            -- A private read applies no rules, so the stricter proxy installs what it would deny.
            withNpmProject strict $ \project -> do
                void $ npmInstallIn project (target <> "@" <> version) >>= shouldSucceed
                installedVersion project target `shouldReturn` Just version
            served <- proxyGet strict (npmTarballPath target version)
            second LBS.length served `shouldSatisfy` servedBytes
            let denied = identityRules [target, publishDredgerName <> "@" <> publishVersion]
            swept <- runDredgerOnce plane ["--once"] (rolloutSweepEnv denied)
            roleExit swept `shouldBe` ExitSuccess
            assertRolloutRemoved strict cache target
            verdaccioVersions strict unaffected `shouldReturn` [psVersion dredgerKeepPkg]
            verdaccioVersions strict publishDredgerName `shouldReturn` [publishVersion]
            assertRefusedNext strict target version
            assertNothingQueued plane queueUrl strict (target <> "@" <> version)
            rescanned <- runDredgerOnce plane ["--once"] (rolloutSweepEnv denied)
            roleExit rescanned `shouldBe` ExitSuccess
            assertRolloutRemoved strict cache target
            verdaccioVersions strict publishDredgerName `shouldReturn` [publishVersion]

    it "restores a version an old Dredger removed while the newer roles permitted it" $ \(plane, proxy, cache) -> do
        let name = psName recoveryReadmitPkg
            version = psVersion recoveryReadmitPkg
        verdaccioArtifact cache name version >>= (`shouldSatisfy` servedBytes)
        (retainedStatus, retained) <- verdaccioArtifactBytes proxy name version
        retainedStatus `shouldBe` 200
        removed <- runDredgerOnce plane ["--once"] (rolloutSweepEnv (identityRules [name]))
        roleExit removed `shouldBe` ExitSuccess
        assertRolloutRemoved proxy cache name
        -- Dredger only deletes, so a completed scan under the intended policy restores nothing.
        converged <- runDredgerOnce plane ["--once"] (rolloutSweepEnv permissiveRules)
        roleExit converged `shouldBe` ExitSuccess
        assertRolloutRemoved proxy cache name
        withNpmProject proxy $ \project -> do
            void $ npmInstallIn project name >>= shouldSucceed
            installedVersion project name `shouldReturn` Just version
        verdaccioAwaitVersions proxy name [version] `shouldReturn` [version]
        (restoredStatus, restored) <- verdaccioArtifactBytes proxy name version
        (restoredStatus, sriSha512Of (toStrict restored)) `shouldBe` (200, sriSha512Of (toStrict retained))
        (fst <$> proxyGet proxy (npmTarballPath name version)) `shouldReturn` 200

    it "leaves the version lost when an old Dredger removed the only remaining bytes" $ \(plane, proxy, cache) -> do
        let name = psName recoveryLostPkg
            version = psVersion recoveryLostPkg
        withPublicArtifactWithheld plane name version $ do
            -- The public source can no longer supply bytes, so this 200 is the retained copy.
            (fst <$> proxyGet proxy (npmTarballPath name version)) `shouldReturn` 200
            removed <- runDredgerOnce plane ["--once"] (rolloutSweepEnv (identityRules [name]))
            roleExit removed `shouldBe` ExitSuccess
            assertRolloutRemoved proxy cache name
            converged <- runDredgerOnce plane ["--once"] (rolloutSweepEnv permissiveRules)
            roleExit converged `shouldBe` ExitSuccess
            assertRolloutRemoved proxy cache name
            -- Threat 109's accepted outcome: agreeing on the permissive policy cannot recreate
            -- bytes, so the request fails for want of a source rather than by a policy refusal.
            (fst <$> proxyGet proxy (npmTarballPath name version)) `shouldReturn` 404
            withNpmProject proxy (\project -> void (npmInstallIn project name >>= shouldFail))
            -- Give the worker a 1.5s window to mirror bytes it never obtained, then read both stores.
            threadDelay 1500000
            assertRolloutRemoved proxy cache name

{- The rollout group's stores: the shared mirror the newer roles fill through a real install, and a
private cache the fixture publisher seeds. The group's proxy boots the intended permissive policy. -}
withRolloutStores :: ((GlobalDataPlane, E2E, E2E) -> IO ()) -> GlobalDataPlane -> IO ()
withRolloutStores action plane =
    withE2EWith defaultE2EConfig{ecExtraEnv = relaxedProxyEnv} boot plane
  where
    boot proxy = withDredgerPrivateCache plane proxy $ \cache -> do
        -- The mirror fills first, because a seeded cache copy would answer privately and
        -- enqueue nothing.
        for_ looserPackages $ \pkg -> do
            void $ npmInstall proxy (psName pkg) >>= shouldSucceed
            verdaccioAwaitVersions proxy (psName pkg) [psVersion pkg] `shouldReturn` [psVersion pkg]
        for_ retainedCachePackages $ \pkg ->
            void $ withPublishProject cache (psName pkg) (psVersion pkg) npmPublishIn >>= shouldSucceed
        awaitListed proxy (map psName looserPackages)
        awaitListed cache (map psName retainedCachePackages)
        action (plane, proxy, cache)

-- The packages the looser-rollout cases start from, retained in both stores before any role moves.
looserPackages :: [PkgSpec]
looserPackages = [recoveryReadmitPkg, recoveryLostPkg]

{- Every package the private cache holds at the start. The stricter case's target is cache-only
here, because its mirror copy arrives later from the old worker it starts itself. -}
retainedCachePackages :: [PkgSpec]
retainedCachePackages = dredgerPkg : looserPackages

-- Both inventories after a rollout cycle, read by name so the group's other packages stay out of it.
assertRolloutRemoved :: E2E -> E2E -> Text -> Expectation
assertRolloutRemoved mirror cache name = do
    verdaccioVersions mirror name `shouldReturn` []
    verdaccioVersions cache name `shouldReturn` []

{- Run a proxy with no embedded worker on the caller's queue, so its admissions wait for a worker
the case starts on its own schedule. -}
withServeOnlyProxy :: GlobalDataPlane -> Text -> [(Text, Text)] -> (E2E -> IO ()) -> IO ()
withServeOnlyProxy plane queueUrl extraEnv action = withE2EWith config action plane
  where
    config =
        defaultE2EConfig
            { ecExtraEnv = extraEnv
            , ecQueueUrl = Just queueUrl
            , ecArgs = ["proxy", "--no-worker"]
            }

{- The older roles' policy, and the stricter one that names a single identity. Each proxy's packument
cache turns over in a second, so a read after another role's write sees current state. -}
permissiveEnv :: [(Text, Text)]
permissiveEnv = [("ECLUSE_RULES", permissiveRules), ("ECLUSE_CACHE__TTL", "1")]

strictEnv :: Text -> [(Text, Text)]
strictEnv denied = [("ECLUSE_RULES", denyingRules [denied]), ("ECLUSE_CACHE__TTL", "1")]

-- The Dredger's configuration for a rollout case: its own policy, and the first-party namespace.
rolloutSweepEnv :: Text -> [(Text, Text)]
rolloutSweepEnv rules = [("ECLUSE_RULES", rules), ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)]

-- | The intended permissive policy: the quarantine every fixture version predates, and no deny.
permissiveRules :: Text
permissiveRules = renderRules [minAgeRule]

-- | 'permissiveRules' plus one identity deny per named package or version.
denyingRules :: [Text] -> Text
denyingRules denied = renderRules (minAgeRule : zipWith identityEntry [1 :: Int ..] denied)

{- Prove the refused reads enqueued nothing, by admitting a sentinel afterwards and draining the
queue with a permissive worker: it publishes the sentinel and never the denied version. -}
assertNothingQueued :: GlobalDataPlane -> Text -> E2E -> Text -> IO ()
assertNothingQueued plane queueUrl proxy denied = do
    void $ npmInstall proxy (psName mirrorPkg) >>= shouldSucceed
    withMirrorRole plane (mirrorRoleEnv queueUrl permissiveRules) $ \drain -> do
        awaitPublication drain (psName mirrorPkg <> "@" <> psVersion mirrorPkg)
        logs <- containerLogs drain
        logs `shouldSatisfy` (not . T.isInfixOf (publishedLine denied))

-- Wait for one version's mirror write, failing with the worker's own log when it never lands.
awaitPublication :: String -> Text -> IO ()
awaitPublication worker published = do
    mirrored <- awaitContainerLog worker (T.isInfixOf (publishedLine published)) 240
    unless mirrored (failWithLog worker ("the worker never published " <> published))

-- The worker's own line for a completed mirror write, as its JSONL record carries it.
publishedLine :: Text -> Text
publishedLine published = "mirrored artifact published: " <> published

-- Fail with the container's own log tail, because a role's reason for not acting lives only there.
failWithLog :: String -> Text -> IO ()
failWithLog container reason = do
    logs <- containerLogs container
    expectationFailure (toString (reason <> ". Its last lines:\n" <> logTail logTailLines logs))

-- The rule policy as ECLUSE_RULES carries it: one JSON object of named rule entries.
renderRules :: [Pair] -> Text
renderRules = decodeUtf8 . toStrict . encode . object

-- Every line the role logged, as the message its JSONL record carried.
auditMessages :: RoleRun -> [Text]
auditMessages run = mapMaybe lineMessage (T.lines (roleOutput run))

sweepMessages :: RoleRun -> [Text]
sweepMessages run = filter isSweepMessage (map withoutTarget (auditMessages run))

isSweepMessage :: Text -> Bool
isSweepMessage message = any (`T.isPrefixOf` message) ["deleting ", "dry run, would delete ", "mirror sweep cycle "]

withoutTarget :: Text -> Text
withoutTarget message
    | any (`T.isPrefixOf` message) ["mirrorTarget ", "privateUpstream "] = T.drop 2 (snd (T.breakOn ": " message))
    | otherwise = message

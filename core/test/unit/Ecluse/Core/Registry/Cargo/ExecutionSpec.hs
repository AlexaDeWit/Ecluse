-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native evidence binds exact admitted identities and keeps private reads outside public retention.
module Ecluse.Core.Registry.Cargo.ExecutionSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.MVar qualified as MVar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec
import UnliftIO.Async (cancel, wait, withAsync)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry.Cargo.Execution (newCargoInstallEvidence)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Install
import Ecluse.Core.Rules.Types (Inability (EvaluationFailed, InstallSignalUnread), Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (Limits (..), defaultLimits, mkProgressFloor)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (sampleArtifact, sampleDetails, unsafeHash)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Fixtures
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery)
import Ecluse.Test.Registry.PyPI.Archive (tarBytes)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Cargo install evidence" $ do
    it "uses the published spelling after a lower-case sparse lookup" $ do
        requested <- expectRight (projectName "mixed_crate")
        for_ [([], False), ([("build.rs", "unexecuted bytes")], True)] $ \(extras, active) -> do
            archive <- namedFixture "Mixed_Crate" "Mixed_Crate" "1.0.0" extras
            subject <- projectedSubject requested "Mixed_Crate" "1.0.0" archive
            renderPackageName (pkgName subject) `shouldBe` "Mixed_Crate"
            pkgCanonical (pkgName subject) `shouldBe` pkgCanonical requested
            map artFilename (toList (pkgArtifacts subject)) `shouldBe` ["Mixed_Crate-1.0.0.crate"]
            provider <- newCargoInstallEvidence defaultLimits
            evidenceReads <- newInstallEvidenceReads provider (evidenceContext (\_ _ pay -> pay (BS.length archive) $> Right archive))
            if active
                then resolved evidenceReads subject >>= (`shouldSatisfy` runs)
                else resolved evidenceReads subject `shouldReturn` InstallDetermined NoCodeOnInstall
    it "refuses a checksum-verified archive whose root or manifest changes the published spelling" $ do
        requested <- expectRight (projectName "mixed_crate")
        for_ [("mixed_crate", "mixed_crate"), ("Mixed_Crate", "mixed_crate")] $ \(rootName, manifestName) -> do
            archive <- namedFixture rootName manifestName "1.0.0" []
            subject <- projectedSubject requested "Mixed_Crate" "1.0.0" archive
            renderPackageName (pkgName subject) `shouldBe` "Mixed_Crate"
            map (map hashValue . artHashes) (toList (pkgArtifacts subject)) `shouldBe` [[sha256Hex archive]]
            provider <- newCargoInstallEvidence defaultLimits
            evidenceReads <- newInstallEvidenceReads provider (evidenceContext (\_ _ pay -> pay (BS.length archive) $> Right archive))
            resolved evidenceReads subject >>= (`shouldSatisfy` permanentFailure)
    it "does not acquire bytes while it builds an immutable subject" $ do
        archive <- fixture "1.2.3" []
        count <- newIORef (0 :: Int)
        provider <- newCargoInstallEvidence defaultLimits
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext (counter count archive))
        void (evidenceReads (details "1.2.3" archive))
        readIORef count `shouldReturn` 0
    it "retains compact verified signals and keeps different checksums separate" $ do
        inert <- fixture "1.2.3" []
        active <- fixture "1.2.3" [("build.rs", "unexecuted bytes")]
        count <- newIORef (0 :: Int)
        provider <- newCargoInstallEvidence defaultLimits
        let artifactRead _ artifact pay = do
                modifyIORef' count (+ 1)
                let bytes = if any ((== sha256Hex active) . hashValue) (artHashes artifact) then active else inert
                pay (BS.length bytes)
                pure (Right bytes)
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext artifactRead)
        resolved evidenceReads (details "1.2.3" inert) `shouldReturn` InstallDetermined NoCodeOnInstall
        resolved evidenceReads (details "1.2.3" active) >>= (`shouldSatisfy` runs)
        resolved evidenceReads (details "1.2.3" inert) `shouldReturn` InstallDetermined NoCodeOnInstall
        readIORef count `shouldReturn` 2
    it "binds different raw versions and refuses a checksum mismatch" $ do
        a <- fixture "1.2.3" []
        b <- fixture "1.2.4" []
        provider <- newCargoInstallEvidence defaultLimits
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext (\_ artifact pay -> let bytes = if artFilename artifact == "example-1.2.3.crate" then a else b in pay (BS.length bytes) $> Right bytes))
        resolved evidenceReads (details "1.2.3" a) `shouldReturn` InstallDetermined NoCodeOnInstall
        resolved evidenceReads (details "1.2.4" b) `shouldReturn` InstallDetermined NoCodeOnInstall
        mismatch <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> pure (Right b)))
        let wrong = (details "1.2.3" a){pkgArtifacts = sampleArtifact{artUrl = "https://registry.test/uncached", artHashes = [unsafeHash SHA256 (sha256Hex a)]} :| []}
        resolved mismatch wrong >>= (`shouldSatisfy` permanentFailure)
    it "keeps a bound subject unchanged after a concurrent metadata replacement" $ do
        a <- fixture "1.2.3" []
        b <- fixture "1.2.3" [("build.rs", "replacement")]
        current <- newIORef (details "1.2.3" a)
        observed <- newIORef []
        provider <- newCargoInstallEvidence defaultLimits
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext (\_ artifact _ -> modifyIORef' observed (artifact :) $> Right a))
        subject <- readIORef current >>= evidenceReads
        writeIORef current (details "1.2.3" b)
        runSubject subject `shouldReturn` InstallDetermined NoCodeOnInstall
        map (map hashValue . artHashes) <$> readIORef observed `shouldReturn` [[sha256Hex a]]
    it "never reuses public evidence for an authenticated store read" $ do
        archive <- fixture "1.2.3" []
        count <- newIORef (0 :: Int)
        provider <- newCargoInstallEvidence defaultLimits
        public <- newInstallEvidenceReads provider (evidenceContext (counter count archive))
        resolved public (details "1.2.3" archive) `shouldReturn` InstallDetermined NoCodeOnInstall
        private <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> pure (Left unavailable))){ieRole = InstallStoreRevocation}
        resolved private (details "1.2.3" archive) `shouldReturn` InstallReadFailed unavailable
        readIORef count `shouldReturn` 1
    it "does not retain transient failures as successful unknown evidence" $ do
        archive <- fixture "1.2.3" []
        count <- newIORef (0 :: Int)
        provider <- newCargoInstallEvidence defaultLimits
        failed <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> pure (Left unavailable)))
        resolved failed (details "1.2.3" archive) `shouldReturn` InstallReadFailed unavailable
        recovered <- newInstallEvidenceReads provider (evidenceContext (counter count archive))
        resolved recovered (details "1.2.3" archive) `shouldReturn` InstallDetermined NoCodeOnInstall
        readIORef count `shouldReturn` 1
    it "refuses unsupported effective manifests instead of asserting no execution" $ do
        archive <- compress <$> tarBytes [("example-1.2.3/Cargo.toml", "[package]\nname='example'\nversion='1.2.3'\nbuild=['one.rs']\n")]
        provider <- newCargoInstallEvidence defaultLimits
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> pure (Right archive)))
        resolved evidenceReads (details "1.2.3" archive) >>= (`shouldSatisfy` permanentFailure)
    it "releases the public flight after cancellation without publishing evidence" $ do
        archive <- fixture "1.2.3" []
        entered <- MVar.newEmptyMVar
        blocked <- MVar.newEmptyMVar
        provider <- newCargoInstallEvidence defaultLimits
        pendingReads <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> MVar.putMVar entered () >> MVar.takeMVar blocked))
        withAsync (resolved pendingReads (details "1.2.3" archive)) $ \owner -> do
            MVar.takeMVar entered
            cancel owner
        recovered <- newInstallEvidenceReads provider (evidenceContext (\_ _ _ -> pure (Right archive)))
        withAsync (resolved recovered (details "1.2.3" archive)) $ \follower -> wait follower `shouldReturn` InstallDetermined NoCodeOnInstall
    it "uses one remaining deadline for an evidence operation" $ do
        archive <- fixture "1.2.3" []
        progress <- expectRight (mkProgressFloor 0.02 0.01 1)
        provider <- newCargoInstallEvidence defaultLimits
        blocked <- MVar.newEmptyMVar
        attempts <- newIORef (0 :: Int)
        let settings = (evidenceContext (\_ _ _ -> modifyIORef' attempts (+ 1) >> MVar.takeMVar blocked)){ieLimits = defaultLimits{progressFloor = progress}}
        evidenceReads <- newInstallEvidenceReads provider settings
        resolved evidenceReads (details "1.2.3" archive) >>= (`shouldSatisfy` transientFailure)
        resolved evidenceReads (details "1.2.4" archive) >>= (`shouldSatisfy` transientFailure)
        readIORef attempts `shouldReturn` 1
        count <- newIORef (0 :: Int)
        freshReads <- newInstallEvidenceReads provider (evidenceContext (counter count archive))
        resolved freshReads (details "1.2.3" archive) `shouldReturn` InstallDetermined NoCodeOnInstall
        readIORef count `shouldReturn` 1
    it "refuses an acquisition beyond the cumulative operation allowance" $ do
        archive <- fixture "1.2.3" []
        count <- newIORef (0 :: Int)
        provider <- newCargoInstallEvidence defaultLimits
        let limits = defaultLimits{maxVersionCount = 1}
        evidenceReads <- newInstallEvidenceReads provider (evidenceContext (counter count archive)){ieLimits = limits}
        resolved evidenceReads (details "1.2.3" archive) `shouldReturn` InstallDetermined NoCodeOnInstall
        resolved evidenceReads (details "1.2.4" archive) >>= (`shouldSatisfy` permanentFailure)
        readIORef count `shouldReturn` 1

evidenceContext :: InstallArtifactRead -> InstallEvidenceContext
evidenceContext artifactRead =
    InstallEvidenceContext
        { ieLimits = defaultLimits
        , ieRole = InstallPublicAdmission
        , ieReadArtifact = artifactRead
        , ieBudget = InstallEvidenceBudget (\action -> Just <$> action (const pass) (const pass))
        , ieFlightBudget = Nothing
        , ieDiagnostic = const pass
        }

counter :: IORef Int -> ByteString -> InstallArtifactRead
counter count bytes _ _ pay = modifyIORef' count (+ 1) >> pay (BS.length bytes) $> Right bytes

resolved :: (PackageDetails -> IO (Maybe InstallEvidenceRead)) -> PackageDetails -> IO InstallEvidenceResult
resolved evidenceReads value = evidenceReads value >>= runSubject

runSubject :: Maybe InstallEvidenceRead -> IO InstallEvidenceResult
runSubject = maybe (fail "native evidence provider omitted a subject") (\(InstallEvidenceRead readFact) -> readFact)

details :: Text -> ByteString -> PackageDetails
details version archive =
    (sampleDetails (mkPackageName Cargo Nothing "example") (mkVersion Cargo version))
        { pkgInstallCode = CodeExecUnknown
        , pkgArtifacts =
            sampleArtifact
                { artFilename = "example-" <> version <> ".crate"
                , artUrl = "https://registry.test/example/" <> version <> "/download"
                , artHashes = [unsafeHash SHA256 (sha256Hex archive)]
                }
                :| []
        }

fixture :: Text -> [(FilePath, ByteString)] -> IO ByteString
fixture = namedFixture "example" "example"

namedFixture :: Text -> Text -> Text -> [(FilePath, ByteString)] -> IO ByteString
namedFixture rootName manifestName version extras = compress <$> tarBytes [(toString rootName <> "-" <> toString version <> "/" <> path, body) | (path, body) <- ("Cargo.toml", "[package]\nname='" <> encodeUtf8 manifestName <> "'\nversion='" <> encodeUtf8 version <> "'\n") : extras]

projectedSubject :: PackageName -> Text -> Text -> ByteString -> IO PackageDetails
projectedSubject requested name version archive = do
    discovery <- expectRight cargoTestDiscovery
    let release = (Fixtures.minimalCargoRelease name version){Fixtures.releaseArchive = archive, Fixtures.releaseChecksum = sha256Hex archive}
    expectRight (projectCargoRecord requested (RecordEntry 11) discovery (Fixtures.cargoIndexRecord release))

compress :: BSL.ByteString -> ByteString
compress = BSL.toStrict . GZip.compress

unavailable :: ReadFault
unavailable = ReadFault (WillResolve Nothing) EvaluationFailed InstallSignalUnread

runs, permanentFailure, transientFailure :: InstallEvidenceResult -> Bool
runs = \case
    InstallDetermined (RunsCodeOnInstall _) -> True
    _ -> False
permanentFailure = \case
    InstallReadFailed (ReadFault WontResolve _ _) -> True
    _ -> False
transientFailure = \case
    InstallReadFailed (ReadFault (WillResolve _) _ _) -> True
    _ -> False

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Controlled native writes and durable records exercise the production worker orchestration.
module Ecluse.Worker.Support.Managed (
    ManagedFixture (..),
    managedFixture,
    managedInventory,
    sourceDocument,
    unmeteredStateBudget,
    managedJob,
    runManaged,
    runManagedWith,
    runManagedWithLog,
) where

import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Katip (LogEnv)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), VerifiedCompanionBytes (..))
import Ecluse.Core.Package
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Registry (PublishError (PublishError), PublishFault (PublishRejected, PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (cargoCached, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..), VersionEvaluation (VersionPresent))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.Publish.Internal (MirrorPublish (..), PreparedMirrorUpload (..), PreparedMirrorVersion (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Worker.Job (JobOutcome, processJob)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerPolicy (..), WorkerStateBudget (..))
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (noopWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (admitRule)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), expectRight)
import Ecluse.Worker.Support.Fixtures (npmPolicy, tarballBytes)
import Ecluse.Worker.Support.Runtime (runWMWith, withWiredRuntime)
import UnliftIO.Exception (throwIO)

-- | Faults mutate independently so a second runtime can reuse only durable target and state facts.
data ManagedFixture = ManagedFixture
    { mfPolicy :: WorkerPolicy
    , mfBinding :: WorkerMirrorState
    , mfRecord :: IORef (Maybe ProjectMirrorRecord)
    , mfTarget :: IORef [Artifact]
    , mfUploads :: IORef [Text]
    , mfProbes :: IORef Int
    , mfResolves :: IORef Int
    , mfSource :: IORef VersionEvaluation
    , mfStateFault :: IORef (Maybe MirrorStateFault)
    , mfRejectFile :: IORef (Maybe Text)
    , mfLoseReply :: IORef Bool
    , mfTargetTransform :: IORef (Artifact -> Artifact)
    , mfOnProbe :: IORef (Int -> IO ())
    , mfCompanions :: IORef (Map Text ByteString)
    }

-- | Unit fixtures keep production region boundaries without claiming process memory admission.
unmeteredStateBudget :: WorkerStateBudget
unmeteredStateBudget = WorkerStateBudget ignore ignore ignore ignore ignore ignore ignore
  where
    ignore = const (pure ())

-- | Three exact-byte files form one release, with optional declared metadata companions.
managedFixture :: Text -> Bool -> IO ManagedFixture
managedFixture location companion = do
    inventory <- managedInventory location companion [1, 2, 3]
    record <- newIORef Nothing
    target <- newIORef []
    uploads <- newIORef []
    probes <- newIORef 0
    resolves <- newIORef 0
    sourceRef <- newIORef (VersionPresent (sourceDocument inventory) Nothing)
    stateFault <- newIORef Nothing
    reject <- newIORef Nothing
    loseReply <- newIORef False
    transform <- newIORef id
    onProbe <- newIORef (const (pure ()))
    companions <- newIORef mempty
    revision <- newIORef (0 :: Int)
    let targetIdentity = stateTarget "https://managed.test/"
        store = fixtureStore targetIdentity record revision stateFault companions
        binding = WorkerMirrorState store targetIdentity defaultLimits (\action -> Just <$> action unmeteredStateBudget)
        publish = fixturePublish record target uploads probes reject loseReply transform onProbe
        resolve _ _ = modifyIORef' resolves (+ 1) >> readIORef sourceRef
        policy = (npmPolicy resolve [admitRule]){wpMirrorState = Just binding, wpPublish = publish}
    pure (ManagedFixture policy binding record target uploads probes resolves sourceRef stateFault reject loseReply transform onProbe companions)

-- | Exact digests and paired source entries for a changing admitted snapshot.
managedInventory :: Text -> Bool -> [Int] -> IO MirrorInventory
managedInventory location companion numbers = do
    template <- mirrorSample numbers companion (location <> "/") "retained source"
    digest <- expectRight (mkHash SHA256 (hexSha256Of tarballBytes))
    let details = inventoryDetails template
        artifacts = fmap (\file -> file{artHashes = [digest], artSize = Nothing}) (pkgArtifacts details)
        source = mirrorSource artifacts "retained source"
    expectRight (restoreMirrorInventory details{pkgArtifacts = artifacts} source)

-- | Restore the selected source representation consumed by the native preparer.
sourceDocument :: MirrorInventory -> VersionDoc
sourceDocument inventory = VersionDoc (inventoryDetails inventory) raw
  where
    raw = case inventoryReadData inventory of
        PyPIMirrorRead document -> Just (fst pypiSimpleCached document)
        NpmMirrorRead _ -> Nothing
        CargoMirrorRead document -> Just (fst cargoCached document)
        RubyGemsMirrorRead document -> Just (fst rubyGemsCached document)

fixtureStore :: StateTarget -> IORef (Maybe ProjectMirrorRecord) -> IORef Int -> IORef (Maybe MirrorStateFault) -> IORef (Map Text ByteString) -> MirrorState
fixtureStore target record revision fault companions =
    MirrorState
        { msReadProject = \_ name _ ->
            readIORef fault >>= \case
                Just failure -> pure (Left failure)
                Nothing -> do
                    current <- readIORef record
                    rev <- StateRevision target name . TE.encodeUtf8 . show <$> readIORef revision
                    pure (Right (maybe StateAbsent (StateLoaded rev) current))
        , msWriteProject = \_ name expected updated -> do
            current <- readIORef record
            number <- readIORef revision
            let rev = StateRevision target name (TE.encodeUtf8 (show number))
            if expected /= (rev <$ current)
                then pure (Right StateConflict)
                else do
                    writeIORef record (Just updated)
                    writeIORef revision (number + 1)
                    pure (Right (StateStored (StateRevision target name (TE.encodeUtf8 (show (number + 1))))))
        , msRetainCompanion = \_ (VerifiedCompanionBytes _ reference bytes) -> do
            modifyIORef' companions (Map.insert (companionDigest reference) bytes)
            pure (Right reference)
        , msReadCompanion = \_ reference -> maybeToRight StateMissingCompanion . Map.lookup (companionDigest reference) <$> readIORef companions
        }

fixturePublish :: IORef (Maybe ProjectMirrorRecord) -> IORef [Artifact] -> IORef [Text] -> IORef Int -> IORef (Maybe Text) -> IORef Bool -> IORef (Artifact -> Artifact) -> IORef (Int -> IO ()) -> MirrorPublish
fixturePublish record target uploads probes reject loseReply transform onProbe =
    MirrorPublish
        { mpMirrorCompanion = Nothing
        , mpNativeComplete = \_ _ _ -> False
        , mpProbeInventory = \_ name -> do
            number <- atomicModifyIORef' probes (\n -> (n + 1, n + 1))
            readIORef onProbe >>= ($ number)
            files <- readIORef target
            let versions = maybe mempty (Map.singleton "1" . PackageDetails name mirrorVersion Nothing NoCodeOnInstall Available) (nonEmpty files)
            pure (Right (PackageInfo name versions mempty []))
        , mpPrepareVersion = \name version doc -> do
            raw <- maybeToRight (PublishSourceUnavailable "missing fixture source") (vdRaw doc >>= snd pypiSimpleCached)
            inventory <- checkedMirrorInventory name version doc (PyPIMirrorRead raw)
            pure $ PreparedMirrorVersion inventory $ \_ file _ -> pure . Right . PreparedMirrorUpload $ \_ -> do
                stored <- readIORef record
                let expected = maybe mempty versionFiles (recordVersion version =<< stored)
                unless (all (\member -> Map.member (artFilename (expectedArtifact member)) expected) (inventoryFiles inventory)) $
                    throwIO (TestContractEscape "upload preceded expected inventory persistence")
                let artifact = expectedArtifact file
                    filename = artFilename artifact
                modifyIORef' uploads (<> [filename])
                failure <- readIORef reject
                if failure == Just filename
                    then pure (Left (PublishRejected (PublishError "injected write failure")))
                    else do
                        change <- readIORef transform
                        modifyIORef' target (\files -> change artifact : filter ((/= filename) . artFilename) files)
                        lost <- readIORef loseReply
                        pure $ if lost then Left (PublishRejected (PublishError "lost reply")) else Right ()
        }

-- | Every test uses the same whole-version demand without a filename constraint.
managedJob :: MirrorJob
managedJob = MirrorJob mirrorPackage mirrorVersion Nothing Nothing

-- | Recreate the worker runtime while preserving the fixture's durable store and native target.
runManaged :: ManagedFixture -> IO JobOutcome
runManaged = runManagedWith managedJob

-- | Run an explicit job against the fixture's existing store and target.
runManagedWith :: MirrorJob -> ManagedFixture -> IO JobOutcome
runManagedWith job fixture = do
    logEnv <- newTestLogEnv
    runManagedWithLog logEnv job fixture

-- | Select the log environment for a managed job without resetting its store.
runManagedWithLog :: LogEnv -> MirrorJob -> ManagedFixture -> IO JobOutcome
runManagedWithLog logEnv job fixture = do
    queue <- newTestMemoryQueue
    withWiredRuntime queue (Map.singleton PyPI (mfPolicy fixture)) noopWorkerMetricsPort $ \runtime ->
        runWMWith logEnv runtime (processJob job)

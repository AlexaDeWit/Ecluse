-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed read inputs shared by recurring benchmarks and isolated residency probes.
module Ecluse.Test.Mirror.Read (
    ReadFixture (..),
    ReadSession,
    ReadResult (..),
    readUnmanagedPreserved,
    makeReadFixture,
    makeReadFixtureFor,
    writeReadFixture,
    loadReadFixture,
    newReadSession,
    newReadSessionFromFiles,
    validateManagedProject,
    advanceReadSession,
    readManagedProject,
    readExchanges,
    readAdmissionCharge,
    readRetainedCharge,
    readLimits,
) where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Crypto.Hash (hashInit, hashUpdate)
import Data.Aeson (Value (Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (getFileSize)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, stateExchangeCharge)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..))
import Ecluse.Core.Package
import Ecluse.Core.Package.Merge (Provenance (TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiPacked)
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData (PyPIMirrorRead), inventoryDetails)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, charge, takeLargestCharge, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfOutputPermille))
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (..))
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Server.Pipeline.Packument.Representation (Contribution (..), assembleServedBody, outputBasisBytes)
import Ecluse.Core.Snapshot (Snapshot (..), digestFromContext)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Mirror.State (mirrorPackage, mirrorSample, mirrorSource)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIPackedChunks)
import Ecluse.Test.Support (expectRight, testMemoryMeter)

-- | Encoded state revisions and the same authorised native listing for both revisions.
data ReadFixture = ReadFixture
    { fixtureTarget :: StateTarget
    , fixtureOld :: ByteString
    , fixtureNew :: ByteString
    , fixturePrivate :: ByteString
    }

-- | Revisions change while the process retains the preceding decoded record.
data ReadSession = ReadSession MirrorView MemoryMeter Manifest (IORef Bool) (IORef (Int, [Maybe StateRevision]))

-- | Force the rendered bytes separately from the reconciliation assertions.
data ReadResult = ReadResult
    { readBody :: ByteString
    , readManagedVersions :: Int
    , readUnmanagedVersions :: Int
    , readPendingVersions :: Int
    , readPrivateRaw :: CachedDoc
    , readUnmanagedRaw :: Maybe CachedDoc
    }

-- | Compare the original packed value outside the measured serving action.
readUnmanagedPreserved :: ReadResult -> Bool
readUnmanagedPreserved result = readUnmanagedRaw result == Just (readPrivateRaw result)

-- | Use the shipped byte and structural bounds for every record and listing.
readLimits :: Limits
readLimits = defaultLimits

-- | Every version has three separately admitted fragments whose original coordinate is zero.
makeReadFixture :: Int -> IO ReadFixture
makeReadFixture = makeReadFixtureFor defaultTarget

-- | Bind recorded identity to the actual configured mirror in an HTTP fixture.
makeReadFixtureFor :: StateTarget -> Int -> IO ReadFixture
makeReadFixtureFor target count = do
    inventories <- traverse (uncurry inventory) [(version, file) | version <- [1 .. count], file <- [1 .. 3]]
    previous <- foldM add Nothing inventories >>= maybe (fail "empty fixture") pure
    addition <- inventory 1 4
    updated <- expectRight (extendProjectRecord readLimits target addition (Just previous))
    unmanaged <- traverse (inventory (count + 1)) [1 .. 3]
    old <- expectRight (encodeProjectRecord readLimits previous)
    new <- expectRight (encodeProjectRecord readLimits updated)
    let fields = concatMap nativeFields (inventories <> [addition] <> unmanaged)
        private = LBS.toStrict (encode (object ["name" .= ("demo" :: Text), "meta" .= object ["api-version" .= ("1.0" :: Text)], "files" .= fields]))
    evaluate (BS.length old + BS.length new + BS.length private) $> ReadFixture target old new private
  where
    add previous value = Just <$> expectRight (extendProjectRecord readLimits target value previous)

inventory :: Int -> Int -> IO MirrorInventory
inventory version file = do
    sample <- mirrorSample [file] False "https://source.test/" "original"
    let details = inventoryDetails sample
        rename = T.replace "1.0" (show version <> ".0")
        files = fmap (\artifact -> artifact{artFilename = rename (artFilename artifact), artUrl = rename (artUrl artifact)}) (pkgArtifacts details)
        updated = details{pkgVersion = mkVersion PyPI (show version), pkgArtifacts = files}
    let source = case mirrorSource files "retained source fields" of
            PyPIMirrorRead document -> PyPIMirrorRead (simpleDocument (simpleEnvelope document) [(key, addFields value) | (key, value) <- simpleFiles document])
            other -> other
    expectRight (restoreMirrorInventory updated source)
  where
    addFields (Object fields) = Object (KeyMap.insert "upload-time" (String "2020-01-01T00:00:00Z") (KeyMap.insert "source-extension" (object ["builder" .= ("release" :: Text), "tags" .= (["linux", "python"] :: [Text])]) fields))
    addFields other = other

nativeFields :: MirrorInventory -> [Value]
nativeFields value = case mirrorSource (pkgArtifacts (inventoryDetails value)) "native fields" of
    PyPIMirrorRead document -> [Object (KeyMap.insert "requires-python" (String ">=3.9") (KeyMap.insert "url" (String (T.replace "https://source.test/" "https://private.test/" url)) fields)) | (_, Object fields) <- simpleFiles document, Just (String url) <- [KeyMap.lookup "url" fields]]
    _ -> []

-- | Persist preparation outside the measured child process.
writeReadFixture :: FilePath -> ReadFixture -> IO ()
writeReadFixture directory fixture = do
    BS.writeFile (directory </> "old.json") (fixtureOld fixture)
    BS.writeFile (directory </> "new.json") (fixtureNew fixture)
    BS.writeFile (directory </> "private.json") (fixturePrivate fixture)

-- | Load encoded input without retaining fixture construction trees.
loadReadFixture :: FilePath -> IO ReadFixture
loadReadFixture directory = ReadFixture defaultTarget <$> BS.readFile (directory </> "old.json") <*> BS.readFile (directory </> "new.json") <*> BS.readFile (directory </> "private.json")

-- | Compute the old record's retention allowance outside the measured request.
readRetainedCharge :: ReadFixture -> IO Int
readRetainedCharge fixture = do
    decoded <- chargedDecodeProjectRecord (const (pure ())) readLimits (fixtureTarget fixture) mirrorPackage (fixtureOld fixture)
    expectRight (decoded >>= projectRecordWeight readLimits)

-- | The backend decodes only loaded revisions and records every conditional state exchange.
newReadSession :: ReadFixture -> IO ReadSession
newReadSession fixture = newSession (fixtureTarget fixture) limits (fixturePrivate fixture) $ \pay newer -> do
    let bytes = if newer then fixtureNew fixture else fixtureOld fixture
    pay (bufferedChunkCharge (BS.length bytes))
    pure bytes
  where
    limits = readLimits{maxMetadataBytes = max (BS.length (fixtureOld fixture)) (BS.length (fixtureNew fixture))}

-- | Allocate each transport body inside its request after charging its actual file size.
newReadSessionFromFiles :: FilePath -> IO ReadSession
newReadSessionFromFiles directory = do
    private <- BS.readFile (directory </> "private.json")
    newSession defaultTarget readLimits private $ \pay newer -> do
        let path = directory </> if newer then "new.json" else "old.json"
        size <- getFileSize path
        when (size > toInteger (maxMetadataBytes readLimits)) (fail "state fixture exceeds the metadata limit")
        pay (bufferedChunkCharge (fromInteger size))
        BS.readFile path

newSession :: StateTarget -> Limits -> ByteString -> (StateCharge -> Bool -> IO ByteString) -> IO ReadSession
newSession target limits privateBytes load = do
    changed <- newIORef False
    exchanges <- newIORef (0, [])
    pool <- newLocalPool 4 (256 * 1024 * 1024)
    retention <- newMirrorRetention pool 3600 (StoreBudget 0 0)
    meter <- testMemoryMeter
    streamed <- expectRight (projectPyPIPackedChunks readLimits mirrorPackage [privateBytes])
    (info, packed) <- expectRight (projectPyPIPacked readLimits mirrorPackage streamed)
    let private = Manifest info (fst pypiPacked packed) (BS.length privateBytes) mempty (digestFromContext (hashUpdate hashInit privateBytes))
        store = MirrorState (fetch changed exchanges) (\_ _ _ _ -> pure (Left StateRetryExhausted)) (\_ _ -> pure (Left StateMissingCompanion)) (\_ _ -> pure (Left StateMissingCompanion))
    pure (ReadSession (newMirrorView limits target store retention pypiMirrorDocuments) meter private changed exchanges)
  where
    fetch changed exchanges pay name held = do
        modifyIORef' exchanges $ \(count, history) ->
            let total = count + 1
                recent = take 3 (held : history)
             in total `seq` length recent `seq` (total, recent)
        pay stateExchangeCharge
        newer <- readIORef changed
        let revision = StateRevision target mirrorPackage (if newer then "two" else "one")
        if held == Just revision
            then pure (Right (StateUnchanged revision))
            else do
                bytes <- load pay newer
                fmap (StateLoaded revision) <$> chargedDecodeProjectRecord pay limits target name bytes

-- | Observe validation alone so the 304 accounting contract excludes listing output work.
validateManagedProject :: ReadSession -> IO Int
validateManagedProject (ReadSession view meter _ _ _) = do
    _ <- takeLargestCharge meter
    outcome <- withMemoryEntry noopMetricsPort meter $ \ticket -> validateMirrorProject view ticket mirrorPackage >>= expectRight
    when (isNothing (join (rightToMaybe outcome))) (fail "missing validated mirror project")
    takeLargestCharge meter

-- | Advance the remote revision without clearing the retained old record.
advanceReadSession :: ReadSession -> IO ()
advanceReadSession (ReadSession _ _ _ changed _) = writeIORef changed True

-- | Count every exchange while retaining only the latest three validators.
readExchanges :: ReadSession -> IO (Int, [Maybe StateRevision])
readExchanges (ReadSession _ _ _ _ exchanges) = second reverse <$> readIORef exchanges

-- | Read and reset the ticket high-water counter after a complete request.
readAdmissionCharge :: ReadSession -> IO Int
readAdmissionCharge (ReadSession _ meter _ _ _) = takeLargestCharge meter

-- | Exercise production validation, reconciliation, packed assembly, and serialisation.
readManagedProject :: ReadSession -> IO ReadResult
readManagedProject (ReadSession view meter private _ _) = do
    outcome <- withMemoryEntry noopMetricsPort meter $ \ticket -> do
        validated <- validateMirrorProject view ticket mirrorPackage >>= expectRight >>= maybe (fail "missing project") pure
        (unmanaged, managed, pending) <- expectRight (uncurry (mirrorProjectViews (mirrorViewDocuments view)) validated (Just private))
        let sources = map contribution (catMaybes [unmanaged, managed])
        plan <- maybe (fail "empty merge") pure (mergePackuments [(srcProvenance source, Snapshot (srcDigest source) (srcInfo source)) | source <- sources])
        charge ticket (scaleCharge (cfOutputPermille (metadataChargeFactors metadata)) (outputBasisBytes plan sources))
        body <- expectRight (metadataSerialise metadata (assembleServedBody metadata "https://proxy.test/simple" sources plan))
        let bytes = LBS.toStrict body
        _ <- evaluate (BS.length bytes)
        pure (ReadResult bytes (versions managed) (versions unmanaged) (Map.size pending) (manifestRaw private) (manifestRaw <$> unmanaged))
    either (const (fail "memory entry refused")) pure outcome
  where
    versions = maybe 0 (Map.size . infoVersions . manifestInfo)
    contribution manifest = Contribution TrustedSource (manifestInfo manifest) (manifestRaw manifest) (manifestDigest manifest) (manifestBodyBytes manifest)
    metadata = adapterMetadata pypiAdapter

defaultTarget :: StateTarget
defaultTarget = stateTarget "https://private.test"

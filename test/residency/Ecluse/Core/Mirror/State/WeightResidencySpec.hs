-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | State-only probes leave no native document or output allowance to hide retained undercharging.
module Ecluse.Core.Mirror.State.WeightResidencySpec (spec, probeMain) where

import Control.Exception (bracket, evaluate)
import Data.Aeson (FromJSON, ToJSON, Value (String), eitherDecodeStrict, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, projectRecordWeight)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package
import Ecluse.Core.Server.MemoryModel.Probe (HeapSample (live), forceShown, sample)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Mirror.Read (ReadFixture (fixtureOld), makeReadFixture, readLimits)
import Ecluse.Test.Mirror.State (mirrorPackage)
import Ecluse.Test.Support (expectRight)

data Measurement = Measurement
    { baselineLive :: Word64
    , heldLive :: Word64
    , releasedLive :: Word64
    , retainedCharge :: Int
    }
    deriving stock (Generic, Show)

instance FromJSON Measurement
instance ToJSON Measurement

spec :: Spec
spec = describe "isolated retained mirror state" $
    for_ [("Decoded", 1000), ("Descriptors", 1), ("Hashes", 1), ("Verified", 1), ("Companions", 1), ("Versions", 1), ("Backing", 1)] $ \(mode, count) ->
        it (mode <> " fits its charge after dropping input and preparation roots") $
            withSystemTempDirectory "ecluse-state-weight" $ \directory -> do
                fixture <- makeReadFixture count
                let path = directory </> "state.json"
                BS.writeFile path (fixtureOld fixture)
                executable <- getExecutablePath
                (status, output, errors) <- readProcessWithExitCode executable ["--mirror-state-probe", mode, path, "+RTS", "-T", "-N1", "-F1", "-A128k", "-RTS"] ""
                unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
                measured <- expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))
                putStrLn ("managed-state-residency " <> mode <> " " <> output)
                checkMeasurement measured

checkMeasurement :: Measurement -> Expectation
checkMeasurement measured = do
    let retained = toInteger (heldLive measured) - toInteger (baselineLive measured)
        residual = max 0 (toInteger (releasedLive measured) - toInteger (baselineLive measured))
    retained `shouldSatisfy` (> 0)
    retained `shouldSatisfy` (<= toInteger (retainedCharge measured))
    residual `shouldSatisfy` (<= 16 * 1024)
    10 * residual `shouldSatisfy` (<= retained)

-- | Warm and release runtime state before measuring a newly decoded, independently forced record.
probeMain :: String -> FilePath -> IO ()
probeMain mode path = do
    bracket (prepare mode path) (freeStablePtr . fst) (observe . fst)
    before <- live <$> sample
    (held, paid) <- bracket (prepare mode path) (freeStablePtr . fst) $ \(root, paid) -> do
        measured <- live <$> sample
        observe root
        pure (measured, paid)
    released <- live <$> sample
    LBS.putStr (encode (Measurement before held released paid))

-- Only the stable pointer and evaluated scalar leave this frame. Show visits opaque parsed keys and descriptors.
{-# NOINLINE prepare #-}
prepare :: String -> FilePath -> IO (StablePtr ProjectMirrorRecord, Int)
prepare mode path = do
    bytes <- BS.readFile path
    record <- chargedDecodeProjectRecord (const (pure ())) readLimits (stateTarget "https://private.test") mirrorPackage bytes >>= expectRight
    shaped <- shape mode record
    void (forceShown shaped)
    paid <- expectRight (projectRecordWeight readLimits shaped) >>= evaluate
    root <- newStablePtr shaped
    pure (root, paid)

observe :: StablePtr ProjectMirrorRecord -> IO ()
observe root = do
    record <- deRefStablePtr root
    evaluate (Map.size (recordVersions record)) >>= \count -> when (count <= 0) (fail "empty state root")

shape :: String -> ProjectMirrorRecord -> IO ProjectMirrorRecord
shape mode record = do
    digest <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
    let change version = case mode of
            "Hashes" -> version{storedFragments = fmap hashFragment (storedFragments version)}
            "Descriptors" -> version{storedFragments = fmap repeatExpected (storedFragments version)}
            "Verified" -> version{storedIdentities = Map.fromList [(show n, digest) | n <- [1 .. 128 :: Int]]}
            "Companions" -> version{storedCompanions = Map.fromList [(show n, CompanionRetained (CompanionRef digest 1)) | n <- [1 .. 128 :: Int]]}
            "Versions" -> version{storedFragments = fmap longVersion (storedFragments version)}
            _ -> version
        backing = T.take 1 (T.replicate 65536 "x")
        envelope = if mode == "Backing" then KeyMap.insert (Key.fromText backing) (String backing) (projectEnvelope record) else projectEnvelope record
    pure record{projectVersions = Map.map change (projectVersions record), projectEnvelope = envelope}
  where
    -- Decode validated the original pairing. Extend the retained graph without changing its source facts.
    hashFragment (StoredMirrorFragment details source expected) =
        let updated = details{pkgArtifacts = fmap (\artifact -> artifact{artHashes = concat (replicate 128 (artHashes artifact))}) (pkgArtifacts details)}
         in StoredMirrorFragment updated source expected
    repeatExpected (StoredMirrorFragment details source expected) = StoredMirrorFragment details source (foldl' (<>) expected (replicate 32 expected))
    longVersion (StoredMirrorFragment details source expected) =
        StoredMirrorFragment details{pkgVersion = mkVersion PyPI (T.intercalate "." (replicate 512 "1"))} source expected

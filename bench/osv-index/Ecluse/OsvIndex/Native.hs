-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bind actual child execution to files and append observed start/end events.
module Ecluse.OsvIndex.Native (fileIdentity, executionIdentity, recordTrial, readEvents, nativeOutputs) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.Time (getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import System.CPUTime (getCPUTime)
import System.Directory (canonicalizePath, doesFileExist, getFileSize)
import System.Environment (getExecutablePath)
import System.FilePath (makeRelative, (</>))
import UnliftIO.Exception (throwIO, tryAny)

import Ecluse.OsvIndex.Contract
import Ecluse.Test.Contract.Manifest (digestFile, failRunner, jsonLine, readJsonFile, writeJsonFile)

-- | Hash an actual file while retaining its path relative to the artifact root.
fileIdentity :: FilePath -> FilePath -> IO FileIdentity
fileIdentity root relative = FileIdentity relative <$> getFileSize (root </> relative) <*> digestFile (root </> relative)

-- | Read the actual hosted execution identity, refusing absent fields.
executionIdentity :: IO Execution
executionIdentity = Execution <$> field "GITHUB_SHA" <*> field "GITHUB_RUN_ID" <*> field "GITHUB_RUN_ATTEMPT" <*> field "RUNNER_ARCH"
  where
    field key = lookupEnv key >>= maybe (failRunner ("missing " <> toText key)) (pure . toText)

-- | The fixed producer outputs whose bytes each native record authenticates.
nativeOutputs :: Trial -> [FilePath]
nativeOutputs trial = [trialDirectory trial </> output]
  where
    output = case trWork trial of
        QueryBatch{} -> "result.json"
        _ -> "db/artifact.db"

-- | Identity hashing precedes timing. Output hashing follows the native end event.
recordTrial :: FilePath -> Trial -> IO () -> IO ()
recordTrial root trial action = do
    manifest <- readJsonFile (root </> "manifest.json")
    manifestHash <- digestFile (root </> "manifest.json")
    rootPath <- canonicalizePath root
    executable <- getExecutablePath >>= canonicalizePath
    binary <- fileIdentity rootPath (makeRelative rootPath executable)
    inputs <- traverse (fileIdentity root) (trialInputs trial)
    execution <- executionIdentity
    arguments <- getArgs
    capabilities <- getNumCapabilities
    ghcrts <- lookupEnv "GHCRTS"
    let binding = Binding trial execution manifestHash binary inputs arguments capabilities ghcrts
        expectedBinary = [viBinary variant | variant <- mfVariants manifest, viVariant variant == trVariant trial]
        identityHeld = mfExecution manifest == execution && expectedBinary == [binary] && arguments == drop 1 (trialCommand root trial) && mfCapabilities manifest == capabilities && mfGhcrts manifest == ghcrts
    prior <- readEvents (root </> "execution.jsonl")
    let sequenceNumber = 1 + length (filter ((== Started) . evPhase) prior)
    start <- event sequenceNumber Started
    BS.appendFile (root </> "execution.jsonl") (jsonLine start)
    cpuStart <- getCPUTime
    result <- tryAny (unless identityHeld (failRunner "native identity differs from the prepared manifest") >> action)
    cpuEnd <- getCPUTime
    end <- event sequenceNumber Finished
    outputs <- case result of
        Right () -> traverse (fileIdentity root) (nativeOutputs trial)
        Left _ -> pure []
    let fault = either (Just . show) (const Nothing) result
    writeJsonFile (root </> trialDirectory trial </> "native.json") (Native binding sequenceNumber start end (cpuEnd - cpuStart) outputs fault)
    BS.appendFile (root </> "execution.jsonl") (jsonLine end)
    either throwIO pure result
  where
    event sequenceNumber phase = Event sequenceNumber trial phase <$> getMonotonicTimeNSec <*> getCurrentTime

-- | Decode the append-only execution journal without treating partial lines as success.
readEvents :: FilePath -> IO [Event]
readEvents path =
    doesFileExist path >>= \exists ->
        if not exists
            then pure []
            else do
                bytes <- BS.readFile path
                traverse (either (failRunner . toText) pure . eitherDecodeStrict') (filter (not . BS.null) (BS.split 10 bytes))

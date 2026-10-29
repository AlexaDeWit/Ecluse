-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Resolve Cargo execution declarations from exact admitted archives, only on rule demand.
module Ecluse.Core.Registry.Cargo.Execution (
    newCargoInstallEvidence,
) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO.Exception (evaluate, finally, try)
import UnliftIO.MVar (withMVar)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Fault (boundedDetail)
import Ecluse.Core.Package
import Ecluse.Core.Registry (ParseError (parseErrorMessage))
import Ecluse.Core.Registry.Cargo.Archive (cargoArchiveExecution, cargoArchiveInflatedBytes, inspectCargoArchive)
import Ecluse.Core.Rules.Effectful (ReadFault (..))
import Ecluse.Core.Rules.Install
import Ecluse.Core.Rules.Types (Inability (AttemptTimedOut, EvaluationFailed, InstallEvidenceFailed), Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (Limits (..), floorServeCapMicros)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), retentionBackend)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Store (newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.Cache.VersionWeight.Fields (detailsFieldsWeight)
import Ecluse.Core.Server.Framing (frameComponents)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, expandWireBytesInteger)
import Ecluse.Core.Version (renderVersion)

-- | Retain compact verified public signals for five minutes. Private store evidence never enters this cache.
newCargoInstallEvidence :: Limits -> IO InstallEvidenceProvider
newCargoInstallEvidence limits = do
    pool <- newLocalPool (min 4096 (maxVersionCount limits)) (min (1024 * 1024) (maxMetadataBytes limits))
    retention <- newPooledRetention pool 300 (StoreBudget 0 0) (\(key, _) -> bufferedChunkCharge (BS.length key))
    flights <- newSingleFlightWithBackend (Just (retentionBackend LocalStorage retention))
    pure . InstallEvidenceProvider $ \context -> do
        serial <- newMVar ()
        started <- getMonotonicTimeNSec
        allowance <- newIORef (0 :: Int, 0 :: Integer)
        pure $ \details -> pure . Just . InstallEvidenceRead $ do
            now <- getMonotonicTimeNSec
            let elapsed = toInteger (now - started) `div` 1000
                remaining = toInteger (floorServeCapMicros (progressFloor (ieLimits context))) - elapsed
                readSubject = withMVar serial $ \() -> withSubjectBudget context details $ \boundedContext -> case immutableSubject details of
                    Left detail -> pure (Left (permanent detail))
                    Right (artifact, checksum) -> do
                        let key = subjectKey details artifact checksum
                            resolve budget = do
                                let acquire = fmap (key,) <$> inspectSubject boundedContext{ieBudget = budget} allowance details artifact checksum
                                if ieRole context == InstallStoreRevocation
                                    then acquire
                                    else resolveSingleFlight (const pass) (const pass) pass flights key acquire
                        case ieFlightBudget context of
                            Nothing -> resolve (ieBudget boundedContext)
                            Just sharing -> withinInstallEvidenceFlight sharing key resolve
            attempted <-
                try $
                    if remaining <= 0
                        then pure (Left deadlineFault)
                        else fromMaybe (Left deadlineFault) <$> timeout (fromInteger remaining) readSubject
            let result = either (\InstallEvidenceBudgetExceeded -> Left (permanent "Cargo execution evidence exceeds its reserved memory envelope")) id attempted
            case result of
                Left fault -> ieDiagnostic context fault $> InstallReadFailed fault
                Right (_, runs) -> pure (InstallDetermined (if runs then RunsCodeOnInstall "Cargo build script or procedural macro declaration" else NoCodeOnInstall))

withSubjectBudget :: InstallEvidenceContext -> PackageDetails -> (InstallEvidenceContext -> IO (Either ReadFault a)) -> IO (Either ReadFault a)
withSubjectBudget context details use = do
    let saturate = fromInteger . min (toInteger (maxBound :: Int))
        keyCharge = saturate (expandWireBytesInteger (detailsFieldsWeight details))
    result <- withInstallEvidenceBudget (ieBudget context) $ \bytes inspection ->
        ( do
            bytes keyCharge
            let subjectBudget = InstallEvidenceBudget (\action -> Just <$> action (\cost -> bytes (saturate (toInteger keyCharge + toInteger cost))) inspection)
            use context{ieBudget = subjectBudget}
        )
            `finally` (inspection 0 >> bytes 0)
    pure (fromMaybe (Left (ReadFault (WillResolve Nothing) EvaluationFailed (InstallEvidenceFailed "evidence memory admission is unavailable"))) result)

immutableSubject :: PackageDetails -> Either Text (Artifact, Text)
immutableSubject details
    | pkgEcosystem (pkgName details) /= Cargo = Left "Cargo execution evidence requires a native Cargo subject"
    | otherwise = case toList (pkgArtifacts details) of
        [artifact] -> case [T.toLower (hashValue digest) | digest <- artHashes artifact, hashAlg digest == SHA256] of
            [checksum] -> Right (artifact, checksum)
            _ -> Left "Cargo execution evidence requires one admitted SHA-256"
        _ -> Left "Cargo execution evidence requires one exact admitted archive"

subjectKey :: PackageDetails -> Artifact -> Text -> ByteString
subjectKey details artifact checksum = frameComponents (map (Just . TE.encodeUtf8) fields)
  where
    fields = ["cargo-execution-v1", renderPackageName (pkgName details), renderVersion (pkgVersion details), artUrl artifact, show (artEntryKey artifact), checksum]

inspectSubject :: InstallEvidenceContext -> IORef (Int, Integer) -> PackageDetails -> Artifact -> Text -> IO (Either ReadFault Bool)
inspectSubject context allowance details artifact checksum = do
    let configured = ieLimits context
    reserved <- atomicModifyIORef' allowance $ \prior@(count, spent) ->
        let remaining = max 0 (toInteger (maxMetadataBytes configured) - spent)
            cap = min (maxMirrorArtifactBytes configured) (fromInteger (remaining `div` 2))
            work = 2 * toInteger cap
         in if cap <= 0 || count >= maxVersionCount configured
                then (prior, Nothing)
                else ((count + 1, spent + work), Just (cap, work))
    case reserved of
        Nothing -> pure (Left (permanent "Cargo execution evidence exceeds the operation count or cumulative byte allowance"))
        Just (cap, work) -> do
            let bounded = configured{maxMirrorArtifactBytes = cap}
            result <- withInstallEvidenceBudget (ieBudget context) $ \bytesCharge inspectCharge ->
                (acquire bounded bytesCharge inspectCharge `finally` inspectCharge 0) `finally` bytesCharge 0
            case result of
                Just (Right (runs, actual)) -> do
                    atomicModifyIORef' allowance (\(count, spent) -> ((count, spent - work + actual), ()))
                    pure (Right runs)
                Just (Left fault) -> pure (Left fault)
                Nothing -> pure (Left (ReadFault (WillResolve Nothing) EvaluationFailed (InstallEvidenceFailed "evidence memory admission is unavailable")))
  where
    acquire limits bytesCharge inspectCharge =
        ieReadArtifact context limits artifact bytesCharge >>= \case
            Left fault -> pure (Left fault)
            Right bytes -> do
                inspected <- inspectCargoArchive limits inspectCharge (renderPackageName (pkgName details)) (renderVersion (pkgVersion details)) (Just checksum) bytes
                case inspected of
                    Left fault -> pure (Left (permanent (parseErrorMessage fault)))
                    Right metadata -> case cargoArchiveExecution metadata of
                        Left fault -> pure (Left (permanent (parseErrorMessage fault)))
                        Right runs -> do
                            determined <- evaluate runs
                            actual <- evaluate (toInteger (BS.length bytes) + toInteger (cargoArchiveInflatedBytes metadata))
                            pure (Right (determined, actual))

permanent :: Text -> ReadFault
permanent detail = ReadFault WontResolve EvaluationFailed (InstallEvidenceFailed (boundedDetail detail))

deadlineFault :: ReadFault
deadlineFault = ReadFault (WillResolve Nothing) EvaluationFailed AttemptTimedOut

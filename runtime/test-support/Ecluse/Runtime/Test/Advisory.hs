-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project the runtime sync into independent contract observations.
module Ecluse.Runtime.Test.Advisory (advisoryDriver) where

import System.Directory (doesFileExist)

import Ecluse.Core.Cve.Slot (currentAdvisoryEtag, currentAdvisorySource, generationInstalledAt, withSlotGeneration)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Runtime.Cve.Sync.Internal (SyncEnv (..), SyncOutcome (..), syncStep)
import Ecluse.Test.Contract.Advisory (AdvisoryDriver (..), AdvisoryFixture, AdvisoryOutcome (..), AdvisorySnapshot (..))
import Ecluse.Test.Cve (namesFix)

-- | The publisher and environment loader control transport without deciding sync verdicts.
advisoryDriver :: ByteString -> (AdvisoryFixture -> IO DbEtag) -> IO SyncEnv -> IO AdvisoryDriver
advisoryDriver bytes publish loadEnv = do
    seen <- newIORef Nothing
    pure
        AdvisoryDriver
            { publishArtifact = publish
            , syncArtifact = do
                env <- loadEnv
                previous <- readIORef seen
                syncStep env previous >>= \case
                    SyncSwapped etag _ -> writeIORef seen (Just etag) $> Accepted etag
                    SyncRejected etag reason -> writeIORef seen (Just etag) $> Refused etag reason
                    other -> pure (Unexpected (show other))
            , observeArtifact = do
                env <- loadEnv
                let slot = syncSlot env
                AdvisorySnapshot
                    <$> readFileBS (syncDbPath env)
                    <*> currentAdvisoryEtag slot
                    <*> currentAdvisorySource slot
                    <*> generationInstalledAt slot
                    <*> withSlotGeneration slot (traverse (\(_, lookup', _) -> namesFix lookup' "pkg-a" "1.0.0"))
                    <*> doesFileExist (syncDbPath env <> ".tmp")
            , acceptedBytes = bytes
            }

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory artifact sync and the write side of "Ecluse.Core.Cve.Slot".
Each mount retries at boot, then polls for new artifacts. An empty slot denies by default.
"Ecluse.Runtime.Cve.Sync.Internal" implements it.
-}
module Ecluse.Runtime.Cve.Sync (
    -- * The injected transport
    DbEtag (..),
    S3CveSource,
    newS3CveSource,
    s3CveFetchFor,

    -- * One sync cycle
    SyncEnv (..),

    -- * The scheduled task
    SyncSchedule (..),
    SyncHooks (..),
    runCveSync,
    bootBackoffDelays,
    absentReportInterval,
) where

import Ecluse.Runtime.Cve.Sync.Internal (
    DbEtag (..),
    S3CveSource,
    SyncEnv (..),
    SyncHooks (..),
    SyncSchedule (..),
    absentReportInterval,
    bootBackoffDelays,
    newS3CveSource,
    runCveSync,
    s3CveFetchFor,
 )

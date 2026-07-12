-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared fixtures for the queue specs: the sample 'MirrorJob's the backend and
buffer tests carry, and the loud unwrapper for backends that should not fault.
-}
module Ecluse.Queue.Support (
    UnexpectedQueueFault (..),
    unwrap,
    sampleJob,
    otherJob,
    thirdJob,
) where

import UnliftIO (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Queue (MirrorJob (..), QueueFault)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unsafeRegistryUrl)

{- | A 'Left' escaping a backend that has no fault to report (the bounded
in-memory backend, the buffered hand-off) is a broken test premise:
re-raise it loudly and typed.
-}
newtype UnexpectedQueueFault = UnexpectedQueueFault QueueFault
    deriving stock (Show)

instance Exception UnexpectedQueueFault

-- | Unwrap a typed queue outcome from a backend under test that should not fault.
unwrap :: IO (Either QueueFault a) -> IO a
unwrap act = act >>= either (throwIO . UnexpectedQueueFault) pure

{- | A sample mirror job. The in-memory queue under test does not inspect a
job's contents -- it only carries it from 'enqueue' to 'receive' -- so one fixed
job suffices for the FIFO / cap / drop-reporting assertions.
-}
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "thing"
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = unsafeRegistryUrl "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobArtifactFilename = "thing-1.0.0.tgz"
        , jobTraceContext = Nothing
        }

{- | A second, distinct job, used to assert FIFO ordering across two enqueues.
It differs from 'sampleJob' only in its version, which is enough to tell the two
apart on receive.
-}
otherJob :: MirrorJob
otherJob = sampleJob{jobVersion = mkVersion Npm "2.0.0"}

{- | A third, distinct job, used by the bounded-queue tests to tell the retained
jobs apart from a dropped-newest one at the cap.
-}
thirdJob :: MirrorJob
thirdJob = sampleJob{jobVersion = mkVersion Npm "3.0.0"}

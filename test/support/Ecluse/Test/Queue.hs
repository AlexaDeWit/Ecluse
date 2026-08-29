-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The production in-memory mirror queue configured for tests, and the sample job the
queue-carrying suites enqueue.

A suite gets the same bounded backend the composition root rolls over to when no
@ECLUSE_QUEUE__URL@ is set. The knobs are test-sized: a depth cap far above what any spec
enqueues, a short idle poll so a receive on an empty queue returns promptly, and a drop
callback that throws, because a drop means a spec outgrew the cap. A spec asserting the
backend's own cap or drop reporting builds its own config instead.
-}
module Ecluse.Test.Queue (
    newTestMemoryQueue,
    sampleJob,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Queue (MirrorJob (..), MirrorQueue)
import Ecluse.Core.Queue.Memory (MemoryQueueConfig (..), newBoundedInMemoryQueue)
import Ecluse.Test.Package (thingName, unsafeFilename, unsafeRegistryUrl, v1_0_0)

{- | A cap-overflow drop from the test queue, carrying the backend's running drop total. It is a
broken test premise, so the typed value fails the test loudly instead of silently losing a job.
-}
newtype UnexpectedTestQueueDrop = UnexpectedTestQueueDrop Int
    deriving stock (Show)

instance Exception UnexpectedTestQueueDrop

-- | The bounded in-memory queue at the test knobs the module header describes.
newTestMemoryQueue :: IO MirrorQueue
newTestMemoryQueue =
    newBoundedInMemoryQueue
        MemoryQueueConfig{memQueueMaxDepth = 512, memQueuePollWaitMicros = 50_000}
        (throwIO . UnexpectedTestQueueDrop)

{- | A sample mirror job. No queue backend inspects a job's contents, so one fixed job serves
the round-trip, FIFO, cap, and drop-reporting assertions.
-}
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = thingName
        , jobVersion = v1_0_0
        , jobArtifactUrl = unsafeRegistryUrl "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobArtifactFilename = unsafeFilename "thing-1.0.0.tgz"
        , jobTraceContext = Nothing
        }

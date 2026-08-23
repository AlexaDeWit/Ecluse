-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The production in-memory mirror queue, configured for tests.

A suite gets the same bounded backend the composition root rolls over to when no
@ECLUSE_QUEUE__URL@ is set. The knobs are test-sized: a depth cap far above what any spec
enqueues, a short idle poll so a receive on an empty queue returns promptly, and a drop
callback that throws, because a drop means a spec outgrew the cap. A spec asserting the
backend's own cap or drop reporting builds its own config instead.
-}
module Ecluse.Test.Queue (
    newTestMemoryQueue,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Queue.Memory (MemoryQueueConfig (..), newBoundedInMemoryQueue)

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

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The production in-memory mirror queue, configured for tests.

Suites that need a 'MirrorQueue' assemble the same bounded backend the
composition root selects for @ECLUSE_QUEUE_BACKEND=memory@
('Ecluse.Core.Queue.Memory.newBoundedInMemoryQueue'), with test-sized knobs
rather than the production ones:

* a depth cap far above what any spec enqueues, so the bounded backend's
  drop-newest overflow shed can never fire under a test's job volume;
* a short idle-poll window, so a 'Ecluse.Core.Queue.receive' on an empty queue
  returns its healthy @[]@ promptly instead of waiting out the production
  long-poll cadence;
* a drop callback that throws, so a drop (a broken test premise: some spec
  outgrew the cap) fails the test loudly instead of silently losing a job the
  spec meant to observe.

A spec asserting the backend's own cap or drop-reporting behaviour builds its
own 'Ecluse.Core.Queue.Memory.MemoryQueueConfig' instead (see
@Ecluse.Queue.MemorySpec@).
-}
module Ecluse.Test.Queue (
    UnexpectedTestQueueDrop (..),
    newTestMemoryQueue,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Queue.Memory (MemoryQueueConfig (..), newBoundedInMemoryQueue)

{- | A cap-overflow drop from the test queue, carrying the backend's running drop
total: a broken test premise (some spec outgrew the cap), surfaced typed so it
fails the test loudly instead of silently losing a job the spec meant to observe.
-}
newtype UnexpectedTestQueueDrop = UnexpectedTestQueueDrop Int
    deriving stock (Show)

instance Exception UnexpectedTestQueueDrop

-- | See the module header for the knobs and why each is what it is.
newTestMemoryQueue :: IO MirrorQueue
newTestMemoryQueue =
    newBoundedInMemoryQueue
        MemoryQueueConfig{memQueueMaxDepth = 512, memQueuePollWaitMicros = 50_000}
        (throwIO . UnexpectedTestQueueDrop)

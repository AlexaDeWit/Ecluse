-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Queue-spec fixtures beyond the shared 'sampleJob': the two sibling jobs an ordering or
cap assertion needs.
-}
module Ecluse.Queue.Support (
    otherJob,
    thirdJob,
) where

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Queue (sampleJob)

{- | A second job for the FIFO-ordering assertion. It differs from 'sampleJob' only in its
version, which is enough to tell the two apart on receive.
-}
otherJob :: MirrorJob
otherJob = sampleJob{jobVersion = mkVersion Npm "2.0.0"}

{- | A third, distinct job. The bounded-queue tests use it to tell the retained jobs
apart from a dropped-newest one at the cap.
-}
thirdJob :: MirrorJob
thirdJob = sampleJob{jobVersion = mkVersion Npm "3.0.0"}

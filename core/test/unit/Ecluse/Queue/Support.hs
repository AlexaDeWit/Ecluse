-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Queue-spec fixtures beyond the shared 'sampleJob': the two sibling jobs an ordering or
cap assertion needs, and the loud unwrapper for backends that should not fault.
-}
module Ecluse.Queue.Support (
    unwrap,
    otherJob,
    thirdJob,
) where

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Queue (sampleJob)
import Ecluse.Test.Support (expectRight)

{- | Unwrap a typed queue outcome from a backend under test that should not fault. A 'Left'
from the bounded in-memory or buffered hand-off backend is a broken test premise.
-}
unwrap :: IO (Either TransportFault a) -> IO a
unwrap act = act >>= expectRight

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

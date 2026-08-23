-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared fixtures for the queue specs: the sample 'MirrorJob's the backend and
buffer tests carry, and the loud unwrapper for backends that should not fault.
-}
module Ecluse.Queue.Support (
    unwrap,
    sampleJob,
    otherJob,
    thirdJob,
) where

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Support (expectRight)

{- | Unwrap a typed queue outcome from a backend under test that should not fault. A 'Left'
from the bounded in-memory or buffered hand-off backend is a broken test premise.
-}
unwrap :: IO (Either TransportFault a) -> IO a
unwrap act = act >>= expectRight

{- | A sample mirror job. The in-memory queue never inspects a job's contents, so one fixed
job serves the FIFO, cap, and drop-reporting assertions.
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

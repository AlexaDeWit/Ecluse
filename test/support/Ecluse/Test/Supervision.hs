-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A fast supervision policy, and a bounded supervised run, for specs that drive 'superviseLoop'.
module Ecluse.Test.Supervision (
    fastPolicy,
    superviseBriefly,
) where

import Katip (KatipContext)
import UnliftIO (MonadUnliftIO, timeout)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition,
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
 )

-- | A policy over the given classifier with a tiny backoff, so a test never sleeps long.
fastPolicy :: (SomeException -> FaultDisposition) -> SupervisionPolicy
fastPolicy classify =
    SupervisionPolicy
        { spLabel = "test-loop"
        , spClassify = classify
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000, bsCapMicros = 8_000}
        }

-- | Rerun @step@ under 'fastPolicy' until it fails up or 300 ms pass, every fault taking @disposition@.
superviseBriefly :: (MonadUnliftIO m, KatipContext m) => FaultDisposition -> m a -> m ()
superviseBriefly disposition step =
    void . timeout 300_000 . tryAny $ superviseLoop (fastPolicy (const disposition)) (void step)

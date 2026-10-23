-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supervision policies and bounded retry pacing.
module Ecluse.Core.Supervision (
    -- * The combinator
    superviseLoop,
    superviseBackground,
    SupervisionPolicy (..),
    transientPolicy,
    FaultDisposition (..),

    -- * Bounded exponential backoff
    BackoffSchedule (..),
    backoffMicros,
    backgroundLoopBackoff,

    -- * Bounded retry pacing
    delayListPolicy,
) where

import Ecluse.Core.Supervision.Internal

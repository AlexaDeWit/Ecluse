-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A refused admission payment, for specs that check a reader or inflater stops at the refusal.
module Ecluse.Server.Admission.Support (
    ChargeRefused (..),
    refuseCharge,
) where

import UnliftIO.Exception (throwIO)

-- | What 'refuseCharge' throws. A spec matches it with @(== ChargeRefused)@.
data ChargeRefused = ChargeRefused
    deriving stock (Eq, Show)

instance Exception ChargeRefused

-- | A charge callback that refuses every payment.
refuseCharge :: Int -> IO ()
refuseCharge _ = throwIO ChargeRefused

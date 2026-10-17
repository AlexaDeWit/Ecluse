-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Extractors over the serve-outcome model ("Ecluse.Core.Server.Response").
A spec reads the decided part it asserts on, instead of pattern matching inside the assertion.
-}
module Ecluse.Test.Server.Response (
    rejectionOf,
    reasonOf,
) where

import Ecluse.Core.Server.Response (
    RejectReason,
    Rejection (rejectionReason),
    ServeDecision (Admit, Reject),
 )

-- | The refusal a decision carries, or 'Nothing' where it admitted the request.
rejectionOf :: ServeDecision -> Maybe Rejection
rejectionOf = \case
    Admit -> Nothing
    Reject rejection -> Just rejection

-- | The refusal reason a decision carries, or 'Nothing' where it admitted the request.
reasonOf :: ServeDecision -> Maybe RejectReason
reasonOf = fmap rejectionReason . rejectionOf

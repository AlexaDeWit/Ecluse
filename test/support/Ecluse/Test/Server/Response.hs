-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Extractors over the serve-outcome model ("Ecluse.Core.Server.Response").
A spec reads the decided part it asserts on, instead of pattern matching inside the assertion.
-}
module Ecluse.Test.Server.Response (
    reasonOf,
) where

import Ecluse.Core.Server.Response (
    RejectReason,
    Rejection (rejectionReason),
    ServeDecision (Admit, Reject),
 )

-- | The refusal reason a decision carries, or 'Nothing' where it admitted the request.
reasonOf :: ServeDecision -> Maybe RejectReason
reasonOf = \case
    Admit -> Nothing
    Reject rejection -> Just (rejectionReason rejection)

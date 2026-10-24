-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Unit-slot admission for metadata requests, and the overflow queue in front of both gates.
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    withServeAdmission,
    serveAdmissionLine,
    serveAdmissionOverflow,

    -- * Internals exported for testing
    newServeAdmissionTuned,
    withOverflow,
) where

import Ecluse.Core.Server.Admission.Internal

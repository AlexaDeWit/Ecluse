-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Unit-slot admission for metadata requests.
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    newServeAdmissionSwept,
    withServeAdmissionE,
    serveAdmissionLevels,
    withServeAdmission,

    -- * Internals exported for testing
    newServeAdmissionTuned,
) where

import Ecluse.Core.Server.Admission.Internal

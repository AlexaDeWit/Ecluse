-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Byte-weighted admission for publish requests.
module Ecluse.Core.Server.Admission.Bytes (
    ByteAdmission,
    newByteAdmission,
    withByteAdmission,

    -- * Internals exported for testing
    newByteAdmissionTuned,
) where

import Ecluse.Core.Server.Admission.Bytes.Internal

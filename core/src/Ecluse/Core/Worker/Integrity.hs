-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror callers use the shared artifact digest verifier.
module Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity) where

import Ecluse.Core.Package.Integrity (IntegrityResult (..), verifyIntegrity)

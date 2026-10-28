-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The verified artifact representation supports pipeline code and residency probes.
module Ecluse.Core.Server.Pipeline.Tarball.Verified.Internal (VerifiedArtifactBody (..)) where

import Ecluse.Core.Server.Conditional (ETag)

-- | A verified body retains the exact bytes beside their local representation validator.
data VerifiedArtifactBody = VerifiedArtifactBody !ByteString !ETag

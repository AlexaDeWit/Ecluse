-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Credential fixtures shared across the suites. 'noCredentialReporters' observes nothing, so a
credential test drives refresh and mint policy without a telemetry substrate.
-}
module Ecluse.Test.Credential (
    noCredentialReporters,
    basicReadsBearerWrites,
) where

import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBasic, AuthBearer), ServicePresentation, StoreOperation (StoreRead, StoreWrite), declaredPresentation)

-- | A store that asks for Basic on reads and Bearer on writes.
basicReadsBearerWrites :: ServicePresentation
basicReadsBearerWrites = declaredPresentation $ \case
    StoreRead -> AuthBasic
    StoreWrite -> AuthBearer

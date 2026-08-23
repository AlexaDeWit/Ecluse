-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Credential-provider test fixtures.

The suites hand 'noCredentialReporters' to the provider constructors as the "do not
observe" argument. A credential test then drives the refresh and mint policy without
wiring a telemetry substrate. The production composition root passes real reporters.
-}
module Ecluse.Test.Credential (
    noCredentialReporters,
) where

import Ecluse.Core.Credential.Refresh (noCredentialReporters)

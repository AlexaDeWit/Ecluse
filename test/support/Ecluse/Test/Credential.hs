-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Credential-provider test fixtures.

'noCredentialReporters' is the inert 'CredentialReporters': a provider built with it
records nothing on either the mint breaker's transitions or a refresh outcome. The
suites hand it to the provider constructors as the "do not observe" argument, so a
credential test drives the refresh/mint policy without wiring a telemetry substrate.
The production composition root passes real reporters instead.
-}
module Ecluse.Test.Credential (
    noCredentialReporters,
) where

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Credential.Refresh (CredentialReporters (..), noRefreshReporter)

-- | Inert observers for both signals: the provider records nothing.
noCredentialReporters :: CredentialReporters
noCredentialReporters = CredentialReporters noBreakerReporter noRefreshReporter

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A test- and dev-only builder for a plain-HTTP loopback 'RegistryUrl', so a suite can dial
an in-process server rather than standing up TLS.

The @dev-http-egress@ Cabal flag (default off) is the only build that compiles this module, so
a release artifact carries no way to construct a non-https registry target. The production
builder is the https-only "Ecluse.Core.Security.Egress".'mkRegistryUrl'.
-}
module Ecluse.Core.Security.Egress.DevHttp (
    loopbackRegistryUrl,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.Egress.Internal (RegistryUrl (RegistryUrl))

{- | Build a 'RegistryUrl' from a loopback URL, bypassing the https-only check. This exists only
in a @dev-http-egress@ build, never in a release artifact.
-}
loopbackRegistryUrl :: Text -> RegistryUrl
loopbackRegistryUrl = RegistryUrl . T.strip

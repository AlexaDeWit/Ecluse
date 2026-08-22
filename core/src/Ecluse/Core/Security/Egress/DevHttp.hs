-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A test- and dev-only escape hatch that builds a 'RegistryUrl' from a plain-HTTP
loopback URL. The integration suites can then dial an in-process @http:\/\/127.0.0.1@
server rather than standing up TLS.

The @dev-http-egress@ Cabal flag (default off) is the only build that exposes this
module. The release library and the shipped executable build with the flag off. They
therefore do not compile this module, and the loopback constructor does not exist in a
release artifact. Nothing can relax the https-only egress posture in production. The
production builder is the https-only "Ecluse.Core.Security.Egress".'mkRegistryUrl'.
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

{- | A test- and dev-only escape hatch that builds a 'RegistryUrl' from a plain-HTTP
loopback URL, so the integration suites can dial an in-process @http:\/\/127.0.0.1@
server rather than standing up TLS.

This module is exposed __only__ under the @dev-http-egress@ Cabal flag (default off).
The release library and the shipped executable are built with the flag off, so this
module is not compiled into them and the loopback constructor does not exist in a
release artifact: the https-only egress posture cannot be relaxed in production. The
production builder is the https-only "Ecluse.Core.Security.Egress".'mkRegistryUrl'.
-}
module Ecluse.Core.Security.Egress.DevHttp (
    loopbackRegistryUrl,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.Egress.Internal (RegistryUrl (RegistryUrl))

{- | Build a 'RegistryUrl' from an @http:\/\/@ (or @https:\/\/@) loopback URL,
__bypassing the https-only check__. For tests and local development only: it exists
only in a @dev-http-egress@ build, never in a release artifact. The URL is trimmed but
otherwise taken as given.
-}
loopbackRegistryUrl :: Text -> RegistryUrl
loopbackRegistryUrl = RegistryUrl . T.strip

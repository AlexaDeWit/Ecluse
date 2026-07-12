-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm test-client __fixtures__: the public-registry defaults the suites build
'NpmClientConfig' values from.

'defaultNpmConfig' is the anonymous public-registry config a suite hands to the npm data
plane ("Ecluse.Core.Registry.Npm"); 'publicRegistryBaseUrl' and 'publicRegistryUrl' are the
canonical public npm registry as text and as an https 'RegistryUrl' (built through
'Ecluse.Test.Package.unsafeRegistryUrl', over the https-only
'Ecluse.Core.Security.Egress.mkRegistryUrl').
-}
module Ecluse.Test.Registry.Npm (
    defaultNpmConfig,
    publicRegistryBaseUrl,
    publicRegistryUrl,
) where

import Network.HTTP.Client (Manager)

import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Test.Package (unsafeRegistryUrl)

{- | The canonical public npm registry base URL, @https://registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | The canonical public npm registry as an https 'RegistryUrl': the
'publicRegistryBaseUrl' text validated through 'unsafeRegistryUrl' (the https-only
'Ecluse.Core.Security.Egress.mkRegistryUrl').
-}
publicRegistryUrl :: RegistryUrl
publicRegistryUrl = unsafeRegistryUrl publicRegistryBaseUrl

{- | An anonymous client config against the public registry ('publicRegistryBaseUrl'),
using the given shared 'Manager' and the secure-default response bounds
('Ecluse.Core.Security.defaultLimits'). Override 'npmBaseUrl'/'npmToken'/'npmLimits' for
a managed backend or a per-deployment budget.
-}
defaultNpmConfig :: Manager -> NpmClientConfig
defaultNpmConfig manager =
    NpmClientConfig
        { npmBaseUrl = publicRegistryBaseUrl
        , npmManager = manager
        , npmToken = Nothing
        , npmLimits = defaultLimits
        }

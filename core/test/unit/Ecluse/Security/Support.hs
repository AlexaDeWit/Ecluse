-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Authority fixtures the host-guard and authority-extraction specs share: the two
'HostPort' builders and the configured upstream set they gate against.
-}
module Ecluse.Security.Support (
    hp,
    hpAt,
    upstreamHosts,
    upstreams,
) where

import Data.Set qualified as Set

import Ecluse.Core.Security (AllowedHostPorts, HostPort (HostPort), allowedHostPorts)

-- | An authority on the https default port: what a URL with no written port dials.
hp :: Text -> HostPort
hp host = HostPort host 443

-- | An authority on an explicit port.
hpAt :: Text -> Word16 -> HostPort
hpAt = HostPort

{- | The raw configured upstream authorities, mixed case on purpose, so a case can extend them
before normalising. Every entry is portless, so each authorises 443 alone.
-}
upstreamHosts :: Set HostPort
upstreamHosts = Set.fromList [hp "registry.npmjs.org", hp "Private.Internal.Example.com"]

{- | The configured upstreams, normalised through 'allowedHostPorts', the only way to obtain
the 'AllowedHostPorts' the host guards take.
-}
upstreams :: AllowedHostPorts
upstreams = allowedHostPorts upstreamHosts

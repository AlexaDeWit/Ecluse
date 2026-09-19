-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The guards over the proxy's data plane, gathered as one import for the fetch and
serve layers that compose them at the boundary.

Each child owns its own contract: where the proxy may fetch in
"Ecluse.Core.Security.Host", the authority those gates compare in
"Ecluse.Core.Security.Authority", and what an upstream response may cost in
"Ecluse.Core.Security.Limits".
-}
module Ecluse.Core.Security (
    module Ecluse.Core.Security.Host,
    module Ecluse.Core.Security.Limits,
    module Ecluse.Core.Security.Authority,
    -- The 'IpAddr' constructors stay internal to the parser and the policy layer, so only
    -- the recogniser is re-exported.
    parseIpLiteral,
) where

import Ecluse.Core.Security.Authority
import Ecluse.Core.Security.Host
import Ecluse.Core.Security.IpLiteral (parseIpLiteral)
import Ecluse.Core.Security.Limits

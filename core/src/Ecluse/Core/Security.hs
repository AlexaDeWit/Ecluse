-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources: client-supplied
package identifiers (the request path), and upstream-supplied artifact locations (a
packument's @dist.tarball@). It then parses whatever an upstream returns. This module
is the pure guard layer that keeps hostile input from steering or exhausting those
steps. It defends three boundaries:

* Where the proxy fetches. 'isAllowedUpstreamHost' restricts outbound fetches to the
  configured upstream @host:port@ pairs. 'isBlockedTarget' rejects internal address
  ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
  position can otherwise reach. Together they are the SSRF gate: a target must be
  both on the allowlist /and/ not an internal address.

* How much an upstream may cost. A 'Limits' budget bounds the algorithmic-complexity
  DoS a hostile or compromised upstream can inflict. 'boundedRead' aborts a streamed
  body past 'maxBodyBytes', and 'checkVersionCount' \/ 'checkArtifactCount' \/
  'checkNestingDepth' reject an oversized or deeply-nested parsed document. Every limit
  fails closed: exceeding one yields 'Left', never a truncated or partial result.

The functions are pure and total. The streamed-body guard ('boundedRead') is
polymorphic over the producing monad. The streaming data plane runs it in 'IO' while
tests drive it purely. They are primitives: the fetch and serve layers compose them at
the boundary (see @docs\/architecture\/registry-model.md@ → "Registry Abstraction" and
@docs\/architecture\/web-layer.md@ → "Multi-ecosystem mounts"). The router's
"Ecluse.Core.Server.Route" shares path-component safety ('isSafeComponent'), and
records the threat model these guards answer.
-}
module Ecluse.Core.Security (
    module Ecluse.Core.Security.Host,
    module Ecluse.Core.Security.Limits,
    -- The host authority extractors ("Ecluse.Core.Security.Authority").
    module Ecluse.Core.Security.Authority,
    -- The IP-literal recogniser: only the surface consumers reach today. The
    -- 'IpAddr' constructors stay internal to the parser and the policy layer.
    parseIpLiteral,
) where

import Ecluse.Core.Security.Authority
import Ecluse.Core.Security.Host
import Ecluse.Core.Security.IpLiteral (parseIpLiteral)
import Ecluse.Core.Security.Limits

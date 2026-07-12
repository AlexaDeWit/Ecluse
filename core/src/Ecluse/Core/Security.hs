-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources -- __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@) -- and then parses whatever an upstream
returns. This module is the pure guard layer that keeps those steps from being
steered or exhausted by hostile input. It defends three boundaries:

* __Where the proxy fetches.__ 'isAllowedUpstreamHost' restricts outbound fetches
  to the configured upstream hosts, and 'isBlockedTarget' rejects internal address
  ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
  position can otherwise reach. Together they are the SSRF gate: a target must be
  both on the allowlist /and/ not an internal address.

* __How much an upstream may cost.__ A 'Limits' budget plus 'boundedRead' (abort a
  streamed body past 'maxBodyBytes') and 'checkVersionCount' \/ 'checkNestingDepth'
  (reject an oversized or deeply-nested parsed document) bound algorithmic-complexity
  DoS from a hostile or compromised upstream. Every limit __fails closed__: exceeding
  one yields 'Left', never a truncated or partial result.

The functions are pure and total; the streamed-body guard ('boundedRead') is
polymorphic over the producing monad so the streaming data plane can run it in
'IO' while tests drive it purely. They are __primitives__: the fetch and serve
layers compose them at the boundary (see @docs\/architecture\/registry-model.md@
→ "Registry Abstraction" and @docs\/architecture\/web-layer.md@ → "Multi-ecosystem
mounts"). Path-component safety is
shared with the router's "Ecluse.Core.Server.Route" ('isSafeComponent'); the threat
model these guards answer is recorded there too.
-}
module Ecluse.Core.Security (
    module Ecluse.Core.Security.Host,
    module Ecluse.Core.Security.Limits,
) where

import Ecluse.Core.Security.Host
import Ecluse.Core.Security.Limits

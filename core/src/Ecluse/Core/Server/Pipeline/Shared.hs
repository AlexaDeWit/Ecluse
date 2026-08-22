-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared utilities for the data-plane handler modules.

Edge authentication, the shed status with its @Retry-After@ hint, and the serve
rejections the integrity floor produces. The packument, tarball, and publish handlers all
use them.
-}
module Ecluse.Core.Server.Pipeline.Shared (
    edgeTokenMatches,
    forwardedCredential,
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
    hRetryAfter,
    shedStatus,
    shedRetryAfter,
) where

import Network.HTTP.Types (Header, HeaderName, Status, status503)
import Network.Wai (Request, requestHeaders)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Registry.Request (credentialRecover)
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Context (MountBinding (bindingCredential))
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    ServeDecision (Reject),
 )

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

{- | The status a brief-wait admission shed renders on the read and publish paths:
@503 Service Unavailable@, the server-capacity signal, not a @429@ rate limit. Every
shed site renders this one value, so no site re-spells the reason phrase.
-}
shedStatus :: Status
shedStatus = status503

{- | The @Retry-After@ header a shed 503 carries: whole seconds equal to the admission
wait budget ('admissionWaitMicros', divided by the microseconds in a second). A shed
client is never told to come back sooner than a queued request waits in-process. The
hint derives from the budget, so the two cannot drift.
-}
shedRetryAfter :: Header
shedRetryAfter = (hRetryAfter, show (admissionWaitMicros `div` 1_000_000))

{- | The shared edge gate against a configured inbound token. With none configured the
edge is open. With one configured the credential the client presented must match it
exactly, and the gate rejects a missing or mismatched credential (deny-by-default). The
match is constant-time. 'Secret' equality compares the full UTF-8 bytes without a
content-dependent early out, so this gate leaks no prefix length of the configured token
through timing.

The packument, tarball, and publish paths all apply the same gate, so it lives here
rather than once per route. It takes the __already-recovered__ credential
('forwardedCredential') rather than the request. A handler that also forwards the
credential upstream therefore scans the headers once and reuses that one recovery for
both.
-}
edgeTokenMatches :: Maybe Secret -> Maybe Secret -> Bool
edgeTokenMatches expected forwarded = case expected of
    Nothing -> True
    Just want -> forwarded == Just want

{- The credential a client of this mount presented, recovered through the mount's own
ecosystem presentation ('bindingCredential'). The result is the token text, or 'Nothing'
when the request carries no credential in that form. The edge gate compares it and a
passthrough read forwards it to the private upstream. A mount therefore accepts exactly
its ecosystem's presentation, and this pipeline names no scheme of its own. -}
forwardedCredential :: MountBinding -> Request -> Maybe Secret
forwardedCredential mount = credentialRecover (bindingCredential mount) . requestHeaders

{- A __public__ version the integrity-presence admission policy refuses. Its selected
artifact carries no integrity digest of any kind, so nothing ties it to a tamper-evident
fingerprint. A deliberate deny-by-default policy refusal ('MissingIntegrity', rendered
@403@), not a rule denial and not a retryable outage. The trusted (private) path uses
'trustedIntegrityMissing' instead, worded for its own context. -}
integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

{- A __public__ version the integrity-floor admission policy refuses. Its selected
artifact carries an integrity digest, but the strongest one is weaker than the configured
minimum algorithm. Nothing then ties its bytes to a collision-resistant fingerprint. A
deliberate deny-by-default policy refusal ('BelowIntegrityFloor', rendered @403@), kept
distinct from 'integrityMissing' so the audit trail says which. The trusted (private)
path uses 'trustedIntegrityBelowFloor' instead. -}
integrityBelowFloor :: ServeDecision
integrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this version's integrity digest is weaker than the configured minimum and cannot be served from a public upstream")

{- A __trusted__ (private) version the trusted integrity floor drops for carrying no
integrity digest at all. The same 'MissingIntegrity' @403@ as the public refusal, worded
for the private path. It surfaces only in the no-survivors body, when no version (private
or public) is admissible. -}
trustedIntegrityMissing :: ServeDecision
trustedIntegrityMissing =
    Reject (Rejection MissingIntegrity "this private version carries no integrity digest and was not served")

{- A __trusted__ (private) version the trusted integrity floor drops. Its strongest
digest is weaker than the configured trusted minimum, which an operator may loosen below
SHA-256. The same 'BelowIntegrityFloor' @403@ as the public refusal, worded for the
private path. -}
trustedIntegrityBelowFloor :: ServeDecision
trustedIntegrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this private version's integrity digest is weaker than the configured trusted minimum and was not served")

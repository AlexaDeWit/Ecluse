-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared utilities for the data-plane handler modules.

Common combinators used across the packument, tarball, and publish handlers: edge
authentication and the shared serve rejection values for integrity-floor enforcement.
-}
module Ecluse.Core.Server.Pipeline.Shared (
    edgeTokenMatches,
    forwardedToken,
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
    hRetryAfter,
    shedStatus,
    shedRetryAfter,
) where

import Data.Text qualified as T
import Network.HTTP.Types (Header, HeaderName, Status, hAuthorization, status503)
import Network.Wai (Request, requestHeaders)

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    ServeDecision (Reject),
 )

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

{- | The HTTP status a brief-wait admission shed renders across the read and publish
paths: @503 Service Unavailable@, the server-capacity signal (not a @429@ rate limit).
Shared so every shed site renders the identical status rather than re-spelling the
reason phrase.
-}
shedStatus :: Status
shedStatus = status503

{- | The @Retry-After@ header a shed 503 carries, in whole seconds equal to the admission
wait budget ('admissionWaitMicros', divided by the microseconds in a second): a shed
client is never told to come back sooner than the interval a queued request waits
in-process, and the two cannot drift because the hint is derived from the budget.
-}
shedRetryAfter :: Header
shedRetryAfter = (hRetryAfter, show (admissionWaitMicros `div` 1_000_000))

{- | The shared edge gate against a configured inbound token: with none configured the
edge is open; with one configured the request's forwarded bearer must match it exactly.
Deny-by-default: a missing or mismatched bearer is rejected. The match is constant-time:
'Secret' equality compares over the full UTF-8 bytes without a content-dependent early
out, so this gate does not leak the configured token's prefix length through timing.

The packument, tarball, and publish paths all apply the same gate, so it is factored
here rather than duplicated per route. It takes the __already-extracted__ bearer
('forwardedToken') rather than the request, so a handler that also forwards the
credential upstream scans the headers for it once and reuses the one extraction for
both.
-}
edgeTokenMatches :: Maybe Secret -> Maybe Secret -> Bool
edgeTokenMatches expected forwarded = case expected of
    Nothing -> True
    Just want -> forwarded == Just want

{- The client's forwarded bearer credential, recovered from the request's
@Authorization: Bearer …@ header. 'Nothing' when no bearer credential is present;
the recovered 'Secret' is what is forwarded to the private upstream and compared
against the edge token. The scheme name is matched case-insensitively (npm sends
@Bearer@), the token taken verbatim after it. -}
forwardedToken :: Request -> Maybe Secret
forwardedToken request = do
    (_, raw) <- find ((== hAuthorization) . fst) (requestHeaders request)
    let value = decodeUtf8 raw
        (scheme, rest) = T.break (== ' ') value
    guard (T.toLower scheme == "bearer")
    let token = T.dropWhile (== ' ') rest
    guard (not (T.null token))
    pure (mkSecret token)

{- A __public__ version refused by the integrity-presence admission policy: its selected
artifact carries no integrity digest of any kind, so it cannot be tied to a
tamper-evident fingerprint. A deliberate deny-by-default policy refusal ('MissingIntegrity',
rendered @403@), not a rule denial and not a retryable outage. The trusted (private) path
uses 'trustedIntegrityMissing' instead, worded for its own context. -}
integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

{- A __public__ version refused by the integrity-floor admission policy: its selected
artifact carries an integrity digest, but the strongest one is weaker than the configured
minimum algorithm, so its bytes cannot be tied to a collision-resistant fingerprint. A
deliberate deny-by-default policy refusal ('BelowIntegrityFloor', rendered @403@),
distinct from 'integrityMissing' so the audit trail says which. The trusted (private) path
uses 'trustedIntegrityBelowFloor' instead. -}
integrityBelowFloor :: ServeDecision
integrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this version's integrity digest is weaker than the configured minimum and cannot be served from a public upstream")

{- A __trusted__ (private) version dropped by the trusted integrity floor for carrying no
integrity digest at all. The same 'MissingIntegrity' @403@ as the public refusal, but
worded for the private path; it surfaces only in the no-survivors body when no version
(private or public) is admissible. -}
trustedIntegrityMissing :: ServeDecision
trustedIntegrityMissing =
    Reject (Rejection MissingIntegrity "this private version carries no integrity digest and was not served")

{- A __trusted__ (private) version dropped by the trusted integrity floor: its strongest
digest is weaker than the configured trusted minimum (which an operator may loosen below
SHA-256). The same 'BelowIntegrityFloor' @403@ as the public refusal, worded for the
private path. -}
trustedIntegrityBelowFloor :: ServeDecision
trustedIntegrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this private version's integrity digest is weaker than the configured trusted minimum and was not served")

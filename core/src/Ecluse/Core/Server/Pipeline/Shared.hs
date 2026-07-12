-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared utilities for the data-plane handler modules.

This module provides the common combinators and shared types used across the packument,
tarball, and publish handlers. It handles edge authentication checks, defines common
HTTP response rendering functions, and declares shared serve rejection values (e.g.,
for integrity floor enforcement).
-}
module Ecluse.Core.Server.Pipeline.Shared (
    recognisedButUnserved,
    notFoundInMount,
    edgeTokenMatches,
    edgeUnauthorised,
    serveOverloaded,
    forwardedToken,
    jsonResponse,
    renderedResponse,
    bodiless,
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
    hRetryAfter,
) where

import Data.Text qualified as T
import Network.HTTP.Types (HeaderName, ResponseHeaders, Status, hAuthorization, hContentType, status401, status404, status501, status503)
import Network.Wai (Request, Response, requestHeaders, responseHeaders, responseLBS, responseStatus)

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Server.Renderer (MountRenderer, RenderedBody (RenderedBody), renderError)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    ServeDecision (Reject),
 )

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

recognisedButUnserved :: MountRenderer -> Response
recognisedButUnserved renderer =
    renderedResponse status501 [] (renderError renderer Nothing "this route is recognised but not yet served by this proxy")

{- | The answer to a request no ecosystem route matched: a @404@ in the mount's own
error surface.

This is the routing layer's __deny by default__, mirroring the rules engine: the front
door serves nothing it was not explicitly taught to. It is the one locally-answered
response that is genuinely shared across ecosystems, because "I do not recognise this"
is the only thing every ecosystem's router must be able to say. Distinct from the
neutral @text\/plain@ @404@ __above__ the mounts, where there is no ecosystem to render
one.
-}
notFoundInMount :: MountRenderer -> Response
notFoundInMount renderer =
    renderedResponse status404 [] (renderError renderer Nothing "not found")

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

-- A @401@ for a request that failed edge authentication, before any upstream
-- fetch; the body is shaped by the mount's renderer.
edgeUnauthorised :: MountRenderer -> Response
edgeUnauthorised renderer =
    renderedResponse status401 [] (renderError renderer Nothing "authentication required")

{- | An admission refusal: the request found the waiting room full, or waited out
its slot budget ("Ecluse.Core.Server.Admission"). The body follows the matched
mount's error surface and the retry hint is deliberately short: capacity, unlike a
policy denial, can clear as soon as one in-flight metadata operation completes,
and a budget-expiry refusal has already waited one such interval in-process.
-}
serveOverloaded :: MountRenderer -> Response
serveOverloaded renderer =
    renderedResponse status503 [(hRetryAfter, "1")] (renderError renderer Nothing "server is busy; retry later")

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

-- A JSON response with the given status, extra headers, and body. Used for the
-- served packument document itself, which is npm JSON.
jsonResponse :: Status -> ResponseHeaders -> LByteString -> Response
jsonResponse status extra =
    responseLBS status ((hContentType, "application/json") : extra)

-- A response built from a renderer's 'RenderedBody': its content type, then any
-- extra headers, then the rendered bytes.
renderedResponse :: Status -> ResponseHeaders -> RenderedBody -> Response
renderedResponse status extra (RenderedBody contentType body) =
    responseLBS status ((hContentType, contentType) : extra) body

-- Strip a response's body while keeping its status and headers -- the bodiless form a
-- HEAD reply takes on every branch (HTTP semantics: a HEAD carries no message body).
-- The headers a GET would carry (notably any relayed @Content-Length@) are preserved.
bodiless :: Response -> Response
bodiless response = responseLBS (responseStatus response) (responseHeaders response) ""

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

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared authentication, admission shedding, flight keys, and refusal handling.
Route handlers keep their own response formats while sharing these policy decisions.
-}
module Ecluse.Core.Server.Pipeline.Shared (
    -- * Edge authentication
    edgeTokenMatches,
    forwardedCredential,
    unauthorisedMessage,
    privateAuthorisationRefusal,

    -- * Admission shed
    withMetadataAdmission,
    withListingAdmission,
    shedStatus,
    shedMessage,
    hRetryAfter,
    shedRetryAfter,
    retryAfterHeaders,
    upstreamCooling,

    -- * Shared work
    flightOf,

    -- * First-party refusals
    firstPartyRule,

    -- * Integrity-floor rejections
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
) where

import Network.HTTP.Types (Header, HeaderName, ResponseHeaders, Status, status503)
import Network.Wai (Request, requestHeaders)
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Credential (ClientCredential (credSecret), Secret)
import Ecluse.Core.Registry.Request (credentialRecover)
import Ecluse.Core.Server.Admission (withServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Cache.Types (CacheKey, cacheKeyIdentity, cacheKeyStore)
import Ecluse.Core.Server.Context (MountBinding (bindingCredential), ServeRuntime (srAdmission, srMemoryMeter, srMetrics))
import Ecluse.Core.Server.Response (
    HelpMessage,
    Refusal,
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    RetryAfter (RetryAfter),
    RuleName (RuleName),
    ServeDecision (Reject),
    Transience (WillResolve),
    mkRefusal,
    rejectUnavailable,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeDecision))

-- | Match the configured inbound secret without content-dependent early exit. An unconfigured edge is open.
edgeTokenMatches :: Maybe Secret -> Maybe ClientCredential -> Bool
edgeTokenMatches expected forwarded = case expected of
    Nothing -> True
    Just want -> fmap credSecret forwarded == Just want

-- | The client's credential, as the mount's credential mapping recovers it from the request headers.
forwardedCredential :: MountBinding -> Request -> Maybe ClientCredential
forwardedCredential mount = credentialRecover (bindingCredential mount) . requestHeaders

-- | Report edge authentication failure without disclosing either token.
unauthorisedMessage :: Text
unauthorisedMessage = "authentication required"

-- | The fixed refusal shared by private metadata and artifact access failures.
privateAuthorisationRefusal :: Maybe HelpMessage -> Refusal
privateAuthorisationRefusal help = mkRefusal help "the private upstream refused access, so public content cannot replace it"

-- | Acquire memory and CPU admission before metadata work. Answer after release, counting a shed once.
withMetadataAdmission :: (MonadUnliftIO m) => ServeRuntime -> m received -> (MemoryTicket -> m a) -> (a -> m received) -> m received
withMetadataAdmission runtime shed gated answer =
    metadataAdmission runtime gated pure >>= maybe (shedCounted runtime shed) answer

{- | As 'withMetadataAdmission', but write the response after the CPU slot is released and before the
memory ticket returns its charge, so the charge covers the body until it is written.
-}
withListingAdmission :: (MonadUnliftIO m) => ServeRuntime -> m received -> (MemoryTicket -> m response) -> (response -> m received) -> m received
withListingAdmission runtime shed gated write =
    metadataAdmission runtime gated write >>= maybe (shedCounted runtime shed) pure

-- The memory entry spans the gated work and @held@, the CPU slot the gated work alone. 'Nothing' is a shed.
metadataAdmission :: (MonadUnliftIO m) => ServeRuntime -> (MemoryTicket -> m a) -> (a -> m b) -> m (Maybe b)
metadataAdmission runtime gated held =
    fmap join . withMemoryEntry metrics (srMemoryMeter runtime) $ \ticket ->
        withServeAdmission metrics (srAdmission runtime) (gated ticket) >>= traverse held
  where
    metrics = srMetrics runtime

shedCounted :: (MonadIO m) => ServeRuntime -> m received -> m received
shedCounted runtime shed = liftIO (mpServeDecision (srMetrics runtime) Metric.Unavailable) >> shed

-- | Use 503 to report server admission capacity, without implying a client rate limit.
shedStatus :: Status
shedStatus = status503

-- | The body every read-path shed answers with, so the three handlers say one thing.
shedMessage :: Text
shedMessage = "server is busy; retry later"

-- | The @Retry-After@ header name.
hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

-- | The shed retry delay, rounded down to whole seconds from 'admissionWaitMicros'.
shedRetryAfter :: Header
shedRetryAfter = (hRetryAfter, show (admissionWaitMicros `div` 1_000_000))

-- | Emit Retry-After only when a decision supplies a delay.
retryAfterHeaders :: Maybe RetryAfter -> ResponseHeaders
retryAfterHeaders = maybe [] (\(RetryAfter secs) -> [(hRetryAfter, show secs)])

-- | A public authority that supplies a positive retry delay.
upstreamCooling :: RetryAfter -> ServeDecision
upstreamCooling retry = rejectUnavailable (WillResolve (Just retry)) "the public upstream is cooling down"

-- | The flight that the shared fetch or render behind a cache key runs under: the key's store and identity.
flightOf :: CacheKey -> FlightKey
flightOf key = FlightKey (cacheKeyStore key) (cacheKeyIdentity key)

-- | Use one rule label for first-party refusals across read paths.
firstPartyRule :: RuleName
firstPartyRule = RuleName "first-party"

-- | A public version with no integrity digest.
integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

-- | A public version whose digest is weaker than the configured minimum.
integrityBelowFloor :: ServeDecision
integrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this version's integrity digest is weaker than the configured minimum and cannot be served from a public upstream")

-- | A private version with no integrity digest.
trustedIntegrityMissing :: ServeDecision
trustedIntegrityMissing =
    Reject (Rejection MissingIntegrity "this private version carries no integrity digest and was not served")

-- | A private version whose digest is weaker than the configured trusted minimum.
trustedIntegrityBelowFloor :: ServeDecision
trustedIntegrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this private version's integrity digest is weaker than the configured trusted minimum and was not served")

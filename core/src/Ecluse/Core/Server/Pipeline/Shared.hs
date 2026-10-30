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

    -- * Private read refusals
    privateReplyInvalid,

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
import UnliftIO (MonadUnliftIO, withRunInIO)

import Ecluse.Core.Credential (ClientCredential (credSecret), Secret)
import Ecluse.Core.Registry.Request (credentialRecover)
import Ecluse.Core.Server.Admission (serveAdmissionLine, serveAdmissionOverflow, withServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, meterLine, tryMemoryEntry)
import Ecluse.Core.Server.Admission.Overflow (Attempt (..), Waited (..), attemptAfter, awaitPlace)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Cache.Types (CacheKey, cacheKeyIdentity, cacheKeyStore)
import Ecluse.Core.Server.Context (MountBinding (bindingCredential), ServeRuntime (srAdmission, srMemoryMeter, srMetrics))
import Ecluse.Core.Server.Response (
    HelpMessage,
    Refusal,
    RejectReason (BelowIntegrityFloor, MissingIntegrity, UpstreamInvalid),
    Rejection (Rejection),
    RetryAfter (RetryAfter),
    RuleName (RuleName),
    ServeDecision (Reject),
    Transience (WillResolve),
    mkRefusal,
    rejectUnavailable,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpMemoryAdmissionShed, mpServeDecision))

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

-- | Acquire memory and CPU admission before metadata work. Answer after both release.
withMetadataAdmission :: (MonadUnliftIO m) => ServeRuntime -> (ResponseHeaders -> m received) -> (MemoryTicket -> m a) -> (a -> m received) -> m received
withMetadataAdmission runtime shed gated =
    metadataAdmission runtime shed gated pure

-- | Write after CPU admission releases, while the memory ticket still covers the response body.
withListingAdmission :: (MonadUnliftIO m) => ServeRuntime -> (ResponseHeaders -> m received) -> (MemoryTicket -> m response) -> (response -> m received) -> m received
withListingAdmission runtime shed gated write =
    metadataAdmission runtime shed gated write pure

metadataAdmission :: (MonadUnliftIO m) => ServeRuntime -> (ResponseHeaders -> m received) -> (MemoryTicket -> m a) -> (a -> m b) -> (b -> m received) -> m received
metadataAdmission runtime shed gated held answer =
    attempt >>= \case
        Entered result -> answer result
        Expired line -> refuse line [shedRetryAfter]
        TurnedAway line -> case serveAdmissionOverflow admission of
            Nothing -> refuse line [shedRetryAfter]
            Just overflow ->
                withRunInIO (\run -> awaitPlace metrics overflow line (run attempt)) >>= \case
                    Placed result -> answer result
                    WaitedOut refusedAt -> refuse refusedAt [shedRetryAfter]
                    QueueFull refusedAt -> refuse refusedAt [shedRetryAfter]
  where
    metrics = srMetrics runtime
    admission = srAdmission runtime
    meter = srMemoryMeter runtime
    refuse line headers = do
        liftIO $ do
            when (line == meterLine meter) (mpMemoryAdmissionShed metrics)
            mpServeDecision metrics Metric.Unavailable
        shed headers
    attempt =
        either (attemptAfter (meterLine meter) . Left) (attemptAfter (serveAdmissionLine admission))
            <$> tryMemoryEntry metrics meter (\ticket -> withServeAdmission metrics admission (gated ticket) >>= traverse held)

-- | Use 503 to report server admission capacity, without implying a client rate limit.
shedStatus :: Status
shedStatus = status503

-- | The body every read-path shed answers with, so the three handlers say one thing.
shedMessage :: Text
shedMessage = "server is busy; retry later"

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

-- | The @502@ refusal for a private reply that was invalid or unusable, on listings and artifacts alike.
privateReplyInvalid :: ServeDecision
privateReplyInvalid = Reject (Rejection UpstreamInvalid "the private upstream returned an invalid or unusable reply")

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

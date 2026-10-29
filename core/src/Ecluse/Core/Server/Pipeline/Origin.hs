-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolve caller-authorised private metadata and anonymous public metadata.
Access refusals, absent packages and unread origins retain distinct outcomes.
-}
module Ecluse.Core.Server.Pipeline.Origin (
    -- * The per-origin outcome
    OriginResult (..),
    OriginMiss (..),
    ObservedReadBoundExceeded (..),
    originManifest,
    originMiss,

    -- * Fetching the two origins
    fetchPrivateOriginObserved,
    fetchPublicOriginObserved,
    withPublicMetadata,
    withPrivateMetadataClient,

    -- * One origin's coordinates
    mountOrigin,
) where

import Katip (Severity (DebugS), logFM, ls)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Fault (RetryAfter)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchCooldown))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataRead))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Metadata (
    Manifest (manifestBodyBytes),
    MetadataClient (fetchFullManifest),
    MetadataError (
        MetadataAbsent,
        MetadataAuthorisationFailure,
        MetadataBoundExceeded,
        MetadataFetch,
        MetadataHttpFailure,
        MetadataNameMismatch,
        MetadataUndecodable
    ),
    VersionDoc (..),
    VersionRead (..),
 )
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (canChargeFullReadWorkspace)
import Ecluse.Core.Registry.Origin (OriginClient (ocClient), OriginFor, Private, Public, anonymousOrigin, chargingFullReadWorkspace, chargingFullReads, chargingNativeReads, controlPublicOrigin, originBaseUrl, originClientOf, perCallerOrigin)
import Ecluse.Core.Registry.Request (BaseRequest)
import Ecluse.Core.Security (LimitError, Limits (progressFloor))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (ChargePart, MemoryTicket, awaitingFlight, charge, chargePart, newChargeRegion, servingFlight, servingPart, stepDown)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille))
import Ecluse.Core.Server.Cache.Store (executePrepared)
import Ecluse.Core.Server.Cache.Types (Source (Source), fullKey, versionKey)
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ServeRuntime (..),
    pdPrivateBase,
    pdPublicBase,
    pdPublicBaseUrl,
 )
import Ecluse.Core.Server.Metadata (MetadataReads, ecosystemMetadataReads, preparePublicVersion, privateMetadataClient, publicMetadataClient, withinRequestCap)
import Ecluse.Core.Server.Pipeline.Diagnostics (logInvalidEntries, logMetadataFailure)
import Ecluse.Core.Server.Pipeline.Shared (flightOf)
import Ecluse.Core.Version (Version)

-- | A request-wide observed read limit, distinct from a transport failure.
newtype ObservedReadBoundExceeded = ObservedReadBoundExceeded LimitError
    deriving stock (Show)

instance Exception ObservedReadBoundExceeded

-- | One origin's contribution, access refusal, identity mismatch, or absence.
data OriginResult
    = OriginBudgetExceeded LimitError
    | -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved Manifest
    | -- | An explicit upstream access refusal, retaining its 401 or 403.
      OriginAuthorisationFailure Int
    | -- | An invalid package identity, contributing a 502 when no valid origin remains.
      OriginNameMismatch
    | -- | The origin answered 404, so it holds no such package. It degrades to no contribution.
      OriginNotFound
    | -- | The origin was not read: unreachable, faulting, or undecodable. It degrades to no contribution.
      OriginUnresolved
    | -- | Public reads are suspended until the authority cooldown expires.
      OriginCooling RetryAfter
    | -- | An unconfigured origin contributes neither metadata nor an availability failure.
      OriginAbsent

-- | Distinguish a settled absence from an unread origin that a retry may resolve.
data OriginMiss
    = -- | The origin holds no such package, or the mount configures no such origin.
      MissAbsent
    | -- | The origin was not read, so nothing yet says whether it holds the package.
      MissUnresolved
    deriving stock (Eq, Show)

-- | The resolved manifest an origin contributed, if any.
originManifest :: OriginResult -> Maybe Manifest
originManifest = \case
    OriginBudgetExceeded _ -> Nothing
    OriginAuthorisationFailure _ -> Nothing
    OriginResolved manifest -> Just manifest
    OriginNameMismatch -> Nothing
    OriginNotFound -> Nothing
    OriginUnresolved -> Nothing
    OriginCooling _ -> Nothing
    OriginAbsent -> Nothing

-- | The miss an origin yielded, or 'Nothing' when it contributed a document or an explicit refusal.
originMiss :: OriginResult -> Maybe OriginMiss
originMiss = \case
    OriginBudgetExceeded _ -> Just MissUnresolved
    OriginAuthorisationFailure _ -> Nothing
    OriginResolved{} -> Nothing
    OriginNameMismatch -> Nothing
    OriginNotFound -> Just MissAbsent
    OriginUnresolved -> Just MissUnresolved
    OriginCooling _ -> Just MissUnresolved
    OriginAbsent -> Just MissAbsent

originResultOf :: Either SomeException (Either MetadataError Manifest) -> OriginResult
originResultOf = \case
    Left fault | Just (ObservedReadBoundExceeded bound) <- fromException fault -> OriginBudgetExceeded bound
    Left _ -> OriginUnresolved
    Right (Left (MetadataAuthorisationFailure code)) -> OriginAuthorisationFailure code
    Right (Left (MetadataNameMismatch _)) -> OriginNameMismatch
    Right (Left MetadataAbsent) -> OriginNotFound
    Right (Left (MetadataHttpFailure _)) -> OriginUnresolved
    Right (Left MetadataUndecodable) -> OriginUnresolved
    Right (Left (MetadataBoundExceeded _)) -> OriginUnresolved
    Right (Left (MetadataFetch (FetchCooldown retry))) -> OriginCooling retry
    Right (Left (MetadataFetch _)) -> OriginUnresolved
    Right (Right manifest) -> OriginResolved manifest

-- | Read caller-authorised private metadata without caching and observe every incoming full-read chunk.
fetchPrivateOriginObserved :: (Int -> IO ()) -> PackumentDeps -> ServeRuntime -> ChargePart -> Maybe ClientCredential -> PackageName -> Handler OriginResult
fetchPrivateOriginObserved observe deps rt part token name = case pdPrivateBase deps of
    Nothing -> pure OriginAbsent
    Just privateBase -> do
        logFM DebugS (ls ("fetching private origin for " <> renderPackageName name))
        origin <- liftIO (fullReadOrigin observe deps part (privateOrigin rt deps privateBase token))
        keepingRead deps part . originResultOf =<< tryAny (withMetadataClient rt deps privateMetadataClient origin (`fetchFullManifest` name))

-- | Cache anonymous public metadata and observe shared-read leader chunks. Cache consumers retain their own document accounting.
fetchPublicOriginObserved :: (Int -> IO ()) -> PackumentDeps -> ServeRuntime -> MemoryTicket -> ChargePart -> PackageName -> Handler OriginResult
fetchPublicOriginObserved observe deps rt ticket part name = do
    logFM DebugS (ls ("fetching public origin for " <> renderPackageName name))
    -- Every request that shares this read waits on it, so whichever request leads it pays with their priority.
    let flight = flightOf (fullKey (publicSource deps) name)
        sharedPart = servingPart flight part
    origin <- liftIO (fullReadOrigin observe deps sharedPart (publicOrigin rt deps))
    keepingRead deps part . originResultOf =<< tryAny (awaitingFlight ticket flight (withMetadataClient rt deps (publicMetadataClient (srMetadataCache rt) (publicSource deps)) origin (`fetchFullManifest` name)))

-- Once its result is evaluated a read holds only its manifest, so its part steps down to the kept charge.
keepingRead :: PackumentDeps -> ChargePart -> OriginResult -> Handler OriginResult
keepingRead deps part result = do
    settled <- evaluateWHNF result
    liftIO (stepDown part (keptCharge deps settled))
    pure settled

keptCharge :: PackumentDeps -> OriginResult -> Int
keptCharge deps = \case
    OriginResolved manifest -> scaleCharge (cfKeptPermille (chargeFactors deps)) (manifestBodyBytes manifest)
    _ -> 0

-- Origin logs use the request's logging context and its selected upstream.
withMetadataClient ::
    ServeRuntime ->
    PackumentDeps ->
    (MetadataReads posture -> client) ->
    OriginFor posture ->
    (client -> IO a) ->
    Handler a
withMetadataClient rt deps settle origin k =
    withRunInIO $ \runInIO ->
        k . settle . withinRequestCap (ecDeadlines (ocClient (originClientOf origin))) (progressFloor (pdLimits deps)) $
            ecosystemMetadataReads
                (metadataRead (pdMetadata deps))
                (srTracing rt)
                (srMetrics rt)
                (\nm err -> runInIO (logMetadataFailure nm baseUrl err))
                (\nm entries -> runInIO (logInvalidEntries nm baseUrl entries))
                (\nm -> runInIO (logFM DebugS (ls ("fetching packument from origin for " <> renderPackageName nm))))
                origin
  where
    baseUrl = originBaseUrl (originClientOf origin)

fullReadOrigin :: (Int -> IO ()) -> PackumentDeps -> ChargePart -> OriginFor posture -> IO (OriginFor posture)
fullReadOrigin observe deps part origin
    | canChargeFullReadWorkspace (metadataRead (pdMetadata deps)) = do
        workspace <- newChargeRegion (chargePart part)
        pure (chargingFullReadWorkspace workspace charged)
    | otherwise = pure charged
  where
    charged = chargingFullReads (\bytes -> observe bytes >> fullReadCharge deps part bytes) origin

fullReadCharge :: PackumentDeps -> ChargePart -> Int -> IO ()
fullReadCharge deps part = chargePart part . scaleCharge (cfFullReadPermille (chargeFactors deps))

chargeFactors :: PackumentDeps -> ChargeFactors
chargeFactors = metadataChargeFactors . pdMetadata

{- | Bypass shared caching so the private upstream authorises each caller's credential.
Static lookups can omit the ticket. Native workspace admission requires one.
-}
withPrivateMetadataClient :: ServeRuntime -> PackumentDeps -> Maybe MemoryTicket -> BaseRequest -> Maybe ClientCredential -> (MetadataClient -> IO a) -> Handler a
withPrivateMetadataClient rt deps ticket base token action = do
    let selected = privateOrigin rt deps base token
    origin <- liftIO (maybe (pure selected) (\held -> newChargeRegion (charge held) >>= \region -> chargingNativeReads (chargeFactors deps) region selected) ticket)
    withMetadataClient rt deps privateMetadataClient origin action

-- | Reserve each consumer's selected facts. Serving consumers never retain the cached source row.
withPublicMetadata :: ServeRuntime -> PackumentDeps -> MemoryTicket -> PackageName -> Version -> (IO (Either MetadataError VersionRead) -> Handler a) -> Handler a
withPublicMetadata rt deps ticket name version action = do
    let flight = flightOf (versionKey (publicSource deps) name version)
    workspace <- liftIO (newChargeRegion (charge (servingFlight flight ticket)))
    origin <- liftIO (chargingNativeReads (chargeFactors deps) workspace (publicOrigin rt deps))
    retained <- liftIO (newChargeRegion (charge ticket))
    readSelected <- withMetadataClient rt deps (preparePublicVersion (srMetadataCache rt) (publicSource deps)) origin $ \prepare ->
        pure (awaitingFlight ticket flight (prepare name version >>= executePrepared))
    action $
        readSelected
            >>= traverse
                ( \result -> do
                    let !served = servingVersion result
                    retained (weighVersion served)
                    pure served
                )

servingVersion :: VersionRead -> VersionRead
servingVersion result = case vrVersion result of
    Nothing -> result
    Just document ->
        let !details = vdDetails document
            !selected = VersionDoc details Nothing
         in VersionRead (Just selected) (vrBodyBytes result) (vrUpstreamLatest result)

privateOrigin :: ServeRuntime -> PackumentDeps -> BaseRequest -> Maybe ClientCredential -> OriginFor Private
privateOrigin rt deps = perCallerOrigin (pdLimits deps) (srPrivateClient rt)

publicOrigin :: ServeRuntime -> PackumentDeps -> OriginFor Public
publicOrigin rt deps = controlPublicOrigin (srPublicControl rt) Nothing (anonymousOrigin (pdLimits deps) (srPublicClient rt) (pdPublicBase deps))

publicSource :: PackumentDeps -> Source
publicSource deps = Source (registryUrlText (pdPublicBaseUrl deps))

-- | Build an origin with the mount's response bound and the caller-selected transport and credential.
mountOrigin :: PackumentDeps -> ExchangeClient -> BaseRequest -> Maybe ClientCredential -> OriginClient
mountOrigin deps client base = originClientOf . perCallerOrigin (pdLimits deps) client base

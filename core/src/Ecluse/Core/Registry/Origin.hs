-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RoleAnnotations #-}

-- | Origin coordinates, limits, and credential posture for registry data-plane reads.
module Ecluse.Core.Registry.Origin (
    OriginClient (..),
    originClient,
    ocBaseUrl,
    originBaseUrl,

    -- * Credential posture, carried in the type
    OriginFor,
    Public,
    Private,
    anonymousOrigin,
    perCallerOrigin,
    originClientOf,
    chargingFullReads,
    chargingFullReadWorkspace,
    chargingSelectedMetadata,
    chargingNativeReads,
    controlPublicOrigin,
) where

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Public (PublicControl)
import Ecluse.Core.Registry.Request (BaseRequest, baseRequestUrl, mkBaseRequest)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind)

-- | One origin's coordinates, credential posture, and response bound.
data OriginClient = OriginClient
    { ocBaseRequest :: BaseRequest
    -- ^ The https-only egress witness the proxy appends a package path to, parsed once.
    , ocClient :: ExchangeClient
    -- ^ The shared connection pool and deadline sweeper to issue requests through.
    , ocToken :: Maybe ClientCredential
    -- ^ 'Nothing' for an anonymous origin. A passthrough read carries the caller's own verbatim.
    , ocLimits :: Limits
    -- ^ The bound every read through this origin is held to, fail-closed past the maximum.
    , ocPublicControl :: Maybe PublicControl
    , ocReadKind :: Maybe PublicReadKind
    , ocChargeSelectedRead :: Int -> IO ()
    -- ^ Reserve the native selected reader's absolute workspace before allocation.
    , ocChargeFullReadWorkspace :: Int -> IO ()
    -- ^ Reserve the full reader's absolute row workspace separately from retained source bytes.
    , ocChargeFullRead :: Int -> IO ()
    -- ^ Pays for each decompressed chunk of a full metadata read before the parser sees it.
    }

-- | Build an origin with caller-supplied limits, transport, and credential posture.
originClient :: Limits -> ExchangeClient -> RegistryUrl -> Maybe ClientCredential -> OriginClient
originClient limits client = originAt limits client . mkBaseRequest

originAt :: Limits -> ExchangeClient -> BaseRequest -> Maybe ClientCredential -> OriginClient
originAt limits client base token =
    OriginClient{ocBaseRequest = base, ocClient = client, ocToken = token, ocLimits = limits, ocChargeFullRead = const pass, ocChargeFullReadWorkspace = const pass, ocChargeSelectedRead = const pass, ocPublicControl = Nothing, ocReadKind = Nothing}

-- | The origin's base URL, read from its one parse so the two cannot disagree.
ocBaseUrl :: OriginClient -> RegistryUrl
ocBaseUrl = baseRequestUrl . ocBaseRequest

-- | The origin's base URL as text.
originBaseUrl :: OriginClient -> Text
originBaseUrl = registryUrlText . ocBaseUrl

-- | Retain credential posture in the type. The nominal role prevents retagging with 'coerce'.
newtype OriginFor (posture :: Type) = OriginFor OriginClient

-- RoleAnnotations is not in GHC2021. Without this line the parameter takes GHC's phantom role,
-- and coerce changes it from any module, constructor in scope or not.
type role OriginFor nominal

-- The two postures. Neither type is inhabited: each names a posture in a type, never a value.
data Public
data Private

-- | An origin dialled with no credential, so no caller's authorisation scopes what it reads.
anonymousOrigin :: Limits -> ExchangeClient -> BaseRequest -> OriginFor Public
anonymousOrigin limits client base = OriginFor (originAt limits client base Nothing)

-- | An origin presenting one caller's credential, so what it reads is scoped to that caller.
perCallerOrigin :: Limits -> ExchangeClient -> BaseRequest -> Maybe ClientCredential -> OriginFor Private
perCallerOrigin limits client base token = OriginFor (originAt limits client base token)

-- | The plain record behind a tagged origin, for the operations that take any origin.
originClientOf :: OriginFor posture -> OriginClient
originClientOf (OriginFor client) = client

-- | The same origin, paying for what its full metadata reads hand on.
chargingFullReads :: (Int -> IO ()) -> OriginFor posture -> OriginFor posture
chargingFullReads charge (OriginFor client) = OriginFor client{ocChargeFullRead = charge}

-- | Bind a full reader's absolute workspace without replacing its incremental source charge.
chargingFullReadWorkspace :: (Int -> IO ()) -> OriginFor posture -> OriginFor posture
chargingFullReadWorkspace pay (OriginFor client) = OriginFor client{ocChargeFullReadWorkspace = pay}

-- | Bind an absolute workspace reservation for native selected metadata reads.
chargingSelectedMetadata :: (Int -> IO ()) -> OriginFor posture -> OriginFor posture
chargingSelectedMetadata pay (OriginFor client) = OriginFor client{ocChargeSelectedRead = pay}

-- | Pay a native read's full-read chunks at the full-read factor and its selected workspace at face value.
chargingNativeReads :: ChargeFactors -> (Int -> IO ()) -> OriginFor posture -> OriginFor posture
chargingNativeReads factors pay = chargingSelectedMetadata pay . chargingFullReads (pay . scaleCharge (cfFullReadPermille factors))

-- | Control anonymous public reads, optionally assigning one kind to both metadata operations.
controlPublicOrigin :: PublicControl -> Maybe PublicReadKind -> OriginFor Public -> OriginFor Public
controlPublicOrigin control kind (OriginFor client) = OriginFor client{ocPublicControl = Just control, ocReadKind = kind}

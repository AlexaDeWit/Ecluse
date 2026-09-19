-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RoleAnnotations #-}

{- | Everything a registry data plane needs to reach one origin: where it is, what to dial it
through, what to present, and what response bound to hold it to. The composition root and the
serve pipeline are the only builders, and nothing here is derived or cached.
-}
module Ecluse.Core.Registry.Origin (
    OriginClient (..),
    originClient,
    originBaseUrl,

    -- * Credential posture, carried in the type
    OriginFor,
    Public,
    Private,
    anonymousOrigin,
    perCallerOrigin,
    originClientOf,
) where

import Network.HTTP.Client (Manager)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

-- | One origin's coordinates, credential posture, and response bound.
data OriginClient = OriginClient
    { ocBaseUrl :: RegistryUrl
    -- ^ The https-only egress witness the proxy appends a package path to.
    , ocManager :: Manager
    -- ^ The shared @http-client@ 'Manager' to issue requests through.
    , ocToken :: Maybe ClientCredential
    -- ^ 'Nothing' for an anonymous origin. A passthrough read carries the caller's own verbatim.
    , ocLimits :: Limits
    -- ^ The bound every read through this origin is held to, fail-closed past the maximum.
    }

{- | One origin from the four things that name it. The bound comes first because a caller
usually holds one and reaches several origins under it.
-}
originClient :: Limits -> Manager -> RegistryUrl -> Maybe ClientCredential -> OriginClient
originClient limits manager baseUrl token =
    OriginClient{ocBaseUrl = baseUrl, ocManager = manager, ocToken = token, ocLimits = limits}

-- | The origin's base URL as text, which is how every request builder takes it.
originBaseUrl :: OriginClient -> Text
originBaseUrl = registryUrlText . ocBaseUrl

{- | An 'OriginClient' whose credential posture its builder fixed. The constructor stays here and
the role annotation below stops 'coerce' retagging one.
-}
newtype OriginFor (posture :: Type) = OriginFor OriginClient

-- RoleAnnotations is not in GHC2021. Without this line the parameter takes GHC's phantom role,
-- and coerce changes it from any module, constructor in scope or not.
type role OriginFor nominal

-- The two postures. Neither type is inhabited: each names a posture in a type, never a value.
data Public
data Private

-- | An origin dialled with no credential, so no caller's authorisation scopes what it reads.
anonymousOrigin :: Limits -> Manager -> RegistryUrl -> OriginFor Public
anonymousOrigin limits manager baseUrl = OriginFor (originClient limits manager baseUrl Nothing)

-- | An origin presenting one caller's credential, so what it reads is scoped to that caller.
perCallerOrigin :: Limits -> Manager -> RegistryUrl -> Maybe ClientCredential -> OriginFor Private
perCallerOrigin limits manager baseUrl token = OriginFor (originClient limits manager baseUrl token)

-- | The plain record behind a tagged origin, for the operations that take any origin.
originClientOf :: OriginFor posture -> OriginClient
originClientOf (OriginFor client) = client

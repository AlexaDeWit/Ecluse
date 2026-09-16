-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Everything a registry data plane needs to reach __one origin__: where it is, what to
dial it through, what to present, and what response bound to hold it to.

Ecosystem-agnostic on purpose. Every adapter's read and relay operations take one of these
rather than four positional arguments, so a second ecosystem's adapter supplies the same
record and no npm name reaches the agnostic layer. The composition root and the serve
pipeline are the only builders: nothing here is derived, and nothing is cached.
-}
module Ecluse.Core.Registry.Origin (
    OriginClient (..),
    originClient,

    -- * Credential posture, carried in the type
    OriginFor,
    Public,
    Private,
    anonymousOrigin,
    perCallerOrigin,
    originClientOf,
) where

import Data.Kind (Type)
import Network.HTTP.Client (Manager)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (RegistryUrl)

-- | One origin's coordinates, credential posture, and response bound.
data OriginClient = OriginClient
    { ocBaseUrl :: RegistryUrl
    {- ^ The origin's base URL as the https-only egress witness
    ('Ecluse.Core.Security.Egress.RegistryUrl'). The proxy appends the package path to it.
    -}
    , ocManager :: Manager
    -- ^ The shared @http-client@ 'Manager' to issue requests through.
    , ocToken :: Maybe ClientCredential
    {- ^ The credential to present on a request through this origin, or 'Nothing' for an
    anonymous one. A passthrough read carries the caller's own pair verbatim.
    -}
    , ocLimits :: Limits
    {- ^ The response-bound budget every read through this origin is held to, fail-closed
    past 'Ecluse.Core.Security.maxBodyBytes'.
    -}
    }

{- | One origin from the four things that name it. Its builders take the bound first, because a
caller usually holds one bound and reaches several origins under it.
-}
originClient :: Limits -> Manager -> RegistryUrl -> Maybe ClientCredential -> OriginClient
originClient limits manager baseUrl token =
    OriginClient{ocBaseUrl = baseUrl, ocManager = manager, ocToken = token, ocLimits = limits}

{- | An 'OriginClient' whose credential posture its builder fixed. The parameter is a phantom, and
the constructor stays here, so no caller outside this module can retag an origin.
-}
newtype OriginFor (posture :: Type) = OriginFor OriginClient

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

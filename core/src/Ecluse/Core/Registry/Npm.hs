-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm read and relay data plane over @http-client@: the bounded metadata fetch and the
first-party publish relay. "Ecluse.Core.Registry.Npm.Wire" and
"Ecluse.Core.Registry.Npm.Project" are the pure decode beside it, and the mirror write's codec
is "Ecluse.Core.Registry.Npm.Publish".

Every request carries the credential the origin was built with, or none. Which credential an
origin holds is settled upstream, so nothing here originates credential policy.
-}
module Ecluse.Core.Registry.Npm (
    -- * Bounded metadata fetch
    fetchMetadataFormBounded,

    -- * First-party publish relay
    relayPublishDocument,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    PublishRelayResponse,
    RegistryResponse,
 )

import Ecluse.Core.Registry.Exchange (boundedFetch, boundedRelay, formThen)
import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Npm.Request (MetadataForm, metadataRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), originBaseUrl)

{- | Fetch a package's metadata in the requested form.
The body read is bounded fail-closed, and every failure is a 'FetchFault' value, never an exception.
-}
fetchMetadataFormBounded ::
    OriginClient ->
    MetadataForm ->
    PackageName ->
    IO (Either FetchFault RegistryResponse)
fetchMetadataFormBounded origin form name =
    formThen
        FetchUrlUnformable
        (boundedFetch (ocManager origin) (ocLimits origin))
        (metadataRequest (originBaseUrl origin) (ocToken origin) form name)

-- | Relay a client's npm publish document to the publication target and return its own response.
relayPublishDocument ::
    OriginClient ->
    PackageName ->
    ByteString ->
    IO (Either FetchFault PublishRelayResponse)
relayPublishDocument origin name document =
    formThen
        FetchUrlUnformable
        (boundedRelay (ocManager origin) (ocLimits origin))
        (publishRequest (originBaseUrl origin) (ocToken origin) name document)

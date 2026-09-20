-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The first-party npm publish relay over the shared bounded transport.
module Ecluse.Core.Registry.Npm (
    -- * First-party publish relay
    relayPublishDocument,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    PublishRelayResponse,
 )

import Ecluse.Core.Registry.Exchange (boundedRelay, formThen)
import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), originBaseUrl)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), maxMetadataBytes)

-- | Relay a client's npm publish document to the publication target and return its own response.
relayPublishDocument ::
    OriginClient ->
    PackageName ->
    ByteString ->
    IO (Either FetchFault PublishRelayResponse)
relayPublishDocument origin name document =
    formThen
        FetchUrlUnformable
        (boundedRelay (ocManager origin) (MetadataBodyLimit (maxMetadataBytes (ocLimits origin))))
        (publishRequest (originBaseUrl origin) (ocToken origin) name document)

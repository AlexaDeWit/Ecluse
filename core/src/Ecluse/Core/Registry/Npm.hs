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
    FetchFault,
    PublishRelayResponse,
 )

import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken), originBaseUrl)
import Ecluse.Core.Registry.Publish (relayPublishRequest)

-- | Relay a client's npm publish document to the publication target and return its own response.
relayPublishDocument ::
    OriginClient ->
    PackageName ->
    ByteString ->
    IO (Either FetchFault PublishRelayResponse)
relayPublishDocument origin name document =
    relayPublishRequest
        origin
        (publishRequest (originBaseUrl origin) (ocToken origin) name document)

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Artifact requests and route replies.
Only the private leg receives the caller's credential.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Types (
    -- * The route's replies
    TarballReplies (..),
    relayResponder,

    -- * One artifact request
    ArtifactRequest (..),
) where

import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status)
import Network.Wai (ResponseReceived, StreamingBody)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Server.Context (PackumentDeps, ServeRuntime)
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe)
import Ecluse.Core.Server.Response (Refusal)
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder))
import Ecluse.Core.Version (Version)

-- | The route-owned ways the tarball pipeline may answer.
data TarballReplies response = TarballReplies
    { tarballError :: Status -> ResponseHeaders -> Refusal -> response
    -- ^ An ecosystem-shaped local error.
    , tarballPrivateRefused :: Int -> ResponseHeaders -> Refusal -> response
    -- ^ The native protocol selects the status and authentication challenge.
    , tarballStream :: Status -> ResponseHeaders -> StreamingBody -> response
    -- ^ A transparent streamed upstream response.
    , tarballEmpty :: Status -> ResponseHeaders -> response
    -- ^ A transparent bodiless upstream response (@304@ or @HEAD@).
    }

-- | Keep the upstream connection open until the selected route response completes.
relayResponder :: TarballReplies response -> (response -> IO received) -> RelayResponder received
relayResponder replies respond =
    RelayResponder
        (\status headers body -> respond (tarballStream replies status headers body))
        (\status headers -> respond (tarballEmpty replies status headers))

-- | One artifact request, as every stage of both legs needs it.
data ArtifactRequest response = ArtifactRequest
    { arMode :: ArtifactServe
    -- ^ Whether the request streams bytes (@GET@) or probes bodiless (@HEAD@).
    , arReplies :: TarballReplies response
    , arRuntime :: ServeRuntime
    , arDeps :: PackumentDeps
    , arValidators :: RequestHeaders
    -- ^ The client's conditional validators, relayed onto both legs' upstream requests.
    , arPackage :: PackageName
    , arVersion :: Version
    , arFile :: Filename
    , arRespond :: response -> IO ResponseReceived
    }

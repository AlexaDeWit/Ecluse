-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The values both legs of the artifact path hold: the route's reply constructors, and the
coordinates of the artifact being served.

The caller's credential is deliberately absent from 'ArtifactRequest'. It is confined to the
private leg, which takes it as its own argument, so the public leg cannot reach one.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Types (
    -- * The route's replies
    TarballReplies (..),

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
import Ecluse.Core.Version (Version)

-- | The route-owned ways the tarball pipeline may answer.
data TarballReplies response = TarballReplies
    { tarballError :: Status -> ResponseHeaders -> Refusal -> response
    -- ^ An ecosystem-shaped local error.
    , tarballStream :: Status -> ResponseHeaders -> StreamingBody -> response
    -- ^ A transparent streamed upstream response.
    , tarballEmpty :: Status -> ResponseHeaders -> response
    -- ^ A transparent bodiless upstream response (@304@ or @HEAD@).
    }

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

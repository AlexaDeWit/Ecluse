-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's entry in the ecosystem adapter registry: the
'Ecluse.Core.Registry.Adapter.Types.RegistryAdapter' assembled from the existing
npm modules.

Pure assembly, no protocol logic of its own: every field is a function an npm
module already exports -- the path grammar ("Ecluse.Core.Registry.Npm.Route"), the
denial renderer ("Ecluse.Core.Registry.Npm.Serve"), the metadata client and served
packument assembly ("Ecluse.Core.Registry.Npm.Metadata",
"Ecluse.Core.Registry.Npm.Filter"), the artifact request builders
("Ecluse.Core.Registry.Npm.Request"), the publish relay
("Ecluse.Core.Registry.Npm"), and the mirror-write codec
("Ecluse.Core.Registry.Npm.Publish"), with the name canonicaliser from
"Ecluse.Core.Registry.Npm.Project".
-}
module Ecluse.Core.Registry.Npm.Adapter (
    npmAdapter,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Adapter.Types (
    AdapterArtifact (..),
    AdapterMetadata (..),
    AdapterPublish (..),
    AdapterServe (..),
    RegistryAdapter (..),
 )
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (NpmClientConfig),
    relayPublishDocument,
 )
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Registry.Npm.Route qualified as NpmRoute

{- | npm's capability record. The artifact builders ignore the response-bound and
manager parameters (npm request formation needs neither), and the publish slice
contributes protocol only: the mirror-write codec's transport (manager, credential
mint, response bound) is supplied at the composition root's marriage.
-}
npmAdapter :: RegistryAdapter
npmAdapter =
    RegistryAdapter
        { adapterEcosystem = Npm
        , adapterServe =
            AdapterServe
                { serveRouter = NpmRoute.npmRouter
                , serveRoutes = NpmRoute.npmRouteSpecs
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataNewClient = \tracing metrics upstream caching logFailure logInvalid logFetch limits manager baseUrl token ->
                    newNpmMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch (NpmClientConfig baseUrl manager token limits)
                , metadataAssemble = assembleMergedPackument
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \_ _ baseUrl token -> NpmRequest.artifactRequestByFile baseUrl token
                , artifactByUrl = \_ _ baseUrl token -> NpmRequest.artifactRequestByUrl baseUrl token
                }
        , adapterPublish =
            AdapterPublish
                { publishRelay = \limits manager targetUrl token ->
                    relayPublishDocument (NpmClientConfig targetUrl manager token limits)
                , publishCanonicaliseName = rightToMaybe . projectName
                , publishCodec = npmPublishCodec
                }
        }

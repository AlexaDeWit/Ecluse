-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's entry in the ecosystem adapter registry: the
'Ecluse.Core.Registry.Adapter.Types.RegistryAdapter' assembled from the existing
npm modules.

Pure assembly, with no protocol logic of its own. Every field is a function an npm
module already exports:

* The path grammar ("Ecluse.Core.Registry.Npm.Route").
* The denial renderer ("Ecluse.Core.Registry.Npm.Serve").
* The credential presentation ("Ecluse.Core.Registry.Npm.Credential").
* The metadata client and the served packument assembly
  ("Ecluse.Core.Registry.Npm.Metadata", "Ecluse.Core.Registry.Npm.Filter").
* The artifact request builders ("Ecluse.Core.Registry.Npm.Request").
* The publish relay ("Ecluse.Core.Registry.Npm").
* The mirror-write codec, with the body-name extractor the anti-shadowing guard
  reads through ("Ecluse.Core.Registry.Npm.Publish").
* The name canonicaliser ("Ecluse.Core.Registry.Npm.Project").
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
import Ecluse.Core.Registry.Npm (relayPublishDocument)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedDocument, serialiseMergedDocument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Publish (declaredNames, npmPublishCodec)
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Registry.Npm.Route qualified as NpmRoute
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocToken))
import Ecluse.Core.Security.Egress (registryUrlText)

-- | npm's capability record.
npmAdapter :: RegistryAdapter
npmAdapter =
    RegistryAdapter
        { adapterEcosystem = Npm
        , adapterServe =
            AdapterServe
                { serveRouter = NpmRoute.npmRouter
                , serveRoutes = NpmRoute.npmRouteSpecs
                , serveCredential = npmCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataNewClient = newNpmMetadataClient
                , metadataAssemble = assembleMergedDocument
                , metadataSerialise = serialiseMergedDocument
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \origin -> NpmRequest.artifactRequestByFile (registryUrlText (ocBaseUrl origin)) (ocToken origin)
                , artifactByUrl = NpmRequest.artifactRequestByUrl
                , -- npm serves tarball bytes from the registry host itself, so there
                  -- is no separate canonical files host to admit.
                  artifactHosts = []
                }
        , adapterPublish =
            AdapterPublish
                { publishRelay = relayPublishDocument
                , publishCanonicaliseName = rightToMaybe . projectName
                , publishDeclaredNames = declaredNames
                , publishCodec = npmPublishCodec
                }
        }

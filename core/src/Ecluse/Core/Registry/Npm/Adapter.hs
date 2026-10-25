-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's entry in the ecosystem adapter registry. Every field names a function the
@Ecluse.Core.Registry.Npm.*@ modules export, so no protocol decision is made here.
-}
module Ecluse.Core.Registry.Npm.Adapter (
    npmAdapter,
    npmPublish,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Rules.Types (RuleType (DenyIfYankedType))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact (..),
    AdapterMetadata (..),
    AdapterPublish (..),
    MirrorRequirements (..),
    MirrorStateRequirement (..),
    PublishRequestBody (..),
    UploadEndpointRequirement (..),
    UploadIdentityRequirement (..),
    WorkerMetadataRead (..),
 )
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Npm (relayPublishDocument)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedDocument, serialiseMergedDocument)
import Ecluse.Core.Registry.Npm.Maintenance (npmMaintenance)
import Ecluse.Core.Registry.Npm.Metadata (npmChargeFactors, npmRead)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec, uploadClaims)
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Registry.Npm.Route qualified as NpmRoute
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseRequest, ocToken))

-- | npm's capability record.
npmAdapter :: RegistryAdapter
npmAdapter =
    RegistryAdapter
        { adapterEcosystem = Npm
        , adapterRuleBlocks = Map.singleton DenyIfYankedType "npm reports no yank flag. Removed releases disappear from its metadata."
        , adapterServe =
            AdapterServe
                { serveRouter = NpmRoute.npmRouter
                , serveRoutes = NpmRoute.npmRouteSpecs
                , serveCredential = npmCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataRead = npmRead
                , metadataAssemble = assembleMergedDocument
                , metadataSerialise = serialiseMergedDocument
                , metadataChargeFactors = npmChargeFactors
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \origin -> NpmRequest.artifactRequestByFile (ocBaseRequest origin) (ocToken origin)
                , artifactByUrl = NpmRequest.artifactRequestByUrl
                , artifactHosts = NpmRequest.npmArtifactHosts
                }
        , adapterProjectName = rightToMaybe . projectName
        , adapterPublish = Just npmPublish
        , adapterMaintenance = npmMaintenance
        }

-- | npm's publish capability, named so a consumer of it alone need not unwrap the optional field.
npmPublish :: AdapterPublish
npmPublish =
    AdapterPublish
        { publishRelay = \origin name -> relayPublishDocument origin name . publishBodyBytes
        , publishUploadClaims = \_ -> pure . uploadClaims . publishBodyBytes
        , publishCodec = npmPublishCodec
        , publishMirrorRequirements = MirrorRequirements CachedWorkerRead NativeVersionState ReadRootUpload
        , publishIdentity = TokenIdentity
        }

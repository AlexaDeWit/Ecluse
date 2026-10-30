-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections pairs optional media-type values with their header names.
{-# LANGUAGE TupleSections #-}

{- | PyPI's adapter and the rule types its ecosystem cannot support.
Its protocol exposes no store listing or deletion. A separate control plane can supply those verbs.
-}
module Ecluse.Core.Registry.PyPI.Adapter (
    pypiAdapter,
    pypiPublish,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types (hContentType)

import Ecluse.Core.Registry.Metadata.Fetch.Preparation (staticRead)
import Ecluse.Core.Registry.Request (StoreOperation (StoreWrite), attachCredential, parseRequestEither, presentedMapping)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact (..),
    AdapterMaintenance (..),
    AdapterMetadata (..),
    AdapterPublish (..),
    MirrorRequirements (..),
    MirrorStateRequirement (..),
    PrivateArtifactAddressing (..),
    PublishExtras (NoPublishExtras),
    PublishRequestBody (..),
    UploadEndpointRequirement (..),
    UploadIdentityRequirement (..),
    WorkerMetadataRead (..),
 )
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken), originBaseUrl)
import Ecluse.Core.Registry.Publish (relayPublishRequest)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Filter (assembleSimpleDocument, serialiseSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (preparePyPIRead, pypiChargeFactors, pypiMirrorRead)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Registry.PyPI.Project (projectName, pypiNameLeadChars)
import Ecluse.Core.Registry.PyPI.Publish (pypiPublishCodec)
import Ecluse.Core.Registry.PyPI.Publish.Multipart qualified as Multipart
import Ecluse.Core.Registry.PyPI.Request qualified as PyPIRequest
import Ecluse.Core.Registry.PyPI.Route qualified as PyPIRoute

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType, DenyInstallTimeExecutionType))

-- | PyPI's capability record.
pypiAdapter :: RegistryAdapter
pypiAdapter =
    RegistryAdapter
        { adapterEcosystem = PyPI
        , adapterRuleBlocks =
            Map.fromList
                [ (DenyInstallTimeExecutionType, "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release.")
                , (DenyIfDeprecatedType, "The PyPI adapter supplies no deprecation flag.")
                , (AllowScopeType, "A PyPI project name has no scope, so this rule can never match one.")
                ]
        , adapterServe =
            AdapterServe
                { serveRouter = PyPIRoute.pypiRouter
                , serveRoutes = PyPIRoute.pypiRouteSpecs
                , serveCredential = pypiCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataRead = preparePyPIRead
                , metadataAssemble = assembleSimpleDocument
                , metadataSerialise = serialiseSimpleDocument
                , metadataChargeFactors = pypiChargeFactors
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \origin -> PyPIRequest.artifactRequestByFile (originBaseUrl origin) (ocToken origin)
                , artifactByUrl = PyPIRequest.artifactRequestByUrl
                , artifactHosts = PyPIRequest.pypiArtifactHosts
                , artifactPrivateAddressing = IndexedArtifactLocation
                }
        , adapterProjectName = rightToMaybe . projectName
        , adapterPublish = Just pypiPublish
        , adapterMaintenance =
            AdapterMaintenance
                { maintenanceListing = Nothing
                , maintenanceVersionDelete = Nothing
                , maintenanceAlphabet = mkNameAlphabet pypiNameLeadChars
                }
        }

-- | Preserve client upload bytes and response statuses without mirror duplicate classification.
pypiPublish :: AdapterPublish
pypiPublish =
    AdapterPublish
        { publishCredential = pypiCredential
        , publishRequestExtras = const (Right NoPublishExtras)
        , publishRelay = \presentation origin _ body ->
            relayPublishRequest
                origin
                ( do
                    base <- parseRequestEither (originBaseUrl origin)
                    pure . attachCredential (presentedMapping presentation StoreWrite pypiCredential) (ocToken origin) $
                        base
                            { method = "POST"
                            , requestHeaders = maybeToList (fmap (hContentType,) (publishBodyContentType body)) <> requestHeaders base
                            , requestBody = RequestBodyBS (publishBodyBytes body)
                            }
                )
        , publishUploadClaims = \limits _ _ body -> Multipart.uploadClaims limits (publishBodyContentType body) (publishBodyBytes body)
        , publishCodec = pypiPublishCodec
        , publishMirrorRequirements = MirrorRequirements (ChargedWorkerRead (staticRead pypiMirrorRead)) (DurableVersionState pypiMirrorDocuments)
        , publishUploadEndpoint = ExplicitUploadEndpoint
        , publishIdentity = NamedIdentity
        }

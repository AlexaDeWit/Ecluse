-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Ruby native formats share admission, mirroring and backend role contracts.
module Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (..), AdapterMaintenance (..), AdapterMetadata (..), PrivateArtifactAddressing (AdmittedIndexedArtifactLocation))
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseRequest, ocToken))
import Ecluse.Core.Registry.Request (artifactGet, artifactRequestByUrl, requestAt)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Metadata (prepareGemRead)
import Ecluse.Core.Registry.RubyGems.Project (projectName, rubyGemsNameChars)
import Ecluse.Core.Registry.RubyGems.Publish (rubyPublish)
import Ecluse.Core.Registry.RubyGems.Request (gemPath)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouteSpecs, rubyGemsRouter)
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType, DenyInstallTimeExecutionType))
import Ecluse.Core.Server.Admission.Types (ChargeFactors)
import Ecluse.Core.Server.Path (mkFilename, unFilename)

-- | Activation requires observed resource factors supplied by composition.
rubyGemsAdapter :: ChargeFactors -> RegistryAdapter
rubyGemsAdapter factors =
    RegistryAdapter
        { adapterEcosystem = RubyGems
        , adapterRuleBlocks =
            Map.fromList
                [ (DenyInstallTimeExecutionType, "RubyGems native builds are normal installation behaviour. This rule is unsupported for the ecosystem.")
                , (DenyIfDeprecatedType, "RubyGems metadata supplies no supported deprecation flag.")
                , (AllowScopeType, "RubyGems package names have no scope.")
                ]
        , adapterServe =
            AdapterServe
                { serveRouter = const rubyGemsRouter
                , serveRoutes = rubyGemsRouteSpecs
                , serveCredential = rubyReadCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataRead = prepareGemRead
                , metadataAssemble = assembleRubyGemsDocument
                , metadataSerialise = serialiseRubyGemsDocument
                , metadataChargeFactors = factors
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \origin _ raw -> do
                    file <- maybeToRight (UnparseableUrl "invalid RubyGems artifact filename") (mkFilename raw)
                    artifactGet rubyReadCredential (ocToken origin) <$> requestAt (ocBaseRequest origin) (gemPath (unFilename file))
                , artifactByUrl = artifactRequestByUrl rubyReadCredential
                , artifactHosts = []
                , artifactPrivateAddressing = AdmittedIndexedArtifactLocation
                }
        , adapterProjectName = rightToMaybe . projectName
        , adapterPublish = Just (rubyPublish factors)
        , adapterMaintenance = AdapterMaintenance Nothing Nothing (mkNameAlphabet rubyGemsNameChars)
        , adapterInstallEvidence = Nothing
        }

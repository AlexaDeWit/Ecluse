-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The CodeArtifact repository the three maintenance specs address, and the @amazonka@ answers
they build over it. The coordinates are one value, so a spec cannot drift from its siblings.
-}
module Ecluse.Runtime.Maintenance.CodeArtifact.Support (
    -- * The repository under test
    npmStore,
    withNpmStore,

    -- * Describe answers
    describing,
    connectedTo,
    routedTo,

    -- * Service refusals
    serviceError,

    -- * Fault detail
    detailOf,
) where

import Lens.Micro ((?~))
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (Header)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Registry.Maintenance (StoreFault (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
 )

-- | The npm repository the maintenance specs address, or 'Nothing' if npm resolved to no format.
npmStore :: Maybe CodeArtifactStore
npmStore = coordinates <$> codeArtifactFormat Npm
  where
    coordinates format =
        CodeArtifactStore
            { casDomain = "acme"
            , casDomainOwner = "111122223333"
            , casRegion = "eu-west-1"
            , casRepository = "mirror"
            , casFormat = format
            }

{- | Run the cases over 'npmStore'. The store's coordinates carry a parsed format, so a spec that
finds none has found a regression rather than an arm to skip.
-}
withNpmStore :: (CodeArtifactStore -> Spec) -> Spec
withNpmStore cases = maybe noNpmFormat cases npmStore
  where
    noNpmFormat =
        it "has a CodeArtifact format for npm" $
            expectationFailure "npm resolved to no CodeArtifact format"

-- | A @DescribeRepository@ answer carrying the given description.
describing :: CA.RepositoryDescription -> CA.DescribeRepositoryResponse
describing description =
    CA.newDescribeRepositoryResponse 200 & (CAL.describeRepositoryResponse_repository ?~ description)

-- | A repository whose own external connection reaches the named registry.
connectedTo :: Text -> CA.RepositoryDescription
connectedTo connection =
    CA.newRepositoryDescription
        & ( CAL.repositoryDescription_externalConnections
                ?~ [CA.newRepositoryExternalConnectionInfo & (CAL.repositoryExternalConnectionInfo_externalConnectionName ?~ connection)]
          )

-- | A repository fed by the named upstream repositories.
routedTo :: [Text] -> CA.RepositoryDescription
routedTo upstreams =
    CA.newRepositoryDescription
        & (CAL.repositoryDescription_upstreams ?~ [CA.newUpstreamRepositoryInfo & (CAL.upstreamRepositoryInfo_repositoryName ?~ name) | name <- upstreams])

-- | A service refusal at the given status, error code, and response headers.
serviceError :: Status -> Text -> [Header] -> AWS.Error
serviceError status code headers =
    AWS.ServiceError (AWS.ServiceError' "CodeArtifact" status headers (AWS.newErrorCode code) Nothing Nothing)

-- | What a store fault says, so an assertion reads the refusal rather than only that one happened.
detailOf :: StoreFault -> Text
detailOf = tfDetail . faultTransport

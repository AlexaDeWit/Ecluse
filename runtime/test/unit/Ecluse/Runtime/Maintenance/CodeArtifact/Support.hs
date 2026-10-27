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
    withPyPIStore,
    withCargoStore,
    withRubyStore,

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

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Registry.Maintenance (StoreFault (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
 )
import Ecluse.Runtime.Test.PackageStore.FixedPages (describing)

-- | The npm repository the maintenance specs address, or 'Nothing' if npm resolved to no format.
npmStore :: Maybe CodeArtifactStore
npmStore = storeFor Npm

-- One set of coordinates, in the ecosystem's format.
storeFor :: Ecosystem -> Maybe CodeArtifactStore
storeFor eco = coordinates <$> codeArtifactFormat eco
  where
    coordinates format =
        CodeArtifactStore
            { casDomain = "acme"
            , casDomainOwner = "111122223333"
            , casRegion = "eu-west-1"
            , casRepository = "mirror"
            , casFormat = format
            }

-- | Require the native npm format before constructing the cases.
withNpmStore :: (CodeArtifactStore -> Spec) -> Spec
withNpmStore = withStore Npm

-- | Require the native PyPI format at the shared repository coordinates.
withPyPIStore :: (CodeArtifactStore -> Spec) -> Spec
withPyPIStore = withStore PyPI

-- | Require the native Cargo format at the shared repository coordinates.
withCargoStore :: (CodeArtifactStore -> Spec) -> Spec
withCargoStore = withStore Cargo

-- | Require the native Ruby format at the shared repository coordinates.
withRubyStore :: (CodeArtifactStore -> Spec) -> Spec
withRubyStore = withStore RubyGems

withStore :: Ecosystem -> (CodeArtifactStore -> Spec) -> Spec
withStore eco cases = maybe missingFormat cases (storeFor eco)
  where
    missingFormat =
        it ("has a CodeArtifact format for " <> toString (ecosystemName eco)) $
            expectationFailure (toString (ecosystemName eco) <> " resolved to no CodeArtifact format")

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

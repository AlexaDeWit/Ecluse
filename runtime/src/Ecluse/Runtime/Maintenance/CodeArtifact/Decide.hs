-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Every decision the CodeArtifact store-maintenance leaf makes, over @amazonka@'s own request
and response types. @amazonka@ is trusted for serialisation, signing, and decoding against the
service model, so what stays ours is which call to build and how to read what comes back. The
read-only calls and the evidence they preserve live in
"Ecluse.Runtime.Maintenance.CodeArtifact.Read".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    -- * Coordinates
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatEcosystem,
    formatToken,

    -- * What the backend does
    codeArtifactFacts,
    deleteCeiling,

    -- * Requests
    listPackagesRequest,
    listVersionsRequest,
    listVersionsResult,
    deleteRequest,
    describeRepositoryRequest,
    describeUpstreamRequest,
    listTagsRequest,

    -- * The walk cursor
    cursorTagRequest,
    cursorUntagRequest,
    cursorOfTags,

    -- * Responses
    packagesOfPage,
    presenceOf,
    foldDeleteResponse,
    classifyRepository,
    repositoryOfStore,
    upstreamLinksOf,
    consentOfTags,
    repositoryOfResponse,
    arnOfDescription,

    -- * Consent marker
    consentDescriptor,

    -- * Faults
    classifyStoreFault,
    describeUpstreamRefusal,
    describeRepositoryGrant,
) where

import Ecluse.Runtime.Maintenance.CodeArtifact.Decide.Internal (
    CodeArtifactStore (..),
    arnOfDescription,
    classifyRepository,
    classifyStoreFault,
    codeArtifactFacts,
    codeArtifactFormat,
    consentDescriptor,
    consentOfTags,
    cursorOfTags,
    cursorTagRequest,
    cursorUntagRequest,
    deleteCeiling,
    deleteRequest,
    describeRepositoryGrant,
    describeRepositoryRequest,
    describeUpstreamRefusal,
    describeUpstreamRequest,
    foldDeleteResponse,
    formatEcosystem,
    formatToken,
    listPackagesRequest,
    listTagsRequest,
    listVersionsRequest,
    listVersionsResult,
    packagesOfPage,
    presenceOf,
    repositoryOfResponse,
    repositoryOfStore,
    upstreamLinksOf,
 )

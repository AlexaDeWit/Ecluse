-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolve a mount's mirror target to the one store backend value every role reads.

The target URL decides. A CodeArtifact endpoint carries its mint identity in its host and mints
its own write token. Any other host takes an operator-supplied static one, so a token can never
pair with an endpoint it was not minted for. The load refuses the two degenerate arrangements:
no static token where none is minted, and a static token beside a minted one. The Dredger, not
the load, still refuses a CodeArtifact path that addresses no repository.
-}
module Ecluse.Config.Target (
    -- * The resolved value
    StoreTag (..),
    storeTagName,
    MintPlan (..),
    CodeArtifactAbsence (..),
    ControlPlane (..),
    StoreBackend (..),

    -- * Resolution
    resolveStoreBackend,
    parseCodeArtifactHost,
    isAccountId,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Config.Types (
    CodeArtifactAbsence (..),
    ConfigError (..),
    ControlPlane (..),
    MintPlan (..),
    StoreBackend (..),
    StoreTag (..),
    storeTagName,
 )
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (nonBlank, registryPath)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatToken,
 )

{- | Resolve the mirror target's store backend from its URL and the mount's write-token keys. A
missing or conflicting static token is refused rather than silently dropped.
-}
resolveStoreBackend ::
    Ecosystem ->
    RegistryUrl ->
    Maybe Secret ->
    Maybe Natural ->
    Either ConfigError StoreBackend
resolveStoreBackend eco url mToken mDuration =
    case parseCodeArtifactHost (hostAddress raw) of
        Just (domain, owner, region) -> case mToken of
            Just _ -> Left (MirrorCredentialConflict eco)
            Nothing -> Right (codeArtifactBackend domain owner region)
        Nothing -> case mToken of
            Just token ->
                Right StoreBackend{sbTag = TagRegistry, sbMint = MintStatic token, sbControl = ControlNone}
            Nothing -> Left (MirrorCredentialTokenMissing eco)
  where
    raw = registryUrlText url

    codeArtifactBackend domain owner region =
        StoreBackend
            { sbTag = TagCodeArtifact
            , sbMint =
                MintCodeArtifact
                    CodeArtifactConfig
                        { caRegion = region
                        , caDomain = domain
                        , caDomainOwner = Just owner
                        , caDurationSeconds = mDuration
                        }
            , sbControl = ControlCodeArtifact (codeArtifactCoordinates eco raw domain owner region)
            }

{- The format the ecosystem maps to, and the repository under it. The path's format segment must be
the mount's own, because a repository's per-format endpoints are separate stores. -}
codeArtifactCoordinates ::
    Ecosystem -> Text -> Text -> Text -> Text -> Either CodeArtifactAbsence CodeArtifactStore
codeArtifactCoordinates eco raw domain owner region = do
    format <- maybeToRight (NoFormatFor eco) (codeArtifactFormat eco)
    repository <-
        maybeToRight
            (NotRepositoryEndpoint (formatToken format))
            (repositoryOfPath (formatToken format) raw)
    pure
        CodeArtifactStore
            { casDomain = domain
            , casDomainOwner = owner
            , casRegion = region
            , casRepository = repository
            , casFormat = format
            }

-- The repository a CodeArtifact endpoint path names, under the expected format segment.
repositoryOfPath :: Text -> Text -> Maybe Text
repositoryOfPath format url = case pathSegments url of
    [pathFormat, repository] | pathFormat == format -> nonBlank repository
    _ -> Nothing

-- The non-empty path segments of an absolute URL, which the egress boundary has already vetted.
pathSegments :: Text -> [Text]
pathSegments = filter (not . T.null) . T.splitOn "/" . registryPath

{- | Parse @{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@ into (domain, owner, region). The owner is the
12-digit account id after the __last__ hyphen, so a domain may carry them. Any other host is 'Nothing'.
-}
parseCodeArtifactHost :: Text -> Maybe (Text, Text, Text)
parseCodeArtifactHost host =
    -- The accepted shape carries exactly one @.d.codeartifact.@ marker. Any other count is not
    -- a CodeArtifact endpoint, refused here rather than by an implicit pattern-match failure.
    case T.splitOn ".d.codeartifact." host of
        [domainOwner, regionTail] -> do
            region <- nonBlank =<< T.stripSuffix ".amazonaws.com" regionTail
            let (domainDash, owner) = T.breakOnEnd "-" domainOwner
            domain <- nonBlank (T.dropEnd 1 domainDash)
            guard (isAccountId owner)
            pure (domain, owner, region)
        _ -> Nothing

{- | Whether a value is a 12-digit AWS account id (shared with the SQS queue-URL
shape validation in "Ecluse.Config.QueueTarget").
-}
isAccountId :: Text -> Bool
isAccountId t = T.compareLength t 12 == EQ && T.all isDigit t

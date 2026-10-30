-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Resolve tagged registry endpoints and their CodeArtifact store identities.
module Ecluse.Config.Target (
    -- * Resolution
    resolveStoreBackend,
    resolveUploadBackend,
    resolvePrivateBackend,
    vetTargetTag,
    vetPrivateRepository,
    vetPublicationRepository,
    parseCodeArtifactHost,
    isAccountId,

    -- * Service credential presentation
    storePresentation,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Config.Types (
    ConfigError (..),
    MirrorEndpoint (..),
    MirrorWrite (..),
    PublicationEndpoint (..),
    StoreBackend (..),
    StoreTag (..),
    Target (..),
 )
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Request (ServicePresentation, declaredPresentation, nativePresentation)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (nonBlank, registryPath)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Runtime.Credential.CodeArtifact.Presentation (codeArtifactAuthMechanism)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatToken,
 )

-- | Resolve the read root and credential identity declared by a mirror target.
resolveStoreBackend :: Ecosystem -> MirrorEndpoint -> Either ConfigError StoreBackend
resolveStoreBackend = resolveMirrorBackend RepositoryEndpoint

-- | Resolve a native upload endpoint without admitting upload paths as read roots.
resolveUploadBackend :: Ecosystem -> MirrorEndpoint -> RegistryUrl -> Either ConfigError StoreBackend
resolveUploadBackend eco endpoint upload = resolveMirrorBackend UploadEndpoint eco endpoint{meUrl = upload}

data EndpointPurpose = RepositoryEndpoint | UploadEndpoint

resolveMirrorBackend :: EndpointPurpose -> Ecosystem -> MirrorEndpoint -> Either ConfigError StoreBackend
resolveMirrorBackend purpose eco endpoint = case meWrite endpoint of
    WriteRegistry token -> Right (BackendRegistry token)
    WriteVerdaccio token consent -> Right (BackendVerdaccio token consent)
    WriteCodeArtifact mDuration -> do
        (domain, owner, region) <- codeArtifactHost eco key (meUrl endpoint)
        store <- codeArtifactStore purpose eco key (registryUrlText (meUrl endpoint)) domain owner region
        Right (BackendCodeArtifact (mintIdentity domain owner region mDuration) store)
  where
    key = case purpose of
        RepositoryEndpoint -> mirrorTargetKey
        UploadEndpoint -> "mirrorTarget.codeArtifact.uploadUrl"

-- | Resolve a CodeArtifact private read root with the default token lifetime.
resolvePrivateBackend :: Ecosystem -> Target -> Either ConfigError (StoreBackend, CodeArtifactStore)
resolvePrivateBackend eco target = do
    (domain, owner, region) <- codeArtifactHost eco privateUpstreamKey (tgtUrl target)
    store <- codeArtifactStore RepositoryEndpoint eco privateUpstreamKey (registryUrlText (tgtUrl target)) domain owner region
    pure (BackendCodeArtifact (mintIdentity domain owner region Nothing) store, store)

-- | Check the host against its declared tag. Repository path checks belong to the endpoint role.
vetTargetTag :: Ecosystem -> Text -> Target -> Either ConfigError ()
vetTargetTag eco key target = case tgtTag target of
    TagCodeArtifact -> void (codeArtifactHost eco (key <> ".codeArtifact.url") (tgtUrl target))
    TagRegistry -> Right ()
    TagVerdaccio -> Right ()

-- | Require a CodeArtifact private read root for the mount format before checking its upstream chain.
vetPrivateRepository :: Ecosystem -> Target -> Either ConfigError ()
vetPrivateRepository eco target = case tgtTag target of
    -- The host is 'vetTargetTag''s to report, so a bad one is left to it rather than named twice.
    TagCodeArtifact | isRight (codeArtifactHost eco privateUpstreamKey (tgtUrl target)) -> void (resolvePrivateBackend eco target)
    _ -> Right ()

-- | A CodeArtifact publication @uploadUrl@ addresses the same repository as its read root.
vetPublicationRepository :: Ecosystem -> PublicationEndpoint -> Either ConfigError ()
vetPublicationRepository eco endpoint = case (tgtTag (peTarget endpoint), peUploadUrl endpoint) of
    (TagCodeArtifact, Just upload) -> do
        original <- store RepositoryEndpoint "publicationTarget.codeArtifact.url" (tgtUrl (peTarget endpoint))
        uploaded <- store UploadEndpoint "publicationTarget.codeArtifact.uploadUrl" upload
        unless (original == uploaded) (Left (ParseError "CodeArtifact publication uploadUrl must address the same read repository"))
    _ -> Right ()
  where
    store purpose key root = do
        (domain, owner, region) <- codeArtifactHost eco key root
        codeArtifactStore purpose eco key (registryUrlText root) domain owner region

privateUpstreamKey :: Text
privateUpstreamKey = "privateUpstream.codeArtifact.url"

mirrorTargetKey :: Text
mirrorTargetKey = "mirrorTarget.codeArtifact.url"

mintIdentity :: Text -> Text -> Text -> Maybe Natural -> CodeArtifactConfig
mintIdentity domain owner region mDuration =
    CodeArtifactConfig
        { caRegion = region
        , caDomain = domain
        , caDomainOwner = Just owner
        , caDurationSeconds = mDuration
        }

codeArtifactHost :: Ecosystem -> Text -> RegistryUrl -> Either ConfigError (Text, Text, Text)
codeArtifactHost eco key url =
    maybeToRight
        (CodeArtifactHostMismatch eco key)
        (parseCodeArtifactHost (hostAddress (registryUrlText url)))

codeArtifactStore :: EndpointPurpose -> Ecosystem -> Text -> Text -> Text -> Text -> Text -> Either ConfigError CodeArtifactStore
codeArtifactStore purpose eco key raw domain owner region = do
    format <- maybeToRight (CodeArtifactFormatUnsupported eco key) (codeArtifactFormat eco)
    repository <- maybeToRight (repositoryError (formatToken format)) (repositoryOfPath purpose (formatToken format) raw)
    pure
        CodeArtifactStore
            { casDomain = domain
            , casDomainOwner = owner
            , casRegion = region
            , casRepository = repository
            , casFormat = format
            }
  where
    repositoryError format = case purpose of
        RepositoryEndpoint -> CodeArtifactRepositoryMissing eco key format
        UploadEndpoint ->
            ParseError
                ( key
                    <> " must address /"
                    <> format
                    <> "/{repository}/"
                    <> if format == "pypi" then " or /pypi/{repository}/legacy/" else ""
                )

repositoryOfPath :: EndpointPurpose -> Text -> Text -> Maybe Text
repositoryOfPath purpose format url = case (purpose, pathSegments url) of
    (_, [pathFormat, repository]) | pathFormat == format -> nonBlank repository
    (UploadEndpoint, ["pypi", repository, "legacy"]) | format == "pypi" -> nonBlank repository
    _ -> Nothing

pathSegments :: Text -> [Text]
pathSegments = filter (not . T.null) . T.splitOn "/" . registryPath

-- | How a declared store asks for an ecosystem's service credential. Only CodeArtifact declares a mechanism.
storePresentation :: StoreTag -> Ecosystem -> ServicePresentation
storePresentation tag eco = case tag of
    TagCodeArtifact -> declaredPresentation (codeArtifactAuthMechanism eco)
    TagRegistry -> nativePresentation
    TagVerdaccio -> nativePresentation

-- | Parse the CodeArtifact host into domain, owner and region, allowing hyphens in the domain.
parseCodeArtifactHost :: Text -> Maybe (Text, Text, Text)
parseCodeArtifactHost host =
    case T.splitOn ".d.codeartifact." host of
        [domainOwner, regionTail] -> do
            region <- nonBlank =<< T.stripSuffix ".amazonaws.com" regionTail
            let (domainDash, owner) = T.breakOnEnd "-" domainOwner
            domain <- nonBlank (T.dropEnd 1 domainDash)
            guard (isAccountId owner)
            pure (domain, owner, region)
        _ -> Nothing

-- | Recognise the 12-digit AWS account id shared by CodeArtifact and SQS URL parsing.
isAccountId :: Text -> Bool
isAccountId t = T.compareLength t 12 == EQ && T.all isDigit t

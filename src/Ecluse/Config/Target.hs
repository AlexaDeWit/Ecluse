-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolve a mount's declared endpoints against the store tag each one names.

The tag is the declaration, and the URL is checked against it. A @codeArtifact@ endpoint must carry
the CodeArtifact host shape, and a @codeArtifact@ mirror target must address a repository under the
mount's own package format, because a repository's per-format endpoints are separate stores. Every
other tag admits any https registry the egress boundary cleared. Each refusal names the key, so a
store this build cannot address is refused at load rather than at the first write.
-}
module Ecluse.Config.Target (
    -- * The resolved value
    StoreTag (..),
    storeTagName,
    MintPlan (..),
    ControlPlane (..),
    StoreBackend (..),
    sbTag,
    sbMint,
    sbControl,

    -- * Resolution
    resolveStoreBackend,
    vetTargetTag,
    parseCodeArtifactHost,
    isAccountId,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Config.Types (
    ConfigError (..),
    ControlPlane (..),
    MintPlan (..),
    MirrorEndpoint (..),
    MirrorWrite (..),
    StoreBackend (..),
    StoreTag (..),
    Target (..),
    sbControl,
    sbMint,
    sbTag,
    storeTagName,
 )
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

{- | Resolve a mirror target's store backend from the tag it declares. Only the CodeArtifact arm
reads the URL, and it refuses one that addresses no repository this mount could mirror into.
-}
resolveStoreBackend :: Ecosystem -> MirrorEndpoint -> Either ConfigError StoreBackend
resolveStoreBackend eco endpoint = case meWrite endpoint of
    WriteRegistry token -> Right (BackendRegistry token)
    WriteVerdaccio token consent -> Right (BackendVerdaccio token consent)
    WriteCodeArtifact mDuration -> do
        (domain, owner, region) <- codeArtifactHost eco "mirrorTarget" (meUrl endpoint)
        store <- codeArtifactStore eco (registryUrlText (meUrl endpoint)) domain owner region
        Right (BackendCodeArtifact (mintIdentity domain owner region mDuration) store)

{- | Vet a read or publish endpoint's URL against its declared tag. Only @codeArtifact@ constrains
the host, and only a mirror target constrains the path, so this is total over the other tags.
-}
vetTargetTag :: Ecosystem -> Text -> Target -> Either ConfigError ()
vetTargetTag eco key target = case tgtTag target of
    TagCodeArtifact -> void (codeArtifactHost eco key (tgtUrl target))
    TagRegistry -> Right ()
    TagVerdaccio -> Right ()

-- The mint identity a CodeArtifact host carries, with the lifetime the operator asked for.
mintIdentity :: Text -> Text -> Text -> Maybe Natural -> CodeArtifactConfig
mintIdentity domain owner region mDuration =
    CodeArtifactConfig
        { caRegion = region
        , caDomain = domain
        , caDomainOwner = Just owner
        , caDurationSeconds = mDuration
        }

-- The CodeArtifact identity a target's host carries, refused under the key it was written at.
codeArtifactHost :: Ecosystem -> Text -> RegistryUrl -> Either ConfigError (Text, Text, Text)
codeArtifactHost eco key url =
    maybeToRight
        (CodeArtifactHostMismatch eco (key <> ".codeArtifact.url"))
        (parseCodeArtifactHost (hostAddress (registryUrlText url)))

-- The repository a mirror target addresses, under the format token its mount's ecosystem maps to.
codeArtifactStore :: Ecosystem -> Text -> Text -> Text -> Text -> Either ConfigError CodeArtifactStore
codeArtifactStore eco raw domain owner region = do
    format <- maybeToRight (CodeArtifactFormatUnsupported eco) (codeArtifactFormat eco)
    repository <-
        maybeToRight
            (CodeArtifactRepositoryMissing eco (formatToken format))
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

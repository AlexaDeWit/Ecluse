-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive a mount's mirror-write credential from its mirror-target URL.

The mirror-target URL is the single source of truth for how the mirror write
authenticates. A CodeArtifact endpoint
(@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@) encodes its whole mint
identity in its host. Such a target therefore dictates a minted token scoped to
exactly the domain the worker writes to. Any other host takes an operator-supplied
static write token.

The credential comes from the same URL the request goes to, so a token can never pair
with an endpoint it was not minted for. The divergence class is unrepresentable
rather than merely guarded. The load refuses two arrangements, so neither degrades
silently. The first is a non-CodeArtifact target with no static token. The second is
a CodeArtifact target that also carries a static token.
-}
module Ecluse.Config.MirrorCredential (
    resolveMirrorCredential,
    parseCodeArtifactHost,
    isAccountId,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Config.Types (ConfigError (..), MirrorCredential (..))
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))

{- | Derive the mirror-write credential from the resolved mirror-target URL. A CodeArtifact
host yields a 'MirrorCodeArtifact' whose identity comes from the host, so the mint is scoped to
the domain the worker writes to. Any other host yields a 'MirrorStatic', and a missing or
conflicting token is refused rather than silently dropped.
-}
resolveMirrorCredential ::
    Ecosystem ->
    RegistryUrl ->
    Maybe Secret ->
    Maybe Natural ->
    Either ConfigError MirrorCredential
resolveMirrorCredential eco url mToken mDuration =
    case parseCodeArtifactHost (hostAddress (registryUrlText url)) of
        Just (domain, owner, region) -> case mToken of
            Just _ -> Left (MirrorCredentialConflict eco)
            Nothing ->
                Right
                    ( MirrorCodeArtifact
                        CodeArtifactConfig
                            { caRegion = region
                            , caDomain = domain
                            , caDomainOwner = Just owner
                            , caDurationSeconds = mDuration
                            }
                    )
        Nothing -> case mToken of
            Just token -> Right (MirrorStatic token)
            Nothing -> Left (MirrorCredentialTokenMissing eco)

{- | Parse a CodeArtifact endpoint host into its (domain, owner, region). The shape is
@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@, where @{owner}@ is the 12-digit
account id after the __last__ hyphen, so a domain may itself contain hyphens. Any other host
yields 'Nothing' and counts as a static-token target, never a bogus owner.
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

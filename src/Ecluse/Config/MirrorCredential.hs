-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive a mount's mirror-write credential from its mirror-target URL.

The mirror-target URL is the single source of truth for how the mirror write is
authenticated. A CodeArtifact endpoint
(@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@) encodes its whole mint
identity in its host, so a CodeArtifact target dictates a minted token scoped to
exactly the domain the worker writes to. Any other host is written with an
operator-supplied static bearer.

Because the credential is derived from the very URL it will be sent to, a token can
never be paired with an endpoint it was not minted for: the divergence class is
unrepresentable rather than merely guarded (issue #808). Two arrangements are refused
at load so neither degrades silently: a non-CodeArtifact target with no static token,
and a CodeArtifact target that also carries a static token.
-}
module Ecluse.Config.MirrorCredential (
    resolveMirrorCredential,
    parseCodeArtifactHost,
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

{- | Derive the mirror-write credential from the resolved mirror-target URL, its
optional static token, and the optional token-duration. A CodeArtifact host yields a
'MirrorCodeArtifact' whose identity is parsed straight from the host (so the mint is
scoped to the domain the worker writes to); any other host yields a 'MirrorStatic'
from the supplied token. The two refusals keep a "derived" credential from ever
meaning a silent one.
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

{- | Parse a CodeArtifact npm endpoint host into its (domain, owner, region). The host
shape is @{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@; the @{owner}@ is the
12-digit account id after the __last__ hyphen of the first label, so a domain may
itself contain hyphens. 'Nothing' for any host that is not this shape -- including one
whose tail after the last hyphen is not an account id, so a hyphen-bearing
non-CodeArtifact host never mis-parses into a bogus owner (and so is treated as a
static-token target, not a CodeArtifact one).
-}
parseCodeArtifactHost :: Text -> Maybe (Text, Text, Text)
parseCodeArtifactHost host =
    -- The accepted shape is exactly one @.d.codeartifact.@ marker, splitting the host
    -- into its @{domain}-{owner}@ label and its @{region}.amazonaws.com@ tail; any
    -- other number of parts (none, or a host carrying the marker twice) is not a
    -- CodeArtifact endpoint and is rejected here rather than via an implicit
    -- pattern-match failure in the 'Maybe' monad.
    case T.splitOn ".d.codeartifact." host of
        [domainOwner, regionTail] -> do
            region <- nonBlank =<< T.stripSuffix ".amazonaws.com" regionTail
            let (domainDash, owner) = T.breakOnEnd "-" domainOwner
            domain <- nonBlank (T.dropEnd 1 domainDash)
            guard (isAccountId owner)
            pure (domain, owner, region)
        _ -> Nothing

-- Whether a value is a 12-digit AWS account id.
isAccountId :: Text -> Bool
isAccountId t = T.compareLength t 12 == EQ && T.all isDigit t

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's credential build: each active mount's resolved 'MintPlan' becomes a
live, process-global 'CredentialProvider' the mount references by its ecosystem.

'MintStatic' becomes a stateless provider. 'MintCodeArtifact' becomes the refresh wrapper around
'newCodeArtifactProvider', which mints once eagerly, so a bad identity fails loudly here as a
'CodeArtifactMintFailed'. AWS credentials come from the ambient container role, never an Écluse
key, and AWS mints per domain, so identities that coincide share one provider and one breaker.
-}
module Ecluse.Composition.Credential (
    -- * Global credential providers
    CredentialProviders,
    initCredentialProviders,
    initializedEcosystems,
    lookupProvider,

    -- * Internals exported for testing
    codeArtifactIdentityGroups,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (BootError (..), refuseOnThrow)
import Ecluse.Config (
    MintPlan (..),
    MirrorTarget (..),
    Mount (..),
    regMirrorTarget,
    sbMint,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig, newCodeArtifactProvider)

{- | The process-global credential providers, keyed by the ecosystem they serve. A mount naming
an ecosystem absent from the keyset has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | Build the global credential providers from the cleared mounts, or every boot error that
blocks one. Each provider mints eagerly, so a bad identity fails here as 'CodeArtifactMintFailed'.
-}
initCredentialProviders :: CredentialReporters -> [Mount] -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reporters mounts = do
    let creds =
            [ (mountEcosystem mount, sbMint (mtBackend target))
            | mount <- mounts
            , Just target <- [regMirrorTarget (mountRegistries mount)]
            ]
    -- The static leaf is stateless, so it stays per mount, unlike a CodeArtifact provider.
    let statics = [(eco, staticProviderFor token) | (eco, MintStatic token) <- creds]
    let caPlans = [(eco, ca) | (eco, MintCodeArtifact ca) <- creds]
    results <- traverse (initSharedCodeArtifact reporters) (codeArtifactIdentityGroups caPlans)
    let (initErrs, shared) = partitionEithers results
    if not (null initErrs)
        then pure (Left (concat initErrs))
        else pure (Right (CredentialProviders (Map.fromList (statics <> concat shared))))

-- One provider per distinct identity, fanned out to every ecosystem in the group, so
-- a shared domain carries one refresh schedule and one breaker rather than one per mount.
initSharedCodeArtifact :: CredentialReporters -> (CodeArtifactConfig, NonEmpty Ecosystem) -> IO (Either [BootError] [(Ecosystem, CredentialProvider)])
initSharedCodeArtifact reporters (caConfig, ecosystems) =
    fmap fannedOut <$> refuseOnThrow CodeArtifactMintFailed (newCodeArtifactProvider reporters caConfig)
  where
    fannedOut provider = [(eco, provider) | eco <- toList ecosystems]

{- | Group the mounts' resolved CodeArtifact identities by distinct 'CodeArtifactConfig'. The
mint's scope is the domain, so one domain shares a provider and a differing duration keeps its own.
-}
codeArtifactIdentityGroups :: [(Ecosystem, CodeArtifactConfig)] -> [(CodeArtifactConfig, NonEmpty Ecosystem)]
codeArtifactIdentityGroups plans =
    Map.toAscList (Map.fromListWith (<>) [(ca, eco :| []) | (eco, ca) <- plans])

-- A static mirror-target write provider from an operator-supplied token.
staticProviderFor :: Secret -> CredentialProvider
staticProviderFor token = staticProvider AuthToken{authSecret = token, authExpiresAt = Nothing}

{- | The set of ecosystems that resolved to an initialised provider: the pure
surface the boot-time credential-reference check reasons over.
-}
initializedEcosystems :: CredentialProviders -> Set Ecosystem
initializedEcosystems (CredentialProviders ps) = Map.keysSet ps

{- | Look up the initialised provider for an ecosystem, 'Nothing' when none is
initialised (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupProvider eco (CredentialProviders ps) = Map.lookup eco ps

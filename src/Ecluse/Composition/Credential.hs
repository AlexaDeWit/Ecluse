-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's credential build: each active mount's resolved 'StoreBackend' becomes a
live, process-global 'CredentialProvider', keyed by ecosystem and labelled by its store tag.

'MintStatic' becomes a stateless provider. 'MintCodeArtifact' becomes the refresh wrapper around
'newCodeArtifactProvider', which mints once eagerly, so a bad identity fails loudly here as a
'CodeArtifactMintFailed'. AWS credentials come from the ambient container role, never an Écluse
key, and AWS mints per domain, so identities that coincide share one provider and one breaker.
-}
module Ecluse.Composition.Credential (
    -- * Global credential providers
    CredentialProviders,
    noCredentialProviders,
    initCredentialProviders,
    initializedEcosystems,
    lookupProvider,

    -- * The telemetry label a store carries
    providerLabel,

    -- * Internals exported for testing
    mirrorBackends,
    codeArtifactIdentityGroups,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (BootError (..), refuseOnThrow)
import Ecluse.Config (
    MintPlan (..),
    MirrorTarget (..),
    Mount (..),
    StoreBackend,
    StoreTag (..),
    regMirrorTarget,
    sbMint,
    sbTag,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Telemetry.Metrics (Provider (ProviderCodeArtifact, ProviderRegistry, ProviderVerdaccio))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig, newCodeArtifactProvider)

{- | The process-global credential providers, keyed by the ecosystem they serve. A mount naming
an ecosystem absent from the keyset has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | No initialised providers: what a boot half that refused before it built any carries onward,
so the halves after it still plan and still report what they refuse.
-}
noCredentialProviders :: CredentialProviders
noCredentialProviders = CredentialProviders Map.empty

{- | The @provider@ metric label a store's credential signals record under. It reads as the
configuration spells the tag, so a dashboard series and a mount's declaration are one word.
-}
providerLabel :: StoreTag -> Provider
providerLabel = \case
    TagRegistry -> ProviderRegistry
    TagCodeArtifact -> ProviderCodeArtifact
    TagVerdaccio -> ProviderVerdaccio

{- | Build the global credential providers from the cleared mounts, or every boot error that
blocks one. Each provider mints eagerly, so a bad identity fails here as 'CodeArtifactMintFailed'.
-}
initCredentialProviders :: (StoreTag -> CredentialReporters) -> [Mount] -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reportersFor mounts = do
    let creds = [(eco, sbTag backend, sbMint backend) | (eco, backend) <- mirrorBackends mounts]
    -- The static leaf is stateless, so it stays per mount, unlike a CodeArtifact provider.
    let statics = [(eco, staticProviderFor token) | (eco, _, MintStatic token) <- creds]
    let caPlans = [(eco, tag, ca) | (eco, tag, MintCodeArtifact ca) <- creds]
    results <- traverse (initSharedCodeArtifact reportersFor) (codeArtifactIdentityGroups caPlans)
    let (initErrs, shared) = partitionEithers results
    if not (null initErrs)
        then pure (Left (concat initErrs))
        else pure (Right (CredentialProviders (Map.fromList (statics <> concat shared))))

{- | Each mirroring mount's ecosystem and the store its mirror write authenticates to. A mount
declaring no mirror target holds no standing credential, so it contributes none.
-}
mirrorBackends :: [Mount] -> [(Ecosystem, StoreBackend)]
mirrorBackends mounts =
    [ (mountEcosystem mount, mtBackend target)
    | mount <- mounts
    , Just target <- [regMirrorTarget (mountRegistries mount)]
    ]

-- One provider per distinct identity, fanned out to every ecosystem in the group, so
-- a shared domain carries one refresh schedule and one breaker rather than one per mount.
initSharedCodeArtifact ::
    (StoreTag -> CredentialReporters) ->
    (CodeArtifactConfig, (StoreTag, NonEmpty Ecosystem)) ->
    IO (Either [BootError] [(Ecosystem, CredentialProvider)])
initSharedCodeArtifact reportersFor (caConfig, (tag, ecosystems)) =
    fmap fannedOut <$> refuseOnThrow CodeArtifactMintFailed (newCodeArtifactProvider (reportersFor tag) caConfig)
  where
    fannedOut provider = [(eco, provider) | eco <- toList ecosystems]

{- | Group the mounts' resolved CodeArtifact identities by distinct 'CodeArtifactConfig'. One
domain shares a provider, its reporters, and its breaker, and a differing duration keeps its own.
-}
codeArtifactIdentityGroups :: [(Ecosystem, StoreTag, CodeArtifactConfig)] -> [(CodeArtifactConfig, (StoreTag, NonEmpty Ecosystem))]
codeArtifactIdentityGroups plans =
    Map.toAscList (Map.fromListWith merge [(ca, (tag, eco :| [])) | (eco, tag, ca) <- plans])
  where
    -- One identity resolves under one tag, so every member agrees and either labels the group.
    merge (tag, ecosystems) (_, more) = (tag, ecosystems <> more)

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

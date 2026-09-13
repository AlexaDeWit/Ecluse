-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Target-bound credential providers built at the composition root.
CodeArtifact consumers with the same mint identity share one refresh provider and breaker.
-}
module Ecluse.Composition.Credential (
    -- * Global credential providers
    CredentialProviders,
    noCredentialProviders,
    initCredentialProviders,
    initTargetCredentialProviders,
    CredentialTarget (..),
    lookupTargetProvider,
    initializedEcosystems,
    lookupProvider,

    -- * The telemetry label a store carries
    providerLabel,

    -- * Internals exported for testing
    mirrorBackends,
    codeArtifactIdentityGroups,
) where

import Data.Foldable1 qualified as Foldable1
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

-- | A credential consumer within one mount. Private-cache reads never use the mirror slot.
data CredentialTarget = MirrorCredential | PrivateCacheCredential
    deriving stock (Eq, Ord, Show)

-- | Providers keyed by the mount and the target that declared their authentication identity.
newtype CredentialProviders = CredentialProviders (Map (Ecosystem, CredentialTarget) CredentialProvider)

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
initCredentialProviders :: (Ecosystem -> StoreTag -> CredentialReporters) -> [Mount] -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reportersFor mounts =
    initTargetCredentialProviders reportersFor [((eco, MirrorCredential), backend) | (eco, backend) <- mirrorBackends mounts]

-- | Build target-bound providers, sharing only matching CodeArtifact mint identities.
initTargetCredentialProviders ::
    (Ecosystem -> StoreTag -> CredentialReporters) ->
    [((Ecosystem, CredentialTarget), StoreBackend)] ->
    IO (Either [BootError] CredentialProviders)
initTargetCredentialProviders reportersFor backends = do
    let creds = [(key, sbTag backend, sbMint backend) | (key, backend) <- backends]
    -- The static leaf is stateless, so it stays per mount, unlike a CodeArtifact provider.
    let statics = [(eco, staticProviderFor token) | (eco, _, MintStatic token) <- creds]
    let caPlans = [(eco, tag, ca) | (eco, tag, MintCodeArtifact ca) <- creds]
    results <- traverse (initSharedCodeArtifact (reportersFor . fst)) (codeArtifactIdentityGroups caPlans)
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

-- Disjoint groups use their smallest ecosystem as a bounded identity for expiry replacement.
-- Each group shares one provider, refresh schedule and breaker.
initSharedCodeArtifact ::
    (Ord key) =>
    (key -> StoreTag -> CredentialReporters) ->
    (CodeArtifactConfig, (StoreTag, NonEmpty key)) ->
    IO (Either [BootError] [(key, CredentialProvider)])
initSharedCodeArtifact reportersFor (caConfig, (tag, ecosystems)) =
    fmap fannedOut <$> refuseOnThrow CodeArtifactMintFailed (newCodeArtifactProvider (reportersFor (Foldable1.minimum ecosystems) tag) caConfig)
  where
    fannedOut provider = [(eco, provider) | eco <- toList ecosystems]

{- | Group the mounts' resolved CodeArtifact identities by distinct 'CodeArtifactConfig'. One
domain shares a provider, its reporters, and its breaker, and a differing duration keeps its own.
-}
codeArtifactIdentityGroups :: [(key, StoreTag, CodeArtifactConfig)] -> [(CodeArtifactConfig, (StoreTag, NonEmpty key))]
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
initializedEcosystems (CredentialProviders ps) = fromList [eco | (eco, MirrorCredential) <- Map.keys ps]

{- | Look up the initialised provider for an ecosystem, 'Nothing' when none is
initialised (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupProvider = lookupTargetProvider MirrorCredential

-- | Look up only the declared target's credential.
lookupTargetProvider :: CredentialTarget -> Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupTargetProvider target eco (CredentialProviders ps) = Map.lookup (eco, target) ps

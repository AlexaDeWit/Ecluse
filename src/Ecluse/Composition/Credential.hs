-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's credential build: turn each active mount's __resolved__
mirror-write credential into a live, process-global 'CredentialProvider'.

== Global providers, per-mount reference

A 'Ecluse.Core.Credential.CredentialProvider' is the service's own cloud identity,
built __once__ here from the cleared mounts and held process-global. A mount
references one by its ecosystem and never holds its own. Config load derives which
credential a mount uses __from the mirror-target URL__
('Ecluse.Config.MirrorCredential.resolveMirrorCredential') and carries it on the mount
as a 'Ecluse.Config.MirrorCredential'. AWS can therefore mint a CodeArtifact token
only for the domain the worker writes to. This module realises that resolved plan:

* 'MirrorStatic': a stateless static provider from the operator-supplied token.
* 'MirrorCodeArtifact': the refresh\/cache wrapper around the CodeArtifact mint leaf
  ('newCodeArtifactProvider'). It mints once eagerly, so a misconfigured identity or a
  missing permission fails loudly here at boot as a 'CodeArtifactMintFailed'. AWS
  credentials come from the ambient container\/task role (the standard chain), never
  from an Écluse key.

Provider granularity follows the credential's real scope, not the mount count. AWS
mints a CodeArtifact token per domain. Mounts whose resolved CodeArtifact identities
coincide ('codeArtifactIdentityGroups') therefore share one provider: one eager boot
mint, one refresh schedule, one breaker. Each still looks its provider up by its own
ecosystem.

The refreshing CodeArtifact provider takes the 'CredentialReporters', so its mint
breaker and its refresh outcomes record to telemetry. The static provider never
refreshes, so they do not concern it. The composition root supplies the deferred
reporters that go live once the telemetry substrate exists. Failures aggregate as
'Ecluse.Composition.BootError.BootError's, so one run reports every domain that failed
to mint.
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
import UnliftIO (tryAny)

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Config (
    MirrorCredential (..),
    MirrorTarget (..),
    Mount (..),
    regMirrorTarget,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig, newCodeArtifactProvider)

{- | The process-global credential providers, keyed by the ecosystem they serve. A
mount references one by ecosystem and never holds its own, so a mount naming an
ecosystem absent from the keyset has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | Build the global credential providers from the boot's cleared mounts, or the boot
errors that block them. Each provider mints eagerly, so a bad identity, region, or
permission fails loud here as 'CodeArtifactMintFailed' rather than at first publish.
-}
initCredentialProviders :: CredentialReporters -> [Mount] -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reporters mounts = do
    let creds =
            [ (mountEcosystem mount, mtCredential target)
            | mount <- mounts
            , Just target <- [regMirrorTarget (mountRegistries mount)]
            ]
    -- The static leaf is stateless, so it stays per mount, unlike a CodeArtifact provider.
    let statics = [(eco, staticProviderFor token) | (eco, MirrorStatic token) <- creds]
    let caPlans = [(eco, ca) | (eco, MirrorCodeArtifact ca) <- creds]
    results <- traverse (initSharedCodeArtifact reporters) (codeArtifactIdentityGroups caPlans)
    let (initErrs, shared) = partitionEithers results
    if not (null initErrs)
        then pure (Left (concat initErrs))
        else pure (Right (CredentialProviders (Map.fromList (statics <> concat shared))))

-- One provider per distinct identity, fanned out to every ecosystem in the group, so
-- a shared domain carries one refresh schedule and one breaker rather than one per mount.
initSharedCodeArtifact :: CredentialReporters -> (CodeArtifactConfig, NonEmpty Ecosystem) -> IO (Either [BootError] [(Ecosystem, CredentialProvider)])
initSharedCodeArtifact reporters (caConfig, ecosystems) =
    tryAny (newCodeArtifactProvider reporters caConfig) <&> \case
        Left err -> Left [CodeArtifactMintFailed (displayExceptionT err)]
        Right provider -> Right [(eco, provider) | eco <- toList ecosystems]

{- | Group the mounts' resolved CodeArtifact identities, one group per distinct
'CodeArtifactConfig'. The mint's scope is the domain, not the repository endpoint, so
mounts in one domain share a provider, while a differing duration keeps its own.
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

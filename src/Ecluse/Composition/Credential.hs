-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's credential build: turn each active mount's __resolved__
mirror-write credential into a live, process-global 'CredentialProvider'.

== Global providers, per-mount reference

A 'Ecluse.Core.Credential.CredentialProvider' is the service's own cloud identity,
built __once__ here from the resolved config and held process-global; a mount
references one by its ecosystem and never holds its own. Which credential each mount
uses is no longer selected here: it is __derived from the mirror-target URL at config
load__ ('Ecluse.Config.MirrorCredential.resolveMirrorCredential') and carried on the
mount as a 'Ecluse.Config.MirrorCredential', so a CodeArtifact token can only ever be
minted for the domain the worker actually writes to (issue #808). This module just
realises that resolved plan:

* 'MirrorStatic' -- a stateless static provider from the operator-supplied token.
* 'MirrorCodeArtifact' -- the refresh\/cache wrapper around the CodeArtifact mint leaf
  ('newCodeArtifactProvider'), which mints once eagerly, so a misconfigured identity or
  a missing permission fails loudly here at boot as a 'CodeArtifactMintFailed'. AWS
  credentials are the ambient container\/task role (the standard chain), never an Écluse
  key.

Provider granularity follows the credential's real scope, not the mount count: a
CodeArtifact token is minted per domain, so mounts whose resolved CodeArtifact
identities coincide ('codeArtifactIdentityGroups') share one provider -- one eager boot
mint, one refresh schedule, one breaker -- while each still looks its provider up by its
own ecosystem.

The 'CredentialReporters' are handed to the refreshing CodeArtifact provider so its mint
breaker and refresh outcomes record to telemetry; the static provider never refreshes,
so they do not concern it. The composition root supplies the deferred reporters that go
live once the telemetry substrate exists. Failures aggregate as
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
    Config (..),
    MirrorCredential (..),
    MirrorTarget (..),
    Mount (..),
    MountRegistries (..),
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig, newCodeArtifactProvider)

{- | The process-global credential providers, keyed by the ecosystem they
serve. Built __once__ at the composition root from the resolved config; a
mount references one by ecosystem and never holds its own.

The keyset (see 'initializedEcosystems') is the boot-check's pure surface -- a mount
that names an ecosystem absent from it has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | Build the global credential providers from the resolved config, or the aggregated
boot errors that block them. Each active mount already carries its resolved
'MirrorCredential' (derived from its mirror-target URL at load), so this only realises
it: a 'MirrorStatic' becomes a stateless static provider; the 'MirrorCodeArtifact'
identities are grouped by domain and each built once, minting eagerly so a bad
identity, region, or permission is a fail-loud 'CodeArtifactMintFailed' here at boot
rather than a first-publish surprise.
-}
initCredentialProviders :: CredentialReporters -> Config -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reporters config = do
    let creds = [(eco, mtCredential (regMirrorTarget (mountRegistries mount))) | (eco, mount) <- Map.toList (configMounts config)]
    -- The static leaf is stateless, so it stays per mount; the CodeArtifact providers
    -- are built once per distinct resolved identity and fanned out to every ecosystem
    -- that resolved it.
    let statics = [(eco, staticProviderFor token) | (eco, MirrorStatic token) <- creds]
    let caPlans = [(eco, ca) | (eco, MirrorCodeArtifact ca) <- creds]
    results <- traverse (initSharedCodeArtifact reporters) (codeArtifactIdentityGroups caPlans)
    let (initErrs, shared) = partitionEithers results
    if not (null initErrs)
        then pure (Left (concat initErrs))
        else pure (Right (CredentialProviders (Map.fromList (statics <> concat shared))))

-- One shared CodeArtifact provider per distinct resolved identity: the generic
-- refresh/cache wrapper around the mint leaf is built once (minting once eagerly, a
-- throw rendered as a 'CodeArtifactMintFailed' boot error so it joins the aggregated
-- failure block) and fanned out to every ecosystem in the group, so a shared domain
-- carries one refresh schedule and one breaker rather than one per mount.
initSharedCodeArtifact :: CredentialReporters -> (CodeArtifactConfig, NonEmpty Ecosystem) -> IO (Either [BootError] [(Ecosystem, CredentialProvider)])
initSharedCodeArtifact reporters (caConfig, ecosystems) =
    tryAny (newCodeArtifactProvider reporters caConfig) <&> \case
        Left err -> Left [CodeArtifactMintFailed (displayExceptionT err)]
        Right provider -> Right [(eco, provider) | eco <- toList ecosystems]

{- | Group the mounts' resolved CodeArtifact identities: one group per distinct
'CodeArtifactConfig' (domain, owner, region, and the requested token duration),
carrying every ecosystem that resolved it. The mint's real scope is the domain,
not the repository endpoint, so ecosystems whose mirror targets live in one
domain legitimately share one provider; a differing duration is a different
requested credential and keeps its own. Pure, so the sharing decision is pinned
without touching AWS.
-}
codeArtifactIdentityGroups :: [(Ecosystem, CodeArtifactConfig)] -> [(CodeArtifactConfig, NonEmpty Ecosystem)]
codeArtifactIdentityGroups plans =
    Map.toAscList (Map.fromListWith (<>) [(ca, eco :| []) | (eco, ca) <- plans])

-- A static mirror-target write provider from an operator-supplied token.
staticProviderFor :: Secret -> CredentialProvider
staticProviderFor token = staticProvider AuthToken{authSecret = token, authExpiresAt = Nothing}

{- | The set of ecosystems that resolved to an initialised provider -- the
pure surface the boot-time credential-reference check reasons over.
-}
initializedEcosystems :: CredentialProviders -> Set Ecosystem
initializedEcosystems (CredentialProviders ps) = Map.keysSet ps

{- | Look up the initialised provider for an ecosystem, 'Nothing' when none is
initialised (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupProvider eco (CredentialProviders ps) = Map.lookup eco ps

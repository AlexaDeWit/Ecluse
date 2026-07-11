{- | The boot-error vocabulary of the composition root: every reason Écluse refuses
to start, and its operator-facing rendering.

Each case is a __fail-loud__ boot failure; the composition root aggregates them so a
single run reports every problem an operator must fix (see
@docs\/architecture\/configuration.md@ → "Validation"). The sibling composition
modules produce these -- credential resolution
("Ecluse.Composition.Credential"), queue-backend selection
("Ecluse.Composition.MirrorQueue"), and the mount\/publish wiring
("Ecluse.Composition") -- and this module is their shared spine, so it holds no
policy of its own beyond the rendering.
-}
module Ecluse.Composition.BootError (
    BootError (..),
    renderBootError,
    mountEnvKey,
) where

import Data.Text qualified as T

import Ecluse.Config (
    CredentialBackend,
    PolicyError,
    QueueBackend (SqsQueue),
    renderPolicyError,
 )
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Wire (renderWire)

{- | A reason the composition root refuses to start. Every case is a __fail-loud__
boot failure; they are aggregated so a single run reports every problem an
operator must fix.
-}
data BootError
    = -- | A rule policy did not resolve (surfaced by 'Ecluse.Config.loadConfig').
      PolicyBootError PolicyError
    | {- | A configured mount's ecosystem has no adapter wired, so it
      cannot be served (a loud miss, never a silent drop). Carries the ecosystem.
      -}
      MissingAdapter Ecosystem
    | {- | A mount names a credential backend with no initialised provider. Carries
      the ecosystem of the mount and the unresolved backend.
      -}
      UnresolvedCredential Ecosystem CredentialBackend
    | {- | The configured mirror-queue backend has no implementation compiled into
      this binary, so no queue can be built for it. Carries the unavailable backend.
      An honest refusal -- never a silent fall-through to a different backend.
      -}
      QueueProviderUnavailable QueueBackend
    | {- | The SQS mirror-queue backend was selected but no AWS region was supplied
      (@AWS_REGION@), so the queue cannot be scoped to a region.
      -}
      QueueRegionMissing
    | {- | A cloud mirror-queue backend (e.g. @sqs@) was selected but no
      @ECLUSE_QUEUE_URL@ was supplied, so there is no queue to send jobs to. The
      in-memory backend does not raise this -- it has no external queue.
      -}
      QueueUrlMissing QueueBackend
    | {- | The configured SQS endpoint override (@AWS_ENDPOINT_URL_SQS@ \/
      @AWS_ENDPOINT_URL@) is not a parseable endpoint URL. Carries the offending value.
      -}
      QueueEndpointMalformed Text
    | {- | The selected mirror-target credential provider has no implementation
      compiled into this binary. Carries the unavailable provider. An honest refusal,
      never a silent fall-through.
      -}
      MirrorCredentialProviderUnavailable CredentialBackend
    | {- | A required CodeArtifact input for the mirror-target token could not be
      resolved from either its explicit key or the mirror-target host. Carries the
      name of the key the operator must set.
      -}
      CodeArtifactConfigMissing Text
    | {- | A CodeArtifact input resolved but is malformed (e.g. a domain owner that is
      not a 12-digit AWS account id). Carries the key and a reason.
      -}
      CodeArtifactConfigInvalid Text Text
    | {- | The eager boot-time CodeArtifact mint threw -- a transient AWS error (worth a
      retry) or a permanent one (a bad domain\/region or missing permission, to be
      fixed). Carries the rendered exception so the cause is legible and aggregated.
      -}
      CodeArtifactMintFailed Text
    | {- | A publication target was configured (@ECLUSE_PUBLICATION_TARGET@) but no
      publish-scope allow-list (@ECLUSE_PUBLISH_SCOPES@) was supplied, so the anti-shadowing
      guard would have nothing to enforce. Refused at boot rather than defaulting to an
      empty allow-list (which would deny every publish) or an open one (which would let
      a client shadow any public name).
      -}
      PublishScopesMissing Ecosystem
    | {- | A static publish credential (@ECLUSE_PUBLICATION_TARGET_TOKEN@) was configured
      without a verifiable inbound edge (@ECLUSE_AUTH_TOKEN@). Écluse would otherwise
      substitute its own standing write credential for a publishing caller who forwards
      none, so an unauthenticated request could publish within the configured scopes
      under Écluse's own identity. Refused at boot so an internal publish credential
      paired with an open edge is unrepresentable -- the write-side counterpart of the
      fail-closed read identity.
      -}
      PublishStaticCredentialNeedsEdge Ecosystem
    deriving stock (Eq, Show)

-- | Render a 'BootError' as a human-facing line for the aggregated failure block.
renderBootError :: BootError -> Text
renderBootError = \case
    PolicyBootError err -> renderPolicyError err
    MissingAdapter eco ->
        "mount " <> ecosystemName eco <> " has no adapter wired in this build"
    UnresolvedCredential eco backend ->
        "mount "
            <> ecosystemName eco
            <> " names credential source "
            <> renderWire backend
            <> ", not initialised in this build"
    QueueProviderUnavailable backend ->
        "mirror queue provider "
            <> renderWire backend
            <> " is not available in this build"
    QueueRegionMissing ->
        "mirror queue provider "
            <> renderWire SqsQueue
            <> " requires AWS_REGION to be set"
    QueueUrlMissing backend ->
        "mirror queue provider "
            <> renderWire backend
            <> " requires ECLUSE_QUEUE_URL to be set"
    QueueEndpointMalformed url ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS / AWS_ENDPOINT_URL) is not a valid endpoint URL: " <> url
    MirrorCredentialProviderUnavailable backend ->
        "mirror-target credential provider "
            <> renderWire backend
            <> " is not available in this build"
    CodeArtifactConfigMissing key ->
        "mirror-target credential provider codeartifact requires "
            <> key
            <> " (set it explicitly, or use a CodeArtifact ECLUSE_MIRROR_TARGET it can be parsed from)"
    CodeArtifactConfigInvalid key reason ->
        "mirror-target credential provider codeartifact: " <> key <> " is invalid (" <> reason <> ")"
    CodeArtifactMintFailed detail ->
        "mirror-target credential provider codeartifact failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry; a permanent one -- bad domain/region or missing permission -- must be fixed)"
    PublishScopesMissing eco ->
        mountEnvKey eco "PUBLICATION_TARGET" <> " is set but " <> mountEnvKey eco "PUBLISH_SCOPES" <> " is empty: a publication target needs a publish-scope allow-list (e.g. @acme) for the anti-shadowing guard."
    PublishStaticCredentialNeedsEdge eco ->
        mountEnvKey eco "PUBLICATION_TARGET_TOKEN" <> " is set but ECLUSE_AUTH_TOKEN is not: a static publish credential needs a verifiable inbound edge."

{- | The full environment key of a mount-scoped setting
(@ECLUSE_MOUNTS__{ECOSYSTEM}__{KEY}@), as the operator must set it -- shared by the
boot-error renderings above and the credential resolution that names missing keys
("Ecluse.Composition.Credential").
-}
mountEnvKey :: Ecosystem -> Text -> Text
mountEnvKey eco key = "ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName eco) <> "__" <> key

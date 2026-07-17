-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
    PolicyError,
    renderPolicyError,
 )
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

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
    | {- | A mount has no initialised mirror-write provider. The credential is derived
      from the mirror-target URL and realised for every active mount, so this is a
      total safety net rather than a reachable operator misconfiguration. Carries the
      ecosystem of the mount.
      -}
      UnresolvedCredential Ecosystem
    | {- | The queue URL names a backend (by its shape) that has no implementation
      compiled into this binary, so no queue can be built for it. Carries the
      provider's name. An honest refusal -- never a silent fall-through to a
      different backend.
      -}
      QueueProviderUnavailable Text
    | {- | An SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is set but no
      @AWS_REGION@ was supplied: an emulator or VPC endpoint does not carry a
      region in its host, so the ambient region must scope it. A real SQS queue
      URL carries its own region and never raises this.
      -}
      QueueRegionMissing
    | {- | @ECLUSE_QUEUE_URL@ is set but its shape names no backend this binary
      knows, so refusing is the only honest move (guessing a backend would send
      mirror jobs somewhere the operator did not point at). Carries the value.
      -}
      QueueUrlUnrecognised Text
    | {- | The configured SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is not a
      parseable endpoint URL. Carries the offending value.
      -}
      QueueEndpointMalformed Text
    | {- | The eager boot-time CodeArtifact mint threw -- a transient AWS error (worth a
      retry) or a permanent one (a bad domain\/region or missing permission, to be
      fixed). Carries the rendered exception so the cause is legible and aggregated.
      -}
      CodeArtifactMintFailed Text
    | {- | A publication target was configured (@ECLUSE_MOUNTS__{ECOSYSTEM}__PUBLICATION_TARGET@)
      but no publish allow-list (@ECLUSE_MOUNTS__{ECOSYSTEM}__PUBLISH_ALLOW@) was
      supplied, so the anti-shadowing
      guard would have nothing to enforce. Refused at boot rather than defaulting to an
      empty allow-list (which would deny every publish) or an open one (which would let
      a client shadow any public name).
      -}
      PublishAllowMissing Ecosystem
    | {- | A static publish credential (@ECLUSE_MOUNTS__{ECOSYSTEM}__PUBLICATION_TARGET_TOKEN@)
      was configured without a verifiable inbound edge (@ECLUSE_AUTH_TOKEN@). Écluse would otherwise
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
    UnresolvedCredential eco ->
        "mount "
            <> ecosystemName eco
            <> " has no initialised mirror-write credential in this build"
    QueueProviderUnavailable provider ->
        "mirror queue provider "
            <> provider
            <> " (named by the ECLUSE_QUEUE_URL shape) is not available in this build"
    QueueRegionMissing ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is set but AWS_REGION is not: an emulator or VPC endpoint does not carry its region, so AWS_REGION must scope it"
    QueueUrlUnrecognised url ->
        "ECLUSE_QUEUE_URL names no queue backend this build knows: "
            <> url
            <> " (expected an SQS queue URL, https://sqs.{region}.amazonaws.com/{account}/{queue}, or a Pub/Sub topic resource, projects/{project}/topics/{topic}; unset it to run the bounded in-memory queue)"
    QueueEndpointMalformed url ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is not a valid endpoint URL: " <> url
    CodeArtifactMintFailed detail ->
        "mirror-target credential provider codeartifact failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry; a permanent one -- bad domain/region or missing permission -- must be fixed)"
    PublishAllowMissing eco ->
        mountEnvKey eco "PUBLICATION_TARGET" <> " is set but " <> mountEnvKey eco "PUBLISH_ALLOW" <> " is empty: a publication target needs a publish allow-list (for npm, scopes such as @acme) for the anti-shadowing guard."
    PublishStaticCredentialNeedsEdge eco ->
        mountEnvKey eco "PUBLICATION_TARGET_TOKEN" <> " is set but ECLUSE_AUTH_TOKEN is not: a static publish credential needs a verifiable inbound edge."

{- | The full environment key of a mount-scoped setting
(@ECLUSE_MOUNTS__{ECOSYSTEM}__{KEY}@), as the operator must set it -- shared by the
boot-error renderings above and the credential resolution that names missing keys
("Ecluse.Composition.Credential").
-}
mountEnvKey :: Ecosystem -> Text -> Text
mountEnvKey eco key = "ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName eco) <> "__" <> key

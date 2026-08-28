-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot-error vocabulary of the composition root: every reason Écluse refuses to start, and
its operator-facing rendering.

Each case is a __fail-loud__ boot failure, and the root aggregates them, so a single run reports
every problem an operator must fix (see @docs\/architecture\/configuration.md@ → "Validation").
This module is the shared spine of the three modules that produce them, so it holds no policy of
its own beyond the rendering.
-}
module Ecluse.Composition.BootError (
    BootError (..),
    renderBootError,
) where

import Data.Text qualified as T

import Ecluse.Config (
    PolicyError,
    renderPolicyError,
 )
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

{- | A reason the composition root refuses to start. The root aggregates them, so a
single run reports every problem an operator must fix.
-}
data BootError
    = -- | A rule policy did not resolve (surfaced by 'Ecluse.Config.loadConfig').
      PolicyBootError PolicyError
    | {- | A configured mount's ecosystem has no adapter wired, so Écluse cannot serve it.
      A loud miss, never a silent drop.
      -}
      MissingAdapter Ecosystem
    | {- | A mount has no initialised mirror-write provider. Every active mount derives its
      credential from its mirror target, so this is a safety net, not a reachable state.
      -}
      UnresolvedCredential Ecosystem
    | {- | The queue URL's shape names a backend this binary compiled no implementation for.
      An honest refusal, never a silent fall-through to a different backend.
      -}
      QueueProviderUnavailable Text
    | {- | An SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is set but @AWS_REGION@ is not.
      An emulator or VPC endpoint carries no region in its host, so the ambient one must scope it.
      -}
      QueueRegionMissing
    | {- | @ECLUSE_QUEUE__URL@ is set but its shape names no backend this binary knows. Guessing
      one would send mirror jobs somewhere the operator did not point at. Carries the value.
      -}
      QueueUrlUnrecognised Text
    | {- | The configured SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is not a parseable
      endpoint URL. It can carry a credential, so the value stays redacted behind the secret.
      -}
      QueueEndpointMalformed Secret
    | {- | The S3 advisory client's endpoint override (@AWS_ENDPOINT_URL@) is not a parseable
      endpoint URL. Refused rather than dropped, so a typo never silently dials real AWS.
      -}
      AwsEndpointMalformed Secret
    | {- | The eager boot-time CodeArtifact mint threw. Carries the rendered exception, which
      tells a transient AWS error from a permanent one to fix.
      -}
      CodeArtifactMintFailed Text
    | {- | A publication target is set with no publish allow-list, so the anti-shadowing guard has
      nothing to enforce. An empty list would deny every publish, an open one shadow any name.
      -}
      PublishAllowMissing Ecosystem
    | {- | A static publish credential is set without a verifiable inbound edge
      (@ECLUSE_SERVER__AUTH_TOKEN@). An unauthenticated request could otherwise publish as Écluse.
      -}
      PublishStaticCredentialNeedsEdge Ecosystem
    | {- | An explicit memory override breaks the combined memory-plan invariant even after every
      tenant shed to its minimum. A computed plan degrades and boots, an operator claim does not.
      -}
      MemoryPlanOverrideUnsafe [Text]
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
            <> " (named by the ECLUSE_QUEUE__URL shape) is not available in this build"
    QueueRegionMissing ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is set but AWS_REGION is not: an emulator or VPC endpoint does not carry its region, so AWS_REGION must scope it"
    QueueUrlUnrecognised url ->
        "ECLUSE_QUEUE__URL names no queue backend this build knows: "
            <> url
            <> " (expected an SQS queue URL, https://sqs.{region}.amazonaws.com/{account}/{queue}, or a Pub/Sub topic resource, projects/{project}/topics/{topic}; unset it to run the bounded in-memory queue)"
    -- Both endpoint values can carry a credential, so each reason names its variable,
    -- never the URL.
    QueueEndpointMalformed{} ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is not a valid endpoint URL"
    AwsEndpointMalformed{} ->
        "the AWS endpoint override (AWS_ENDPOINT_URL) is not a valid endpoint URL"
    CodeArtifactMintFailed detail ->
        "mirror-target credential provider codeartifact failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry. A permanent one, such as a bad domain or region or a missing permission, must be fixed)"
    PublishAllowMissing eco ->
        mountKeyRef eco "publicationTarget" <> " is set but " <> mountKeyRef eco "publishAllow" <> " is empty: a publication target needs a publish allow-list (for npm, scopes such as @acme) for the anti-shadowing guard."
    PublishStaticCredentialNeedsEdge eco ->
        mountKeyRef eco "publicationTargetToken" <> " is set but ECLUSE_SERVER__AUTH_TOKEN is not: a static publish credential needs a verifiable inbound edge."
    MemoryPlanOverrideUnsafe details ->
        "memory plan refused: " <> T.intercalate "; " details

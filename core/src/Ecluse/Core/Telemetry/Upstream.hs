-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Public HTTP attempt kinds and raw outcomes shared by observation and telemetry.
module Ecluse.Core.Telemetry.Upstream (
    PublicReadKind (..),
    publicReadKindName,
    ExchangeOutcome (..),
    exchangeOutcomeName,
) where

-- | The public operation that caused an HTTP attempt.
data PublicReadKind = Listing | SelectedRead | Artifact | Companion | WorkerMetadata | WorkerArtifact
    deriving stock (Eq, Show)

-- | The bounded wire name of a public request kind.
publicReadKindName :: PublicReadKind -> Text
publicReadKindName = \case
    Listing -> "listing"
    SelectedRead -> "selected_read"
    Artifact -> "artifact"
    Companion -> "companion"
    WorkerMetadata -> "worker_metadata"
    WorkerArtifact -> "worker_artifact"

-- | A response status or an interruption before response headers arrive.
data ExchangeOutcome = ResponseStatus Int | TransportFailure | Cancelled
    deriving stock (Eq, Show)

-- | Keep exact HTTP codes distinct from transport failure and cancellation.
exchangeOutcomeName :: ExchangeOutcome -> Text
exchangeOutcomeName = \case
    ResponseStatus code -> show code
    TransportFailure -> "transport_failure"
    Cancelled -> "cancelled"

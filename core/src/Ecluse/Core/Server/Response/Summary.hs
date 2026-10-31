-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Listing refusal text, with a compact form that discards per-entry diagnostics.
module Ecluse.Core.Server.Response.Summary (
    detailedRejections,
    summarisedRejections,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Server.Response (
    RejectReason (..),
    Rejection (..),
    RuleName (..),
    ServeDecision (..),
 )

-- | Join full rejection messages for responses that keep per-entry diagnostics.
detailedRejections :: [ServeDecision] -> Text
detailedRejections decisions = case mapMaybe rejectionText decisions of
    [] -> emptyListing
    reasons -> T.intercalate "; " reasons
  where
    rejectionText Admit = Nothing
    rejectionText (Reject rejection) = Just (rejectionMessage rejection)

-- | Count rejection entries by typed cause, without reading per-version messages or policy reasons.
summarisedRejections :: [ServeDecision] -> Text
summarisedRejections decisions
    | Map.null counts = emptyListing
    | otherwise = T.intercalate ". " (map renderGroup (Map.toAscList counts))
  where
    counts = foldl' countDecision Map.empty decisions
    countDecision acc Admit = acc
    countDecision acc (Reject rejection) = Map.insertWith (+) (causeOf (rejectionReason rejection)) (1 :: Int) acc
    renderGroup (cause, count) = causeText cause <> " (rejection entries: " <> show count <> ")"

data Cause
    = Policy RuleName
    | IntegrityMissing
    | IntegrityBelowFloor
    | VettingUnavailable
    | InvalidUpstream
    deriving stock (Eq, Ord)

causeOf :: RejectReason -> Cause
causeOf = \case
    ByPolicy (RuleName name) _ -> Policy (RuleName (T.copy name))
    MissingIntegrity -> IntegrityMissing
    BelowIntegrityFloor -> IntegrityBelowFloor
    Unavailable{} -> VettingUnavailable
    UpstreamInvalid -> InvalidUpstream

causeText :: Cause -> Text
causeText = \case
    Policy (RuleName name) -> name <> ": " <> policyExplanation name
    IntegrityMissing -> "MissingIntegrity: artifacts lack an integrity digest"
    IntegrityBelowFloor -> "BelowIntegrityFloor: artifact digests fall below the configured integrity floor"
    VettingUnavailable -> "Unavailable: versions could not be vetted"
    InvalidUpstream -> "UpstreamInvalid: an upstream document names a different package"

policyExplanation :: Text -> Text
policyExplanation = \case
    "BlockedByDefault" -> "no allow rule matched"
    "DenyInstallTimeExecution" -> "installation runs code"
    "DenyExternalDependencies" -> "dependencies install from outside the registry"
    "DenyIfKnownMalicious" -> "known malicious advisories affect the versions"
    "DenyIfCve" -> "advisories deny admission under the configured severity policy"
    "DenyIfEpss" -> "advisories deny admission under the configured exploit probability policy"
    "DenyIfYanked" -> "the versions are yanked"
    "DenyIfDeprecated" -> "the versions are deprecated"
    "DenyByIdentity" -> "the operator revoked the package identity"
    _ -> "the policy rule denied admission"

emptyListing :: Text
emptyListing = "no versions are available for this package"

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Map policy and upstream outcomes to HTTP statuses. Artifact requests use one outcome,
while packuments choose a status from the surviving versions. Ecosystem contracts own response bodies.
-}
module Ecluse.Core.Server.Response (
    -- * Serve outcomes
    ServeDecision (..),
    Rejection (..),
    RejectReason (..),
    Transience (..),
    RetryAfter (..),
    RuleName (..),
    rejectUnavailable,
    serveDecisionOf,

    -- * Concrete-artifact status
    ArtifactStatus (..),
    artifactStatus,
    artifactHttpStatus,

    -- * Packument status (over the merged survivor set)
    PackumentStatus (..),
    packumentStatus,
    longestRetry,

    -- * Denial help text
    HelpMessage,
    mkHelpMessage,
    appendHelp,

    -- * A refusal's two parts
    Refusal (..),
    mkRefusal,
    renderRefusal,
    refusalHelpBytes,
) where

import Data.Semigroup (Max (Max, getMax))
import Data.Text qualified as T
import Network.HTTP.Types (Status, status200, status403, status404, status500, status502, status503)

import Ecluse.Core.Rules.Render (renderDecision)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    Reason,
    RetryAfter (..),
    RuleEvidence,
    Transience (..),
 )

-- | The outcome of deciding a request: serve it, or refuse it with a reason. Every client-facing reply renders one.
data ServeDecision
    = -- | Serve the request (the @200@ stream for an artifact).
      Admit
    | -- | Refuse the request, with the reason and a client-facing message.
      Reject Rejection
    deriving stock (Eq, Show)

-- | A refusal: /why/ the request was refused, and an intuitive message for the client.
data Rejection = Rejection
    { rejectionReason :: RejectReason
    -- ^ The cause of the refusal, which decides the status.
    , rejectionMessage :: Text
    -- ^ The client-facing explanation (the rendered decision, or the cause).
    }
    deriving stock (Eq, Show)

-- | Why a request was refused. A policy refusal is final for this request. An /inability to decide/ carries a 'Transience' for @503@ or @500@.
data RejectReason
    = -- | A rule denied the version (deny-by-default included): the deciding rule, and the reasons the decision gave.
      ByPolicy RuleName [Reason]
    | -- | The version could not be vetted. Refuse it, with transience indicating whether a retry can help.
      Unavailable Transience
    | -- | A public artifact lacks a digest, so admission cannot verify its bytes. Trusted private artifacts are exempt.
      MissingIntegrity
    | -- | A public artifact's strongest digest falls below the configured floor. Trusted private artifacts are exempt.
      BelowIntegrityFloor
    | -- | An upstream reply was invalid or unusable, such as a document that names a different package.
      UpstreamInvalid
    deriving stock (Eq, Show)

-- | The name of the rule that decided a refusal, for the audit trail and the denial body.
newtype RuleName = RuleName Text
    deriving stock (Eq, Ord, Show)

-- | Project a rules 'Decision' into a serve outcome. A version no rule could vet is refused as 'Unavailable'.
serveDecisionOf :: RuleEvidence -> Decision -> ServeDecision
-- Strict in the evidence, so a caller's 'completeEvidence' is read as two fields and never built.
serveDecisionOf !evidence decision = case decision of
    Admitted{} -> Admit
    Blocked name _ reason -> Reject (rejectAs (ByPolicy (RuleName name) [reason]))
    BlockedByDefault reasons -> Reject (rejectAs (ByPolicy (RuleName "BlockedByDefault") reasons))
    Undecidable transience _ -> rejectUnavailable transience rendered
  where
    rendered = renderDecision evidence decision

    rejectAs :: RejectReason -> Rejection
    rejectAs reason = Rejection reason rendered

-- | Refuse a request that could not be decided. A caller states the 'Transience', and 'artifactStatus' picks the status.
rejectUnavailable :: Transience -> Text -> ServeDecision
rejectUnavailable transience message = Reject (Rejection (Unavailable transience) message)

-- | The HTTP status a __concrete-artifact__ request renders to. 'PackumentStatus' chooses a packument's over its survivors.
data ArtifactStatus
    = -- | @200@: admitted, so the proxy streams the artifact.
      Ok
    | -- | @403@: refused by policy. The route's response contract shapes the body.
      Forbidden
    | -- | @503@: a transient inability to decide. A known 'RetryAfter' becomes the header.
      Unavailable' (Maybe RetryAfter)
    | -- | @500@: a permanent or internal inability to decide. Not retryable.
      ServerError
    | -- | @502@: an upstream reply was invalid or unusable.
      BadGateway
    | -- | @404@: the upstream did not have the artifact (forwarded miss).
      NotFound
    deriving stock (Eq, Show)

-- | A serve outcome's concrete-artifact status: @503@ only where it will resolve. It never yields 'NotFound'.
artifactStatus :: ServeDecision -> ArtifactStatus
artifactStatus = \case
    Admit -> Ok
    Reject rej -> case rejectionReason rej of
        ByPolicy{} -> Forbidden
        MissingIntegrity -> Forbidden
        BelowIntegrityFloor -> Forbidden
        Unavailable (WillResolve retryAfter) -> Unavailable' retryAfter
        Unavailable WontResolve -> ServerError
        UpstreamInvalid -> BadGateway

-- | The HTTP status an 'ArtifactStatus' renders as.
artifactHttpStatus :: ArtifactStatus -> Status
artifactHttpStatus = \case
    Ok -> status200
    Forbidden -> status403
    Unavailable'{} -> status503
    ServerError -> status500
    BadGateway -> status502
    NotFound -> status404

-- | The HTTP status a __packument__ request renders to. There is no @404@: absence is decided before the merge.
data PackumentStatus
    = -- | @200@: at least one version survived, so the proxy serves the merged, filtered packument.
      PackumentOk
    | -- | @403@: no version survived and every exclusion was a policy denial. The response body collects the denial reasons.
      PackumentForbidden
    | -- | @503@: no version survived and an exclusion can recover. A suggested delay becomes @Retry-After@.
      PackumentUnavailable (Maybe RetryAfter)
    | -- | @502@: no valid origin remained and an upstream reply was invalid or unusable.
      PackumentBadGateway
    | -- | @500@: no version survived, no exclusion is retryable, and one is a permanent or internal inability to decide. Retrying cannot help.
      PackumentServerError
    deriving stock (Eq, Show)

-- | With no survivor the most recoverable cause wins (@503@ over @502@, as an origin may yet answer). An empty input is a @403@.
packumentStatus :: [ServeDecision] -> PackumentStatus
packumentStatus decisions
    | tallyAdmit tally = PackumentOk
    | not (null willResolveDelays) = PackumentUnavailable (longestRetry willResolveDelays)
    | tallyUpstreamInvalid tally = PackumentBadGateway
    | tallyWontResolve tally = PackumentServerError
    | otherwise = PackumentForbidden
  where
    -- One strict pass collects every signal the guards weigh, so the all-denied path walks
    -- the exclusions once, not once per guard.
    tally :: PackumentTally
    tally = foldl' weighDecision (PackumentTally False [] False False) decisions

    willResolveDelays :: [Maybe RetryAfter]
    willResolveDelays = tallyWillResolveDelays tally

-- A deny-by-default cause (policy or admission refusal) leaves no signal of its own,
-- because an empty tally is exactly the @403@ floor.
weighDecision :: PackumentTally -> ServeDecision -> PackumentTally
weighDecision acc = \case
    Admit -> acc{tallyAdmit = True}
    Reject rej -> case rejectionReason rej of
        Unavailable (WillResolve delay) ->
            acc{tallyWillResolveDelays = delay : tallyWillResolveDelays acc}
        UpstreamInvalid -> acc{tallyUpstreamInvalid = True}
        Unavailable WontResolve -> acc{tallyWontResolve = True}
        ByPolicy{} -> acc
        MissingIntegrity -> acc
        BelowIntegrityFloor -> acc

-- The signals 'packumentStatus' weighs, accumulated in one pass. Strict fields keep the tally from
-- building thunks across a large survivor set.
data PackumentTally = PackumentTally
    { tallyAdmit :: Bool
    -- ^ At least one 'Admit' was seen, so the merged document has a survivor.
    , tallyWillResolveDelays :: [Maybe RetryAfter]
    -- ^ The suggested delay of every transient ('WillResolve') exclusion.
    , tallyUpstreamInvalid :: Bool
    -- ^ An upstream reply was invalid or unusable.
    , tallyWontResolve :: Bool
    -- ^ An exclusion was a permanent ('WontResolve') inability to decide.
    }

-- | The longest suggested 'RetryAfter' among transient causes, or 'Nothing' when none suggested one.
longestRetry :: [Maybe RetryAfter] -> Maybe RetryAfter
longestRetry = fmap getMax . foldMap (fmap Max)

-- | An operator-configured message appended to every denial. Stored trimmed, so an all-blank value contributes nothing.
newtype HelpMessage = HelpMessage Text
    deriving stock (Eq, Show)

-- | Build a 'HelpMessage', trimming surrounding whitespace.
mkHelpMessage :: Text -> HelpMessage
mkHelpMessage = HelpMessage . T.strip

-- | Append a non-blank operator 'HelpMessage' to a denial message, separated by a single space.
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help = renderRefusal . mkRefusal help

-- | A refusal's text in two parts, so an ecosystem without an envelope for both still keeps the help message.
data Refusal = Refusal
    { refusalReason :: Text
    -- ^ Why Écluse refused, in its own words. Always present.
    , refusalHelp :: Maybe Text
    -- ^ The operator's help message, absent when none is configured or it is blank.
    }
    deriving stock (Eq, Show)

-- | Pair a decided reason with the mount's configured help message, if it has a non-blank one.
mkRefusal :: Maybe HelpMessage -> Text -> Refusal
mkRefusal help message = Refusal message (nonBlankHelp =<< help)
  where
    nonBlankHelp (HelpMessage h) = if T.null h then Nothing else Just h

-- | The refusal as one line: the reason, with the help message appended after a single space.
renderRefusal :: Refusal -> Text
renderRefusal (Refusal reason help) = maybe reason ((T.strip reason <> " ") <>) help

-- | The help message alone as body bytes, absent when none is configured.
refusalHelpBytes :: Refusal -> Maybe LByteString
refusalHelpBytes = fmap (fromStrict . encodeUtf8) . refusalHelp

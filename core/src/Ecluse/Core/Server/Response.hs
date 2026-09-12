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
) where

import Data.Semigroup (Max (Max, getMax))
import Data.Text qualified as T
import Network.HTTP.Types (Status, status200, status403, status404, status500, status503)

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Rules (renderDecision)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    RetryAfter (..),
    Transience (..),
    completeEvidence,
 )

{- | The outcome of deciding a request: serve it, or refuse it with a reason. Every client-facing
reply renders one of these.
-}
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

{- | Why a request was refused. A policy refusal is final for this request. An unavailability is
an /inability to decide/, whose 'Transience' separates a retryable @503@ from a terminal @500@.
-}
data RejectReason
    = {- | A rule denied the version (including deny-by-default). The 'RuleName'
      is the rule that decided, for the audit trail and the denial body.
      -}
      ByPolicy RuleName
    | -- | The version could not be vetted. Refuse it, with transience indicating whether a retry can help.
      Unavailable Transience
    | {- | A public artifact lacks a digest, so admission cannot verify its bytes and refuses with @403@.
      Trusted private artifacts are exempt.
      -}
      MissingIntegrity
    | {- | A public artifact's strongest digest falls below the configured floor, so admission refuses with @403@.
      Trusted private artifacts are exempt.
      -}
      BelowIntegrityFloor
    | {- | An upstream packument names a different package and cannot enter the merge.
      When no valid origin remains, the packument request returns @502@.
      -}
      UpstreamInvalid
    deriving stock (Eq, Show)

{- | The name of the rule that decided a refusal, carried for the audit trail and the denial
body.
-}
newtype RuleName = RuleName Text
    deriving stock (Eq, Ord, Show)

{- | Project a rules 'Decision' into a serve outcome. An 'Undecidable' decision rejects as
'Unavailable', which is fail-closed: a version no rule could vet is never admitted.
-}
serveDecisionOf :: PackageDetails -> Decision -> ServeDecision
serveDecisionOf pd decision = case decision of
    Admitted{} -> Admit
    Blocked name _ _ -> Reject (rejectAs (ByPolicy (RuleName name)))
    BlockedByDefault{} -> Reject (rejectAs (ByPolicy (RuleName "BlockedByDefault")))
    Undecidable transience _ -> rejectUnavailable transience rendered
  where
    rendered = renderDecision (completeEvidence pd) decision

    rejectAs :: RejectReason -> Rejection
    rejectAs reason = Rejection reason rendered

{- | Refuse a request that could not be decided. The 'Transience' it carries is what
'artifactStatus' renders as a @503@ or a @500@, so a caller states that rather than a status.
-}
rejectUnavailable :: Transience -> Text -> ServeDecision
rejectUnavailable transience message = Reject (Rejection (Unavailable transience) message)

{- | The HTTP status a __concrete-artifact__ request renders to. A packument request has no single
status, because the pipeline chooses one over the survivors: 'PackumentStatus' models that.
-}
data ArtifactStatus
    = -- | @200@: admitted, so the proxy streams the artifact.
      Ok
    | -- | @403@: refused by policy. The route's response contract shapes the body.
      Forbidden
    | {- | @503@: a transient inability to decide. The 'RetryAfter', if known,
      becomes the @Retry-After@ header.
      -}
      Unavailable' (Maybe RetryAfter)
    | -- | @500@: a permanent or internal inability to decide. Not retryable.
      ServerError
    | -- | @404@: the upstream did not have the artifact (forwarded miss).
      NotFound
    deriving stock (Eq, Show)

{- | Map a serve outcome to its concrete-artifact status: @503@ only where it will resolve, so a
'WontResolve' unavailability is a @500@. An upstream @404@ is no serve decision and never appears.
-}
artifactStatus :: ServeDecision -> ArtifactStatus
artifactStatus = \case
    Admit -> Ok
    Reject rej -> case rejectionReason rej of
        ByPolicy{} -> Forbidden
        MissingIntegrity -> Forbidden
        BelowIntegrityFloor -> Forbidden
        Unavailable (WillResolve retryAfter) -> Unavailable' retryAfter
        Unavailable WontResolve -> ServerError
        -- The artifact path never validates a packument name, so this cause does not arise here. A
        -- misbehaving upstream on this path is an internal inability to serve.
        UpstreamInvalid -> ServerError

-- | The HTTP status an 'ArtifactStatus' renders as. Pure and total.
artifactHttpStatus :: ArtifactStatus -> Status
artifactHttpStatus = \case
    Ok -> status200
    Forbidden -> status403
    Unavailable'{} -> status503
    ServerError -> status500
    NotFound -> status404

{- | The HTTP status a __packument__ request renders to, chosen over the merged survivor set. There
is no @404@: the package exists, and a genuine absence is decided before the merge.
-}
data PackumentStatus
    = -- | @200@: at least one version survived, so the proxy serves the merged, filtered packument.
      PackumentOk
    | {- | @403@: no version survived and every exclusion was a policy denial. The
      response body collects the denial reasons.
      -}
      PackumentForbidden
    | {- | @503@: no version survived and an exclusion can recover.
      A suggested delay becomes the @Retry-After@ header.
      -}
      PackumentUnavailable (Maybe RetryAfter)
    | {- | @502@: no valid origin remained and an upstream packument named a different package.
      This gateway fault differs from absence or a retryable outage.
      -}
      PackumentBadGateway
    | {- | @500@: no version survived, no exclusion is retryable, and at least one is
      a permanent or internal inability to decide. Retrying cannot help.
      -}
      PackumentServerError
    deriving stock (Eq, Show)

{- | A packument's status from the per-version outcomes: with no survivor the most recoverable cause
wins, @502@ under @503@ as a transient origin may yet answer, and an empty input is a @403@.
-}
packumentStatus :: [ServeDecision] -> PackumentStatus
packumentStatus decisions
    | tallyAdmit tally = PackumentOk
    | not (null willResolveDelays) = PackumentUnavailable (longestRetry willResolveDelays)
    | tallyUpstreamInvalid tally = PackumentBadGateway
    | tallyWontResolve tally = PackumentServerError
    | otherwise = PackumentForbidden
  where
    -- One strict pass over the outcomes collects every signal the guards weigh, so the
    -- all-denied path walks the exclusions once, not once per guard.
    tally :: PackumentTally
    tally = foldl' weigh (PackumentTally False [] False False) decisions

    willResolveDelays :: [Maybe RetryAfter]
    willResolveDelays = tallyWillResolveDelays tally

    weigh :: PackumentTally -> ServeDecision -> PackumentTally
    weigh acc = \case
        Admit -> acc{tallyAdmit = True}
        Reject rej -> case rejectionReason rej of
            Unavailable (WillResolve delay) ->
                acc{tallyWillResolveDelays = delay : tallyWillResolveDelays acc}
            UpstreamInvalid -> acc{tallyUpstreamInvalid = True}
            Unavailable WontResolve -> acc{tallyWontResolve = True}
            -- A deny-by-default cause (policy or admission refusal) leaves no signal
            -- of its own. An empty tally is exactly the @403@ floor.
            ByPolicy{} -> acc
            MissingIntegrity -> acc
            BelowIntegrityFloor -> acc

{- | The signals 'packumentStatus' weighs, accumulated in one pass. The fields are strict
('StrictData'), so the tally does not thunk across a large survivor set.
-}
data PackumentTally = PackumentTally
    { tallyAdmit :: Bool
    -- ^ At least one 'Admit' was seen, so the merged document has a survivor.
    , tallyWillResolveDelays :: [Maybe RetryAfter]
    -- ^ The suggested delay of every transient ('WillResolve') exclusion.
    , tallyUpstreamInvalid :: Bool
    -- ^ A responding upstream returned a packument naming a different package.
    , tallyWontResolve :: Bool
    -- ^ An exclusion was a permanent ('WontResolve') inability to decide.
    }

{- | The longest suggested 'RetryAfter' among transient causes, or 'Nothing' when
none of them suggested a delay.
-}
longestRetry :: [Maybe RetryAfter] -> Maybe RetryAfter
longestRetry = fmap getMax . foldMap (fmap Max)

{- | An operator-configured message appended to every denial, typically where to ask for help.
Stored trimmed, so an all-blank value contributes nothing.
-}
newtype HelpMessage = HelpMessage Text
    deriving stock (Eq, Show)

-- | Build a 'HelpMessage', trimming surrounding whitespace.
mkHelpMessage :: Text -> HelpMessage
mkHelpMessage = HelpMessage . T.strip

{- | Append a non-blank operator 'HelpMessage' to a denial message, separated by a single space.
A blank or absent help message contributes nothing.
-}
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help = renderRefusal . mkRefusal help

{- | A refusal's text in its two parts, so an ecosystem renders whichever its own denial surface
carries and the help message is not dropped for one with no envelope to hold both.
-}
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

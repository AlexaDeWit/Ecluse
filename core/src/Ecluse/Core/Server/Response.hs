-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve-outcome model, the per-outcome status mapping, and the agnostic
shape of an error body.

Every client-facing reply is the rendering of one __serve outcome__ -- admit the
request, or reject it -- so that an error maps to the status a client can act on
rather than a generic 403\/500. The model and the per-outcome status mapping live
here; the WAI layer that turns an 'ArtifactStatus' into an actual response and
streams the body is separate (see @docs\/architecture\/web-layer.md@).

This module decides the HTTP /status/ of a refusal but holds __no body shape of
its own__: the bytes a client reads an error from are an ecosystem's (npm's
@{"error": …}@ JSON, a different surface for PyPI). The ecosystem's route-scoped
'Ecluse.Core.Server.Contract.ResponseContract' supplies the response constructor and
codec; the agnostic pipeline selects it through injected reply factories. 'appendHelp'
is the ecosystem-neutral operation those factories reuse: joining the operator help
message onto a denial.

== The outcome model

A 'ServeDecision' is 'Admit' or 'Reject' with a 'Rejection' carrying a
'RejectReason'. A rejection is either __by policy__ (a rule denied the version,
including deny-by-default) or __unavailable__ -- the version could not be decided,
carrying its 'Transience': whether the evaluator believes the condition will
self-heal. The whole verdict pipeline ("Ecluse.Core.Rules") feeds this: a rules
'Decision' projects to a 'ServeDecision' via 'serveDecisionOf'.

== Status follows the cause

For a __concrete artifact__ (one specific version) the outcome renders to a
single 'ArtifactStatus'. The load-bearing rule is __503 only when we believe it
will resolve__ -- a transient upstream\/advisory condition invites a retry, while a
permanent or internal inability to decide ('WontResolve') is a @500@, because
retrying it cannot help and we should not invite it. A policy rejection is a
@403@ whose body the route's response contract shapes. A __packument__ request has
no single status -- its versions are filtered and the status is chosen over the
surviving set -- so this module deliberately maps __per outcome__, not per request.

An operator help message, when configured, is appended to every denial
('appendHelp') so clients are told where to ask; how the joined text is then
wrapped into bytes is the route contract's.
-}
module Ecluse.Core.Server.Response (
    -- * Serve outcomes
    ServeDecision (..),
    Rejection (..),
    RejectReason (..),
    Transience (..),
    RetryAfter (..),
    RuleName (..),
    serveDecisionOf,

    -- * Concrete-artifact status
    ArtifactStatus (..),
    artifactStatus,
    artifactStatusCode,

    -- * Packument status (over the merged survivor set)
    PackumentStatus (..),
    packumentStatus,
    packumentStatusCode,
    longestRetry,

    -- * Denial help text
    HelpMessage,
    mkHelpMessage,
    appendHelp,
) where

import Data.Semigroup (Max (Max, getMax))
import Data.Text qualified as T

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Rules (renderDecision)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    RetryAfter (..),
    Transience (..),
 )

{- | The outcome of deciding a request: serve it, or refuse it with a reason.

Every client-facing reply renders one of these. 'Admit' carries no payload -- the
artifact or packument is what is then streamed; 'Reject' carries the 'Rejection'
that explains the refusal and selects the status.
-}
data ServeDecision
    = -- | Serve the request (the @200@ stream for an artifact).
      Admit
    | -- | Refuse the request, with the reason and a client-facing message.
      Reject Rejection
    deriving stock (Eq, Show)

{- | A refusal: /why/ it was refused, and an intuitive message for the client.
The 'rejectionReason' selects the HTTP status; the 'rejectionMessage' is the
human-facing text rendered into the response body.
-}
data Rejection = Rejection
    { rejectionReason :: RejectReason
    -- ^ The cause of the refusal, which decides the status.
    , rejectionMessage :: Text
    -- ^ The client-facing explanation (the rendered decision, or the cause).
    }
    deriving stock (Eq, Show)

{- | Why a request was refused.

A policy refusal is a deliberate verdict and is final for this request; an
unavailability is an /inability to decide/ and carries whether it is expected to
self-heal, which is what separates a retryable @503@ from a terminal @500@\/@403@.
-}
data RejectReason
    = {- | A rule denied the version (including deny-by-default). The 'RuleName'
      is the rule that decided, for the audit trail and the denial body.
      -}
      ByPolicy RuleName
    | {- | The version could not be decided -- an effectful rule the evaluator
      needed could not be consulted (advisory source down, timeout). This is
      __fail-closed__: a never-vetted version is not admitted just because the
      scanner is unreachable. The 'Transience' says whether a retry can help.
      -}
      Unavailable Transience
    | {- | The version's selected artifact carries __no integrity digest of any
      kind__ (neither an SRI nor a legacy shasum), so its bytes cannot be tied to
      a tamper-evident fingerprint. A version without an integrity check is
      inadmissible from an /untrusted/ (public) upstream -- there is nothing to
      detect a divergence against -- so admission refuses it outright. This is a
      deliberate, deny-by-default __admission policy__, not a rule decision and not
      a retryable inability: it maps to a @403@. The trusted private upstream is
      exempt; this reason never arises on that path.
      -}
      MissingIntegrity
    | {- | The version's selected artifact carries an integrity digest, but its
      strongest one is __weaker than the configured minimum algorithm__ (e.g. a
      legacy SHA-1 shasum only, under the default SHA-256 floor). A collision-broken
      digest cannot tie the bytes to a tamper-evident fingerprint, so it is
      inadmissible from an /untrusted/ (public) upstream -- distinct from
      'MissingIntegrity' (which has no digest at all) so the audit trail says which.
      A deny-by-default __admission policy__ that maps to a @403@; the trusted private
      upstream is exempt and this reason never arises on that path.
      -}
      BelowIntegrityFloor
    | {- | A responding upstream returned an __invalid response__ for the requested
      package -- its packument self-reported a name for a /different/ package, so that
      origin is untrusted for this request and its contribution is dropped. It is not
      a policy verdict and not a retryable inability but a /gateway/ fault: when no
      origin yields a valid packument and a responding one was invalid this way, the
      packument request maps to a @502@. Distinct from a genuine absence (no such
      package at all), which is not refused this way. Arises on the packument path
      only -- the artifact path never validates a packument name.
      -}
      UpstreamInvalid
    deriving stock (Eq, Show)

{- | The name of the rule that decided a refusal, carried for the audit trail and
the denial body. A 'newtype' over the 'Ecluse.Core.Rules.ruleName' text, so a rule
identity carries a distinct type rather than a bare 'Text'.
-}
newtype RuleName = RuleName Text
    deriving stock (Eq, Ord, Show)

{- | Project a rules 'Decision' (see "Ecluse.Core.Rules") into a serve outcome. Pure
and total.

An 'Admitted' decision admits; a 'Blocked' or 'BlockedByDefault' decision rejects
'ByPolicy', naming the deciding rule and carrying the human-readable
'renderDecision' as the message. An 'Undecidable' decision (a fail-closed rule that
could not be computed) rejects as 'Unavailable', carrying its 'Transience' so the
status mapping can choose @503@ vs @500@ -- __fail-closed__, exactly as a denial
removes a version, but flagged retryable when the cause may self-heal. Only an
admission admits.
-}
serveDecisionOf :: PackageDetails -> Decision -> ServeDecision
serveDecisionOf pd decision = case decision of
    Admitted{} -> Admit
    Blocked name _ -> Reject (rejectAs (ByPolicy (RuleName name)))
    BlockedByDefault{} -> Reject (rejectAs (ByPolicy (RuleName "BlockedByDefault")))
    Undecidable transience _ -> Reject (rejectAs (Unavailable transience))
  where
    rejectAs :: RejectReason -> Rejection
    rejectAs reason = Rejection reason (renderDecision pd decision)

{- | The HTTP status a __concrete-artifact__ request renders to. A domain sum
type (not a raw code) so the mapping is total and the WAI layer reads off an
exhaustive set; 'artifactStatusCode' gives the numeric code.

A packument request has no single status -- its versions are filtered and a status
is chosen over the survivors -- so this type models only the concrete-artifact
case.
-}
data ArtifactStatus
    = -- | @200@ -- admitted; the artifact is streamed.
      Ok
    | -- | @403@ -- refused by policy; the body is shaped by the route's response contract.
      Forbidden
    | {- | @503@ -- a transient inability to decide; the 'RetryAfter', if known,
      becomes the @Retry-After@ header.
      -}
      Unavailable' (Maybe RetryAfter)
    | -- | @500@ -- a permanent or internal inability to decide; not retryable.
      ServerError
    | -- | @404@ -- the upstream did not have the artifact (forwarded miss).
      NotFound
    deriving stock (Eq, Show)

{- | Map a serve outcome to its concrete-artifact status. Pure and total.

@403@ for a policy refusal; @503@ when an unavailability 'WillResolve' (a retry
may help); @500@ when it 'WontResolve'. __@503@ only when we believe it will
resolve__ -- a permanent or internal inability is a @500@, since retrying it
cannot help. A @404@ upstream miss is not a serve /decision/ (the version exists
unless upstream says otherwise), so it is not produced here.
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
        -- A packument-path validation cause; the artifact path never validates a
        -- packument name, so this does not arise here. A misbehaving upstream on the
        -- artifact path is already an internal inability to serve, so it maps to @500@.
        UpstreamInvalid -> ServerError

-- | The numeric HTTP status code for an 'ArtifactStatus'. Pure and total.
artifactStatusCode :: ArtifactStatus -> Int
artifactStatusCode = \case
    Ok -> 200
    Forbidden -> 403
    Unavailable'{} -> 503
    ServerError -> 500
    NotFound -> 404

{- | The HTTP status a __packument__ request renders to, chosen once the merged
survivor set is known. A packument has no single per-version status -- its versions
are filtered and merged across upstreams -- so the status is chosen __over the
survivors__: with at least one survivor the document is served; with none, the
status follows the most recoverable cause among the exclusions (see
'packumentStatus').

A domain sum (not a raw code) so the mapping is total and the WAI layer reads an
exhaustive set; 'packumentStatusCode' gives the numeric code. There is no @404@: a
packument whose versions were all withheld is __not__ a miss -- the package exists,
so a genuine upstream absence (no such package at all) is a separate concern of the
serve layer, decided before the merge.
-}
data PackumentStatus
    = -- | @200@ -- at least one version survived; the merged, filtered packument is served.
      PackumentOk
    | {- | @403@ -- no version survived and every exclusion was a policy denial; the
      response body collects the denial reasons.
      -}
      PackumentForbidden
    | {- | @503@ -- no version survived, but at least one exclusion may self-heal (a
      transient rule outcome, or a needed upstream that was unavailable), so a retry
      may yet yield survivors. The 'RetryAfter', if any was suggested, becomes the
      @Retry-After@ header.
      -}
      PackumentUnavailable (Maybe RetryAfter)
    | {- | @502@ -- no version survived because a responding upstream returned an
      __invalid response__ (a packument self-reporting a different package's name),
      and no origin yielded a valid packument. A gateway fault, distinct from a
      genuine absence (no such package) and from a retryable outage: the upstream
      answered, but with a document for the wrong package.
      -}
      PackumentBadGateway
    | {- | @500@ -- no version survived, no exclusion is retryable, and at least one is
      a permanent or internal inability to decide; retrying cannot help.
      -}
      PackumentServerError
    deriving stock (Eq, Show)

{- | Choose a packument's status from the per-version serve outcomes weighed for it:
the 'Admit's for surviving versions (trusted, or rule-approved) and the 'Reject's
for excluded ones -- plus any 'Reject' a needed-but-unavailable upstream contributes.
Pure and total.

Any 'Admit' means the merged document has a survivor, so it is served
('PackumentOk'). With no survivor the status follows the __most recoverable cause__
among the exclusions, so a retry is invited exactly when it might produce survivors:

* any 'Unavailable' 'WillResolve' → @503@, suggesting the longest 'RetryAfter' any
  such cause asked for (so every transient cause has likely cleared by then);
* else any 'UpstreamInvalid' → @502@ (a responding upstream returned a packument for
  a different package; ranked above the terminal @500@\/@403@ because it names a
  concrete, actionable gateway fault, but below the retryable @503@ since a transient
  origin may yet come back with a valid document);
* else any 'Unavailable' 'WontResolve' → @500@ (a permanent inability -- a retry
  cannot help, so it is not dressed up as a retryable @503@);
* else every exclusion is a deny-by-default cause -- a 'ByPolicy' rule denial or an
  admission refusal ('MissingIntegrity' or 'BelowIntegrityFloor'), __including the
  degenerate empty input__ → @403@: there is nothing to serve and nothing invites a
  retry.

Never @404@: the versions existed and were withheld (see 'PackumentStatus').
-}
packumentStatus :: [ServeDecision] -> PackumentStatus
packumentStatus decisions
    | tallyAdmit tally = PackumentOk
    | not (null willResolveDelays) = PackumentUnavailable (longestRetry willResolveDelays)
    | tallyUpstreamInvalid tally = PackumentBadGateway
    | tallyWontResolve tally = PackumentServerError
    | otherwise = PackumentForbidden
  where
    -- One strict pass over the outcomes collects every signal the guards weigh, so
    -- the all-denied path no longer walks the exclusions once per guard.
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
            -- A deny-by-default cause (policy or admission refusal): it leaves no
            -- signal of its own; an empty tally is exactly the @403@ floor.
            ByPolicy{} -> acc
            MissingIntegrity -> acc
            BelowIntegrityFloor -> acc

{- | The signals 'packumentStatus' weighs over the per-version serve outcomes,
accumulated in a single pass: whether any version was admitted, the suggested
retry delays of every transient exclusion (consumed by 'longestRetry'), and
whether a gateway fault or a permanent inability to decide was seen among the
exclusions. The fields are strict ('StrictData'), so the booleans are forced as
the tally is built rather than thunking across a large survivor set.
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

-- | The numeric HTTP status code for a 'PackumentStatus'. Pure and total.
packumentStatusCode :: PackumentStatus -> Int
packumentStatusCode = \case
    PackumentOk -> 200
    PackumentForbidden -> 403
    PackumentUnavailable{} -> 503
    PackumentBadGateway -> 502
    PackumentServerError -> 500

{- | An operator-configured message appended to every denial -- typically where to
ask for help (e.g. a support channel). Stored trimmed of surrounding whitespace
so it joins the denial text with a single separating space and an all-blank value
contributes nothing.
-}
newtype HelpMessage = HelpMessage Text
    deriving stock (Eq, Show)

-- | Build a 'HelpMessage', trimming surrounding whitespace.
mkHelpMessage :: Text -> HelpMessage
mkHelpMessage = HelpMessage . T.strip

{- | Append a non-blank operator 'HelpMessage' to a denial message, separated by a
single space; a blank or absent help message contributes nothing.

This is the ecosystem-neutral part of denial rendering -- every ecosystem appends
the operator's help text the same way. How the joined text is then wrapped into body
bytes is the route contract's concern.
-}
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help message =
    case help of
        Just (HelpMessage h) | not (T.null h) -> T.strip message <> " " <> h
        _ -> message

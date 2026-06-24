{- | The serve-outcome model, the per-outcome status mapping, and the agnostic
shape of an error body.

Every client-facing reply is the rendering of one __serve outcome__ — admit the
request, or reject it — so that an error maps to the status a client can act on
rather than a generic 403\/500. The model and the per-outcome status mapping live
here; the WAI layer that turns an 'ArtifactStatus' into an actual response and
streams the body is separate (see @docs\/architecture\/web-layer.md@).

This module decides the HTTP /status/ of a refusal but holds __no body shape of
its own__: the bytes a client reads an error from are an ecosystem's (npm's
@{"error": …}@ JSON, a different surface for PyPI), so a mount supplies a
'MountRenderer' — chosen at the composition root alongside its path grammar — and
the agnostic web layer never names one. 'appendHelp' is the ecosystem-neutral part
the renderer reuses: joining the operator help message onto a denial.

== The outcome model

A 'ServeDecision' is 'Admit' or 'Reject' with a 'Rejection' carrying a
'RejectReason'. A rejection is either __by policy__ (a rule denied the version,
including deny-by-default) or __unavailable__ — the version could not be decided,
carrying its 'Transience': whether the evaluator believes the condition will
self-heal. The whole verdict pipeline ("Ecluse.Rules") feeds this: a rules
'Decision' projects to a 'ServeDecision' via 'serveDecisionOf'.

== Status follows the cause

For a __concrete artifact__ (one specific version) the outcome renders to a
single 'ArtifactStatus'. The load-bearing rule is __503 only when we believe it
will resolve__ — a transient upstream\/advisory condition invites a retry, while a
permanent or internal inability to decide ('WontResolve') is a @500@, because
retrying it cannot help and we should not invite it. A policy rejection is a
@403@ whose body the mount's 'MountRenderer' shapes. A __packument__ request has
no single status — its versions are filtered and the status is chosen over the
surviving set — so this module deliberately maps __per outcome__, not per request.

An operator help message, when configured, is appended to every denial
('appendHelp') so clients are told where to ask; how the joined text is then
wrapped into bytes is the mount renderer's.
-}
module Ecluse.Server.Response (
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

    -- * Denial rendering
    HelpMessage,
    mkHelpMessage,
    unHelpMessage,
    appendHelp,
    RenderedBody (..),
    MountRenderer (..),
) where

import Data.Semigroup (Max (Max, getMax))
import Data.Text qualified as T

import Ecluse.Package (PackageDetails)
import Ecluse.Rules (renderDecision, ruleName)
import Ecluse.Rules.Types (
    Decision (Approved, ApprovedEffectful, Denied, DeniedByDefault, DeniedEffectful, Undecidable),
    RetryAfter (..),
    Transience (..),
 )

-- ── serve outcomes ───────────────────────────────────────────────────────────

{- | The outcome of deciding a request: serve it, or refuse it with a reason.

Every client-facing reply renders one of these. 'Admit' carries no payload — the
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
    | {- | The version could not be decided — an effectful rule the evaluator
      needed could not be consulted (advisory source down, timeout). This is
      __fail-closed__: a never-vetted version is not admitted just because the
      scanner is unreachable. The 'Transience' says whether a retry can help.
      -}
      Unavailable Transience
    | {- | The version's selected artifact carries __no integrity digest of any
      kind__ (neither an SRI nor a legacy shasum), so its bytes cannot be tied to
      a tamper-evident fingerprint. A version without an integrity check is
      inadmissible from an /untrusted/ (public) upstream — there is nothing to
      detect a divergence against — so admission refuses it outright. This is a
      deliberate, deny-by-default __admission policy__, not a rule decision and not
      a retryable inability: it maps to a @403@. The trusted private upstream is
      exempt; this reason never arises on that path.
      -}
      MissingIntegrity
    deriving stock (Eq, Show)

{- | The name of the rule that decided a refusal, carried for the audit trail and
the denial body. A 'newtype' over the 'Ecluse.Rules.ruleName' text so a rule
identity is not just any string.
-}
newtype RuleName = RuleName Text
    deriving stock (Eq, Ord, Show)

{- | Project a rules 'Decision' (see "Ecluse.Rules") into a serve outcome. Pure
and total.

An 'Approved' decision admits; a 'Denied' or 'DeniedByDefault' decision rejects
'ByPolicy', naming the deciding rule and carrying the human-readable
'renderDecision' as the message. An 'Undecidable' decision (a needed effectful rule
could not be consulted) rejects as 'Unavailable', carrying its 'Transience' so the
status mapping can choose @503@ vs @500@ — __fail-closed__, exactly as a denial
removes a version, but flagged retryable when the cause may self-heal. Only an
approval admits.
-}
serveDecisionOf :: PackageDetails -> Decision -> ServeDecision
serveDecisionOf pd decision = case decision of
    Approved{} -> Admit
    ApprovedEffectful{} -> Admit
    Denied rule _ -> Reject (rejectAs (ByPolicy (RuleName (ruleName rule))))
    DeniedEffectful name _ -> Reject (rejectAs (ByPolicy (RuleName name)))
    DeniedByDefault{} -> Reject (rejectAs (ByPolicy (RuleName "DeniedByDefault")))
    Undecidable transience _ -> Reject (rejectAs (Unavailable transience))
  where
    rejectAs :: RejectReason -> Rejection
    rejectAs reason = Rejection reason (renderDecision pd decision)

-- ── concrete-artifact status ─────────────────────────────────────────────────

{- | The HTTP status a __concrete-artifact__ request renders to. A domain sum
type (not a raw code) so the mapping is total and the WAI layer reads off an
exhaustive set; 'artifactStatusCode' gives the numeric code.

A packument request has no single status — its versions are filtered and a status
is chosen over the survivors — so this type models only the concrete-artifact
case.
-}
data ArtifactStatus
    = -- | @200@ — admitted; the artifact is streamed.
      Ok
    | -- | @403@ — refused by policy; the body is shaped by the mount's 'MountRenderer'.
      Forbidden
    | {- | @503@ — a transient inability to decide; the 'RetryAfter', if known,
      becomes the @Retry-After@ header.
      -}
      Unavailable' (Maybe RetryAfter)
    | -- | @500@ — a permanent or internal inability to decide; not retryable.
      ServerError
    | -- | @404@ — the upstream did not have the artifact (forwarded miss).
      NotFound
    deriving stock (Eq, Show)

{- | Map a serve outcome to its concrete-artifact status. Pure and total.

@403@ for a policy refusal; @503@ when an unavailability 'WillResolve' (a retry
may help); @500@ when it 'WontResolve'. __@503@ only when we believe it will
resolve__ — a permanent or internal inability is a @500@, since retrying it
cannot help. A @404@ upstream miss is not a serve /decision/ (the version exists
unless upstream says otherwise), so it is not produced here.
-}
artifactStatus :: ServeDecision -> ArtifactStatus
artifactStatus = \case
    Admit -> Ok
    Reject rej -> case rejectionReason rej of
        ByPolicy{} -> Forbidden
        MissingIntegrity -> Forbidden
        Unavailable (WillResolve retryAfter) -> Unavailable' retryAfter
        Unavailable WontResolve -> ServerError

-- | The numeric HTTP status code for an 'ArtifactStatus'. Pure and total.
artifactStatusCode :: ArtifactStatus -> Int
artifactStatusCode = \case
    Ok -> 200
    Forbidden -> 403
    Unavailable'{} -> 503
    ServerError -> 500
    NotFound -> 404

-- ── packument status (over the merged survivor set) ──────────────────────────

{- | The HTTP status a __packument__ request renders to, chosen once the merged
survivor set is known. A packument has no single per-version status — its versions
are filtered and merged across upstreams — so the status is chosen __over the
survivors__: with at least one survivor the document is served; with none, the
status follows the most recoverable cause among the exclusions (see
'packumentStatus').

A domain sum (not a raw code) so the mapping is total and the WAI layer reads an
exhaustive set; 'packumentStatusCode' gives the numeric code. There is no @404@: a
packument whose versions were all withheld is __not__ a miss — the package exists,
so a genuine upstream absence (no such package at all) is a separate concern of the
serve layer, decided before the merge.
-}
data PackumentStatus
    = -- | @200@ — at least one version survived; the merged, filtered packument is served.
      PackumentOk
    | {- | @403@ — no version survived and every exclusion was a policy denial; the
      response body collects the denial reasons.
      -}
      PackumentForbidden
    | {- | @503@ — no version survived, but at least one exclusion may self-heal (a
      transient rule outcome, or a needed upstream that was unavailable), so a retry
      may yet yield survivors. The 'RetryAfter', if any was suggested, becomes the
      @Retry-After@ header.
      -}
      PackumentUnavailable (Maybe RetryAfter)
    | {- | @500@ — no version survived, no exclusion is retryable, and at least one is
      a permanent or internal inability to decide; retrying cannot help.
      -}
      PackumentServerError
    deriving stock (Eq, Show)

{- | Choose a packument's status from the per-version serve outcomes weighed for it:
the 'Admit's for surviving versions (trusted, or rule-approved) and the 'Reject's
for excluded ones — plus any 'Reject' a needed-but-unavailable upstream contributes.
Pure and total.

Any 'Admit' means the merged document has a survivor, so it is served
('PackumentOk'). With no survivor the status follows the __most recoverable cause__
among the exclusions, so a retry is invited exactly when it might produce survivors:

* any 'Unavailable' 'WillResolve' → @503@, suggesting the longest 'RetryAfter' any
  such cause asked for (so every transient cause has likely cleared by then);
* else any 'Unavailable' 'WontResolve' → @500@ (a permanent inability — a retry
  cannot help, so it is not dressed up as a retryable @503@);
* else every exclusion is a deny-by-default cause — a 'ByPolicy' rule denial or a
  'MissingIntegrity' admission refusal (__including the degenerate empty input__) →
  @403@: there is nothing to serve and nothing invites a retry.

Never @404@: the versions existed and were withheld (see 'PackumentStatus').
-}
packumentStatus :: [ServeDecision] -> PackumentStatus
packumentStatus decisions
    | any isAdmit decisions = PackumentOk
    | not (null willResolveDelays) = PackumentUnavailable (longestRetry willResolveDelays)
    | anyWontResolve = PackumentServerError
    | otherwise = PackumentForbidden
  where
    reasons :: [RejectReason]
    reasons = [rejectionReason rej | Reject rej <- decisions]

    willResolveDelays :: [Maybe RetryAfter]
    willResolveDelays = [delay | Unavailable (WillResolve delay) <- reasons]

    anyWontResolve :: Bool
    anyWontResolve = not (null [() | Unavailable WontResolve <- reasons])

    isAdmit :: ServeDecision -> Bool
    isAdmit = \case
        Admit -> True
        Reject{} -> False

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
    PackumentServerError -> 500

-- ── denial rendering ─────────────────────────────────────────────────────────

{- | An operator-configured message appended to every denial — typically where to
ask for help (e.g. a support channel). Stored trimmed of surrounding whitespace
so it joins the denial text with a single separating space and an all-blank value
contributes nothing.
-}
newtype HelpMessage = HelpMessage Text
    deriving stock (Eq, Show)

-- | Build a 'HelpMessage', trimming surrounding whitespace.
mkHelpMessage :: Text -> HelpMessage
mkHelpMessage = HelpMessage . T.strip

-- | The trimmed help-message text.
unHelpMessage :: HelpMessage -> Text
unHelpMessage (HelpMessage t) = t

{- | Append a non-blank operator 'HelpMessage' to a denial message, separated by a
single space; a blank or absent help message contributes nothing.

This is the ecosystem-neutral part of denial rendering — every ecosystem appends
the operator's help text the same way. How the joined text is then wrapped into
body bytes is the mount's 'MountRenderer'.
-}
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help message =
    case help of
        Just (HelpMessage h) | not (T.null h) -> T.strip message <> " " <> h
        _ -> message

{- | A rendered error body: its @Content-Type@ and the bytes.

The agnostic serve layer chooses the HTTP /status/; the body shape — JSON, plain
text, HTML — is the mount's, so a 'MountRenderer' returns this pair and the WAI
layer reads the content type off it rather than assuming one.
-}
data RenderedBody = RenderedBody
    { renderedContentType :: ByteString
    -- ^ The @Content-Type@ the body is tagged with (e.g. @application\/json@).
    , renderedBytes :: LByteString
    -- ^ The encoded error body.
    }
    deriving stock (Eq, Show)

{- | A mount's ecosystem-specific error renderer — the Handle that keeps the npm
@{"error": …}@ shape (and any other ecosystem's) out of the agnostic web layer.

The status machinery here is ecosystem-agnostic, but the body a client reads an
error from is not: an npm client expects a JSON @{"error": …}@ object, a PyPI
client a different surface. Each mount supplies a renderer, chosen at the
composition root alongside its path grammar, so the web layer holds no body shape
of its own. 'renderError' shapes a denial or meta-route error (a @403@\/@404@\/@501@
body) from the optional operator help message and the human-facing reason.
-}
newtype MountRenderer = MountRenderer
    { renderError :: Maybe HelpMessage -> Text -> RenderedBody
    }

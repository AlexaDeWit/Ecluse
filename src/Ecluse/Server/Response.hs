{- | The serve-outcome model and its rendering to npm-shaped responses.

Every client-facing reply is the rendering of one __serve outcome__ — admit the
request, or reject it — so that an error maps to the status a client can act on
rather than a generic 403\/500. The model and the per-outcome status mapping live
here; the WAI layer that turns an 'ArtifactStatus' into an actual response and
streams the body is separate (see @docs\/architecture\/web-layer.md@).

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
@403@ with a 'denialBody'. A __packument__ request has no single status — its
versions are filtered and the status is chosen over the surviving set — so this
module deliberately maps __per outcome__, not per request.

The denial body is npm's @{"error": …}@ object; an operator's help message, when
configured, is appended to every denial so clients are told where to ask.
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

    -- * Denial body
    HelpMessage,
    mkHelpMessage,
    unHelpMessage,
    denialBody,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T

import Ecluse.Package (PackageDetails)
import Ecluse.Rules (renderDecision, ruleName)
import Ecluse.Rules.Types (Decision (Approved, Denied, DeniedByDefault))

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
    = -- | A rule denied the version (including deny-by-default). The 'RuleName'
      -- is the rule that decided, for the audit trail and the denial body.
      ByPolicy RuleName
    | -- | The version could not be decided — an effectful rule the evaluator
      -- needed could not be consulted (advisory source down, timeout). This is
      -- __fail-closed__: a never-vetted version is not admitted just because the
      -- scanner is unreachable. The 'Transience' says whether a retry can help.
      Unavailable Transience
    deriving stock (Eq, Show)

{- | Whether an 'Unavailable' condition is expected to resolve on its own.

This is the single distinction the status mapping turns on: a transient cause
(@WillResolve@) is worth retrying; a permanent or internal one (@WontResolve@) is
not, so it must not be dressed up as a retryable @503@.
-}
data Transience
    = -- | Transient — a retry may succeed (upstream @5xx@\/@429@, an advisory
      -- source briefly down). The optional 'RetryAfter' is the delay to suggest.
      WillResolve (Maybe RetryAfter)
    | -- | Not expected to self-heal (an internal or parse error). Retrying
      -- cannot help, so the request is a @500@, never a @503@.
      WontResolve
    deriving stock (Eq, Show)

{- | A @Retry-After@ delay, in whole seconds. A 'newtype' so a raw count of
seconds is never confused with some other integer when it reaches the response
header.
-}
newtype RetryAfter = RetryAfter Int
    deriving stock (Eq, Ord, Show)

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
'renderDecision' as the message. Only a denial rejects — an approval is never
turned into a 'Rejection'.
-}
serveDecisionOf :: PackageDetails -> Decision -> ServeDecision
serveDecisionOf pd decision = case decision of
    Approved{} -> Admit
    Denied rule _ -> Reject (rejectAs (RuleName (ruleName rule)))
    DeniedByDefault{} -> Reject (rejectAs (RuleName "DeniedByDefault"))
  where
    rejectAs :: RuleName -> Rejection
    rejectAs name = Rejection (ByPolicy name) (renderDecision pd decision)

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
    | -- | @403@ — refused by policy; the body is a 'denialBody'.
      Forbidden
    | -- | @503@ — a transient inability to decide; the 'RetryAfter', if known,
      -- becomes the @Retry-After@ header.
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

-- ── denial body ──────────────────────────────────────────────────────────────

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

{- | Render a denial body as the npm error object — @{"error": …}@ — whose
@error@ string is the message with the help message, if any, appended.

npm clients read the human-facing reason from this object (preferring @message@,
then @error@); Écluse emits the @error@ key, matching npm's own denial bodies. A
blank or absent 'HelpMessage' is omitted rather than appended as empty text.
-}
denialBody :: Maybe HelpMessage -> Text -> LByteString
denialBody help message =
    Aeson.encode (object ["error" .= appendHelp help message])

-- | Append a non-blank help message to the denial text, separated by one space.
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help message =
    case help of
        Just (HelpMessage h) | not (T.null h) -> T.strip message <> " " <> h
        _ -> message

{- | The per-request context the serve path reads through, and the handler monad
over it.

Mount dispatch ("Ecluse.Server") matches a request to one 'MountBinding' — a
mount's __complete__ ecosystem wiring — then runs the route's handler in 'Handler',
a reader over a 'RequestCtx' pairing that binding with the composition-root 'Env'.
A handler reads its per-mount dependencies (the classifier, the packument-serve
dependencies, the error renderer, the path prefix) and the shared composition root
from that one context, rather than taking them as explicit arguments threaded down
the pipeline.

'RequestCtx' is a concrete record with plain accessors ('ctxEnv', 'ctxMount'),
matching the concrete 'App' newtype: there is no @Has@-class indirection. The
handler monad layers over the same @katip@ base as 'Ecluse.App.App', so a
structured log call composes uniformly across the service and request layers.

This module sits below "Ecluse.Server" (dispatch) and "Ecluse.Server.Pipeline"
(the packument handler) so both can share these types without an import cycle; the
binding is __constructed__ at the composition root (see "Ecluse").
-}
module Ecluse.Server.Context (
    -- * Packument-serve dependencies
    PackumentDeps (..),

    -- * Mount binding
    MountBinding (..),

    -- * Per-request context
    RequestCtx (..),

    -- * The handler monad
    Handler (..),
    runHandler,
) where

import Data.Time (UTCTime)
import Katip (Katip, KatipContext)
import Katip.Monadic (KatipContextT, runKatipContextT)
import UnliftIO (MonadUnliftIO)

import Ecluse.Credential (Secret)
import Ecluse.Env (Env, envLogEnv)
import Ecluse.Rules.Effectful (PrecededEffectfulRule)
import Ecluse.Rules.Types (PrecededRule)
import Ecluse.Security (TarballHostPolicy)
import Ecluse.Server.Response (HelpMessage, MountRenderer)
import Ecluse.Server.Route (Classifier)

-- ── packument-serve dependencies ──────────────────────────────────────────────

{- | The per-mount inputs the serve handlers need beyond the composition-root
'Env': the two upstream endpoints, the mount's externally-visible base URL, the
mirror-target endpoint, its resolved rule policy, the edge auth token, the
wall-clock source, and the operator help message.

These are a mount-level concern, resolved at the composition root (a separate
concern) and carried on the mount's 'MountBinding'; a handler reads exactly what it
needs to decide and serve from the 'RequestCtx' it runs in. Both the packument and
the tarball paths share these deps — the tarball path additionally gates one
version and enqueues a mirror job to 'pdMirrorTarget' — so the name is retained for
continuity rather than narrowed to one route.
-}
data PackumentDeps = PackumentDeps
    { pdPrivateBaseUrl :: Text
    -- ^ The private upstream base URL; under @passthrough@, reads forward the client's credential.
    , pdPublicBaseUrl :: Text
    -- ^ The public upstream base URL; reads are anonymous (no client credential).
    , pdMountBaseUrl :: Text
    {- ^ The mount's externally-visible base URL, under which served @dist.tarball@
    URLs are rewritten so artifacts are fetched back through the gate.
    -}
    , pdMirrorTarget :: Text
    {- ^ The mount's mirror-target endpoint — where the demand-driven mirror worker
    publishes an approved artifact. Carried on the enqueued
    'Ecluse.Queue.MirrorJob' as its publish destination; the serve path never reads
    or writes it itself.
    -}
    , pdRules :: [PrecededRule]
    -- ^ The mount's resolved pure rule policy, evaluated against every public version.
    , pdEffectfulRules :: [PrecededEffectfulRule]
    {- ^ The mount's effectful rule policy (advisory lookups, per-version fetches),
    layered on the pure tier per "Ecluse.Rules.Effectful". Empty when no effectful
    rule is configured, in which case the effectful tier is skipped and gating
    reduces exactly to the pure tier.
    -}
    , pdTarballHostPolicy :: TarballHostPolicy
    {- ^ Whether a tarball may be fetched from a @dist.tarball@ host that differs
    from the upstream that served the packument
    ('Ecluse.Security.SameHostAsPackument' by default, the secure reading of the
    host allowlist; relaxed to 'Ecluse.Security.AnyAllowlistedHost' by
    @PROXY_RESPECT_UPSTREAM_TARBALL_HOST@).
    -}
    , pdInboundToken :: Maybe Secret
    {- ^ The optional inbound token a client must present (@PROXY_AUTH_TOKEN@);
    'Nothing' leaves the edge open (the network layer guards it).
    -}
    , pdNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'Ecluse.Rules.Types.EvalContext'.
    Injected so the time-sensitive age gate is deterministic under test.
    -}
    , pdHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to every denial body, if configured.
    }

-- ── mount binding ─────────────────────────────────────────────────────────────

{- | A mount: a path prefix bound to a registry, carrying that registry's
__complete__ ecosystem wiring. Dispatch matches a request's leading path segments
to 'bindingPrefix', strips them, and routes the remainder through the rest of the
binding (see "Ecluse.Server").

The prefix is a 'NonEmpty' list of segments (@"npm" :| []@ for a @\/npm@ mount):
every registry is path-mounted, so a root mount — which would force a URL change
on every consumer the day a second ecosystem is added — is __unrepresentable__
rather than merely discouraged. Bundling the classifier, serve dependencies, and
renderer into one record means a mount cannot be half-wired: there is no default
to fall back to.
-}
data MountBinding = MountBinding
    { bindingPrefix :: NonEmpty Text
    -- ^ The leading path segments this mount is served under; never empty.
    , bindingClassifier :: Classifier
    -- ^ The ecosystem path grammar mapping this mount's native path to an 'Ecluse.Server.Route.Route'.
    , bindingPackumentDeps :: Maybe PackumentDeps
    {- ^ The packument-serve dependencies, when wired; 'Nothing' leaves the
    packument route recognised-but-unserved (the @501@ stub).
    -}
    , bindingRenderer :: MountRenderer
    {- ^ This mount's renderer for error\/denial bodies — the ecosystem surface an
    in-mount @403@\/@404@\/@501@ is shaped into.
    -}
    }

-- ── per-request context ───────────────────────────────────────────────────────

{- | The context one request is served through: the composition-root 'Env' paired
with the 'MountBinding' the request matched. A concrete record with plain
accessors — 'ctxEnv' and 'ctxMount' — so a handler reads the shared root and its
per-mount wiring from one place rather than as explicit arguments.

Dispatch builds it once per request (see "Ecluse.Server"); the handler reads it
through the 'Handler' reader.
-}
data RequestCtx = RequestCtx
    { ctxEnv :: Env
    -- ^ The composition root — the handles, the shared HTTP manager, the caches, the logger.
    , ctxMount :: MountBinding
    -- ^ The mount the request matched, carrying its complete ecosystem wiring.
    }

-- ── the handler monad ─────────────────────────────────────────────────────────

{- | The request hot path's monad: a reader over the per-request 'RequestCtx'
layered on @katip@'s logging context.

A @newtype@ over @'ReaderT' 'RequestCtx' ('KatipContextT' 'IO')@ so its instances
are this module's to control and call sites name one concrete monad. The derived
instances give reader access to the context ('MonadReader' 'RequestCtx'), arbitrary
effects ('MonadIO'), the unlift capability ('MonadUnliftIO') the serve path's
@concurrently@\/@bracket@ need, and the @katip@ classes ('Katip', 'KatipContext')
so a structured log call composes — over the same base as 'Ecluse.App.App', so the
service and request layers log uniformly.

The @katip@ base is a reader, never a 'StateT', so logging context behaves
correctly across the serve path's concurrent fetches (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").
-}
newtype Handler a = Handler
    { unHandler :: ReaderT RequestCtx (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader RequestCtx
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

{- | Run a 'Handler' against the 'RequestCtx' dispatch built for the request,
yielding the underlying 'IO' action Warp's continuation runs in. This is the
boundary where the serve path's 'Handler' code is discharged to 'IO'.

The @katip@ context is initialised empty under the namespace the request's 'LogEnv'
(read from 'ctxEnv') already carries; a handler narrows the namespace or adds
package\/version\/rule context with @katip@'s combinators as it logs.
-}
runHandler :: RequestCtx -> Handler a -> IO a
runHandler ctx action =
    runKatipContextT (envLogEnv (ctxEnv ctx)) () mempty (runReaderT (unHandler action) ctx)

{- | The application monad for Écluse's effectful shell.

@'App'@ is the orchestration monad for the worker\/service layer: code that needs
the composition root reads it from the 'Env' and runs effects in 'IO'. It is a
reader over 'Env' that also carries the @katip@ logging state, so the structured
log stream composes through it without hand-threaded plumbing. It derives
'MonadUnliftIO', so @unliftio@'s resource and concurrency combinators (@bracket@,
@finally@, @async@) lift into the reader without manual unlifting — the property
the supervised worker and the advisory-sync task rely on (see "Ecluse.Env").

The request hot path runs in its own reader over a per-request context — see
'Ecluse.Server.Context.Handler' — so a handler reads the matched mount and the
composition root from one place rather than taking them as explicit arguments.
Both monads layer over the same @katip@ base, so logging composes uniformly across
the service and request layers.

This module is part of the imperative shell; the pure core (rules, parsers,
renderers) never imports it (see @docs\/architecture\/technology-stack.md@ →
"Key Decisions").
-}
module Ecluse.App (
    -- * The application monad
    App (..),
    runApp,
) where

import Katip (Katip, KatipContext)
import Katip.Monadic (KatipContextT, runKatipContextT)
import UnliftIO (MonadUnliftIO)

import Ecluse.Env (Env, envDdContext, envLogEnv)
import Ecluse.Telemetry.Correlation (ddPayloadNow)

{- | The effectful orchestration monad: a reader over the composition-root 'Env'
layered on @katip@'s logging context.

It is a @newtype@ rather than a bare transformer stack so that its instances are
this module's to control, and so call sites name a single concrete monad. The
derived instances give the usual reader access ('MonadReader' 'Env'), arbitrary
effects ('MonadIO'), the unlift capability ('MonadUnliftIO') that lets
@bracket@\/@async@ run in 'App', and the @katip@ logging classes ('Katip',
'KatipContext') so a structured log call composes without explicit plumbing.

The @katip@ base ('KatipContextT') is a reader over the log environment and the
current context\/namespace — never a 'StateT' — so the logging state behaves
correctly across @forkIO@\/@async@ (each forked action carries the context it was
spawned with), and shared mutable state stays in 'Env' as 'TVar's
(see @docs\/architecture\/technology-stack.md@ → "Key Decisions").
-}
newtype App a = App
    { unApp :: ReaderT Env (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

{- | Run an 'App' computation against a composition-root 'Env', yielding the
underlying 'IO' action. This is the boundary where the shell's 'App' code is
discharged to 'IO' — for instance where a plain-'IO' entry point runs a
service-layer action over the 'Env' it built.

The @katip@ context is initialised with the @dd@ object (the resolved service
identity, plus the active span's ids when one is in scope — see
"Ecluse.Telemetry.Correlation"), so every line a service-layer action emits carries
@dd@; an action narrows the namespace or adds context with @katip@'s
@katipAddNamespace@\/@katipAddContext@ on top as it logs (the worker re-stamps the
@dd@ ids inside a job span, where a tighter span is active).
-}
runApp :: Env -> App a -> IO a
runApp env action = do
    dd <- ddPayloadNow (envDdContext env)
    runKatipContextT (envLogEnv env) dd mempty (runReaderT (unApp action) env)

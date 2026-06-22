{- | The application monad for Écluse's effectful shell.

@'App' = 'ReaderT' 'Env' 'IO'@ is the orchestration monad for the
worker\/service layer: code that needs the composition root reads it from the
'Env' and runs effects in 'IO'. It derives 'MonadUnliftIO', so @unliftio@'s
resource and concurrency combinators (@bracket@, @finally@, @async@) lift into
the reader without manual unlifting — the property the supervised worker and
the advisory-sync task rely on (see "Ecluse.Env").

The request hot path is deliberately /not/ written in 'App': HTTP handlers run
in plain 'IO' taking an 'Env' argument, so the serve path carries no transformer
lifting. 'App' is for the background\/service layer, where the ergonomics of
'ReaderT' over hand-threaded 'Env' pay off.

This module is part of the imperative shell; the pure core (rules, parsers,
renderers) never imports it (see @docs\/architecture\/technology-stack.md@ →
"Key Decisions").
-}
module Ecluse.App (
    -- * The application monad
    App (..),
    runApp,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Env (Env)

{- | The effectful orchestration monad: a reader over the composition-root 'Env'
in 'IO'.

It is a @newtype@ rather than a bare 'ReaderT' synonym so that its instances —
notably 'MonadUnliftIO' — are this module's to control, and so call sites name a
single concrete monad. The derived instances give the usual reader access
('MonadReader' 'Env'), arbitrary effects ('MonadIO'), and the unlift capability
('MonadUnliftIO') that lets @bracket@\/@async@ run in 'App'.
-}
newtype App a = App
    { unApp :: ReaderT Env IO a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadUnliftIO
        )

{- | Run an 'App' computation against a composition-root 'Env', yielding the
underlying 'IO' action. This is the boundary where the shell's 'App' code is
discharged to 'IO' — for instance where a plain-'IO' entry point runs a
service-layer action over the 'Env' it built.
-}
runApp :: Env -> App a -> IO a
runApp env action = runReaderT (unApp action) env

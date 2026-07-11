-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The one failure type the load benchmarks harness raises.

The load benchmarks tier is inform-only: it never fails on a slow or degraded result. Its
only red state is a __literal failure__ -- the harness cannot boot, @oha@ cannot run, a
report does not parse, or a scenario served nothing. That is surfaced as this typed
exception (a non-zero exit), rather than a stringly throw, per @STYLE.md@ section 11.
-}
module Ecluse.BenchLoad.Error (
    BenchLoadError (..),
    benchFail,
) where

import Control.Exception (throwIO)

-- | A literal load-harness failure, carrying a human-facing reason.
newtype BenchLoadError = BenchLoadError Text
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Abort the harness with a 'BenchLoadError' carrying the given message.
benchFail :: Text -> IO a
benchFail = throwIO . BenchLoadError

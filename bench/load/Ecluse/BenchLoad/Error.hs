-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The one failure the load harness raises, as a typed exception and a non-zero exit. It never
fails on a slow result. It fails when a fixture or a proxy cannot boot, when @oha@ cannot run, when
a report does not parse, and when a run breaks an invariant in "Ecluse.BenchLoad.Verdict".
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

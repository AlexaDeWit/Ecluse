-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Écluse: a supply-chain policy proxy for package registries.

Écluse applies a configurable resilience policy between clients and a package registry before any
dependency reaches a build. It hosts no packages: it governs only what may be fetched from, and
mirrored to, the operator's own backends. The rules engine is __deny by default__. Mirroring is
demand-driven, so a mirror write never runs on a request's critical path. 'run' starts each command
through "Ecluse.Startup" with the shipped adapters.
-}
module Ecluse (
    -- * Entry point
    run,
) where

import GHC.IO.Encoding (setLocaleEncoding)
import System.IO (hSetEncoding, utf8)

import Ecluse.CLI (execCLI)
import Ecluse.Service (mountBindingFor)
import Ecluse.Startup (runWith)

-- | Set UTF-8 output whatever the host locale, then parse the command line and run that command.
run :: IO ()
run = do
    useUtf8
    execCLI >>= runWith mountBindingFor

-- A host with no locale leaves GHC on ASCII, which fails on the first non-ASCII character written.
-- The locale encoding also reaches each handle opened later.
useUtf8 :: IO ()
useUtf8 = do
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

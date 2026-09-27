-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Écluse: a supply-chain policy proxy for package registries.

Écluse sits between clients and a package registry and applies a configurable resilience policy
before any dependency reaches a build. It hosts no packages: the operator's own backend stores them,
and Écluse governs only what may be fetched from, and mirrored to, those backends. The rules engine
is __deny by default__ and mirroring is demand-driven, so a mirror write never runs on a request's
critical path. 'run', the entry point the @ecluse@ executable invokes, reads the command line and
starts that command through "Ecluse.Startup" with the shipped adapters.
-}
module Ecluse (
    -- * Entry point
    run,
) where

import Ecluse.CLI (execCLI)
import Ecluse.Service (mountBindingFor)
import Ecluse.Startup (runWith)

run :: IO ()
run = execCLI >>= runWith mountBindingFor

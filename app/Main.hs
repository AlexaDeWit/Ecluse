-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @ecluse@ executable entry point.

Thin on purpose (see @AGENTS.md@): it only hands off to 'Ecluse.run', so all
behaviour lives in the library, where a test can reach it.
-}
module Main (main) where

import Ecluse (run)

main :: IO ()
main = run

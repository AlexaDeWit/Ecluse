-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @ecluse@ executable entry point.

Deliberately thin (see @AGENTS.md@): it only hands off to 'Ecluse.run', so all
behaviour lives in the library where it is testable.
-}
module Main (main) where

import Ecluse (run)

main :: IO ()
main = run

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Scoping a case's process environment.

The process environment is one mutable value the whole suite shares, so a case that reads it
depends on what every other case left behind. 'withEnvVars' clears the keys in play before it
applies the case's own, which is what makes a case order-independent.
-}
module Ecluse.Test.Env (
    withEnvVars,
    ambientAwsEntries,
    withAmbientAws,
) where

import System.Environment (setEnv, unsetEnv)
import UnliftIO.Exception (bracket_)

{- | Run one case with the named keys cleared, then its own entries alone, and cleared again on
exit. The caller names every key any case in its module sets, not just the ones it sets here.
-}
withEnvVars :: [String] -> [(String, String)] -> IO a -> IO a
withEnvVars keys entries = bracket_ enter (traverse_ unsetEnv keys)
  where
    enter = traverse_ unsetEnv keys >> traverse_ (uncurry setEnv) entries

-- | An AWS identity the credential discovery finds, for a case that must reach a resolved one.
ambientAwsEntries :: [(String, String)]
ambientAwsEntries =
    [ ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("AWS_REGION", "us-east-1")
    ]

-- | Run the case under 'ambientAwsEntries' alone.
withAmbientAws :: IO a -> IO a
withAmbientAws = withEnvVars (map fst ambientAwsEntries) ambientAwsEntries

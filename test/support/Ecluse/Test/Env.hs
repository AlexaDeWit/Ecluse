-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Scoped process environment fixtures. Callers must run serially.
module Ecluse.Test.Env (
    withEnvVars,
    withEnvOverrides,
    ambientAwsEntries,
    withAmbientAws,
) where

import System.Environment (setEnv, unsetEnv)
import UnliftIO.Exception (bracket)

{- | Clear the named keys, apply entries, then restore every touched key on exit.
Callers must run serially because the process environment is shared.
-}
withEnvVars :: [String] -> [(String, String)] -> IO a -> IO a
withEnvVars keys entries =
    withEnvChanges (map (,Nothing) keys <> map (second Just) entries)

-- | Restore all touched keys after success, failure, or cancellation. Callers must run serially.
withEnvOverrides :: [(String, String)] -> IO a -> IO a
withEnvOverrides = withEnvChanges . map (second Just)

withEnvChanges :: [(String, Maybe String)] -> IO a -> IO a
withEnvChanges entries action = bracket save (traverse_ apply) (const (traverse_ apply entries >> action))
  where
    save =
        traverse
            ( \key -> do
                value <- lookupEnv key
                pure (key, value)
            )
            (ordNub (map fst entries))
    apply (key, value) = maybe (unsetEnv key) (setEnv key) value

-- | An AWS identity the credential discovery finds, for a case that must reach a resolved one.
ambientAwsEntries :: [(String, String)]
ambientAwsEntries =
    [ ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("AWS_REGION", "us-east-1")
    ]

-- | Scope dummy AWS credentials to the action, restoring the caller's values on exit.
withAmbientAws :: IO a -> IO a
withAmbientAws = withEnvOverrides ambientAwsEntries

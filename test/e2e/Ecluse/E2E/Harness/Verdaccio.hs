-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Verdaccio (
    verdaccioHasVersion,
    verdaccioHasVersionNow,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Client (
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Types (statusCode)
import UnliftIO (handleAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.E2E.Harness.Types

{- | Poll Verdaccio (the mirror) until it serves the given version of a package, or the
timeout lapses. A 'False' means the version never appeared within the patience window.
-}
verdaccioHasVersion :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersion e2e pkg version = go (40 :: Int)
  where
    go 0 = pure False
    go n = do
        present <- verdaccioHasVersionNow e2e pkg version
        if present then pure True else threadDelay 500000 >> go (n - 1)

{- | Check once whether the mirror already serves a version, with no retry. Use it for an
absent-now precondition, to skip the patience window 'verdaccioHasVersion' spends.
-}
verdaccioHasVersionNow :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersionNow e2e pkg version =
    handleAny (\_ -> pure False) $ do
        req <- parseRequest (toString (e2eVerdaccio e2e <> "/" <> pkg))
        resp <- httpLbs req (e2eManager e2e)
        pure
            ( statusCode (responseStatus resp) == 200
                && version `T.isInfixOf` decodeUtf8 (LBS.toStrict (responseBody resp))
            )

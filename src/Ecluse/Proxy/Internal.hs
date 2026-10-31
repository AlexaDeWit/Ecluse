-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The proxy listener's innards, exported for its specs. Importing it opts out of "Ecluse.Proxy"'s stability promise.
module Ecluse.Proxy.Internal (
    warpExceptionHook,
) where

import Katip (LogEnv, Severity (ErrorS), SimpleLogPayload, katipAddContext, logFM, runKatipContextT, sl)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Ecluse.Core.Fault.Http (displayFaultT)
import Ecluse.Runtime.Log (moduleField)
import Ecluse.Runtime.Server (serverModule)

-- | Warp's exception hook over the process logger. Routine client disconnects stay out of the log, so an aborted download does not spam it.
warpExceptionHook :: LogEnv -> Maybe Wai.Request -> SomeException -> IO ()
warpExceptionHook logEnv mRequest err =
    when (Warp.defaultShouldDisplayException err) $
        runKatipContextT logEnv (mempty :: SimpleLogPayload) "server" $
            katipAddContext payload $
                logFM ErrorS "a fault escaped to the server (a post-commit teardown, or warp's own connection handling)"
  where
    payload =
        moduleField serverModule
            <> sl "path" (maybe ("unknown" :: Text) (decodeUtf8 . Wai.rawPathInfo) mRequest)
            <> sl "detail" (displayFaultT err)

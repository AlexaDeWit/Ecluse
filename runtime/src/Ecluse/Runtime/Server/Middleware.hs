-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The front door's request timeout and its control-plane health probes, @\/livez@ and @\/readyz@.
The request-body cap is a route concern, enforced where the only body-consuming route (publish)
reads its body.
-}
module Ecluse.Runtime.Server.Middleware (
    -- * The request timeout
    requestTimeout,

    -- * Control-plane health probes
    probeApplication,

    -- * Neutral response shapes
    jsonResponse,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Status, hContentType, status200, status404, status503)
import Network.Wai (Application, Middleware, Response, pathInfo, responseLBS)

import Ecluse.Core.Deadline (Deadlines, timeout)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Latched, Routable),
    routable,
 )
import Ecluse.Core.Worker (Liveness (liveHealthy, liveLastPoll))

{- | Answer an empty @503@ for a request that outlives this many seconds, about one sweep late. Like
wai-extra's @timeout@, it answers even when a response has already begun.
-}
requestTimeout :: Deadlines -> Int -> Middleware
requestTimeout deadlines seconds app request respond =
    timeout deadlines (seconds * 1_000_000) (app request respond) >>= maybe (respond (responseLBS status503 [] "")) pure

{- | The control-plane health probes, answered above any mount: @\/livez@ and @\/readyz@ from the
injected checks. Any other path is a @404@.
-}
probeApplication :: IO Readiness -> IO Liveness -> Application
probeApplication checkReady checkLiveness request respond =
    case pathInfo request of
        ["livez"] -> checkLiveness >>= respond . livenessResponse
        ["readyz"] -> checkReady >>= respond . readinessResponse
        _ -> respond notFound

{- The @\/livez@ body carries the loop's last recorded progress beside the verdict, so a
dedicated worker fleet's orchestrator can judge staleness rather than only pass or fail. -}
livenessResponse :: Liveness -> Response
livenessResponse liveness
    | liveHealthy liveness = body status200 "live"
    | otherwise = body status503 "liveness check failed"
  where
    body :: Status -> Text -> Response
    body status label =
        jsonResponse status (encode (object ["status" .= label, "lastPoll" .= liveLastPoll liveness]))

{- The body names every configured mount beside the verdict, so an operator sees which ecosystem
awaits its advisory database while the others keep serving. -}
readinessResponse :: Readiness -> Response
readinessResponse verdict = case verdict of
    Routable mounts -> withMounts readyLabel mounts
    AwaitingMounts mounts -> withMounts awaitingLabel mounts
    Latched -> statusOnly status "halted"
  where
    -- 'routable' alone decides the status, and the match decides only what the body reports.
    status = bool status503 status200 (routable verdict)
    withMounts label mounts =
        jsonResponse status (encode (object ["status" .= label, "mounts" .= mountsOf mounts]))

-- A mount reports the two words the whole verdict reports, under its configured ecosystem key.
mountsOf :: Map.Map Ecosystem MountReadiness -> Value
mountsOf mounts =
    object [Key.fromText (ecosystemName eco) .= mountLabel mount | (eco, mount) <- Map.toList mounts]

mountLabel :: MountReadiness -> Text
mountLabel = \case
    MountReady -> readyLabel
    MountAwaitingFirstSync -> awaitingLabel

readyLabel, awaitingLabel :: Text
readyLabel = "ready"
-- Only a mount that denies on the advisory database waits, so the label names what is missing and
-- which role publishes it rather than reporting a generic startup delay.
awaitingLabel = "awaiting the advisory database that ecluse pilot publishes"

-- A probe body carrying the verdict alone, for a state no mount detail explains.
statusOnly :: Status -> Text -> Response
statusOnly status label = jsonResponse status (encode (object ["status" .= label]))

-- This tier sits above the mounts, so no ecosystem shapes the body of an unmounted path.
notFound :: Response
notFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n"

-- | A JSON response with the given status and body, tagged @application\/json@.
jsonResponse :: Status -> LByteString -> Response
jsonResponse status =
    responseLBS status [(hContentType, "application/json")]

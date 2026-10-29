-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Native requests pass the edge check and admission, run under the serve-path cap, and bound
actual upstream chunks and examined source documents independently.
-}
module Ecluse.Core.Registry.RubyGems.Handlers.Budget (
    withNativeRequest,
    newNativeReadBudget,
    newNativeDocumentBudget,
) where

import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Request, ResponseReceived)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.Exchange (withinServeCapOr)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.RubyGems.Route.Response (RubyResponse, rubyReplies)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (progressFloor))
import Ecluse.Core.Server.Admission.Meter (MemoryTicket)
import Ecluse.Core.Server.Context (Handler, MountBinding (bindingPackumentDeps), PackumentDeps (pdInboundToken, pdLimits), ServeRuntime (srPublicClient), ctxMount, ctxRuntime)
import Ecluse.Core.Server.Pipeline.Origin (ObservedReadBoundExceeded (ObservedReadBoundExceeded))
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (packumentUnauthorised, packumentUnavailable))
import Ecluse.Core.Server.Pipeline.Shared (edgeTokenMatches, forwardedCredential, shedMessage, unauthorisedMessage)
import Ecluse.Core.Server.Response (mkRefusal)

{- | Refuse a caller the edge does not authorise, then run @action@ under @admit@ and the serve-path
cap, answering @expired@ once the cap passes and handing the result to @continue@.
-}
withNativeRequest ::
    (ServeRuntime -> (ResponseHeaders -> Handler ResponseReceived) -> (MemoryTicket -> Handler a) -> (a -> Handler ResponseReceived) -> Handler ResponseReceived) ->
    Request ->
    (RubyResponse -> Handler ResponseReceived) ->
    (PackumentDeps -> a) ->
    (MemoryTicket -> PackumentDeps -> Handler a) ->
    (a -> Handler ResponseReceived) ->
    Handler ResponseReceived
withNativeRequest admit request refuse expired action continue = do
    mount <- asks ctxMount
    runtime <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
    if not (edgeTokenMatches (pdInboundToken deps) (forwardedCredential mount request))
        then refuse (packumentUnauthorised rubyReplies [] (mkRefusal Nothing unauthorisedMessage))
        else
            admit
                runtime
                (\headers -> refuse (packumentUnavailable rubyReplies headers (mkRefusal Nothing shedMessage)))
                (\ticket -> withRunInIO $ \run -> withinServeCapOr (ecDeadlines (srPublicClient runtime)) (progressFloor (pdLimits deps)) (expired deps) (run (action ticket deps)))
                continue

-- | Refuse before another source chunk reaches its parser when cumulative input exceeds the cap.
newNativeReadBudget :: Int -> IO (Int -> IO ())
newNativeReadBudget cap = do
    count <- newNativeByteCounter cap
    pure $ \bytes -> do
        exceeded <- count bytes
        when exceeded (throwIO (ObservedReadBoundExceeded (BodyTooLarge (MetadataBodyLimit cap))))

-- | Refuse before admission when examined source documents, including cache hits, exceed the cap.
newNativeDocumentBudget :: Int -> response -> IO (Int -> IO (Either response ()))
newNativeDocumentBudget cap refusal = do
    count <- newNativeByteCounter cap
    pure $ \bytes -> do
        exceeded <- count bytes
        pure (if exceeded then Left refusal else Right ())

newNativeByteCounter :: Int -> IO (Int -> IO Bool)
newNativeByteCounter cap = do
    total <- newIORef (0 :: Integer)
    pure $ \bytes ->
        atomicModifyIORef' total (\seen -> let next = seen + toInteger (max 0 bytes) in (next, next > toInteger cap))

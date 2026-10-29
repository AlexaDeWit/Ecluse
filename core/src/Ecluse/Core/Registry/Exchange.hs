-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded registry exchanges under the progress floor, the response wait, and optional public authority control.
module Ecluse.Core.Registry.Exchange (
    -- * The bounded exchange
    boundedExchange,
    runExchange,
    runExchangeUsing,
    runExchangeUsingObserved,
    singleAttemptSettings,
    newSingleAttemptManager,
    withSuccessBody,
    withSuccessBodyUsing,
    boundedRelay,

    -- * The serve-path cap
    withinServeCap,
    withinServeCapOr,

    -- * Source digests
    digestingRead,

    -- * Metered reads
    chargedRead,
    chargedReadWith,
    accumulatingCharge,
    accumulatingChargeFrom,
    boundedChargedRead,
    boundedIncrementalRead,

    -- * Request formation
    formThen,
) where

import Crypto.Hash.IO (hashMutableInit, hashMutableUpdate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    BodyReader,
    Manager,
    ManagerSettings (managerRetryableException),
    Request (requestBody),
    Response (responseStatus),
    brRead,
    newManager,
    responseBody,
    responseHeaders,
 )
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (Status, statusCode)
import UnliftIO (try)

import Ecluse.Core.Deadline (Deadlines, timeout)
import Ecluse.Core.Fault (TransportCause (TransportTimeout), transportFault)
import Ecluse.Core.Fault.Http (classifyTransport, typedTlsManagerSettings)
import Ecluse.Core.Registry (
    BodyOutcome (SuccessBody, UnreadStatus),
    FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport),
    PublishRelayResponse (..),
    UrlFormationError,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Progress (meteredReader, meteredUpload, watched)
import Ecluse.Core.Registry.Public (ReadControl (Uncontrolled), withReadControl)
import Ecluse.Core.Registry.ResponseWait (withResponse)
import Ecluse.Core.Security (
    BodyLimit,
    LimitError,
    ProgressFloor,
    boundedRead,
    floorMinBytes,
    floorServeCapMicros,
    floorWindowMicros,
 )
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Snapshot (ContentDigest, digestFromMutableContext)

-- | Destructive clients must return uncertain transport failures for reassessment before retry.
singleAttemptSettings :: ManagerSettings -> ManagerSettings
singleAttemptSettings settings = settings{managerRetryableException = const False}

-- | A 'typedTlsManagerSettings' manager under 'singleAttemptSettings'. @newTlsManagerWith@ would replace its retry check and restore the replay on a reused connection.
newSingleAttemptManager :: IO Manager
newSingleAttemptManager = newManager (singleAttemptSettings typedTlsManagerSettings)

-- | Project status, decompressed byte count, and body. Transport failures retain their typed cause.
boundedExchange :: (Int -> Int -> ByteString -> a) -> ExchangeClient -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault a)
boundedExchange project client progress limits request =
    runExchange client progress request (readBounded project limits)

readBounded :: (Int -> Int -> ByteString -> a) -> BodyLimit -> Response BodyReader -> IO (Either LimitError a)
readBounded project limits response =
    fmap (uncurry (project (statusCode (responseStatus response))))
        <$> boundedRead limits (brRead (responseBody response))

-- | The exchange keeping the answered status alongside the body, for the first-party relay.
boundedRelay :: ExchangeClient -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault PublishRelayResponse)
boundedRelay =
    boundedExchange $ \status _ body ->
        PublishRelayResponse{relayStatus = status, relayBody = LBS.fromStrict body}

-- | Consume a response under the progress floor and the response wait, classifying transport and body-limit failures.
runExchange :: ExchangeClient -> ProgressFloor -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchange = runExchangeUsing Uncontrolled

-- | Retain the controlled response lifetime for header-aware bounded consumers.
runExchangeUsing :: ReadControl -> ExchangeClient -> ProgressFloor -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchangeUsing = runExchangeUsingObserved (\_ _ -> pass)

-- | Observe actual origin headers before cooldown handling can replace their response.
runExchangeUsingObserved :: (Status -> ResponseHeaders -> IO ()) -> ReadControl -> ExchangeClient -> ProgressFloor -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchangeUsingObserved observe control client progress request readResponse =
    watched
        (ecDeadlines client)
        progress
        ( \watch ->
            try
                ( withReadControl control (metered watch) $ \observed recordHeaders ->
                    withResponse client observed $ \response -> do
                        observe (responseStatus response) (responseHeaders response)
                        cooldown <- recordHeaders (responseStatus response) (responseHeaders response)
                        case cooldown of
                            Just retry -> pure (Left (FetchCooldown retry))
                            Nothing -> first FetchBoundExceeded <$> readResponse (meteredReader watch <$> response)
                )
        )
        <&> \case
            Nothing -> Left (belowFloor progress)
            Just (Left httpErr) -> Left (FetchTransport (classifyTransport httpErr))
            Just (Right result) -> join result
  where
    metered watch = request{requestBody = meteredUpload watch (requestBody request)}

-- | Fail an action that outlives the floor's serve-path cap with the transport timeout, about one sweep late.
withinServeCap :: Deadlines -> ProgressFloor -> (FetchFault -> e) -> IO (Either e a) -> IO (Either e a)
withinServeCap deadlines progress inject = withinServeCapOr deadlines progress (Left (inject capExceeded))
  where
    capExceeded = FetchTransport (transportFault TransportTimeout ("the upstream exchange outlived its " <> seconds (floorServeCapMicros progress) <> "-second serve-path cap"))

-- | Answer @expired@ once an action outlives the floor's serve-path cap, about one sweep late.
withinServeCapOr :: Deadlines -> ProgressFloor -> a -> IO a -> IO a
withinServeCapOr deadlines progress expired action =
    fromMaybe expired <$> timeout deadlines (floorServeCapMicros progress) action

belowFloor :: ProgressFloor -> FetchFault
belowFloor progress =
    FetchTransport . transportFault TransportTimeout $
        "the exchange moved fewer than "
            <> show (floorMinBytes progress)
            <> " body bytes, counting both directions together, in a "
            <> seconds (floorWindowMicros progress)
            <> "-second progress window"

-- Whole seconds as an operator wrote them, and a fraction only where one exists.
seconds :: Int -> Text
seconds micros = case micros `divMod` 1_000_000 of
    (whole, 0) -> show whole
    _ -> show (fromIntegral micros / 1_000_000 :: Double)

-- | Consume a 2xx body within the response lifetime. A status outside 2xx never reaches the consumer.
withSuccessBody :: ExchangeClient -> ProgressFloor -> (IO ByteString -> IO (Either LimitError a)) -> Request -> IO (Either FetchFault (BodyOutcome a))
withSuccessBody = withSuccessBodyUsing Uncontrolled

-- | Apply public control only to the HTTP exchange that supplies this body.
withSuccessBodyUsing :: ReadControl -> ExchangeClient -> ProgressFloor -> (IO ByteString -> IO (Either LimitError a)) -> Request -> IO (Either FetchFault (BodyOutcome a))
withSuccessBodyUsing control client progress consume request = runExchangeUsing control client progress request $ \response -> do
    let code = statusCode (responseStatus response)
    if isSuccessStatus code
        then fmap (SuccessBody code) <$> consume (brRead (responseBody response))
        else pure (Right (UnreadStatus code))

-- | Run a consumer over a source that hashes each chunk it passes on. A success carries the digest of every chunk read.
digestingRead :: (IO ByteString -> IO (Either e a)) -> IO ByteString -> IO (Either e (a, ContentDigest))
digestingRead consume readChunk = do
    context <- hashMutableInit
    let next = do
            chunk <- readChunk
            hashMutableUpdate context chunk
            pure chunk
    consume next >>= traverse (\result -> (result,) <$> digestFromMutableContext context)

-- | Pay for each chunk's length before the consumer sees it.
chargedRead :: (Int -> IO ()) -> IO ByteString -> IO ByteString
chargedRead charge = chargedReadWith (charge . BS.length)

-- | Pay for the observed chunk before a parser or buffer retains it.
chargedReadWith :: (ByteString -> IO ()) -> IO ByteString -> IO ByteString
chargedReadWith charge readChunk = do
    chunk <- readChunk
    charge chunk
    pure chunk

-- | Convert per-chunk costs to an absolute requirement within one read operation.
accumulatingCharge :: (Int -> IO ()) -> IO (Int -> IO ())
accumulatingCharge charge = do
    (add, _) <- accumulatingChargeFrom 0 charge
    pure $ \bytes -> when (bytes > 0) (add bytes)

-- | Report the saturated reservation, including after callback failure. Zero additions charge too, but construction does not charge the seed.
accumulatingChargeFrom :: Integer -> (Int -> IO ()) -> IO (Int -> IO (), IO Int)
accumulatingChargeFrom initial charge = do
    held <- newIORef initial
    let total = fromInteger . min (toInteger (maxBound :: Int))
        add bytes = do
            required <- atomicModifyIORef' held (\previous -> let next = previous + toInteger bytes in (next, next))
            charge (total required)
    pure (add, total <$> readIORef held)

-- | Pay the running total of buffered chunks and their final strict copy before the bounded reader retains them.
boundedChargedRead :: (Int -> IO ()) -> BodyLimit -> IO ByteString -> IO (Either LimitError (Int, ByteString))
boundedChargedRead charge limits readChunk = accumulatingCharge charge >>= \retain -> boundedIncrementalRead retain limits readChunk

-- | Pay each buffered chunk's own charge, never a running total, before the bounded reader retains it.
boundedIncrementalRead :: (Int -> IO ()) -> BodyLimit -> IO ByteString -> IO (Either LimitError (Int, ByteString))
boundedIncrementalRead charge limits = boundedRead limits . chargedRead (\bytes -> let cost = bufferedChunkCharge bytes in when (cost > 0) (charge cost))

-- | Report request-formation and exchange failures through the same error channel.
formThen ::
    (UrlFormationError -> fault) ->
    (Request -> IO (Either fault a)) ->
    Either UrlFormationError Request ->
    IO (Either fault a)
formThen unformable = either (pure . Left . unformable)

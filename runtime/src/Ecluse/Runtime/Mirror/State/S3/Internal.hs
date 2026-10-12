-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

-- | Conditional S3 metadata exchanges bound response bytes before SDK decoding.
module Ecluse.Runtime.Mirror.State.S3.Internal (
    ObjectRead (..),
    ObjectWrite (..),
    ObjectFault (..),
    readObject,
    writeObject,
) where

import Amazonka qualified as AWS
import Amazonka.Env.Hooks qualified as Hooks
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Amazonka.Types qualified as Types
import Conduit (ConduitT, MonadResource, ResourceT, await, runResourceT, sealConduitT, yield, ($$++), (.|))
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as C
import Lens.Micro ((%~), (?~), (^.))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (catch, throwIO)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), TransportFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Mirror.State.Codec (saturatingStateCharge, stateExchangeCharge)
import Ecluse.Core.Mirror.State.Internal (StateCharge)
import Ecluse.Core.Registry.Exchange (chargedRead)
import Ecluse.Core.Registry.Progress (Watch, meteredReader, meteredUpload, watched)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (..), boundedRead)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge)
import Ecluse.Core.Stream (boundBytes)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)

-- | A 304 carries only the exact revision supplied to this exchange.
data ObjectRead = ObjectAbsent | ObjectUnchanged ByteString | ObjectLoaded ByteString ByteString
    deriving stock (Eq, Show)

-- | Conditional writes never overwrite a revision they did not observe.
data ObjectWrite = ObjectStored ByteString | ObjectConflict
    deriving stock (Eq, Show)

-- | Missing revision evidence and ambiguous absence remain unavailable.
data ObjectFault
    = ObjectTransport TransportFault
    | ObjectTooLarge Int
    | ObjectNoRevision
    | ObjectUnknownAbsence
    deriving stock (Eq, Show)

-- These wrappers preserve generated request construction while adding pre-signing conditions.
-- The associated response types require TypeFamilies, as the Amazonka request class does.

-- A missing expected revision means create-if-absent.
data ConditionalPut = ConditionalPut (Maybe ByteString) S3.PutObject

-- Both successful streaming bodies and error XML pass through the wire ceiling.
data BoundedGet = BoundedGet (Maybe ByteString) S3.GetObject

-- The absence fallback limits XML before the generated decoder buffers it.
newtype BoundedList = BoundedList S3.ListObjectsV2

instance AWS.AWSRequest ConditionalPut where
    type AWSResponse ConditionalPut = RawResponse
    request overrides (ConditionalPut revision native) =
        coerce (AWS.request overrides native & Types.request_headers %~ condition)
      where
        condition headers =
            maybe ("If-None-Match", "*") ("If-Match",) revision
                : filter (\(name, _) -> name /= "If-Match" && name /= "If-None-Match") headers
    response = captureResponse

instance AWS.AWSRequest BoundedGet where
    type AWSResponse BoundedGet = RawResponse
    request overrides (BoundedGet revision native) = coerce (AWS.request overrides native & Types.request_headers %~ (\headers -> maybe headers (\etag -> ("If-None-Match", etag) : headers) revision))
    response = captureResponse

instance AWS.AWSRequest BoundedList where
    type AWSResponse BoundedList = RawResponse
    request overrides (BoundedList native) = coerce (AWS.request overrides native)
    response = captureResponse

data RawResponse = RawResponse AWS.Service (Types.ClientResponse Types.ClientBody)

captureResponse :: (MonadResource m) => (LByteString -> IO LByteString) -> AWS.Service -> Proxy a -> Types.ClientResponse Types.ClientBody -> m (Either AWS.Error (Types.ClientResponse RawResponse))
captureResponse _ service _ response = pure (Right (RawResponse service response <$ response))

newtype ObjectLimit = ObjectLimit Int
    deriving stock (Show)

instance Exception ObjectLimit

sendBounded :: (AWS.AWSRequest request, AWS.AWSResponse request ~ RawResponse, AWS.AWSRequest native) => StateCharge -> Int -> Bool -> AWS.Env -> Limits -> request -> Proxy native -> ResourceT IO (Either AWS.Error (AWS.AWSResponse native))
sendBounded charge requestCharge consumesBody env limits request native = do
    liftIO $ charge (saturatingStateCharge (toInteger stateExchangeCharge + toInteger requestCharge))
    received <- withRunInIO $ \run -> watched (progressFloor limits) $ \watch -> run $ do
        let metered =
                env
                    { AWS.hooks =
                        Hooks.clientRequestHook
                            ( \previous current outgoing -> do
                                requestWithHooks <- previous current outgoing
                                pure requestWithHooks{HTTP.requestBody = meteredUpload watch (HTTP.requestBody requestWithHooks)}
                            )
                            (AWS.hooks env)
                    }
        AWS.sendEither (AWS.once metered{AWS.retryCheck = \_ _ -> False}) request >>= \case
            Left err -> pure (Left err)
            Right (RawResponse service response) -> do
                (wireCharge, bytes) <- readSource charge requestCharge watch limits (HTTP.responseBody response)
                pure (Right (wireCharge, bytes, service, fmap (const (yield bytes)) response))
    case received of
        Nothing -> throwIO ObjectProgress
        Just (Left err) -> pure (Left err)
        Just (Right (wireCharge, bytes, service, response)) -> do
            let decodeCharge = if consumesBody || statusCode (HTTP.responseStatus response) >= 300 then decodedDocumentCharge bytes else 0
            liftIO $ charge (saturatingStateCharge (toInteger wireCharge + toInteger decodeCharge))
            fmap HTTP.responseBody <$> AWS.response pure service native response

readSource :: StateCharge -> Int -> Watch -> Limits -> Types.ClientBody -> ResourceT IO (Int, ByteString)
readSource charge requestCharge watch limits source = withRunInIO $ \run -> do
    wireCharge <- newIORef (toInteger stateExchangeCharge + toInteger requestCharge)
    let payChunk size = do
            total <- atomicModifyIORef' wireCharge (\previous -> let nextCharge = previous + toInteger (bufferedChunkCharge size) in (nextCharge, nextCharge))
            charge (saturatingStateCharge total)
    pending <- newIORef (sealConduitT source)
    let next = do
            current <- readIORef pending
            (rest, chunk) <- run (current $$++ await)
            writeIORef pending rest
            case chunk of
                Nothing -> pure BS.empty
                Just bytes
                    | BS.null bytes -> next
                    | otherwise -> pure bytes
    boundedRead (MetadataBodyLimit (maxMetadataBytes limits)) (chargedRead payChunk (meteredReader watch next)) >>= \case
        Left _ -> throwIO (ObjectLimit (maxMetadataBytes limits))
        Right (_, bytes) -> (,bytes) . saturatingStateCharge <$> readIORef wireCharge

data ObjectProgress = ObjectProgress
    deriving stock (Show)

instance Exception ObjectProgress

byteCap :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
byteCap cap = boundBytes cap (const (throwIO (ObjectLimit cap)))

-- | GET returns bytes and their revision in the same resource lifetime.
readObject :: StateCharge -> AWS.Env -> Limits -> Text -> Text -> Maybe ByteString -> IO (Either ObjectFault ObjectRead)
readObject charge env limits bucket key revision = boundedCall $ do
    result <- runResourceT $ do
        response <- sendBounded charge 0 False env limits (BoundedGet revision request) (Proxy :: Proxy S3.GetObject)
        case response of
            Left err -> pure (Left err)
            Right received | received ^. S3L.getObjectResponse_httpStatus == 304 -> pure (Right unchanged)
            Right received -> do
                for_ (received ^. S3L.getObjectResponse_contentLength) $ \size ->
                    when (size > fromIntegral cap) (throwIO (ObjectLimit cap))
                chunks <- AWS.sinkBody (received ^. S3L.getObjectResponse_body) (byteCap cap .| C.sinkList)
                pure (Right (ObjectLoaded <$> objectRevision (received ^. S3L.getObjectResponse_eTag) <*> pure (BS.concat chunks)))
    case result of
        Right loaded -> pure loaded
        Left err
            | serviceStatus err == Just 304 -> pure unchanged
            | serviceStatus err == Just 404 && serviceCode err == Just "NoSuchKey" -> pure (Right ObjectAbsent)
            | serviceStatus err == Just 403 -> absenceFallback charge env limits bucket key
            | otherwise -> pure (Left (ObjectTransport (classifyAwsTransport err)))
  where
    cap = maxMetadataBytes limits
    unchanged = maybe (Left ObjectNoRevision) (Right . ObjectUnchanged) revision
    request = S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key)

-- | A failed or uncertain write returns to the caller for reread and union before another attempt.
writeObject :: StateCharge -> AWS.Env -> Limits -> Text -> Text -> Maybe ByteString -> ByteString -> IO (Either ObjectFault ObjectWrite)
writeObject charge env limits bucket key revision bytes
    | BS.length bytes > maxMetadataBytes limits = pure (Left (ObjectTooLarge (maxMetadataBytes limits)))
    | otherwise = boundedCall $ do
        result <- runResourceT (sendBounded charge (bufferedChunkCharge (BS.length bytes)) True env limits request (Proxy :: Proxy S3.PutObject))
        pure $ case result of
            Right response -> ObjectStored <$> objectRevision (response ^. S3L.putObjectResponse_eTag)
            Left err
                | serviceStatus err `elem` [Just 409, Just 412] -> Right ObjectConflict
                | otherwise -> Left (ObjectTransport (classifyAwsTransport err))
  where
    request = ConditionalPut revision (S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key) (AWS.toBody bytes))

absenceFallback :: StateCharge -> AWS.Env -> Limits -> Text -> Text -> IO (Either ObjectFault ObjectRead)
absenceFallback charge env limits bucket key = do
    result <- runResourceT (sendBounded charge 0 True env limits request (Proxy :: Proxy S3.ListObjectsV2))
    pure $ case result of
        Right response
            | response ^. S3L.listObjectsV2Response_isTruncated == Just False
            , null (fromMaybe [] (response ^. S3L.listObjectsV2Response_contents)) ->
                Right ObjectAbsent
            | otherwise -> Left ObjectUnknownAbsence
        Left err -> Left (ObjectTransport (classifyAwsTransport err))
  where
    request =
        BoundedList
            ( S3.newListObjectsV2 (S3.BucketName bucket)
                & S3L.listObjectsV2_prefix
                ?~ key
                    & S3L.listObjectsV2_maxKeys
                ?~ 1
            )

objectRevision :: Maybe S3.ETag -> Either ObjectFault ByteString
objectRevision (Just (S3.ETag value)) | not (BS.null value) = Right value
objectRevision _ = Left ObjectNoRevision

serviceStatus :: AWS.Error -> Maybe Int
serviceStatus (AWS.ServiceError err) = Just (statusCode (err ^. AWS.serviceError_status))
serviceStatus _ = Nothing

serviceCode :: AWS.Error -> Maybe Text
serviceCode (AWS.ServiceError err) = case err ^. AWS.serviceError_code of
    AWS.ErrorCode value -> Just value
serviceCode _ = Nothing

boundedCall :: IO (Either ObjectFault a) -> IO (Either ObjectFault a)
boundedCall action =
    action
        `catch` (\(ObjectLimit cap) -> pure (Left (ObjectTooLarge cap)))
        `catch` (\ObjectProgress -> pure (Left (ObjectTransport (transportFault TransportTimeout "the mirror state exchange fell below its progress floor"))))
        `catch` (\(err :: AWS.Error) -> pure (Left (ObjectTransport (classifyAwsTransport err))))
        `catch` (\(err :: HTTP.HttpException) -> pure (Left (ObjectTransport (classifyTransport err))))

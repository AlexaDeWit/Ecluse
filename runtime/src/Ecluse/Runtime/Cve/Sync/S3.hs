-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | S3 advisory transport over a shared credential environment, with bounded downloads.
module Ecluse.Runtime.Cve.Sync.S3 (
    S3CveSource,
    newS3CveSource,
    s3CveFetchFor,
    s3CveFetch,
    OsvDbCapExceeded (..),
    cappedAt,
) where

import Conduit (ConduitT, runResourceT, (.|))
import Data.Conduit.Combinators qualified as C
import UnliftIO.Exception (catch, throwIO)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Lens.Micro ((^.))

import Ecluse.Core.Cve.Types (DbEtag (..), OsvDbFetchFault (..))
import Ecluse.Core.Stream (boundBytes)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport, foldTransportEscapes, serviceStatus)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync.Step (CveFetch (..), FetchedObject (..))

-- | The __confined__ exception 'cappedAt' throws from inside a conduit, which 's3Download' folds into 'OsvDbTooLarge'.
newtype OsvDbCapExceeded = OsvDbCapExceeded Int
    deriving stock (Eq, Show)

instance Exception OsvDbCapExceeded

-- | An S3-backed advisory-fetch source over one @amazonka@ t'Amazonka.Env.Env', so every mount's 'CveFetch' shares one credential discovery.
newtype S3CveSource = S3CveSource
    { s3CveFetchFor :: Text -> Text -> Int -> CveFetch
    -- ^ A 'CveFetch' against one bucket, object key, and byte cap, over the captured env.
    }

-- | Build an 'S3CveSource' over one S3 @amazonka@ env, honouring the resolved endpoint override.
newS3CveSource :: Maybe AwsEndpoint -> IO S3CveSource
newS3CveSource mEndpoint = do
    awsEnv <- buildS3Env mEndpoint
    pure (S3CveSource (s3CveFetch awsEnv))

-- | A 'CveFetch' for one bucket, object key and byte cap over an @amazonka@ env.
s3CveFetch :: AWS.Env -> Text -> Text -> Int -> CveFetch
s3CveFetch awsEnv bucket key maxBytes =
    CveFetch
        { fetchHead = s3Head awsEnv bucket key
        , fetchDownload = s3Download awsEnv bucket key maxBytes
        }

s3Head :: AWS.Env -> Text -> Text -> IO (Either OsvDbFetchFault (Maybe FetchedObject))
s3Head awsEnv bucket key =
    runResourceT (AWS.sendEither awsEnv (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key))) <&> \case
        Right resp ->
            let observed etag = FetchedObject (dbEtag etag) (resp ^. S3L.headObjectResponse_lastModified)
             in maybe (Left OsvDbNoEtag) (Right . Just . observed) (resp ^. S3L.headObjectResponse_eTag)
        Left err
            | serviceStatus err == Just 404 -> Right Nothing
            | otherwise -> Left (OsvDbTransport (classifyAwsTransport err))

s3Download :: AWS.Env -> Text -> Text -> Int -> FilePath -> IO (Either OsvDbFetchFault FetchedObject)
s3Download awsEnv bucket key maxBytes dest = foldFetchEscapes . runResourceT $ do
    resp <- AWS.send awsEnv (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key))
    -- The declared length fails fast. The streaming cap is the enforcement: a
    -- declared length is not a guarantee.
    for_ (resp ^. S3L.getObjectResponse_contentLength) $ \len ->
        when (len > fromIntegral maxBytes) (throwIO (OsvDbCapExceeded maxBytes))
    AWS.sinkBody (resp ^. S3L.getObjectResponse_body) (cappedAt maxBytes .| C.sinkFile dest)
    let fetched etag = FetchedObject{foEtag = dbEtag etag, foPushedAt = resp ^. S3L.getObjectResponse_lastModified}
    pure (maybe (Left OsvDbNoEtag) (Right . fetched) (resp ^. S3L.getObjectResponse_eTag))

foldFetchEscapes :: IO (Either OsvDbFetchFault FetchedObject) -> IO (Either OsvDbFetchFault FetchedObject)
foldFetchEscapes act =
    foldTransportEscapes OsvDbTransport act
        `catch` (\(OsvDbCapExceeded n) -> pure (Left (OsvDbTooLarge n)))

dbEtag :: S3.ETag -> DbEtag
dbEtag (S3.ETag bytes) = DbEtag (decodeUtf8 bytes)

-- | A breach throws 'OsvDbCapExceeded' before yielding the excess chunk. The S3 adapter folds it into 'OsvDbTooLarge'.
cappedAt :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
cappedAt maxBytes = boundBytes maxBytes (const (throwIO (OsvDbCapExceeded maxBytes)))

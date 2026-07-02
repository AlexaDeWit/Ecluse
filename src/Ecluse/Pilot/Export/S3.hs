{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Export.S3 (
    uploadOsvToS3,
) where

import Amazonka
import Amazonka.S3 (defaultService)
import Amazonka.S3.PutObject
import Amazonka.S3.Types (BucketName (..), ObjectKey (..))
import Data.Text qualified as T
import Katip (KatipContext, Severity (..), logFM, ls)
import System.FilePath (takeFileName)

uploadOsvToS3 :: (KatipContext m) => Text -> FilePath -> m ()
uploadOsvToS3 bucketPath dbFile = do
    let bucket = BucketName bucketPath
        objKey = ObjectKey (T.pack (takeFileName dbFile))
    logFM InfoS (ls ("Uploading OSV database to S3 bucket " <> bucketPath <> " as " <> T.pack (takeFileName dbFile)))

    baseEnv <- liftIO $ newEnv discover

    envUrl <- liftIO $ lookupEnv "AWS_ENDPOINT_URL_S3"
    let parseEndpoint url = do
            let (scheme, rest) = break (== ':') url
            secure <- case scheme of
                "http" -> Just False
                "https" -> Just True
                _ -> Nothing
            let rest2 = drop 3 rest
            let (host, portStr) = break (== ':') rest2
            let port = if null portStr then (if secure then 443 else 80) else fromMaybe 80 (readMaybe (drop 1 portStr))
            Just (secure, encodeUtf8 (T.pack host), port)

    let env = case envUrl >>= parseEndpoint of
            Just (secure, host, port) ->
                configureService (setEndpoint secure host port defaultService) baseEnv
            Nothing -> baseEnv

    body <- liftIO $ hashedFile dbFile
    let req = newPutObject bucket objKey (toBody body)

    _ <- liftIO $ runResourceT $ send env req
    logFM InfoS (ls ("Successfully uploaded OSV database to S3 bucket " <> bucketPath))

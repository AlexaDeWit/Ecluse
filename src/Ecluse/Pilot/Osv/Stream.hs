{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Osv.Stream (
    streamOsvUrl,
    parseOsvStream,
) where

import Codec.Archive.Zip.Conduit.Types (ZipEntry (..))
import Codec.Archive.Zip.Conduit.UnZip (unZipStream)
import Conduit
import Data.Aeson (decodeStrict)
import Data.ByteString qualified as BS
import Katip (KatipContext, Severity (..), logFM, ls)
import Network.HTTP.Simple (getResponseBody, httpSource, parseRequest)
import OpenTelemetry.Context qualified as Ctx
import OpenTelemetry.Trace (SpanKind (Internal), defaultSpanArguments, kind, makeTracer, tracerOptions)
import OpenTelemetry.Trace.Core (createSpan, endSpan)

import Ecluse.Pilot.Osv (ExtractedOsv, OsvAdvisory, extractFromAdvisory)
import Ecluse.Telemetry (Telemetry, telemetryTracerProvider)

-- | Fetch the OSV zip and stream its contents
streamOsvUrl :: (MonadResource m, MonadThrow m, KatipContext m) => Telemetry -> String -> ConduitT i ExtractedOsv m ()
streamOsvUrl telemetry urlStr = do
    lift $ logFM InfoS (ls ("Initializing OSV stream from URL: " <> urlStr))
    let mTracer = (\tp -> makeTracer tp "ecluse" tracerOptions) <$> telemetryTracerProvider telemetry
    bracketP
        (traverse (\t -> createSpan t Ctx.empty "ecluse.pilot.osv.stream" defaultSpanArguments{kind = Internal}) mTracer)
        (mapM_ (`endSpan` Nothing))
        ( \_ -> do
            req <- liftIO $ parseRequest urlStr
            httpSource req (\res -> getResponseBody res .| parseOsvStream telemetry)
        )

-- | Parse the zip stream and emit ExtractedOsv
parseOsvStream :: (MonadResource m, MonadThrow m, KatipContext m) => Telemetry -> ConduitT ByteString ExtractedOsv m ()
parseOsvStream telemetry = do
    lift $ logFM InfoS (ls ("Starting OSV zip extraction and parsing pipeline" :: String))
    let mTracer = (\tp -> makeTracer tp "ecluse" tracerOptions) <$> telemetryTracerProvider telemetry
    bracketP
        (traverse (\t -> createSpan t Ctx.empty "ecluse.pilot.osv.parse" defaultSpanArguments{kind = Internal}) mTracer)
        (mapM_ (`endSpan` Nothing))
        (\_ -> void (transPipe liftIO unZipStream) .| processZipEntries)

processZipEntries :: (MonadThrow m, KatipContext m) => ConduitT (Either ZipEntry ByteString) ExtractedOsv m ()
processZipEntries =
    await >>= \case
        Nothing -> lift $ logFM InfoS (ls ("OSV stream fully processed" :: String))
        Just (Left entry) -> do
            fileBytes <- collectFile
            case decodeStrict fileBytes :: Maybe OsvAdvisory of
                Just adv -> yieldMany (extractFromAdvisory adv)
                Nothing -> do
                    let zipNameTxt = case zipEntryName entry of
                            Left txt -> txt
                            Right bs -> decodeUtf8With lenientDecode bs
                    lift $ logFM WarningS (ls ("Failed to parse OSV advisory JSON from entry: " <> zipNameTxt))
            processZipEntries
        Just (Right _) -> processZipEntries

collectFile :: (Monad m) => ConduitT (Either ZipEntry ByteString) o m ByteString
collectFile = go []
  where
    go acc =
        await >>= \case
            Nothing -> pure (BS.concat (reverse acc))
            Just (Left entry) -> do
                leftover (Left entry)
                pure (BS.concat (reverse acc))
            Just (Right bs) -> go (bs : acc)

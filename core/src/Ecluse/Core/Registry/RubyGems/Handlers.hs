-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Named-gem requests reuse the shared per-package admission pipeline under one ticket.
module Ecluse.Core.Registry.RubyGems.Handlers (
    parseBatchQuery,
    serveDependencies,
) where

import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai (Request, ResponseReceived, rawQueryString)
import UnliftIO (withRunInIO)

import Ecluse.Core.Deadline qualified as Deadline
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..))
import Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeDocumentBudget, newNativeReadBudget)
import Ecluse.Core.Registry.RubyGems.Marshal (encodeDependencies)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Route.Response (RubyResponse, rubyBadRequest, rubyReplies)
import Ecluse.Core.Registry.RubyGems.Serving (marshalDependencyRows)
import Ecluse.Core.Security.Limits (Limits (..), floorServeCapMicros)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (charge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfOutputPermille))
import Ecluse.Core.Server.Context (Handler, MountBinding (bindingPackumentDeps), PackumentDeps (..), ServeRuntime (..), ctxMount, ctxRuntime)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), answerMetadataBytes, mapPackumentReplies, withObservedAdmittedDocument)
import Ecluse.Core.Server.Pipeline.Shared (edgeTokenMatches, forwardedCredential, shedMessage, unauthorisedMessage, withListingAdmission)
import Ecluse.Core.Server.Response (mkRefusal)

-- | Bound the complete raw query and accept only one native named-gem parameter.
parseBatchQuery :: Limits -> ByteString -> Either ParseError [PackageName]
parseBatchQuery limits raw = do
    when (BS.length raw > maxMetadataBytes limits) (bad "the dependency query exceeds its byte bound")
    unless (validEscapes raw) (bad "the dependency query contains a malformed escape")
    case parseQuery raw of
        [] -> Right []
        [("gems", Just encoded)] -> do
            text <- first (const (ParseError "the dependency query is not UTF-8")) (TE.decodeUtf8' encoded)
            let names = T.splitOn "," text
            when (length names > 100) (bad "the dependency query exceeds 100 package names")
            checked <- traverse projectName names
            pure (deduplicate Set.empty checked)
        _ -> bad "the dependency query requires one gems parameter"
  where
    bad = Left . ParseError
    deduplicate seen = \case
        [] -> []
        name : rest | Set.member name seen -> deduplicate seen rest
        name : rest -> name : deduplicate (Set.insert name seen) rest

validEscapes :: ByteString -> Bool
validEscapes = walk . BS.unpack
  where
    walk [] = True
    walk (37 : a : b : rest) = hex a && hex b && walk rest
    walk (37 : _) = False
    walk (_ : rest) = walk rest
    hex c = (c >= 48 && c <= 57) || (c >= 65 && c <= 70) || (c >= 97 && c <= 102)

-- | A single deadline and reservation cover every named package and the final Marshal response.
serveDependencies :: Request -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveDependencies request respond = do
    mount <- asks ctxMount
    rt <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
        reply = liftIO . respond
        unavailable = packumentUnavailable rubyReplies [] (mkRefusal (pdHelp deps) "the dependency batch exceeded its deadline")
    if not (edgeTokenMatches (pdInboundToken deps) (forwardedCredential mount request))
        then reply (packumentUnauthorised rubyReplies [] (mkRefusal Nothing unauthorisedMessage))
        else
            if BS.length (rawQueryString request) > maxMetadataBytes (pdLimits deps)
                then reply (rubyBadRequest "the dependency query exceeds its byte bound")
                else
                    withListingAdmission
                        rt
                        (\headers -> reply (packumentUnavailable rubyReplies headers (mkRefusal Nothing shedMessage)))
                        ( \ticket -> withRunInIO $ \run ->
                            fromMaybe unavailable
                                <$> Deadline.timeout
                                    (ecDeadlines (srPublicClient rt))
                                    (floorServeCapMicros (progressFloor (pdLimits deps)))
                                    ( run $ do
                                        liftIO (charge ticket (4 * BS.length (rawQueryString request)))
                                        case parseBatchQuery (pdLimits deps) (rawQueryString request) of
                                            Left fault -> pure (rubyBadRequest (parseErrorMessage fault))
                                            Right names -> do
                                                observe <- liftIO (newNativeReadBudget (maxMetadataBytes (pdLimits deps)))
                                                checkDocument <- liftIO (newNativeDocumentBudget (maxMetadataBytes (pdLimits deps)) (Left (rubyBadRequest "the dependency batch exceeds its cumulative metadata bound")))
                                                batch ticket deps observe checkDocument 0 0 [] names
                                    )
                        )
                        reply
  where
    batch ticket deps observe checkDocument bytes rows accumulated = \case
        [] -> do
            let limit = maxMetadataBytes (pdLimits deps)
            liftIO (charge ticket (fromInteger (min (toInteger (maxBound :: Int)) (2 * bytes + 4096))))
            case encodeDependencies limit (concat (reverse accumulated)) of
                Left fault -> pure (rubyBadRequest (parseErrorMessage fault))
                Right encoded -> answerMetadataBytes rubyReplies deps ticket request encoded
        name : rest -> do
            selected <-
                withObservedAdmittedDocument
                    (mapPackumentReplies Left rubyReplies)
                    ticket
                    observe
                    checkDocument
                    Nothing
                    name
                    request
                    (Right Nothing)
                    ( \cached sourceBytes _ -> do
                        liftIO (charge ticket (scaleCharge (cfOutputPermille (metadataChargeFactors (pdMetadata deps))) sourceBytes))
                        pure (Right (Just (cached, sourceBytes)))
                    )
            case selected of
                Left refusal -> pure refusal
                Right Nothing -> batch ticket deps observe checkDocument bytes rows accumulated rest
                Right (Just (cached, sourceBytes)) -> case snd rubyGemsCached cached of
                    Nothing -> pure (packumentInternal rubyReplies [] (mkRefusal Nothing "the admitted document is not RubyGems metadata"))
                    Just document -> do
                        let nativeRows = gemRows document
                            nextBytes = bytes + toInteger sourceBytes
                            nextRows = rows + toInteger (length nativeRows)
                        if nextBytes > toInteger (maxMetadataBytes (pdLimits deps)) || nextRows > toInteger (maxArtifactCount (pdLimits deps)) || nextRows > toInteger (maxVersionCount (pdLimits deps))
                            then pure (rubyBadRequest "the dependency batch exceeds its cumulative metadata bound")
                            else do
                                let converted = marshalDependencyRows document
                                batch ticket deps observe checkDocument nextBytes nextRows (converted : accumulated) rest

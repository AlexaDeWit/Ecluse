-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The S3 publication of a compiled OSV artifact, guarded against older and sharply reduced data.

Each attempt reads the destination's metadata with HEAD, asks "Ecluse.Core.Osv.Publication"
whether the candidate may replace what it found, and writes the artifact and its new metadata
in one conditional PUT over that revision. A conflict rereads and decides again. The metadata
carries the durable baseline, so the guard reads no object history and no stored database.
-}
module Ecluse.Runtime.Pilot.Export (
    Published (..),
    PublicationStop (..),
    StoreFault (..),
    RereadFinding (..),
    renderPublicationStop,
    maxPublicationAttempts,
    publishCandidate,
) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Conduit (runResourceT)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Katip (KatipContext, Severity (..), katipAddContext, logFM, ls, sl)
import Lens.Micro ((.~), (^.))
import System.Directory (getFileSize)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (handle, try)

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, renderTransportFault, transportFault)
import Ecluse.Core.Osv.Compile (OsvCandidate, candidateFacts, candidatePath, stampCandidateNonce)
import Ecluse.Core.Osv.Publication
import Ecluse.Core.Osv.Publication.Envelope (decodeEnvelope, encodeEnvelope)
import Ecluse.Core.Telemetry.Record (timedSeconds)
import Ecluse.Core.Telemetry.Span (withOptionalSpan)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport, serviceStatus)
import Ecluse.Runtime.Aws.S3.Conditional (Capped (..), ConditionalPut (..), ResponseTooLarge (..), WritePrecondition (..), absenceListing, isWriteConflict, listingProvesAbsence)
import OpenTelemetry.Trace.Core (Span, SpanKind (Client), SpanStatus (Error), TracerProvider, addAttribute, setStatus)

-- | A publication that landed: the revision the store reported, and the decision it carried out.
data Published = Published
    { pubRevision :: Maybe ObjectRevision
    , pubPlan :: PublicationPlan
    }
    deriving stock (Eq, Show)

-- | Why a publication stopped without replacing the stored artifact.
data PublicationStop
    = -- | The guard refused the candidate.
      PublicationRefused PublicationRefusal
    | -- | The store could not be read or written as the guard requires.
      PublicationFaulted StoreFault
    deriving stock (Eq, Show)

instance Exception PublicationStop where
    displayException = toString . renderPublicationStop

-- | A store condition that leaves the publication's outcome unproven, so nothing is promoted.
data StoreFault
    = -- | A request failed outright.
      StoreTransport TransportFault
    | -- | The object exists, and HEAD was refused, so its metadata cannot be read.
      StoreReadDenied
    | -- | HEAD was refused, and the listing could not prove the key absent.
      StoreAbsenceUnknown
    | -- | Both HEAD and the one-key listing were refused.
      StoreListDenied
    | -- | A HEAD or listing response passed its byte bound, carried.
      StoreResponseTooLarge Int
    | -- | HEAD answered without an ETag or a length.
      StoreNoRevision
    | -- | The store refused a conditional write as unimplemented.
      StoreConditionalWriteUnsupported
    | -- | The new metadata cannot be written.
      StoreEnvelopeRefused EnvelopeFault
    | -- | Every attempt lost a race to another writer.
      StoreConflictsExhausted Int
    | -- | The write failed without proof either way, and the reread found no write of this publication.
      StoreWriteUncertain TransportFault RereadFinding
    deriving stock (Eq, Show)

-- | What the reread after an unproven write found.
data RereadFinding
    = RereadNoObject
    | RereadOtherRevision ObjectRevision
    | RereadFailed StoreFault
    deriving stock (Eq, Show)

-- | How many conditional writes one publication attempts before it gives up a race.
maxPublicationAttempts :: Int
maxPublicationAttempts = 3

-- | The stop as an operator reads it.
renderPublicationStop :: PublicationStop -> Text
renderPublicationStop = \case
    PublicationRefused refusal -> renderPublicationRefusal refusal
    PublicationFaulted fault -> renderStoreFault fault

renderStoreFault :: StoreFault -> Text
renderStoreFault = \case
    StoreTransport fault -> "the store request failed: " <> renderTransportFault fault
    StoreReadDenied -> "the store refused HEAD on an existing object, so its publication metadata cannot be read. Pilot needs s3:GetObject"
    StoreAbsenceUnknown -> "the store refused HEAD, and its listing did not prove the key absent. Pilot needs s3:GetObject and prefix-scoped s3:ListBucket"
    StoreListDenied -> "the store refused HEAD and the one-key listing, so a missing object and a denied read look alike. Pilot needs s3:GetObject and s3:ListBucket limited to the advisory prefix"
    StoreResponseTooLarge bytes -> "the store's response passed its " <> show bytes <> "-byte bound"
    StoreNoRevision -> "the store answered HEAD without an ETag or a length"
    StoreConditionalWriteUnsupported -> "the store does not implement conditional writes, which guarded publication requires"
    StoreEnvelopeRefused fault -> "the new artifact's metadata cannot be written: " <> renderEnvelopeFault fault
    StoreConflictsExhausted attempts -> "another writer replaced the object during each of " <> show attempts <> " attempts"
    StoreWriteUncertain fault finding -> "the write failed without proof either way (" <> renderTransportFault fault <> "), and a reread " <> renderFinding finding

renderFinding :: RereadFinding -> Text
renderFinding = \case
    RereadNoObject -> "found no object"
    RereadOtherRevision (ObjectRevision tag) -> "found revision " <> tag <> ", which no attempt of this publication wrote"
    RereadFailed fault -> "could not read the object: " <> renderStoreFault fault

-- | Publish the candidate under @bucket@ and @key@, deciding each attempt on a fresh HEAD. Each attempt's nonce identifies it if it lands unseen.
publishCandidate :: (MonadUnliftIO m, KatipContext m) => Maybe TracerProvider -> AWS.Env -> ShrinkPolicy -> BaselineCapability -> Maybe Rebaseline -> Text -> Text -> OsvCandidate -> m (Either PublicationStop Published)
publishCandidate mTracerProvider env policy capability rebaseline bucket key candidate =
    withOptionalSpan mTracerProvider Client "ecluse.pilot.osv.upload" $ \mSpan -> do
        size <- liftIO (getFileSize (candidatePath candidate))
        stampUploadSpan mSpan bucket key size
        (outcome, elapsed) <- timedSeconds (attempt 1 [])
        katipAddContext (sl "bucket" bucket <> sl "object_key" key <> sl "bytes" size <> sl "duration_s" elapsed) $
            either (reportStop mSpan) reportPublished outcome
        pure outcome
  where
    attempt number attempted =
        liftIO (observeStore env bucket key) >>= \case
            Left fault -> pure (Left (PublicationFaulted fault))
            Right observed
                | Just landed <- ownPublication attempted observed -> pure (Right landed)
                | otherwise -> do
                    nonce <- newPublicationNonce
                    stampCandidateNonce candidate nonce
                    bytes <- liftIO (getFileSize (candidatePath candidate))
                    let request = PublicationRequest (candidateFacts candidate) bytes nonce rebaseline
                    case planPublication policy capability request observed of
                        Left refusal -> pure (Left (PublicationRefused refusal))
                        Right plan -> writePlanned number (plan : attempted) plan
    writePlanned number attempted plan = case encodeEnvelope (plState plan) of
        Left fault -> pure (Left (PublicationFaulted (StoreEnvelopeRefused fault)))
        Right envelope ->
            liftIO (putCandidate env bucket key (candidatePath candidate) (plPrecondition plan) envelope) >>= \case
                Right (Right revision) -> pure (Right (Published revision plan))
                Left (ResponseTooLarge bytes) -> reconcile attempted (transportFault TransportProtocol ("the response passed its " <> show bytes <> "-byte bound"))
                Right (Left err)
                    | isWriteConflict err && number < maxPublicationAttempts -> do
                        logFM WarningS (ls ("Another writer replaced " <> key <> " during publication, so Pilot rereads it and decides again"))
                        attempt (number + 1) attempted
                    | isWriteConflict err -> pure (Left (PublicationFaulted (StoreConflictsExhausted number)))
                    | serviceStatus err == Just 501 -> pure (Left (PublicationFaulted StoreConditionalWriteUnsupported))
                    | otherwise -> reconcile attempted (classifyAwsTransport err)
    -- A failed response does not prove the write was lost, so the nonce on a reread decides.
    reconcile attempted fault =
        liftIO (observeStore env bucket key) <&> \case
            Right observed | Just landed <- ownPublication attempted observed -> Right landed
            Right StoreAbsent -> uncertain RereadNoObject
            Right (StorePresent revision _ _) -> uncertain (RereadOtherRevision revision)
            Left readFault -> uncertain (RereadFailed readFault)
      where
        uncertain = Left . PublicationFaulted . StoreWriteUncertain fault

-- An object that carries the nonce of one of this publication's own attempts is that attempt, landed.
ownPublication :: [PublicationPlan] -> StoreObservation -> Maybe Published
ownPublication attempted = \case
    StorePresent revision _ (Right stored) ->
        Published (Just revision) <$> find ((== psNonce stored) . psNonce . plState) attempted
    _ -> Nothing

-- Read the destination's revision, size, and publication metadata without its body. A refused HEAD
-- falls back on a one-key listing, so a missing key is told apart from a denied read.
observeStore :: AWS.Env -> Text -> Text -> IO (Either StoreFault StoreObservation)
observeStore env bucket key =
    capped $
        runResourceT (AWS.sendEither env (Capped (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key)))) >>= \case
            Right response -> pure (presentObject response)
            Left err
                | serviceStatus err == Just 404 -> pure (Right StoreAbsent)
                | serviceStatus err == Just 403 -> deniedHead env bucket key
                | otherwise -> pure (Left (StoreTransport (classifyAwsTransport err)))
  where
    capped = handle (\(ResponseTooLarge bytes) -> pure (Left (StoreResponseTooLarge bytes)))

presentObject :: S3.HeadObjectResponse -> Either StoreFault StoreObservation
presentObject response = case (response ^. S3L.headObjectResponse_eTag, response ^. S3L.headObjectResponse_contentLength) of
    (Just (S3.ETag tag), Just size) | not (T.null (decodeUtf8 tag)) -> Right (StorePresent (ObjectRevision (decodeUtf8 tag)) size stored)
    _ -> Left StoreNoRevision
  where
    stored = decodeEnvelope (HashMap.toList (response ^. S3L.headObjectResponse_metadata))

deniedHead :: AWS.Env -> Text -> Text -> IO (Either StoreFault StoreObservation)
deniedHead env bucket key =
    runResourceT (AWS.sendEither env (Capped (absenceListing bucket key))) <&> \case
        Right listing
            | listingProvesAbsence listing -> Right StoreAbsent
            | any ((== S3.ObjectKey key) . (^. S3L.object_key)) (fromMaybe [] (listing ^. S3L.listObjectsV2Response_contents)) -> Left StoreReadDenied
            | otherwise -> Left StoreAbsenceUnknown
        Left err
            | serviceStatus err == Just 403 -> Left StoreListDenied
            | otherwise -> Left (StoreTransport (classifyAwsTransport err))

-- The body streams from the candidate file, and amazonka's own retry is off: a resent PUT would
-- reread the file, and the guard rereads the store itself before it tries again.
putCandidate :: AWS.Env -> Text -> Text -> FilePath -> Precondition -> [(Text, Text)] -> IO (Either ResponseTooLarge (Either AWS.Error (Maybe ObjectRevision)))
putCandidate env bucket key path precondition envelope = try $ do
    body <- AWS.chunkedFile 1048576 path
    let native = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key) body & S3L.putObject_metadata .~ HashMap.fromList envelope
    result <- runResourceT (AWS.sendEither (AWS.once env{AWS.retryCheck = \_ _ -> False}) (Capped (ConditionalPut (writePrecondition precondition) native)))
    pure (fmap (\response -> (\(S3.ETag tag) -> ObjectRevision (decodeUtf8 tag)) <$> response ^. S3L.putObjectResponse_eTag) result)

writePrecondition :: Precondition -> WritePrecondition
writePrecondition = \case
    CreateOnly -> IfAbsent
    ReplaceRevision (ObjectRevision tag) -> IfRevision (encodeUtf8 tag)

stampUploadSpan :: (MonadIO m) => Maybe Span -> Text -> Text -> Integer -> m ()
stampUploadSpan mSpan bucket key size =
    whenJust mSpan $ \sp -> do
        addAttribute sp "ecluse.osv.bucket" bucket
        addAttribute sp "ecluse.osv.object_key" key
        addAttribute sp "ecluse.osv.bytes" (show size :: Text)

reportPublished :: (KatipContext m) => Published -> m ()
reportPublished published =
    katipAddContext (sl "baseline_bytes" (psBaselineBytes stored) <> sl "epss_credit_bytes" (psEpssCreditBytes stored)) $
        logFM InfoS (ls ("Published the advisory artifact: OSV " <> renderSourceOrder (plOsvOrder plan) <> ", EPSS " <> maybe "unavailable" renderSourceOrder (plEpssOrder plan) <> ", baseline " <> renderAdjustment (psAdjustment stored)))
  where
    plan = pubPlan published
    stored = plState plan

reportStop :: (KatipContext m) => Maybe Span -> PublicationStop -> m ()
reportStop mSpan stop = do
    whenJust mSpan $ \sp -> setStatus sp (Error ("advisory publication stopped: " <> renderPublicationStop stop))
    logFM ErrorS (ls ("Advisory publication stopped: " <> renderPublicationStop stop))

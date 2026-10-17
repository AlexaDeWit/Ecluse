-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The publication state as S3 user metadata, written in the same request as the artifact.

A metadata-only read returns it, so Pilot orders and sizes a publication without downloading the
stored database. Every value is printable ASCII, and the envelope stays under S3's user-metadata
limit. A missing key, an unknown version, and a bad value are distinct faults, so an object that
predates guarded publication never reads as an empty store.
-}
module Ecluse.Core.Osv.Publication.Envelope (
    envelopeVersion,
    maxEnvelopeBytes,
    encodeEnvelope,
    decodeEnvelope,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Universe.Class (Universe (..))
import Data.Universe.Class qualified as Universe
import Data.Universe.Generic (universeGeneric)

import Ecluse.Core.Osv.Publication.Types (
    Adjustment,
    EnvelopeFault (..),
    PublishedState (..),
    SourceMarker (..),
    parsePublicationNonce,
    parseSourceDigestHex,
    parseSourceIdentityHex,
    renderAdjustment,
    renderPublicationNonce,
    sourceDigestHex,
    sourceIdentityHex,
 )
import Ecluse.Core.Osv.Schema (EpssStatus (EnrichmentAvailable, EnrichmentUnavailable), renderEpssStatus)
import Ecluse.Core.Text (readDecimalText)
import Ecluse.Core.Text.Iso8601 (readIso8601Utc, renderIso8601Utc)

-- | The envelope layout this build writes, and the only one it reads.
envelopeVersion :: Text
envelopeVersion = "1"

-- | S3's user-metadata limit: the UTF-8 bytes of every key and value together.
maxEnvelopeBytes :: Int
maxEnvelopeBytes = 2048

data EnvelopeKey
    = KeyVersion
    | KeyEcosystem
    | KeySchemaEpoch
    | KeyOsvSource
    | KeyOsvLastModified
    | KeyOsvDigest
    | KeyEpssStatus
    | KeyEpssSource
    | KeyEpssLastModified
    | KeyEpssDigest
    | KeyRelevantRows
    | KeyScoredRows
    | KeyBaselineBytes
    | KeyEpssCreditBytes
    | KeyNonce
    | KeyAdjustment
    deriving stock (Eq, Generic)

instance Universe EnvelopeKey where universe = universeGeneric

-- S3 stores metadata keys in lower case, so every key is written that way.
renderKey :: EnvelopeKey -> Text
renderKey = \case
    KeyVersion -> "publication-version"
    KeyEcosystem -> "ecosystem"
    KeySchemaEpoch -> "schema-epoch"
    KeyOsvSource -> "osv-source"
    KeyOsvLastModified -> "osv-last-modified"
    KeyOsvDigest -> "osv-digest"
    KeyEpssStatus -> "epss-status"
    KeyEpssSource -> "epss-source"
    KeyEpssLastModified -> "epss-last-modified"
    KeyEpssDigest -> "epss-digest"
    KeyRelevantRows -> "relevant-rows"
    KeyScoredRows -> "scored-rows"
    KeyBaselineBytes -> "baseline-bytes"
    KeyEpssCreditBytes -> "epss-credit-bytes"
    KeyNonce -> "publication-nonce"
    KeyAdjustment -> "baseline-adjustment"

-- | Render the state for a write. A value outside printable ASCII, or a state past the limit, is refused.
encodeEnvelope :: PublishedState -> Either EnvelopeFault [(Text, Text)]
encodeEnvelope published = do
    for_ pairs $ \(key, value) -> unless (T.all printableAscii value) (Left (EnvelopeMalformed key))
    within pairs
  where
    pairs = [(renderKey key, renderValue published key) | key <- Universe.universe]

renderValue :: PublishedState -> EnvelopeKey -> Text
renderValue published = \case
    KeyVersion -> envelopeVersion
    KeyEcosystem -> psEcosystem published
    KeySchemaEpoch -> show (psSchemaEpoch published)
    KeyOsvSource -> identityText (psOsv published)
    KeyOsvLastModified -> stampText (psOsv published)
    KeyOsvDigest -> digestText (psOsv published)
    KeyEpssStatus -> renderEpssStatus (psEpssStatus published)
    KeyEpssSource -> maybe noMarker identityText (psLastEpss published)
    KeyEpssLastModified -> maybe noMarker stampText (psLastEpss published)
    KeyEpssDigest -> maybe noMarker digestText (psLastEpss published)
    KeyRelevantRows -> show (psRelevantRows published)
    KeyScoredRows -> show (psScoredRows published)
    KeyBaselineBytes -> show (psBaselineBytes published)
    KeyEpssCreditBytes -> show (psEpssCreditBytes published)
    KeyNonce -> renderPublicationNonce (psNonce published)
    KeyAdjustment -> renderAdjustment (psAdjustment published)
  where
    identityText = (identityLabel <>) . sourceIdentityHex . smIdentity
    digestText = (digestLabel <>) . sourceDigestHex . smDigest
    stampText = maybe unknownStamp renderIso8601Utc . smLastModified

-- | Read the state a stored object carries. Keys compare without case, as S3 may return them in any.
decodeEnvelope :: [(Text, Text)] -> Either EnvelopeFault PublishedState
decodeEnvelope raw = do
    void (within raw)
    whenJust (lookupKey KeyVersion) $ \version ->
        unless (version == envelopeVersion) (Left (EnvelopeUnsupported (T.take 32 version)))
    whenJust (nonEmpty [renderKey key | key <- Universe.universe, isNothing (lookupKey key)]) (Left . EnvelopeMissing)
    osv <- marker KeyOsvSource KeyOsvLastModified KeyOsvDigest
    status <- field KeyEpssStatus epssStatusOf
    lastEpss <- epssMarker
    when (status == EnrichmentAvailable && isNothing lastEpss) (Left (EnvelopeMalformed (renderKey KeyEpssStatus)))
    PublishedState
        <$> field KeyEcosystem ecosystemOf
        <*> field KeySchemaEpoch (fmap fromInteger . boundedCount)
        <*> pure osv
        <*> pure status
        <*> pure lastEpss
        <*> field KeyRelevantRows boundedCount
        <*> field KeyScoredRows boundedCount
        <*> field KeyBaselineBytes boundedCount
        <*> field KeyEpssCreditBytes boundedCount
        <*> field KeyNonce parsePublicationNonce
        <*> field KeyAdjustment adjustmentOf
  where
    lowered = [(T.toLower key, value) | (key, value) <- raw]
    lookupKey key = T.strip <$> lookup (renderKey key) lowered
    field key parse = maybe (Left (EnvelopeMalformed (renderKey key))) Right (parse =<< lookupKey key)
    marker identityKey stamp digest =
        SourceMarker
            <$> field identityKey (parseSourceIdentityHex <=< T.stripPrefix identityLabel)
            <*> field stamp stampOf
            <*> field digest (parseSourceDigestHex <=< T.stripPrefix digestLabel)
    epssKeys = [KeyEpssSource, KeyEpssLastModified, KeyEpssDigest]
    epssMarker
        | all ((== Just noMarker) . lookupKey) epssKeys = Right Nothing
        | otherwise = Just <$> marker KeyEpssSource KeyEpssLastModified KeyEpssDigest

within :: [(Text, Text)] -> Either EnvelopeFault [(Text, Text)]
within pairs
    | size > maxEnvelopeBytes = Left (EnvelopeTooLarge size)
    | otherwise = Right pairs
  where
    size = sum [utf8Length key + utf8Length value | (key, value) <- pairs]
    utf8Length text = BS.length (encodeUtf8 text :: ByteString)

-- An absent timestamp is written as a word, so it never reads as a missing key.
stampOf :: Text -> Maybe (Maybe UTCTime)
stampOf value
    | value == unknownStamp = Just Nothing
    | otherwise = Just <$> readIso8601Utc value

epssStatusOf :: Text -> Maybe EpssStatus
epssStatusOf value = find ((== value) . renderEpssStatus) [EnrichmentAvailable, EnrichmentUnavailable]

ecosystemOf :: Text -> Maybe Text
ecosystemOf value = value <$ guard (not (T.null value) && T.compareLength value 64 /= GT && T.all printableAscii value)

-- Counts fit 'Int64', which also bounds what a hostile value can make the comparison hold.
boundedCount :: Text -> Maybe Integer
boundedCount value = do
    count <- readDecimalText value
    count <$ guard (count <= toInteger (maxBound :: Int64))

adjustmentOf :: Text -> Maybe Adjustment
adjustmentOf value = find ((== value) . renderAdjustment) Universe.universe

printableAscii :: Char -> Bool
printableAscii c = c >= ' ' && c <= '~'

-- The labels name each hash's input, so a reader never compares a URL hash with a body hash.
identityLabel, digestLabel, unknownStamp, noMarker :: Text
identityLabel = "sha256-url:"
digestLabel = "sha256-wire:"
unknownStamp = "unknown"
noMarker = "none"

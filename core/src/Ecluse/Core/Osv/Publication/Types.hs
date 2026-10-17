-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The vocabulary of guarded advisory publication, shared by the compile that gathers a
candidate's facts, the decision in "Ecluse.Core.Osv.Publication", and the stored metadata in
"Ecluse.Core.Osv.Publication.Envelope".
-}
module Ecluse.Core.Osv.Publication.Types (
    -- * Source evidence
    SourceIdentity,
    sourceIdentityOf,
    sourceIdentityHex,
    parseSourceIdentityHex,
    SourceDigest,
    sourceDigestOf,
    sourceDigestHex,
    parseSourceDigestHex,
    SourceMarker (..),

    -- * Revisions
    PublicationNonce,
    newPublicationNonce,
    renderPublicationNonce,
    parsePublicationNonce,
    ObjectRevision (..),

    -- * What a publication compares
    CandidateFacts (..),
    PublishedState (..),
    Adjustment (..),
    renderAdjustment,
    EnvelopeFault (..),
    renderEnvelopeFault,
    StoreObservation (..),

    -- * Policy
    Percent,
    mkPercent,
    unPercent,
    renderPercent,
    ShrinkPolicy (..),
    BaselineCapability (..),
    renderBaselineCapability,
    Rebaseline (..),
    epssBytesPerScoredRow,
) where

import Crypto.Random (getRandomBytes)
import Data.Char (isHexDigit, isUpper)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

import Ecluse.Core.Osv.Schema (EpssStatus)
import Ecluse.Core.Package.Hash (hexDigestText, sha256Hex)
import Ecluse.Core.Snapshot (ContentDigest, digestBytes)

-- | A source's fixed-width identity: the SHA-256 of its credential-free URL.
newtype SourceIdentity = SourceIdentity Text
    deriving stock (Eq, Show)

-- | Identify a source by its credential-free URL ('Ecluse.Core.Security.Authority.credentialFreeUrl').
sourceIdentityOf :: Text -> SourceIdentity
sourceIdentityOf url = SourceIdentity (sha256Hex (encodeUtf8 url))

-- | The identity's lowercase hex.
sourceIdentityHex :: SourceIdentity -> Text
sourceIdentityHex (SourceIdentity hex) = hex

-- | Read an identity back from its lowercase hex.
parseSourceIdentityHex :: Text -> Maybe SourceIdentity
parseSourceIdentityHex = fmap SourceIdentity . sha256HexText

-- | The SHA-256 of one complete source response body, before Pilot decompresses it.
newtype SourceDigest = SourceDigest Text
    deriving stock (Eq, Show)

-- | The digest of a fetch that read its source to the end ('Ecluse.Core.Stream.digestWhole').
sourceDigestOf :: ContentDigest -> SourceDigest
sourceDigestOf = SourceDigest . hexDigestText . digestBytes

-- | The digest's lowercase hex.
sourceDigestHex :: SourceDigest -> Text
sourceDigestHex (SourceDigest hex) = hex

-- | Read a digest back from its lowercase hex.
parseSourceDigestHex :: Text -> Maybe SourceDigest
parseSourceDigestHex = fmap SourceDigest . sha256HexText

-- | One source's snapshot. A digest proves equal bytes only, so only @Last-Modified@ orders two snapshots.
data SourceMarker = SourceMarker
    { smIdentity :: SourceIdentity
    , smLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the source answered with, at the precision it supplied.
    , smDigest :: SourceDigest
    }
    deriving stock (Eq, Show)

-- | A fixed-width value inside each attempted artifact. An ETag follows content, so this gives every publication its own revision.
newtype PublicationNonce = PublicationNonce Text
    deriving stock (Eq, Show)

-- | A fresh nonce of 128 random bits.
newPublicationNonce :: (MonadIO m) => m PublicationNonce
newPublicationNonce = PublicationNonce . hexDigestText <$> liftIO (getRandomBytes nonceBytes)

-- | The nonce's stored form: 32 lowercase hex digits.
renderPublicationNonce :: PublicationNonce -> Text
renderPublicationNonce (PublicationNonce hex) = hex

-- | Read a nonce back from its stored form.
parsePublicationNonce :: Text -> Maybe PublicationNonce
parsePublicationNonce = fmap PublicationNonce . lowerHexOfLength (2 * nonceBytes)

nonceBytes :: Int
nonceBytes = 16

-- | An object store's revision of the stored artifact, its ETag as the store reported it.
newtype ObjectRevision = ObjectRevision Text
    deriving stock (Eq, Show)

-- | What one compile learned. It claims nothing about the stored predecessor.
data CandidateFacts = CandidateFacts
    { cfEcosystem :: Text
    -- ^ The OSV ecosystem the artifact holds, as its object key names it.
    , cfOsv :: SourceMarker
    , cfEpss :: Maybe SourceMarker
    -- ^ The EPSS feed this compile joined, or 'Nothing' when optional enrichment was unavailable.
    , cfRelevantRows :: Integer
    , cfScoredRows :: Integer
    -- ^ Advisory ranges with an EPSS score, counted per row rather than per advisory.
    }
    deriving stock (Eq, Show)

-- | The publication state an accepted artifact carries beside it in the store.
data PublishedState = PublishedState
    { psEcosystem :: Text
    , psSchemaEpoch :: Int
    , psOsv :: SourceMarker
    , psEpssStatus :: EpssStatus
    , psLastEpss :: Maybe SourceMarker
    -- ^ The last EPSS feed a publication joined, kept through an optional outage. Present whenever enrichment is available.
    , psRelevantRows :: Integer
    , psScoredRows :: Integer
    , psBaselineBytes :: Integer
    -- ^ The durable high-water size a later publication's shrink is measured from.
    , psEpssCreditBytes :: Integer
    -- ^ The share of the baseline an optional EPSS outage removed from the comparison.
    , psNonce :: PublicationNonce
    , psAdjustment :: Adjustment
    }
    deriving stock (Eq, Show)

-- | How a publication set the size baseline it records.
data Adjustment
    = -- | The first publication under this object key, which starts the baseline at its own size.
      InitialBaseline
    | -- | An ordinary publication, with no EPSS credit in force.
      OrdinaryBaseline
    | -- | A publication measured with an EPSS outage credit in force.
      EpssOutageCredit
    | -- | An operator's one-shot rebaseline, which resets the baseline to this artifact's size.
      OperatorRebaseline
    deriving stock (Eq, Generic, Show)

instance Universe Adjustment where universe = universeGeneric

-- | The adjustment's stored and logged spelling.
renderAdjustment :: Adjustment -> Text
renderAdjustment = \case
    InitialBaseline -> "initial"
    OrdinaryBaseline -> "ordinary"
    EpssOutageCredit -> "epss-outage-credit"
    OperatorRebaseline -> "operator-rebaseline"

-- | Why the stored object's publication metadata cannot be read as a predecessor.
data EnvelopeFault
    = -- | The object carries none of these metadata keys.
      EnvelopeMissing (NonEmpty Text)
    | -- | The metadata names a publication version this build does not read.
      EnvelopeUnsupported Text
    | -- | This key's value does not parse, or contradicts another key.
      EnvelopeMalformed Text
    | -- | The metadata passes the store's size limit, in bytes.
      EnvelopeTooLarge Int
    deriving stock (Eq, Show)

-- | The fault as an operator reads it.
renderEnvelopeFault :: EnvelopeFault -> Text
renderEnvelopeFault = \case
    EnvelopeMissing keys -> "the publication metadata lacks " <> T.intercalate ", " (toList keys)
    EnvelopeUnsupported version -> "the publication metadata has version " <> version <> ", which this build does not read"
    EnvelopeMalformed key -> "the publication metadata " <> key <> " does not parse"
    EnvelopeTooLarge bytes -> "the publication metadata is " <> show bytes <> " bytes, past the store's limit"

-- | What one metadata-only read of the destination found.
data StoreObservation
    = -- | No object holds the key.
      StoreAbsent
    | -- | An object holds the key, with its revision, its size in bytes, and its decoded metadata.
      StorePresent ObjectRevision Integer (Either EnvelopeFault PublishedState)
    deriving stock (Eq, Show)

-- | A share of a size, from 0 to 100 percent, held exactly.
newtype Percent = Percent Rational
    deriving stock (Eq, Ord, Show)

-- | A percentage from 0 to 100. Anything outside that range is 'Nothing'.
mkPercent :: Rational -> Maybe Percent
mkPercent share
    | share >= 0 && share <= 100 = Just (Percent share)
    | otherwise = Nothing

-- | The percentage as a share, so 2% is 2.
unPercent :: Percent -> Rational
unPercent (Percent share) = share

-- | The percentage as an operator reads it.
renderPercent :: Percent -> Text
renderPercent (Percent share)
    | denominator share == 1 = show (numerator share) <> "%"
    | otherwise = show (fromRational share :: Double) <> "%"

-- | The configured shrink limits a publication is held to.
data ShrinkPolicy = ShrinkPolicy
    { spMaxShrink :: Percent
    -- ^ How far below the effective baseline a publication may fall.
    , spEpssLossCap :: Percent
    -- ^ The most an optional EPSS outage may credit, as a share of the previous artifact's size.
    }
    deriving stock (Eq, Show)

-- | The size baseline a store can keep. A weaker store declares it, and an outage never selects it.
data BaselineCapability
    = -- | The high-water size travels with each object, so no history read is needed.
      DurableBaseline
    | -- | Only the previous object's own size is known.
      PreviousObjectBaseline
    | -- | No size is known, so no shrink check runs.
      NoBaseline
    deriving stock (Eq, Show)

-- | The capability as the posture line and the publication log read it.
renderBaselineCapability :: BaselineCapability -> Text
renderBaselineCapability = \case
    DurableBaseline -> "durable baseline"
    PreviousObjectBaseline -> "previous-object baseline"
    NoBaseline -> "no baseline"

-- | An operator's one-shot approval to reset the baseline, bound to the predecessor it names.
data Rebaseline = Rebaseline
    { rbExpected :: ObjectRevision
    , rbMaxShrink :: Percent
    -- ^ The shrink this one publication may take in place of the configured limit.
    }
    deriving stock (Eq, Show)

-- | The bytes one scored range row adds over an unscored one, measured on the shipped table layout.
epssBytesPerScoredRow :: Integer
epssBytesPerScoredRow = 12

sha256HexText :: Text -> Maybe Text
sha256HexText = lowerHexOfLength 64

lowerHexOfLength :: Int -> Text -> Maybe Text
lowerHexOfLength width raw
    | T.compareLength raw width == EQ && T.all (\c -> isHexDigit c && not (isUpper c)) raw = Just raw
    | otherwise = Nothing

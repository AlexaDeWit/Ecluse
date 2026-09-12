-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | What one advisory artifact records about the sources it was compiled from, and how old
those sources say their data is.

Pilot writes these values into the artifact's @meta@ table ("Ecluse.Core.Osv.Schema") and the
consumer decodes them back at open. They are diagnostics and ordering evidence: nothing here
refuses a version. A source that declares no value records no row, so absence stays
distinguishable from a guess.
-}
module Ecluse.Core.Osv.Provenance (
    -- * The recorded sources
    AdvisoryProvenance (..),
    noProvenance,
    provenanceRows,
    decodeProvenance,

    -- * Source timestamps
    parseSourceTime,
    parseHttpDate,

    -- * The quiet-time reading
    QuietTime (..),
    defaultQuietTime,
    ProvenanceSource (..),
    SourceAge (..),
    sourceAges,
    sourceQuiet,
    renderSourceAge,
) where

import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (Day, NominalDiffTime, UTCTime (UTCTime), defaultTimeLocale, diffUTCTime, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Ecluse.Core.Osv.Schema (
    MetaKey (
        MetaEpssLastModified,
        MetaEpssModelVersion,
        MetaEpssScoreDate,
        MetaEpssSource,
        MetaOsvLastModified,
        MetaOsvNewestModified,
        MetaOsvSource
    ),
    renderMetaKey,
 )
import Ecluse.Core.Text (renderIso8601Utc)

{- | The sources one artifact was compiled from, as they described themselves. Every field is
'Nothing' when the source supplied no such value, or when an older artifact predates the key.
-}
data AdvisoryProvenance = AdvisoryProvenance
    { apOsvSource :: Maybe Text
    -- ^ The advisory export's credential-free URL.
    , apOsvLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the export answered the successful fetch with.
    , apOsvNewestModified :: Maybe UTCTime
    -- ^ The newest @modified@ across the advisory records the pass read.
    , apEpssSource :: Maybe Text
    -- ^ The EPSS feed's credential-free URL.
    , apEpssLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the feed answered the successful fetch with.
    , apEpssScoreDate :: Maybe UTCTime
    -- ^ The @score_date@ the feed declares, a bare date read as its UTC start of day.
    , apEpssModelVersion :: Maybe Text
    -- ^ The scoring model the feed declares.
    }
    deriving stock (Eq, Show)

-- | Nothing recorded, which is what an artifact compiled before these keys decodes to.
noProvenance :: AdvisoryProvenance
noProvenance = AdvisoryProvenance Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | The @meta@ rows one provenance writes. A value the source did not supply writes no row.
provenanceRows :: AdvisoryProvenance -> [(Text, Text)]
provenanceRows prov =
    catMaybes
        [ row MetaOsvSource (apOsvSource prov)
        , row MetaOsvLastModified (renderIso8601Utc <$> apOsvLastModified prov)
        , row MetaOsvNewestModified (renderIso8601Utc <$> apOsvNewestModified prov)
        , row MetaEpssSource (apEpssSource prov)
        , row MetaEpssLastModified (renderIso8601Utc <$> apEpssLastModified prov)
        , row MetaEpssScoreDate (renderIso8601Utc <$> apEpssScoreDate prov)
        , row MetaEpssModelVersion (apEpssModelVersion prov)
        ]
  where
    row key = fmap (renderMetaKey key,)

{- | Read the provenance an artifact's @meta@ rows carry. An absent key, an over-long value,
and an unreadable timestamp all read as absence, never as a fault.
-}
decodeProvenance :: [(Text, Text)] -> AdvisoryProvenance
decodeProvenance rows =
    noProvenance
        { apOsvSource = text maxSourceLength MetaOsvSource
        , apOsvLastModified = stamp MetaOsvLastModified
        , apOsvNewestModified = stamp MetaOsvNewestModified
        , apEpssSource = text maxSourceLength MetaEpssSource
        , apEpssLastModified = stamp MetaEpssLastModified
        , apEpssScoreDate = stamp MetaEpssScoreDate
        , apEpssModelVersion = text maxLabelLength MetaEpssModelVersion
        }
  where
    text limit key = do
        value <- lookup (renderMetaKey key) rows
        guard (T.compareLength value limit /= GT)
        pure value
    stamp key = parseSourceTime =<< text maxStampLength key

-- Bounds on what a decoded value may be, so an artifact cannot hand the consumer an
-- unbounded string. Each is several times the longest value Pilot writes.
maxSourceLength, maxStampLength, maxLabelLength :: Int
maxSourceLength = 2048
maxStampLength = 64
maxLabelLength = 128

{- | An RFC 3339 timestamp, or a bare date read as its UTC start of day. Anything else is
'Nothing', which records no value rather than an invented one.
-}
parseSourceTime :: Text -> Maybe UTCTime
parseSourceTime raw = rfc3339 <|> offsetWithoutColon <|> startOfDay
  where
    value = toString (T.strip raw)
    rfc3339 = iso8601ParseM value :: Maybe UTCTime
    -- The EPSS feed writes its offset as @+0000@, which RFC 3339 does not admit.
    offsetWithoutColon = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" value
    startOfDay = (`UTCTime` 0) <$> (iso8601ParseM value :: Maybe Day)

-- | The instant an HTTP @Last-Modified@ header names, or 'Nothing' when it is unreadable.
parseHttpDate :: Text -> Maybe UTCTime
parseHttpDate raw = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (toString (T.strip raw))

{- | How old each source may be before Pilot raises its alarm. Loud ecosystems take a short
threshold and slow ones a long threshold, so a quiet feed is not read as a stalled one.
-}
data QuietTime = QuietTime
    { qtOsv :: NominalDiffTime
    -- ^ The threshold for this ecosystem's advisory export.
    , qtEpss :: NominalDiffTime
    -- ^ The threshold for the EPSS feed, which is one feed across every ecosystem.
    }
    deriving stock (Eq, Show)

-- | Seven days, the threshold a source with no configured value of its own is judged by.
defaultQuietTime :: NominalDiffTime
defaultQuietTime = 604800

-- | Which upstream an age was read from.
data ProvenanceSource
    = -- | The ecosystem's advisory export, aged by its newest record @modified@.
      OsvExport
    | -- | The EPSS feed, aged by its declared @score_date@.
      EpssFeed
    deriving stock (Eq, Show)

-- | One source's age at an instant, with the threshold that decides whether it is quiet.
data SourceAge = SourceAge
    { saSource :: ProvenanceSource
    , saUrl :: Text
    -- ^ The credential-free source URL, or @\<unrecorded\>@ when the artifact carries none.
    , saAge :: NominalDiffTime
    , saThreshold :: NominalDiffTime
    }
    deriving stock (Eq, Show)

{- | The ages this provenance supports at @now@. A source that recorded no timestamp yields no
age, because nothing about it can be read as quiet or fresh.
-}
sourceAges :: UTCTime -> QuietTime -> AdvisoryProvenance -> [SourceAge]
sourceAges now quiet prov =
    catMaybes
        [ ageOf OsvExport (apOsvSource prov) (qtOsv quiet) (apOsvNewestModified prov)
        , ageOf EpssFeed (apEpssSource prov) (qtEpss quiet) (apEpssScoreDate prov)
        ]
  where
    ageOf source url threshold mStamp = do
        stamp <- mStamp
        pure
            SourceAge
                { saSource = source
                , saUrl = fromMaybe "<unrecorded>" url
                , saAge = diffUTCTime now stamp
                , saThreshold = threshold
                }

-- | Whether this source has gone longer than its threshold without changing.
sourceQuiet :: SourceAge -> Bool
sourceQuiet reading = saAge reading > saThreshold reading

-- | The age as an operator reads it, in the seconds its threshold is configured in.
renderSourceAge :: SourceAge -> Text
renderSourceAge reading =
    sourceName (saSource reading)
        <> " "
        <> saUrl reading
        <> " last changed "
        <> seconds (saAge reading)
        <> "s ago, quiet-time threshold "
        <> seconds (saThreshold reading)
        <> "s"
  where
    seconds :: NominalDiffTime -> Text
    seconds d = show (truncate d :: Integer)

sourceName :: ProvenanceSource -> Text
sourceName = \case
    OsvExport -> "OSV export"
    EpssFeed -> "EPSS feed"

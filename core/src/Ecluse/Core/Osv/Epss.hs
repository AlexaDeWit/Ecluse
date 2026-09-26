-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The FIRST.org EPSS feed, the exploitability score Pilot joins onto each advisory.

Pilot joins the scores through advisory aliases ("Ecluse.Core.Osv.Advisory"). Every compile
attempts the feed. A failed fetch is an 'EpssFeedFailure', and the ecosystem's 'EpssRequirement'
decides whether it stops publication. Individual missing scores remain absent.
-}
module Ecluse.Core.Osv.Epss (
    -- * The feed
    maxEpssFeedBytes,
    EpssFeed (..),
    fetchEpssScores,
    EpssFeedTooLarge (..),
    EpssFeedEmpty (..),
    EpssFeedTruncated (..),

    -- * One compile's attempt
    EpssFeedFailure (..),
    renderEpssFeedFailure,
    classifyEpssFailure,
    acquireEpssFeed,
    EpssEnrichment (..),
    resolveEnrichment,
    enrichedFeed,
    enrichmentStatus,

    -- * The score table
    EpssScores,
    mkEpssScores,
    epssForIds,
    epssScoreCount,

    -- * One feed row
    parseEpssLine,

    -- * The feed's preamble
    EpssPreamble (..),
    parseEpssPreamble,
) where

import Conduit
import Control.Monad.Catch (MonadMask)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as C
import Data.Foldable1 qualified as Foldable1
import Data.Map.Strict qualified as Map
import Data.Streaming.Zlib (
    PopperRes (PRDone, PRError, PRNext),
    WindowBits (WindowBits),
    ZlibException,
    feedInflate,
    finishInflate,
    initInflate,
    isCompleteInflate,
 )
import Data.Text qualified as T
import Data.Time (UTCTime)
import Katip (KatipContext, Severity (InfoS), logFM, ls)
import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException), HttpExceptionContent (StatusCodeException), responseStatus)
import Network.HTTP.Simple (getResponseBody, getResponseHeader, httpSource, parseRequest, setRequestCheckStatus)
import Network.HTTP.Types.Header (hLastModified)
import Network.HTTP.Types.Status (statusCode)
import UnliftIO.Exception (tryJust)

import Ecluse.Core.Fault (TransportCause, TransportFault (tfCause), renderTransportCause)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Osv.Provenance (lastModifiedOf, parseSourceTime)
import Ecluse.Core.Osv.Retry (defaultOsvRetryPolicy, withOsvRetry)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired), EpssStatus (EnrichmentAvailable, EnrichmentUnavailable))
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Stream (boundBytes)

{- | The byte ceiling Pilot fetches under, 64 MiB, applied to the served stream and again to its
expansion. The feed is one short row per scored CVE, so the headroom is several times over.
-}
maxEpssFeedBytes :: Int
maxEpssFeedBytes = 64 * 1024 * 1024

{- | The feed passed a byte ceiling, so the fetch refused it whole. Each carries that ceiling
and the bytes seen when it tripped, which is the ceiling plus at most one chunk.
-}
data EpssFeedTooLarge
    = -- | The compressed stream the host served, so an endless one cannot hang the pass.
      CompressedTooLarge Int Int
    | -- | Its expansion under gzip, which is what a compression bomb inflates.
      DecompressedTooLarge Int Int
    deriving stock (Eq, Show)

instance Exception EpssFeedTooLarge

{- | The feed decoded to no scores at all: an error page served as 200, or a column order the row
decode no longer reads. Whole-feed failure remains distinct from an individual missing score.
-}
data EpssFeedEmpty = EpssFeedEmpty
    deriving stock (Eq, Show)

instance Exception EpssFeedEmpty

-- | The gzip stream ended before its end-of-stream marker, so the rows it carried may be a fraction.
data EpssFeedTruncated = EpssFeedTruncated
    deriving stock (Eq, Show)

instance Exception EpssFeedTruncated

{- | One fetch of the feed: the scores it carries, and what it says about itself. The feed
declares its own score date, so a stalled feed is visible without a second source of truth.
-}
data EpssFeed = EpssFeed
    { efScores :: EpssScores
    , efLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the fetch was answered with.
    , efScoreDate :: Maybe UTCTime
    -- ^ The @score_date@ the preamble declares, a bare date read as its UTC start of day.
    , efModelVersion :: Maybe Text
    -- ^ The scoring model the preamble declares.
    }
    deriving stock (Eq, Show)

{- | What the feed's leading comment line declares. FIRST.org writes it as
@#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000@.
-}
data EpssPreamble = EpssPreamble
    { epScoreDate :: Maybe UTCTime
    , epModelVersion :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | Read the feed's leading comment line. A line that is not a comment, a comment naming
neither field, and a date the grammar cannot read all yield absence, never a substitute value.
-}
parseEpssPreamble :: ByteString -> EpssPreamble
parseEpssPreamble raw = case T.stripPrefix "#" (decodeUtf8 raw) of
    Nothing -> EpssPreamble Nothing Nothing
    Just body ->
        EpssPreamble
            { epScoreDate = parseSourceTime =<< field "score_date" body
            , epModelVersion = field "model_version" body
            }
  where
    -- The score date holds colons of its own, so each field splits on its first one only.
    field name body = find (not . T.null) (mapMaybe (valueOf name) (T.splitOn "," body))
    valueOf name entry =
        let (key, value) = T.breakOn ":" entry
         in if T.strip key == name then Just (T.strip (T.drop 1 value)) else Nothing

{- | The scores from one fetch of the feed. Keys are upper-cased CVE ids, so a case
difference between the feed and an advisory's aliases cannot silently miss the join.
-}
newtype EpssScores = EpssScores (Map Text Double)
    deriving stock (Eq, Show)

{- | Build a score table from @(CVE id, probability)@ rows. A duplicate id keeps the higher
score, the fail-closed direction for a rule that denies above a threshold.
-}
mkEpssScores :: [(Text, Double)] -> EpssScores
mkEpssScores = foldl' (flip addScore) (EpssScores Map.empty)

addScore :: (Text, Double) -> EpssScores -> EpssScores
addScore (cve, score) (EpssScores scores) = EpssScores (Map.insertWith max (T.toUpper cve) score scores)

{- | The highest score the feed carries for any of these identifiers, or 'Nothing' when it
scores none of them.
-}
epssForIds :: EpssScores -> [Text] -> Maybe Double
epssForIds (EpssScores scores) ids = viaNonEmpty Foldable1.maximum (mapMaybe lookupScore ids)
  where
    lookupScore i = Map.lookup (T.toUpper i) scores

-- | How many CVEs the table scores.
epssScoreCount :: EpssScores -> Int
epssScoreCount (EpssScores scores) = Map.size scores

{- | One feed row as @(CVE id, probability)@. A comment, the header, an unreadable row, and a
score outside @[0, 1]@ all yield 'Nothing', so one bad row drops out instead of failing the pass.
-}
parseEpssLine :: ByteString -> Maybe (Text, Double)
parseEpssLine raw = case T.splitOn "," (decodeUtf8 raw) of
    (cve : score : _) -> (,) <$> identifier (T.strip cve) <*> probability (T.strip score)
    _ -> Nothing
  where
    identifier t = if T.null t then Nothing else Just t
    probability t = do
        p <- readMaybe (toString t)
        guard (p >= 0 && p <= 1)
        pure p

{- | Fetch the feed and decode it into a score table, bounded by @cap@ bytes on each side of
decompression. A non-2xx, undecodable, over-large, or scoreless feed throws.
-}
fetchEpssScores :: (MonadResource m, MonadThrow m, KatipContext m) => Int -> String -> m EpssFeed
fetchEpssScores cap urlStr = do
    -- 'setRequestCheckStatus' throws at the header boundary, so a 502 reaches the caller's
    -- backoff as a retryable fault instead of feeding an error page to the decompressor.
    req <- liftIO (setRequestCheckStatus <$> parseRequest urlStr)
    -- The header is read from the response that carried the rows, so the date and the scores
    -- describe one fetch.
    (decoded, served) <- runConduit $ httpSource req $ \res -> do
        accumulated <- getResponseBody res .| decodeEpssFeed cap
        pure (accumulated, lastModifiedOf (getResponseHeader hLastModified res))
    let scores = faScores decoded
    when (epssScoreCount scores == 0) (throwM EpssFeedEmpty)
    logFM InfoS (ls ("Ingested " <> show (epssScoreCount scores) <> " EPSS scores from " <> authorityLabel (toText urlStr)))
    pure
        EpssFeed
            { efScores = scores
            , efLastModified = served
            , efScoreDate = epScoreDate (faPreamble decoded)
            , efModelVersion = epModelVersion (faPreamble decoded)
            }

-- The running decode of one feed: the preamble the first line carries, and the scores the
-- rest of them do.
data FeedAccum = FeedAccum
    { faFirst :: !Bool
    , faPreamble :: EpssPreamble
    , faScores :: EpssScores
    }

-- The feed's wire form: gzip, then CSV rows. Bounding the served stream keeps an endless one
-- from hanging the pass, and bounding its expansion keeps a bomb from exhausting the heap.
decodeEpssFeed :: (MonadIO m, MonadThrow m) => Int -> ConduitT ByteString o m FeedAccum
decodeEpssFeed cap =
    boundBytes cap (throwM . CompressedTooLarge cap)
        .| ungzipWhole
        .| boundBytes cap (throwM . DecompressedTooLarge cap)
        .| C.linesUnboundedAscii
        .| C.foldl addLine (FeedAccum True (EpssPreamble Nothing Nothing) (mkEpssScores []))

-- One gzip member, which must reach its end-of-stream marker. 'Data.Conduit.Zlib.ungzip' passes a
-- cut stream through, and its rows would read downstream as a complete table.
ungzipWhole :: (MonadIO m, MonadThrow m) => ConduitT ByteString ByteString m ()
ungzipWhole = liftIO (initInflate (WindowBits 31)) >>= feed
  where
    feed inflate =
        await >>= \case
            Nothing -> throwM EpssFeedTruncated
            Just chunk -> do
                liftIO (feedInflate inflate chunk) >>= drain
                complete <- liftIO (isCompleteInflate inflate)
                if complete
                    then liftIO (finishInflate inflate) >>= \rest -> unless (BS.null rest) (yield rest)
                    else feed inflate
    drain popper =
        liftIO popper >>= \case
            PRDone -> pass
            PRNext out -> yield out >> drain popper
            PRError err -> throwM err

-- Only the first line can be the preamble, so a comment further down the feed cannot restate
-- the score date.
addLine :: FeedAccum -> ByteString -> FeedAccum
addLine acc line
    | faFirst acc = acc{faFirst = False, faPreamble = parseEpssPreamble line, faScores = scored}
    | otherwise = acc{faScores = scored}
  where
    scored = maybe (faScores acc) (`addScore` faScores acc) (parseEpssLine line)

-- | Why one attempt at the feed produced no scores.
data EpssFeedFailure
    = -- | The feed host answered with this non-2xx status.
      EpssFeedStatus Int
    | -- | The transport could not deliver the feed.
      EpssFeedTransport TransportCause
    | -- | The feed passed a byte ceiling.
      EpssFeedOversize EpssFeedTooLarge
    | -- | The feed is not valid gzip, or its stream ended early.
      EpssFeedUndecodable
    | -- | The feed decoded to no scores.
      EpssFeedNoScores
    deriving stock (Eq, Show)

-- | The failure as an operator reads it. It never names the feed URL, which can carry a credential.
renderEpssFeedFailure :: EpssFeedFailure -> Text
renderEpssFeedFailure = \case
    EpssFeedStatus code -> "the feed answered HTTP " <> show code
    EpssFeedTransport cause -> renderTransportCause cause
    EpssFeedOversize (CompressedTooLarge cap _) -> "the served feed passed its " <> show cap <> "-byte ceiling"
    EpssFeedOversize (DecompressedTooLarge cap _) -> "the decompressed feed passed its " <> show cap <> "-byte ceiling"
    EpssFeedUndecodable -> "the feed is not a complete gzip stream"
    EpssFeedNoScores -> "the feed carried no scores"

{- | The feed failures a compile can continue past. An invalid URL is a configuration fault, and
anything unnamed here may be a bug, so both stay exceptions rather than read as an outage.
-}
classifyEpssFailure :: SomeException -> Maybe EpssFeedFailure
classifyEpssFailure err
    | Just http <- fromException err = httpFailure http
    | Just tooLarge <- fromException err = Just (EpssFeedOversize tooLarge)
    | Just EpssFeedEmpty <- fromException err = Just EpssFeedNoScores
    | Just (_ :: ZlibException) <- fromException err = Just EpssFeedUndecodable
    | Just EpssFeedTruncated <- fromException err = Just EpssFeedUndecodable
    | otherwise = Nothing

httpFailure :: HttpException -> Maybe EpssFeedFailure
httpFailure = \case
    InvalidUrlException{} -> Nothing
    HttpExceptionRequest _ (StatusCodeException response _) -> Just (EpssFeedStatus (statusCode (responseStatus response)))
    other -> Just (EpssFeedTransport (tfCause (classifyTransport other)))

{- | Fetch the feed under the advisory retry policy. A failure 'classifyEpssFailure' names returns
as a value, and every other exception, a cancellation included, propagates.
-}
acquireEpssFeed :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Int -> String -> m (Either EpssFeedFailure EpssFeed)
acquireEpssFeed cap url = tryJust classifyEpssFailure (withOsvRetry defaultOsvRetryPolicy (fetchEpssScores cap url))

-- | What one compile joins onto its advisories.
data EpssEnrichment
    = -- | The feed arrived, so its scores join and its provenance is recorded.
      EpssEnriched EpssFeed
    | -- | The feed failed where the ecosystem does not require it, so no score joins.
      EpssUnavailable EpssFeedFailure
    deriving stock (Eq, Show)

{- | Settle one attempt under the ecosystem's requirement. A failure stays 'Left' where the
ecosystem requires enrichment, so an unavailable feed never stands in for a required one.
-}
resolveEnrichment :: EpssRequirement -> Either EpssFeedFailure EpssFeed -> Either EpssFeedFailure EpssEnrichment
resolveEnrichment requirement = \case
    Right feed -> Right (EpssEnriched feed)
    Left failure -> case requirement of
        EpssRequired -> Left failure
        EpssOptional -> Right (EpssUnavailable failure)

-- | The feed an enrichment joined, if one arrived.
enrichedFeed :: EpssEnrichment -> Maybe EpssFeed
enrichedFeed = \case
    EpssEnriched feed -> Just feed
    EpssUnavailable _ -> Nothing

-- | The status the artifact records for this enrichment.
enrichmentStatus :: EpssEnrichment -> EpssStatus
enrichmentStatus = \case
    EpssEnriched _ -> EnrichmentAvailable
    EpssUnavailable _ -> EnrichmentUnavailable

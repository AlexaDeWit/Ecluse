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
    maxEpssLineBytes,
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
    Popper,
    PopperRes (PRDone, PRError, PRNext),
    WindowBits (WindowBits),
    ZlibException,
    feedInflate,
    finishInflate,
    getUnusedInflate,
    initInflate,
    isCompleteInflate,
 )
import Data.Text qualified as T
import Data.Time (UTCTime)
import Katip (KatipContext, Severity (InfoS), logFM, ls)
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Network.HTTP.Simple (getResponseBody, getResponseHeader, httpSource, parseRequest, setRequestCheckStatus)
import Network.HTTP.Types.Header (hLastModified)
import UnliftIO.Exception (tryJust)

import Ecluse.Core.Fault (HttpFault, renderHttpFault)
import Ecluse.Core.Fault.Http (httpFault)
import Ecluse.Core.Osv.Provenance (lastModifiedOf, parseSourceTime)
import Ecluse.Core.Osv.Retry (defaultOsvRetryPolicy, withOsvRetry)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired), EpssStatus (EnrichmentAvailable, EnrichmentUnavailable))
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Core.Stream (boundBytes, boundLines, digestWhole)

-- | 64 MiB, applied to the served stream and again to its expansion: several times a feed of one short row per scored CVE.
maxEpssFeedBytes :: Int
maxEpssFeedBytes = 64 * 1024 * 1024

-- | The longest feed line Pilot holds, 4 KiB. A scored row is under 100 bytes, so a longer line is no row.
maxEpssLineBytes :: Int
maxEpssLineBytes = 4 * 1024

-- | The feed passed a byte ceiling, so the fetch refused it whole. Each carries the ceiling and the bytes seen when it tripped.
data EpssFeedTooLarge
    = -- | The compressed stream the host served, so an endless one cannot hang the pass.
      CompressedTooLarge Int Int
    | -- | Its expansion under gzip, which is what a compression bomb inflates.
      DecompressedTooLarge Int Int
    | -- | One line of the expansion ('maxEpssLineBytes'), however the stream was chunked.
      LineTooLarge Int Int
    deriving stock (Eq, Show)

instance Exception EpssFeedTooLarge

-- | The feed decoded to no scores at all, as an error page served as 200 does. One missing score is not this.
data EpssFeedEmpty = EpssFeedEmpty
    deriving stock (Eq, Show)

instance Exception EpssFeedEmpty

-- | A gzip member ended before its end-of-stream marker, so the rows it carried may be a fraction.
data EpssFeedTruncated = EpssFeedTruncated
    deriving stock (Eq, Show)

instance Exception EpssFeedTruncated

-- | One fetch of the feed: its scores, and what it says about itself, so a stalled feed shows without a second source.
data EpssFeed = EpssFeed
    { efScores :: EpssScores
    , efLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the fetch was answered with.
    , efScoreDate :: Maybe UTCTime
    -- ^ The @score_date@ the preamble declares, a bare date read as its UTC start of day.
    , efModelVersion :: Maybe Text
    -- ^ The scoring model the preamble declares.
    , efDigest :: ContentDigest
    -- ^ The digest of the whole served feed, before decompression.
    }
    deriving stock (Eq, Show)

-- | What the feed's leading comment declares, written as @#model_version:v2026.08.01,score_date:2026-08-29T00:00:00+0000@.
data EpssPreamble = EpssPreamble
    { epScoreDate :: Maybe UTCTime
    , epModelVersion :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Read the feed's leading comment line. Anything this cannot read yields absence, never a substitute value.
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

-- | The scores from one fetch, keyed by upper-cased CVE id so a case difference cannot miss the join.
newtype EpssScores = EpssScores (Map Text Double)
    deriving stock (Eq, Show)

-- | Build a score table. A duplicate id keeps the higher score, the fail-closed direction for a threshold deny.
mkEpssScores :: [(Text, Double)] -> EpssScores
mkEpssScores = foldl' (flip addScore) (EpssScores Map.empty)

addScore :: (Text, Double) -> EpssScores -> EpssScores
addScore (cve, score) (EpssScores scores) = EpssScores (Map.insertWith max (T.toUpper cve) score scores)

-- | The highest score the feed carries for any of these identifiers.
epssForIds :: EpssScores -> [Text] -> Maybe Double
epssForIds (EpssScores scores) ids = viaNonEmpty Foldable1.maximum (mapMaybe lookupScore ids)
  where
    lookupScore i = Map.lookup (T.toUpper i) scores

-- | How many CVEs the table scores.
epssScoreCount :: EpssScores -> Int
epssScoreCount (EpssScores scores) = Map.size scores

-- | One feed row as @(CVE id, probability)@. Anything else, a score outside @[0, 1]@ included, drops out rather than failing the pass.
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

-- | Fetch and decode the feed, bounded by @cap@ bytes on each side of decompression. A non-2xx, undecodable, over-large, or scoreless feed throws.
fetchEpssScores :: (MonadResource m, MonadThrow m, KatipContext m) => Int -> String -> m EpssFeed
fetchEpssScores cap urlStr = do
    -- 'setRequestCheckStatus' throws at the header boundary, so a 502 reaches the caller's
    -- backoff as a retryable fault instead of feeding an error page to the decompressor.
    req <- liftIO (setRequestCheckStatus <$> parseRequest urlStr)
    -- The header is read from the response that carried the rows, so the date and the scores
    -- describe one fetch.
    ((digest, decoded), served) <- runConduit $ httpSource req $ \res -> do
        accumulated <- getResponseBody res .| decodeEpssFeed cap
        pure (accumulated, lastModifiedOf (getResponseHeader hLastModified res))
    let scores = faScores decoded
    when (epssScoreCount scores == 0) (throwM EpssFeedEmpty)
    logFM InfoS (ls ("Ingested " <> show (epssScoreCount scores) <> " EPSS scores from " <> dialledAuthorityLabel (toText urlStr)))
    pure
        EpssFeed
            { efScores = scores
            , efLastModified = served
            , efScoreDate = epScoreDate (faPreamble decoded)
            , efModelVersion = epModelVersion (faPreamble decoded)
            , efDigest = digest
            }

-- The running decode of one feed: the preamble the first line carries, and the scores the
-- rest of them do.
data FeedAccum = FeedAccum
    { faFirst :: !Bool
    , faPreamble :: EpssPreamble
    , faScores :: EpssScores
    }

-- The feed's wire form: gzip, then CSV rows. The served and expanded bounds cap the bytes a pass
-- reads, and the line bound caps what one unfinished row holds, however the stream is chunked.
decodeEpssFeed :: (MonadIO m, MonadThrow m) => Int -> ConduitT ByteString o m (ContentDigest, FeedAccum)
decodeEpssFeed cap =
    fuseBoth
        (boundBytes cap (throwM . CompressedTooLarge cap) .| digestWhole)
        ( ungzipWhole
            .| boundBytes cap (throwM . DecompressedTooLarge cap)
            .| boundLines maxEpssLineBytes (throwM . LineTooLarge maxEpssLineBytes)
            .| C.foldl addLine (FeedAccum True (EpssPreamble Nothing Nothing) (mkEpssScores []))
        )

-- Every gzip member to the end of input. Any byte that does not begin a whole member, zero padding
-- included, fails the feed. 'Data.Conduit.Zlib.ungzip' stops after one member and passes a cut one.
ungzipWhole :: (MonadIO m, MonadThrow m) => ConduitT ByteString ByteString m ()
ungzipWhole = await >>= maybe truncatedFeed inflateMember

-- One member from the bytes in hand. Input that ends before its end-of-stream marker truncates it.
inflateMember :: (MonadIO m, MonadThrow m) => ByteString -> ConduitT ByteString ByteString m ()
inflateMember opening = liftIO (initInflate (WindowBits 31)) >>= (`feed` opening)
  where
    feed inflate chunk = do
        liftIO (feedInflate inflate chunk) >>= drainInflate
        complete <- liftIO (isCompleteInflate inflate)
        if complete
            then do
                liftIO (finishInflate inflate) >>= yieldNonEmpty
                liftIO (getUnusedInflate inflate) >>= afterMember
            else await >>= maybe truncatedFeed (feed inflate)

-- Any byte after a complete member starts another, so trailing bytes are read, not ignored.
afterMember :: (MonadIO m, MonadThrow m) => ByteString -> ConduitT ByteString ByteString m ()
afterMember rest
    | BS.null rest = await >>= maybe pass afterMember
    | otherwise = inflateMember rest

drainInflate :: (MonadIO m, MonadThrow m) => Popper -> ConduitT i ByteString m ()
drainInflate popper =
    liftIO popper >>= \case
        PRDone -> pass
        PRNext out -> yieldNonEmpty out >> drainInflate popper
        PRError err -> throwM err

yieldNonEmpty :: (Monad m) => ByteString -> ConduitT i ByteString m ()
yieldNonEmpty out = unless (BS.null out) (yield out)

-- A throw, because only an exception stops the fetch mid-stream. 'acquireEpssFeed' reads it back.
truncatedFeed :: (MonadThrow m) => ConduitT i o m a
truncatedFeed = throwM EpssFeedTruncated

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
    = -- | The fetch failed: the feed host's status, or the transport's fault.
      EpssFeedFetch HttpFault
    | -- | The feed passed a byte ceiling.
      EpssFeedOversize EpssFeedTooLarge
    | -- | The feed is not valid gzip, or its stream ended early.
      EpssFeedUndecodable
    | -- | The feed decoded to no scores.
      EpssFeedNoScores
    deriving stock (Eq, Show)

-- | The failure as an operator reads it, after the feed's @host:port@. It never names the feed URL, which can carry a credential.
renderEpssFeedFailure :: Text -> EpssFeedFailure -> Text
renderEpssFeedFailure feed = \case
    EpssFeedFetch fault -> renderHttpFault feed fault
    EpssFeedOversize tooLarge -> feed <> ": " <> renderTooLarge tooLarge
    EpssFeedUndecodable -> feed <> ": the feed is not a complete gzip stream"
    EpssFeedNoScores -> feed <> ": the feed carried no scores"

renderTooLarge :: EpssFeedTooLarge -> Text
renderTooLarge = \case
    CompressedTooLarge cap _ -> "the served feed passed its " <> show cap <> "-byte ceiling"
    DecompressedTooLarge cap _ -> "the decompressed feed passed its " <> show cap <> "-byte ceiling"
    LineTooLarge cap _ -> "a feed line passed its " <> show cap <> "-byte ceiling"

-- | The feed failures a compile can continue past. An invalid URL or an unnamed exception is a fault, never an outage.
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
    other -> Just (EpssFeedFetch (httpFault other))

-- | Fetch the feed under the advisory retry policy. A failure 'classifyEpssFailure' names is a value. Anything else propagates.
acquireEpssFeed :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Int -> String -> m (Either EpssFeedFailure EpssFeed)
acquireEpssFeed cap url = tryJust classifyEpssFailure (withOsvRetry (dialledAuthorityLabel (toText url)) defaultOsvRetryPolicy (fetchEpssScores cap url))

-- | What one compile joins onto its advisories.
data EpssEnrichment
    = -- | The feed arrived, so its scores join and its provenance is recorded.
      EpssEnriched EpssFeed
    | -- | The feed failed where the ecosystem does not require it, so no score joins.
      EpssUnavailable EpssFeedFailure
    deriving stock (Eq, Show)

-- | Settle one attempt under the ecosystem's requirement, so an unavailable feed never stands in for a required one.
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

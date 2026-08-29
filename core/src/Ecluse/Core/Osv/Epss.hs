-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The FIRST.org EPSS feed, the exploitability score Pilot joins onto each advisory.

EPSS is the probability that a vulnerability is exploited in the wild within 30 days, keyed
by CVE id. The OSV payload carries no such score, so Pilot fetches the daily feed itself and
joins it through an advisory's aliases ("Ecluse.Core.Osv.Advisory"). The feed is a gzipped
CSV. The fetch refuses an over-large one whole rather than truncating it, because a partial
table reads downstream as unscored, which a deny-on-EPSS rule counts as exceeding.
-}
module Ecluse.Core.Osv.Epss (
    -- * The feed
    epssFeedUrl,
    maxEpssFeedBytes,
    fetchEpssScores,
    EpssFeedTooLarge (..),

    -- * The score table
    EpssScores,
    mkEpssScores,
    epssForIds,
    epssScoreCount,

    -- * One feed row
    parseEpssLine,
) where

import Conduit
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Zlib (ungzip)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Katip (KatipContext, Severity (InfoS), logFM, ls)
import Network.HTTP.Simple (getResponseBody, httpSource, parseRequest, setRequestCheckStatus)

import Ecluse.Core.Security.Authority (authorityLabel)

{- | The daily feed of every scored CVE, over HTTPS. The host is the one an operator
allowlists for Pilot's egress, and it redirects only within itself, to the dated file.
-}
epssFeedUrl :: String
epssFeedUrl = "https://epss.empiricalsecurity.com/epss_scores-current.csv.gz"

{- | The decompressed ceiling Pilot fetches under, 64 MiB. The feed is one short row per
scored CVE, so this leaves several times the headroom the CVE catalogue's growth needs.
-}
maxEpssFeedBytes :: Int
maxEpssFeedBytes = 64 * 1024 * 1024

{- | The feed decompressed past the fetch's ceiling (carried), so the fetch refused it. A
compression bomb inflates no further than that ceiling before this stops it.
-}
newtype EpssFeedTooLarge = EpssFeedTooLarge Int
    deriving stock (Eq, Show)

instance Exception EpssFeedTooLarge

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
epssForIds (EpssScores scores) ids = case mapMaybe lookupScore ids of
    [] -> Nothing
    (s : ss) -> Just (foldl' max s ss)
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

{- | Fetch the feed and decode it into a score table, bounded by @cap@ decompressed bytes. A
non-2xx response, an undecodable archive, and an over-large feed all throw, because a pass that
cannot score its advisories must fail rather than publish one whose scores all read as absent.
-}
fetchEpssScores :: (MonadResource m, MonadThrow m, KatipContext m) => Int -> String -> m EpssScores
fetchEpssScores cap urlStr = do
    -- 'setRequestCheckStatus' throws at the header boundary, so a 502 reaches the caller's
    -- backoff as a retryable fault instead of feeding an error page to the decompressor.
    req <- liftIO (setRequestCheckStatus <$> parseRequest urlStr)
    scores <- runConduit (httpSource req (\res -> getResponseBody res .| decodeEpssFeed cap))
    logFM InfoS (ls ("Ingested " <> show (epssScoreCount scores) <> " EPSS scores from " <> authorityLabel (toText urlStr)))
    pure scores

-- The feed's wire form: gzip, then CSV rows. The byte bound sits before the line split, so
-- a bomb cannot build one unbounded line either.
decodeEpssFeed :: (MonadIO m, MonadThrow m) => Int -> ConduitT ByteString o m EpssScores
decodeEpssFeed cap =
    transPipe liftIO ungzip
        .| boundDecompressed cap
        .| C.linesUnboundedAscii
        .| C.foldl addRow (EpssScores Map.empty)
  where
    addRow acc line = maybe acc (`addScore` acc) (parseEpssLine line)

-- Pass the stream through until it breaches the ceiling, then refuse the feed. It never
-- truncates, because a short table is indistinguishable downstream from a complete one.
boundDecompressed :: (MonadThrow m) => Int -> ConduitT ByteString ByteString m ()
boundDecompressed cap = go 0
  where
    go !seen =
        await >>= \case
            Nothing -> pass
            Just chunk ->
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then throwM (EpssFeedTooLarge cap)
                        else yield chunk >> go seen'

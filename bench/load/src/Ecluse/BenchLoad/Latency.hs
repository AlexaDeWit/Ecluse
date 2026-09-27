-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

-- | Nearest-rank latency percentiles over successful requests only, so a shed never lowers them.
module Ecluse.BenchLoad.Latency (
    Percentiles (..),
    noPercentiles,
    percentiles,
    isSuccessStatus,
) where

import Data.Aeson (FromJSON, ToJSON)

-- | Latency percentiles in milliseconds. Each is 'Nothing' when no request succeeded.
data Percentiles = Percentiles
    { pP50Ms :: Maybe Double
    , pP90Ms :: Maybe Double
    , pP99Ms :: Maybe Double
    , pP999Ms :: Maybe Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The percentiles of a run with no successful request.
noPercentiles :: Percentiles
noPercentiles = Percentiles Nothing Nothing Nothing Nothing

-- | Percentiles of latencies given in seconds, in any order.
percentiles :: [Double] -> Percentiles
percentiles latencies =
    Percentiles (at 0.50) (at 0.90) (at 0.99) (at 0.999)
  where
    sorted = sort latencies
    count = length sorted
    at :: Double -> Maybe Double
    at q = (* 1_000) <$> sorted !!? min (count - 1) (max 0 (ceiling (q * fromIntegral count) - 1))

-- | A 2xx or 3xx status: the harness counts a @304@ revalidation as a success.
isSuccessStatus :: Int -> Bool
isSuccessStatus status = status >= 200 && status < 400

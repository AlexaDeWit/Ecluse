-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared Markdown rendering for reports and generated site pages.
module Ecluse.Report.Markdown (cells, fixed, signedPercent) where

import Data.Text qualified as T
import Numeric (showFFloat)

-- | One table row from its cells, without escaping their contents.
cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- | A number to a fixed count of decimal places.
fixed :: Int -> Double -> Text
fixed places x = toText (showFFloat (Just places) x "")

-- | A change to one decimal place with its sign. A change that rounds to zero carries none.
signedPercent :: Double -> Text
signedPercent percent
    | magnitude == fixed 1 0 = magnitude <> "%"
    | percent > 0 = "+" <> magnitude <> "%"
    | otherwise = "-" <> magnitude <> "%"
  where
    magnitude = fixed 1 (abs percent)

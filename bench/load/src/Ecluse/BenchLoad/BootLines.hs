-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The runtime and admission decisions a proxy logged at boot, read back from its JSON log.
The report quotes the lines as the build under test wrote them, and derives the limits from them
where they parse, so a build that renames or removes a control shows its own lines and no figure.
-}
module Ecluse.BenchLoad.BootLines (
    bootMessages,
    BootLimits (..),
    bootLimits,
    admittedListings,
) where

import Data.Aeson (FromJSON, ToJSON, Value (Object, String), decodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T

-- | The @message@ of every JSON log line that states a runtime or admission decision.
bootMessages :: [ByteString] -> [Text]
bootMessages = filter decision . mapMaybe message
  where
    message line = case decodeStrict line of
        Just (Object o) | Just (String m) <- KeyMap.lookup "message" o -> Just m
        _ -> Nothing
    decision m = any (`T.isPrefixOf` m) ["runtime:", "memory plan:", "metadata admission"]

-- | The limits a proxy resolved, in bytes or requests. Each is 'Nothing' when no line states it.
data BootLimits = BootLimits
    { blCpuAdmission :: Maybe Int
    , blMaterialBudgetBytes :: Maybe Int
    , blFullOriginBytes :: Maybe Int
    , blListingOutputBytes :: Maybe Int
    , blCacheBytes :: Maybe Int
    , blCacheEntries :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Read the limits from the boot messages.
bootLimits :: [Text] -> BootLimits
bootLimits messages =
    BootLimits
        { blCpuAdmission = after "runtime: serve admission "
        , blMaterialBudgetBytes = after "memory plan: material estimate budget "
        , blFullOriginBytes = estimate ["full", "origin"]
        , blListingOutputBytes = estimate ["listing", "output"]
        , blCacheBytes = after "memory plan: cache byte bound "
        , blCacheEntries = after "memory plan: cache entry bound "
        }
  where
    after prefix = listToMaybe (mapMaybe (firstNumber <=< T.stripPrefix prefix) messages)
    estimate label = listToMaybe (mapMaybe (numberAfter label . words . T.replace "," " ") estimateLines)
    estimateLines = filter ("metadata admission estimates:" `T.isPrefixOf`) messages

firstNumber :: Text -> Maybe Int
firstNumber = readMaybe . toString <=< listToMaybe . words

numberAfter :: [Text] -> [Text] -> Maybe Int
numberAfter label ws = case ws of
    [] -> Nothing
    _ : rest
        | label `isPrefixOf` ws -> readMaybe . toString =<< listToMaybe (drop (length label) ws)
        | otherwise -> numberAfter label rest

{- | How many two-origin listings the material budget admits at once, the binding limit on a
cold listing. A listing heavier than the whole budget still runs alone, so the floor is one.
-}
admittedListings :: BootLimits -> Maybe Int
admittedListings limits = do
    budget <- blMaterialBudgetBytes limits
    listing <- blListingOutputBytes limits
    origin <- blFullOriginBytes limits
    let weight = listing + 2 * origin
    guard (weight > 0)
    pure (max 1 (budget `div` weight))

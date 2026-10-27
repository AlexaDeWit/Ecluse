-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Actual upstream chunks share one native request byte ceiling across both origins.
module Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeReadBudget) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Server.Pipeline.Origin (ObservedReadBoundExceeded (ObservedReadBoundExceeded))

-- | Refuse before another source chunk reaches its parser when cumulative input exceeds the cap.
newNativeReadBudget :: Int -> IO (Int -> IO ())
newNativeReadBudget cap = do
    total <- newIORef (0 :: Integer)
    pure $ \bytes -> do
        exceeded <- atomicModifyIORef' total (\seen -> let next = seen + toInteger (max 0 bytes) in (next, next > toInteger cap))
        when exceeded (throwIO (ObservedReadBoundExceeded (BodyTooLarge (MetadataBodyLimit cap))))

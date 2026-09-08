-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Byte limits for advisory streams. Callers own the breach effect, so ingestion
and runtime downloads can share the traversal without sharing error types.
-}
module Ecluse.Core.Stream (boundBytes) where

import Conduit (ConduitT, await, yield)
import Data.ByteString qualified as BS

{- | Preserve chunks up to the byte cap. On breach, pass the observed byte count
to the action and stop without yielding the excess chunk, even if the action returns.
-}
boundBytes :: (Monad m) => Int -> (Int -> m ()) -> ConduitT ByteString ByteString m ()
boundBytes cap onBreach = go 0
  where
    go !seen =
        await >>= \case
            Nothing -> pass
            Just chunk ->
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then lift (onBreach seen')
                        else yield chunk >> go seen'

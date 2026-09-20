-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure chunk inputs for the production registry stream driver.
module Ecluse.Test.Registry.JsonStream (parseJsonChunks) where

import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Ecluse.Core.Registry.JsonStream (StreamResult, readJsonStream)
import Ecluse.Core.Security (BodyLimit, LimitError)

-- | Run the same incremental driver against explicit chunks for pure callers and boundary tests.
parseJsonChunks :: BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> [ByteString] -> Either LimitError (StreamResult s)
parseJsonChunks bound parser step initial = evalState (readJsonStream bound parser step initial next)
  where
    next = state $ \case
        [] -> (BS.empty, [])
        chunk : rest -> (chunk, rest)

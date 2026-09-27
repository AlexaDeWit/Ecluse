-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure chunk inputs for the production registry stream driver, and a check for shared keys.
module Ecluse.Test.Registry.JsonStream (parseJsonChunks, sharesKey) where

import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import System.Mem.StableName (makeStableName)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Registry.JsonStream (StreamResult, readJsonStream)
import Ecluse.Core.Security (BodyLimit, LimitError)

-- | Run the same incremental driver against explicit chunks for pure callers and boundary tests.
parseJsonChunks :: BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> [ByteString] -> Either LimitError (StreamResult s)
parseJsonChunks bound parser step initial = evalState (readJsonStream bound parser step initial next)
  where
    next = state $ \case
        [] -> (BS.empty, [])
        chunk : rest -> (chunk, rest)

-- | Whether at least two objects hold the member and all of them hold its key as one heap object.
sharesKey :: Key.Key -> [Value] -> IO Bool
sharesKey key objects = do
    names <- traverse (makeStableName <=< evaluate) [Key.toText held | Object fields <- objects, held <- KeyMap.keys fields, held == key]
    pure $ case names of
        stable : rest@(_ : _) -> all (== stable) rest
        _ -> False

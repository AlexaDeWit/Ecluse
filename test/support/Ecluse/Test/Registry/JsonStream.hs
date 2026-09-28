-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure chunk inputs for the production registry stream driver, and checks for shared keys and strings.
module Ecluse.Test.Registry.JsonStream (parseJsonChunks, sharesKey, sharesString) where

import Data.Aeson (Value (Object, String))
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
sharesKey key objects = oneObject [Key.toText held | Object fields <- objects, held <- KeyMap.keys fields, held == key]

-- | Whether at least two of the values are the string and all of them hold its text as one heap object.
sharesString :: Text -> [Value] -> IO Bool
sharesString text values = oneObject [held | String held <- values, held == text]

oneObject :: [Text] -> IO Bool
oneObject texts = do
    names <- traverse (makeStableName <=< evaluate) texts
    pure $ case names of
        stable : rest@(_ : _) -> all (== stable) rest
        _ -> False

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Incremental registry extraction with a complete-source digest and bounded input chunks.
module Ecluse.Core.Registry.JsonStream (
    StreamResult (..),
    readJsonStream,
    retainedValue,
    retainedObject,
    retainedArray,
    retainedScalar,
) where

import Crypto.Hash (hashInit, hashUpdate)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Data.Vector qualified as V

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Security (BodyLimit, LimitError (BodyTooLarge), bodyLimitBytes)
import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)

-- | Extracted data and identity of the complete decompressed source, including ignored fields.
data StreamResult a = StreamResult
    { streamValue :: Either ParseError a
    , streamBytes :: Int
    , streamDigest :: ContentDigest
    }
    deriving stock (Eq, Show)

-- | Drain successful bodies even when extraction ends early. An empty chunk ends the response.
readJsonStream :: (Monad m) => BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> m ByteString -> m (Either LimitError (StreamResult s))
readJsonStream bound parser step initial readChunk = go 0 hashInit initial (J.runParser parser)
  where
    go !seen !digest !acc output = case output of
        J.ParseYield value next -> case step acc value of
            Left fault -> pure (Left fault)
            Right updated -> go seen digest updated next
        J.ParseFailed err -> pure (Right (StreamResult (Left (ParseError (toText err))) seen (digestFromContext digest)))
        _ -> do
            chunk <- readChunk
            if BS.null chunk
                then pure . Right $ StreamResult (finish acc output) seen (digestFromContext digest)
                else
                    if BS.length chunk > bodyLimitBytes bound - seen
                        then pure (Left (BodyTooLarge bound))
                        else feed (seen + BS.length chunk) (hashUpdate digest chunk) acc output chunk
    feed seen digest acc output chunk = case output of
        J.ParseYield value next -> case step acc value of
            Left fault -> pure (Left fault)
            Right updated -> feed seen digest updated next chunk
        J.ParseNeedData next
            | not (BS.null chunk) ->
                let (piece, remaining) = BS.splitAt 32768 chunk
                 in feed seen digest acc (next piece) remaining
        J.ParseDone _ -> go seen digest acc (J.ParseDone BS.empty)
        _ -> go seen digest acc output
    finish acc = \case
        J.ParseDone _ -> Right acc
        _ -> Left (ParseError "incomplete registry JSON")

-- | Decode a retained field within a structural budget. Unknown fields never call this parser.
retainedValue :: Int -> J.Parser Value
retainedValue depth
    | depth <= 0 = J.mapWithFailure (const (Left "retained JSON nesting limit")) (pure ())
    | otherwise =
        retainedScalar
            <|> retainedArray (retainedValue (depth - 1))
            <|> retainedObject (const (retainedValue (depth - 1)))

-- | Materialise only fields whose key selects a parser. Duplicate keys keep their first value.
retainedObject :: (Text -> J.Parser Value) -> J.Parser Value
retainedObject select = Object <$> J.catMaybeI (J.foldI insert Nothing events)
  where
    field key = (Key.fromText (T.copy key),) <$> select key
    events = J.objectFound Nothing Nothing (Just <$> J.objectKeyValues field)
    insert fields Nothing = Just (fromMaybe mempty fields)
    insert fields (Just (!key, !value)) =
        let !current = fromMaybe mempty fields
            !updated = if KeyMap.member key current then current else KeyMap.insert key value current
         in Just updated

-- | Retain array positions in source order, without accepting a non-array as an empty array.
retainedArray :: J.Parser Value -> J.Parser Value
retainedArray parser = Array . V.fromList . reverse <$> J.catMaybeI (J.foldI collect Nothing events)
  where
    events = J.arrayFound Nothing Nothing (Just <$> J.arrayOf parser)
    collect values Nothing = Just (fromMaybe [] values)
    collect values (Just !value) = Just (value : fromMaybe [] values)

-- | Read a scalar without materialising an object or array when the field has the wrong shape.
retainedScalar :: J.Parser Value
retainedScalar = (String . T.copy <$> J.string) <|> (Number <$> J.number) <|> (Bool <$> J.bool) <|> (Null <$ J.jNull)

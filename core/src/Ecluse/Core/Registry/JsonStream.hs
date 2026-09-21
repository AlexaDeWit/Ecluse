-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Incremental registry extraction with a complete-source digest and bounded input chunks.
module Ecluse.Core.Registry.JsonStream (
    StreamResult (..),
    readJsonStream,
    retainedValue,
    withinRetainedDepth,
    retainedObjectOr,
    retainedScalar,
    retainedObjectWith,
    retainedArrayWith,
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
retainedValue depth =
    withinRetainedDepth depth $
        retainedObjectWith
            (retainedArrayWith retainedScalar child)
            (const child)
  where
    child = retainedValue (depth - 1)

-- | Charge the parsed value's own level, including empty containers. Children need one less level.
withinRetainedDepth :: Int -> J.Parser a -> J.Parser a
withinRetainedDepth budget parser
    | budget <= 0 = J.mapWithFailure (const (Left "retained JSON nesting limit")) (pure ())
    | otherwise = parser

-- | Supply an invalid-shape witness without traversing a valid object through a parallel fallback.
retainedObjectOr :: Value -> (Text -> J.Parser Value) -> J.Parser Value
retainedObjectOr fallback = fmap (fromMaybe fallback) . foldRetained . objectEvents

-- | Select object events before folding. The fallback handles scalars and other container shapes.
retainedObjectWith :: J.Parser Value -> (Text -> J.Parser Value) -> J.Parser Value
retainedObjectWith fallback select = J.catMaybeI (foldRetained (objectEvents select <|> (OtherValue <$> fallback)))

-- | Select array events before folding. A container fallback must yield only its completed value.
retainedArrayWith :: J.Parser Value -> J.Parser Value -> J.Parser Value
retainedArrayWith fallback parser = J.catMaybeI (foldRetained (arrayEvents parser <|> (OtherValue <$> fallback)))

data RetainedEvent = BeginObject | ObjectField Key.Key Value | BeginArray | ArrayItem Value | OtherValue Value | EndContainer

data Retained = Missing | ObjectFields (KeyMap.KeyMap Value) | ArrayItems [Value] | ScalarValue Value

objectEvents :: (Text -> J.Parser Value) -> J.Parser RetainedEvent
objectEvents select = J.objectFound BeginObject EndContainer (J.objectKeyValues field)
  where
    field key = ObjectField (Key.fromText (T.copy key)) <$> select key

arrayEvents :: J.Parser Value -> J.Parser RetainedEvent
arrayEvents parser = J.arrayFound BeginArray EndContainer (ArrayItem <$> J.arrayOf parser)

-- The fallback contributes one completed value. Unmatched first shapes still skip their input.
foldRetained :: J.Parser RetainedEvent -> J.Parser (Maybe Value)
foldRetained = fmap finish . J.foldI collect Missing
  where
    collect _ BeginObject = ObjectFields mempty
    collect _ BeginArray = ArrayItems []
    collect (ObjectFields fields) (ObjectField key value) =
        ObjectFields (if KeyMap.member key fields then fields else KeyMap.insert key value fields)
    collect (ArrayItems values) (ArrayItem value) = ArrayItems (value : values)
    collect _ (OtherValue value) = ScalarValue value
    collect current _ = current
    finish Missing = Nothing
    finish (ObjectFields fields) = Just (Object fields)
    finish (ArrayItems values) = Just (Array (V.fromList (reverse values)))
    finish (ScalarValue value) = Just value

-- | Read a scalar without materialising an object or array when the field has the wrong shape.
retainedScalar :: J.Parser Value
retainedScalar = (String . T.copy <$> J.string) <|> (Number <$> J.number) <|> (Bool <$> J.bool) <|> (Null <$ J.jNull)

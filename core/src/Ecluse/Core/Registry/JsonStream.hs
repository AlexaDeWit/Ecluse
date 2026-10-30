-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Incremental registry extraction within a decompressed body ceiling.
The maintained lexer copies decoded strings and keys into owned arrays, including
chunk-spanning tokens. Opaque cursors can retain input slices until the walk advances.
-}
module Ecluse.Core.Registry.JsonStream (
    -- * Bounded reads
    StreamResult (..),
    Steps (..),
    Step,
    readSteps,
    readStepsWithEnd,
    readJsonStream,
    maxChunkBytes,

    -- * Retained values
    retainedValue,
    withinRetainedDepth,
    nestingLimit,
    isNestingLimit,
    isJsonSpace,
    Members,
    everyMember,
    retainedScalar,
    retainedObjectWith,
    retainedArrayWith,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.Parser qualified as J
import Data.Vector qualified as V

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.JsonStream.Internal (Members (..))
import Ecluse.Core.Security (BodyLimit, LimitError (BodyTooLarge), bodyLimitBytes)

-- | Extracted data and the size of the complete decompressed source, including ignored fields.
data StreamResult a = StreamResult
    { streamValue :: Either ParseError a
    , streamBytes :: Int
    }
    deriving stock (Eq, Show)

-- | A suspended, failed, refused or finished read. Writing reads resume in their own effect.
data Steps m s
    = NeedData (ByteString -> m (Steps m s))
    | Failed Text
    | Refused LimitError
    | Finished s

-- | A read with no effect of its own.
type Step = Steps Identity

-- | Enforce the body ceiling with pieces of at most 32 KiB, then drain until an empty chunk ends the body.
readSteps :: (Monad n) => (forall a. m a -> n a) -> BodyLimit -> Steps m s -> n ByteString -> n (Either LimitError (StreamResult s))
readSteps run = readStepsWithEnd run pure

-- | Finalise a framed read at EOF without feeding an invented source byte to its consumer.
readStepsWithEnd :: (Monad n) => (forall a. m a -> n a) -> (Steps m s -> n (Steps m s)) -> BodyLimit -> Steps m s -> n ByteString -> n (Either LimitError (StreamResult s))
readStepsWithEnd run end bound start readChunk = go 0 start
  where
    go !seen step = case step of
        Refused fault -> pure (Left fault)
        Failed err -> pure (Right (StreamResult (Left (ParseError err)) seen))
        _ -> do
            chunk <- readChunk
            if BS.null chunk
                then do
                    ended <- end step
                    pure $ case ended of
                        Refused fault -> Left fault
                        Failed err -> Right (StreamResult (Left (ParseError err)) seen)
                        _ -> Right (StreamResult (finish ended) seen)
                else
                    if BS.length chunk > bodyLimitBytes bound - seen
                        then pure (Left (BodyTooLarge bound))
                        else feed (seen + BS.length chunk) step chunk
    feed seen step chunk = case step of
        NeedData next
            | not (BS.null chunk) -> do
                let (piece, remaining) = BS.splitAt maxChunkBytes chunk
                resumed <- run (next piece)
                feed seen resumed remaining
        _ -> go seen step
    finish = \case
        Finished result -> Right result
        _ -> Left (ParseError "incomplete registry JSON")

-- | Fold each value the parser yields through the step, as 'readSteps' feeds it.
readJsonStream :: (Monad m) => BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> m ByteString -> m (Either LimitError (StreamResult s))
readJsonStream bound parser step initial = readSteps (pure . runIdentity) bound (parserSteps initial (J.runParser parser))
  where
    parserSteps !acc = \case
        J.ParseYield value next -> either Refused (`parserSteps` next) (step acc value)
        J.ParseNeedData next -> NeedData (Identity . parserSteps acc . next)
        J.ParseFailed err -> Failed (toText err)
        J.ParseDone _ -> Finished acc

-- | Decode a retained field within a structural budget. Unknown fields never call this parser.
retainedValue :: Int -> J.Parser Value
retainedValue depth =
    withinRetainedDepth depth $
        retainedObjectWith
            (retainedArrayWith retainedScalar child)
            (everyMember child)
  where
    child = retainedValue (depth - 1)

-- | The parse error that marks a retained value past its structural budget.
nestingLimit :: Text
nestingLimit = "retained JSON nesting limit"

-- | Distinguish a retained-depth refusal from malformed JSON.
isNestingLimit :: ParseError -> Bool
isNestingLimit (ParseError message) = message == nestingLimit

-- | JSON permits only space, tab, CR and LF as whitespace.
isJsonSpace :: Word8 -> Bool
isJsonSpace byte = byte == 32 || byte == 10 || byte == 13 || byte == 9

-- | Charge the parsed value's own level, including empty containers. Children need one less level.
withinRetainedDepth :: Int -> J.Parser a -> J.Parser a
withinRetainedDepth budget parser
    | budget <= 0 = J.mapWithFailure (\() -> Left (toString nestingLimit)) (pure ())
    | otherwise = parser

-- | Retain every member with one parser, each under its own key.
everyMember :: J.Parser Value -> Members
everyMember = EveryMember

-- | Select object events before folding. The fallback handles scalars and other container shapes.
retainedObjectWith :: J.Parser Value -> Members -> J.Parser Value
retainedObjectWith fallback members = J.catMaybeI (foldRetained (objectEvents members <|> (OtherValue <$> fallback)))

-- | Select array events before folding. A container fallback must yield only its completed value.
retainedArrayWith :: J.Parser Value -> J.Parser Value -> J.Parser Value
retainedArrayWith fallback parser = J.catMaybeI (foldRetained (arrayEvents parser <|> (OtherValue <$> fallback)))

data RetainedEvent = BeginObject | ObjectField Key.Key Value | BeginArray | ArrayItem Value | OtherValue Value | EndContainer

data Retained = Missing | ObjectFields (KeyMap.KeyMap Value) | ArrayItems [Value] | ScalarValue Value

objectEvents :: Members -> J.Parser RetainedEvent
objectEvents members = J.objectFound BeginObject EndContainer (J.objectKeyValues (memberEvent members))

memberEvent :: Members -> Text -> J.Parser RetainedEvent
memberEvent = \case
    NamedMembers named -> \name -> maybe mempty (\(key, parser) -> ObjectField key <$> parser) (HashMap.lookup name named)
    EveryMember parser -> \name -> ObjectField (Key.fromText name) <$> parser

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
retainedScalar = (String <$> J.string) <|> (Number <$> J.number) <|> (Bool <$> J.bool) <|> (Null <$ J.jNull)

-- | The largest piece of a chunk a read feeds its consumer at once.
maxChunkBytes :: Int
maxChunkBytes = 32768

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Incremental registry extraction within a decompressed body ceiling. With the vendored json-stream
and text 2.1.3, decoded strings and keys own their arrays, including chunk-spanning tokens.
See <https://github.com/ondrap/json-stream/blob/537a43a775e64f50dc63c373193323de98619799/Data/JsonStream/Unescape.hs decoder storage>.
-}
module Ecluse.Core.Registry.JsonStream (
    -- * Bounded reads
    StreamResult (..),
    Steps (..),
    Step,
    readSteps,
    readJsonStream,

    -- * Retained values
    retainedValue,
    withinRetainedDepth,
    Members,
    namedMembers,
    everyMember,
    retainedObjectOr,
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
import Ecluse.Core.Security (BodyLimit, LimitError (BodyTooLarge), bodyLimitBytes)

-- | Extracted data and the size of the complete decompressed source, including ignored fields.
data StreamResult a = StreamResult
    { streamValue :: Either ParseError a
    , streamBytes :: Int
    }
    deriving stock (Eq, Show)

{- | A read in progress: it needs input, stops on a parse error or a refused value, or has finished.
A read that writes as it goes resumes in its own effect.
-}
data Steps m s
    = NeedData (ByteString -> m (Steps m s))
    | Failed Text
    | Refused LimitError
    | Finished s

-- | A read with no effect of its own.
type Step = Steps Identity

{- | Feed a read in pieces of at most 32 KiB, within the body ceiling, and drain the body after the
read finishes. An empty chunk ends the body. The read resumes in its effect, run in the reader's.
-}
readSteps :: (Monad n) => (forall a. m a -> n a) -> BodyLimit -> Steps m s -> n ByteString -> n (Either LimitError (StreamResult s))
readSteps run bound start readChunk = go 0 start
  where
    go !seen step = case step of
        Refused fault -> pure (Left fault)
        Failed err -> pure (Right (StreamResult (Left (ParseError err)) seen))
        _ -> do
            chunk <- readChunk
            if BS.null chunk
                then pure . Right $ StreamResult (finish step) seen
                else
                    if BS.length chunk > bodyLimitBytes bound - seen
                        then pure (Left (BodyTooLarge bound))
                        else feed (seen + BS.length chunk) step chunk
    feed seen step chunk = case step of
        NeedData next
            | not (BS.null chunk) -> do
                let (piece, remaining) = BS.splitAt 32768 chunk
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

-- | Charge the parsed value's own level, including empty containers. Children need one less level.
withinRetainedDepth :: Int -> J.Parser a -> J.Parser a
withinRetainedDepth budget parser
    | budget <= 0 = J.mapWithFailure (const (Left "retained JSON nesting limit")) (pure ())
    | otherwise = parser

{- | Which members of an object are retained, with what parser, and under which key. Every object
read with one 'Members' value holds each name it knows under one shared key.
-}
data Members
    = NamedMembers (HashMap Text (Key.Key, J.Parser Value))
    | EveryMember (J.Parser Value)

-- | Retain only the named members. The first entry for a name wins.
namedMembers :: [(Text, J.Parser Value)] -> Members
namedMembers entries = NamedMembers (HashMap.fromListWith (\_ earlier -> earlier) [(name, (Key.fromText name, parser)) | (name, parser) <- entries])

-- | Retain every member with one parser, each under its own key.
everyMember :: J.Parser Value -> Members
everyMember = EveryMember

-- | Supply an invalid-shape witness without traversing a valid object through a parallel fallback.
retainedObjectOr :: Value -> Members -> J.Parser Value
retainedObjectOr fallback = fmap (fromMaybe fallback) . foldRetained . objectEvents

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

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Incremental registry extraction within a decompressed body ceiling, and the per-document sharing
of what it retains. With json-stream 0.4.6.1 and text 2.1.3, decoded strings and keys own their
arrays, including chunk-spanning tokens.
See <https://github.com/ondrap/json-stream/blob/537a43a775e64f50dc63c373193323de98619799/Data/JsonStream/Unescape.hs decoder storage>.
-}
module Ecluse.Core.Registry.JsonStream (
    StreamResult (..),
    readJsonStream,
    retainedValue,
    withinRetainedDepth,
    Members,
    namedMembers,
    knownMembers,
    everyMember,
    retainedObjectOr,
    retainedScalar,
    retainedObjectWith,
    retainedArrayWith,
    InternTable,
    internTableKeeping,
    internValue,
    internText,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.Parser qualified as J
import Data.Map.Internal (Map (Bin, Tip))
import Data.Vector qualified as V

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Security (BodyLimit, LimitError (BodyTooLarge), bodyLimitBytes)
import Ecluse.Core.Text (ownedText)

-- | Extracted data and the size of the complete decompressed source, including ignored fields.
data StreamResult a = StreamResult
    { streamValue :: Either ParseError a
    , streamBytes :: Int
    }
    deriving stock (Eq, Show)

-- | Drain successful bodies even when extraction ends early. An empty chunk ends the response.
readJsonStream :: (Monad m) => BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> m ByteString -> m (Either LimitError (StreamResult s))
readJsonStream bound parser step initial readChunk = go 0 initial (J.runParser parser)
  where
    go !seen !acc output = case output of
        J.ParseYield value next -> case step acc value of
            Left fault -> pure (Left fault)
            Right updated -> go seen updated next
        J.ParseFailed err -> pure (Right (StreamResult (Left (ParseError (toText err))) seen))
        _ -> do
            chunk <- readChunk
            if BS.null chunk
                then pure . Right $ StreamResult (finish acc output) seen
                else
                    if BS.length chunk > bodyLimitBytes bound - seen
                        then pure (Left (BodyTooLarge bound))
                        else feed (seen + BS.length chunk) acc output chunk
    feed seen acc output chunk = case output of
        J.ParseYield value next -> case step acc value of
            Left fault -> pure (Left fault)
            Right updated -> feed seen updated next chunk
        J.ParseNeedData next
            | not (BS.null chunk) ->
                let (piece, remaining) = BS.splitAt 32768 chunk
                 in feed seen acc (next piece) remaining
        J.ParseDone _ -> go seen acc (J.ParseDone BS.empty)
        _ -> go seen acc output
    finish acc = \case
        J.ParseDone _ -> Right acc
        _ -> Left (ParseError "incomplete registry JSON")

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
    | KnownMembers (HashMap Text Key.Key) (J.Parser Value)
    | EveryMember (J.Parser Value)

-- | Retain only the named members. The first entry for a name wins.
namedMembers :: [(Text, J.Parser Value)] -> Members
namedMembers entries = NamedMembers (HashMap.fromListWith (\_ earlier -> earlier) [(name, (Key.fromText name, parser)) | (name, parser) <- entries])

-- | Retain every member with one parser, sharing the key of each listed name.
knownMembers :: [Text] -> J.Parser Value -> Members
knownMembers names = KnownMembers (HashMap.fromList [(name, Key.fromText name) | name <- names])

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
    KnownMembers known parser -> \name -> ObjectField (HashMap.findWithDefault (Key.fromText name) name known) <$> parser
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

{- | The first copy of each key and string that one document's retained values hold. Keep one table
per document and drop it when the read ends, so no table outlives the documents it serves.
-}
newtype InternTable = InternTable (HashMap Text Copy)

-- A key and a string with the same text share one copy. A key's copy says what happens to its values.
data Copy = Copy Text Value Values

data Values = ShareValues | KeepValues

{- | A table for one document that keeps the values of the named members as read. Name the members
whose values differ in every release or file, so they never enter the table.
-}
internTableKeeping :: [Text] -> InternTable
internTableKeeping names = InternTable (HashMap.fromList [(name, Copy name (String name) KeepValues) | name <- names])

{- | The value with every key and string replaced by the table's copy. A text the table lacks is
stored as an owned copy. Member order, first-occurrence precedence and every text are unchanged.
-}
internValue :: InternTable -> Value -> (InternTable, Value)
internValue table value = case intern table value of
    Interned held shared -> (held, shared)

-- | The table's copy of a text, such as a member name read outside a retained value.
internText :: InternTable -> Text -> (InternTable, Text)
internText table text = case copyOf table text of
    Interned held (Copy shared _ _) -> (held, shared)

data Interned a = Interned InternTable a
    deriving stock (Functor)

intern :: InternTable -> Value -> Interned Value
intern table = \case
    String text -> (\(Copy _ shared _) -> shared) <$> copyOf table text
    Object members -> Object . KeyMap.fromMap <$> internMembers table (KeyMap.toMap members)
    Array items -> Array . V.fromListN (V.length items) . reverse <$> V.foldl' internItem (Interned table []) items
    other -> Interned table other
  where
    internItem (Interned held shared) item = (: shared) <$> intern held item

-- Data.Map.Internal is outside containers' PVP guarantee. Its constructors rebuild each node once, and
-- the public route (foldlWithKey' into fromDistinctDescList) allocates about 111 more bytes per member.
internMembers :: InternTable -> Map Key.Key Value -> Interned (Map Key.Key Value)
internMembers table = \case
    Tip -> Interned table Tip
    Bin size key value left right ->
        let !(Interned afterLeft left') = internMembers table left
            !(Interned afterKey (Copy name _ values)) = copyOf afterLeft (Key.toText key)
            !(Interned afterValue value') = case values of
                ShareValues -> intern afterKey value
                KeepValues -> Interned afterKey value
            !(Interned afterRight right') = internMembers afterValue right
         in Interned afterRight (Bin size (Key.fromText name) value' left' right')

copyOf :: InternTable -> Text -> Interned Copy
copyOf table@(InternTable copies) text = case HashMap.lookup text copies of
    Just known -> Interned table known
    Nothing ->
        let owned = ownedText text
            copy = Copy owned (String owned) ShareValues
         in Interned (InternTable (HashMap.insert owned copy copies)) copy

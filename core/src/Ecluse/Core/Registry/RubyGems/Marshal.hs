-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded Ruby Marshal 4.8 data. Class names remain inert strings.
module Ecluse.Core.Registry.RubyGems.Marshal (
    MarshalValue (..),
    decodeMarshal,
    marshalText,
    stripInstanceVariables,
) where

import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Security.Limits (Limits (..))

-- | A data tree with resolved, acyclic references and no executable hooks.
data MarshalValue
    = MNil
    | MBool Bool
    | MInteger Integer
    | MFloat Double
    | MString ByteString
    | MSymbol ByteString
    | MArray [MarshalValue]
    | MHash [(MarshalValue, MarshalValue)]
    | MObject ByteString [(ByteString, MarshalValue)]
    | MUserDefined ByteString ByteString
    | MUserMarshal ByteString MarshalValue
    | MInstance MarshalValue [(ByteString, MarshalValue)]
    deriving stock (Eq, Show)

data Cursor = Cursor
    { remaining :: ByteString
    , symbols :: IntMap.IntMap ByteString
    , objects :: IntMap.IntMap (Maybe (MarshalValue, Int, Int))
    , nodes :: Int
    , stringBytes :: Int
    , bounds :: Limits
    }

type Decoder = StateT Cursor (Either ParseError)

-- | Decode exactly one object, refusing unsupported tags, cycles and invalid references.
decodeMarshal :: Limits -> ByteString -> Either ParseError MarshalValue
decodeMarshal limits bytes = do
    when (BS.length bytes > maxMetadataBytes limits) (parseRefusal "Marshal body exceeds its byte bound")
    (value, cursor) <- runStateT parser (Cursor bytes IntMap.empty IntMap.empty 0 0 limits)
    unless (BS.null (remaining cursor)) (parseRefusal "Marshal has trailing bytes")
    pure value
  where
    parser = do
        header <- takeBytes 2
        unless (header == "\x04\x08") (refuse "unsupported Marshal version")
        readValue 0

readValue :: Int -> Decoder MarshalValue
readValue depth = do
    limits <- gets bounds
    when (depth > maxNestingDepth limits) (refuse "Marshal nesting exceeds its bound")
    countNode
    byte >>= \case
        48 -> pure MNil
        84 -> pure (MBool True)
        70 -> pure (MBool False)
        105 -> MInteger <$> integer
        102 -> tracked $ do
            raw <- string
            when (BS.length raw > 64) (refuse "Marshal float exceeds its byte bound")
            case readMaybe (B8.unpack raw) of
                Just value | not (isNaN value || isInfinite value) -> pure (MFloat value)
                _ -> refuse "invalid or non-finite Marshal float"
        34 -> tracked (MString <$> string)
        58 -> MSymbol <$> defineSymbol
        59 -> MSymbol <$> referenceSymbol
        64 -> do
            index <- natural
            table <- gets objects
            case IntMap.lookup index table of
                Just (Just (value, cost, height)) -> do
                    cursor <- get
                    when (depth + height > maxNestingDepth (bounds cursor)) (refuse "Marshal referenced depth exceeds its bound")
                    let cap = min (maxArtifactCount (bounds cursor)) (maxMetadataBytes (bounds cursor) `div` 8)
                    when (cost > cap - nodes cursor) (refuse "Marshal reference work exceeds its bound")
                    modify' (\current -> current{nodes = nodes current + cost})
                    pure value
                _ -> refuse "invalid or cyclic Marshal object reference"
        91 -> tracked (MArray <$> counted (readValue (depth + 1)))
        123 -> tracked (readHash (depth + 1))
        111 -> tracked $ MObject <$> symbol <*> attributes (depth + 1)
        117 -> tracked $ MUserDefined <$> symbol <*> string
        85 -> tracked $ MUserMarshal <$> symbol <*> readValue (depth + 1)
        73 -> MInstance <$> readValue (depth + 1) <*> attributes (depth + 1)
        _ -> refuse "unsupported Marshal tag"

readHash :: Int -> Decoder MarshalValue
readHash depth = do
    entries <- counted ((,) <$> readValue depth <*> readValue depth)
    keys <- traverse (lift . hashKey . fst) entries
    unless (Set.size (Set.fromList keys) == length keys) (refuse "duplicate Marshal hash key")
    pure (MHash entries)
  where
    hashKey value = case stripInstanceVariables value of
        MString bytes -> Right (False, bytes)
        MSymbol bytes -> Right (True, bytes)
        _ -> parseRefusal "unsupported Marshal hash key"

tracked :: Decoder MarshalValue -> Decoder MarshalValue
tracked parse = do
    before <- gets nodes
    table <- gets objects
    let index = IntMap.size table
    modify' (\cursor -> cursor{objects = IntMap.insert index Nothing table})
    value <- parse
    after <- gets nodes
    modify' (\cursor -> cursor{objects = IntMap.insert index (Just (value, after - before + 1, valueDepth value)) (objects cursor)})
    pure value

valueDepth :: MarshalValue -> Int
valueDepth = \case
    MArray values -> descendants values
    MHash entries -> descendants (concatMap (\(key, value) -> [key, value]) entries)
    MObject _ entries -> descendants (map snd entries)
    MUserMarshal _ value -> 1 + valueDepth value
    MInstance value entries -> descendants (value : map snd entries)
    _ -> 0
  where
    descendants values = foldl' max 0 (map ((1 +) . valueDepth) values)

attributes :: Int -> Decoder [(ByteString, MarshalValue)]
attributes depth = do
    values <- counted ((,) <$> symbol <*> readValue depth)
    unless (length values == length (ordNub (map fst values))) (refuse "duplicate Marshal instance variable")
    pure values

symbol :: Decoder ByteString
symbol = do
    countNode
    byte >>= \case
        58 -> defineSymbol
        59 -> referenceSymbol
        _ -> refuse "Marshal class or field name is not a symbol"

defineSymbol :: Decoder ByteString
defineSymbol = do
    value <- string
    table <- gets symbols
    modify' (\cursor -> cursor{symbols = IntMap.insert (IntMap.size table) value table})
    pure value

referenceSymbol :: Decoder ByteString
referenceSymbol = do
    index <- natural
    table <- gets symbols
    maybe (refuse "invalid Marshal symbol reference") pure (IntMap.lookup index table)

countNode :: Decoder ()
countNode = do
    limits <- gets bounds
    count <- gets nodes
    when (count >= maxMetadataBytes limits `div` 8 || count >= maxArtifactCount limits) (refuse "Marshal node count exceeds its bound")
    modify' (\cursor -> cursor{nodes = count + 1})

counted :: Decoder a -> Decoder [a]
counted parse = do
    count <- natural
    cap <- gets (maxArtifactCount . bounds)
    available <- gets (BS.length . remaining)
    when (count > cap || count > available) (refuse "Marshal collection count exceeds its bound")
    replicateM count parse

string :: Decoder ByteString
string = do
    count <- natural
    cursor <- get
    when (count > maxMetadataBytes (bounds cursor) - stringBytes cursor) (refuse "Marshal strings exceed their byte bound")
    modify' (\current -> current{stringBytes = stringBytes current + count})
    BS.copy <$> takeBytes count

natural :: Decoder Int
natural = do
    value <- integer
    if value < 0 || value > toInteger (maxBound :: Int)
        then refuse "invalid Marshal length or reference"
        else pure (fromInteger value)

integer :: Decoder Integer
integer = do
    tag <- byte
    let signed = if tag >= 128 then fromIntegral tag - 256 else fromIntegral tag :: Int
    case signed of
        0 -> pure 0
        n | n > 4 -> pure (toInteger (n - 5))
        n | n < -4 -> pure (toInteger (n + 5))
        n -> do
            bytes <- takeBytes (abs n)
            let magnitude = BS.foldr (\part value -> toInteger part + value * 256) 0 bytes
            pure (if n < 0 then magnitude - (1 `shiftL` (8 * abs n)) else magnitude)

byte :: Decoder Word8
byte = do
    bytes <- takeBytes 1
    maybe (refuse "truncated Marshal input") (pure . fst) (BS.uncons bytes)

takeBytes :: Int -> Decoder ByteString
takeBytes count = do
    cursor <- get
    when (count < 0 || count > BS.length (remaining cursor)) (refuse "truncated Marshal input")
    let (bytes, rest) = BS.splitAt count (remaining cursor)
    put cursor{remaining = rest}
    pure bytes

refuse :: Text -> Decoder a
refuse = lift . parseRefusal

-- | Remove inert encoding and instance-variable wrappers when reading scalar fields.
stripInstanceVariables :: MarshalValue -> MarshalValue
stripInstanceVariables (MInstance value _) = stripInstanceVariables value
stripInstanceVariables value = value

-- | Require a valid UTF-8 native string, preserving its spelling.
marshalText :: MarshalValue -> Either ParseError Text
marshalText value = case stripInstanceVariables value of
    MString bytes -> first (const (ParseError "Marshal string is not UTF-8")) (TE.decodeUtf8' bytes)
    _ -> parseRefusal "expected Marshal string"

-- SPDX-FileCopyrightText: 2015 Ondrej Palkovsky
-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{- | Incremental selective JSON parsing with json-stream's inherited matching and chunk rules.
Use 'eitherDecode' for reported failures. List-producing entry points throw 'ErrorCall'.
Unselected values are skipped without establishing whole-document JSON validity.
-}
module Data.JsonStream.Parser (
    -- * The @Parser@ type
    Parser,
    ParseOutput (..),

    -- * Parsing functions
    runParser,
    runParser',
    parseByteString,
    parseLazyByteString,

    -- * Aeson in-place replacement functions
    decode,
    eitherDecode,
    decodeStrict,
    eitherDecodeStrict,

    -- * FromJSON parser
    value,
    valueWith,
    string,
    byteString,

    -- * Constant space parsers
    safeString,
    number,
    integer,
    real,
    bool,
    jNull,
    safeByteString,

    -- * Structure operators
    (.:),
    (.:?),
    (.|),
    (.!),

    -- * Structure parsers
    objectWithKey,
    objectItems,
    objectValues,
    objectKeyValues,
    arrayOf,
    arrayWithIndexOf,
    indexedArrayOf,
    nullable,

    -- * Fast structure parser
    objectOf,
    Object,

    -- * Parsing modifiers
    filterI,
    takeI,
    mapWithFailure,
    manyReverse,
    foldI,
    foldMapI,
    unFoldI,
    catMaybeI,

    -- * SAX-like parsers
    arrayFound,
    objectFound,
) where

import Control.Applicative (Alternative (..), optional)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as AEK
import qualified Data.Aeson.KeyMap as AEK
import qualified Data.Aeson.Types as AE
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Char (isSpace)
import Data.Scientific (
    Scientific,
    isInteger,
    toBoundedInteger,
    toRealFloat,
 )
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Foreign.C.Types (CLong)

import Control.Exception (ErrorCall (..), throw)
import Data.JsonStream.CLexer (tokenParser, unescapeText)
import Data.JsonStream.TokenParser (Element (..), TokenResult (..))
import Data.JsonStream.Unescape (unsafeDecodeASCII)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

objectKeyStringLimit :: Int
objectKeyStringLimit = 65536

data ParseResult v
    = MoreData ~(Parser v, BS.ByteString -> TokenResult)
    | Failed ~String
    | Done ~BS.ByteString ~TokenResult
    | Yield ~v ~(ParseResult v)

instance Functor ParseResult where
    fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
    fmap _ (Failed err) = Failed err
    fmap _ (Done ctx tok) = Done ctx tok
    fmap f (Yield v np) = Yield (f v) (fmap f np)

-- | A selective parser that can yield multiple values before requesting another input piece.
newtype Parser a = Parser
    { callParse :: TokenResult -> ParseResult a
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \d -> fmap f (p d)

yieldResults :: [a] -> ParseResult a -> ParseResult a
yieldResults values end = foldr Yield end values

{- | '<*>' will run both parsers in parallel and combine results.
Results use the Cartesian product in input order.

>>> :set -XOverloadedStrings
>>> let text = "[{\"name\": \"John\", \"age\": 20}, {\"age\": 30, \"name\": \"Frank\"}]"
>>> let parser = arrayOf $ (,) <$> "name" .: string <*> "age"  .: integer
>>> parseByteString parser text :: [(T.Text,Int)]
[("John",20),("Frank",30)]
-}
instance Applicative Parser where
    pure x = Parser $ \tok -> process (callParse ignoreVal tok)
      where
        process (Failed err) = Failed err
        process (Done ctx tok) = Yield x (Done ctx tok)
        process (MoreData (np, ntok)) = MoreData (Parser (process . callParse np), ntok)
        process _ = Failed "Internal error in pure, ignoreVal doesn't yield"

    (<*>) m1 m2 = Parser $ \tok -> process ([], []) (callParse m1 tok) (callParse m2 tok)
      where
        process ([], _) (Done ctx ntok) _ = Done ctx ntok -- Optimize, return immediately when first parser fails
        process (lst1, lst2) (Yield v np1) p2 = process (v : lst1, lst2) np1 p2
        process (lst1, lst2) p1 (Yield v np2) = process (lst1, v : lst2) p1 np2
        process (lst1, lst2) (Done ctx ntok) (Done{}) =
            yieldResults [mx my | mx <- reverse lst1, my <- reverse lst2] (Done ctx ntok)
        process lsts (MoreData (np1, ntok1)) (MoreData (np2, _)) =
            MoreData (Parser (\tok -> process lsts (callParse np1 tok) (callParse np2 tok)), ntok1)
        process _ (Failed err) _ = Failed err
        process _ _ (Failed err) = Failed err
        process _ _ _ = Failed "Unexpected error in parallel processing <*>."

{- | '<>' will run both parsers in parallel yielding from both as the data comes

>>> :m +Data.Monoid
>>> let test = "[{\"key1\": [1,2], \"key2\": [5,6], \"key3\": [8,9]}]"
>>> let parser = arrayOf $ "key1" .: (arrayOf value) <> "key2" .: (arrayOf value)
>>> parseByteString parser test :: [Int]
[1,2,5,6]
-}
instance Monoid (Parser a) where
    mempty = ignoreVal
    mappend = (<>)

instance Semigroup (Parser a) where
    (<>) m1 m2 =
        Parser $ \tok -> process (callParse m1 tok) (callParse m2 tok)
      where
        process (Yield v np1) p2 = Yield v (process np1 p2)
        process p1 (Yield v np2) = Yield v (process p1 np2)
        process (Done ctx ntok) Done{} = Done ctx ntok
        process (MoreData (np1, ntok)) (MoreData (np2, _)) =
            MoreData (Parser $ \tok -> process (callParse np1 tok) (callParse np2 tok), ntok)
        process (Failed err) _ = Failed err
        process _ (Failed err) = Failed err
        process _ _ = Failed "Unexpected error in parallel processing <|>"

{- | Prefer matches from the first parser. Use the second when the first yields no values.

>>> let json = "[{\"key1\": [1,2], \"key2\": [5,6], \"key3\": [8,9]}]"
>>> let parser = arrayOf $ "key1" .: (arrayOf value) <|> "key2" .: (arrayOf value)
>>> parseByteString parser json :: [Int]
[1,2]
>>> let parser = arrayOf $ "key-non" .: (arrayOf value) <|> "key2" .: (arrayOf value)
>>> parseByteString parser json :: [Int]
[5,6]

>>> let json = "[{\"keys\":[1,2], \"values\":[5,6]}, {\"keys\":[9,8], \"values\":[7,6]}]"
>>> let parser = arrayOf $ (,) <$> many ("keys" .: arrayOf integer) <*> many ("values" .: arrayOf integer)
>>> parseByteString parser json :: [([Int], [Int])]
[([1,2],[5,6]),([9,8],[7,6])]
-}
instance Alternative Parser where
    empty = ignoreVal
    m1 <|> m2 = Parser $ \tok -> process [] (callParse m1 tok) (Just $ callParse m2 tok)
      where
        -- First returned item -> disable second parser
        process _ (Yield v np1) _ = Yield v (process [] np1 Nothing)
        -- First done with disabled second -> exit
        process _ (Done ctx ntok) Nothing = Done ctx ntok
        -- Both done but second not disabled -> yield items from the second
        process lst (Done ctx ntok) (Just (Done{})) = yieldResults (reverse lst) (Done ctx ntok)
        -- Second yield - remember data
        process lst np1 (Just (Yield v np2)) = process (v : lst) np1 (Just np2)
        -- Moredata processing
        process lst (MoreData (np1, ntok)) Nothing =
            MoreData (Parser $ \tok -> process lst (callParse np1 tok) Nothing, ntok)
        process lst (MoreData (np1, ntok)) (Just (MoreData (np2, _))) =
            MoreData (Parser $ \tok -> process lst (callParse np1 tok) (Just $ callParse np2 tok), ntok)
        process _ (Failed err) _ = Failed err
        process _ _ (Just (Failed err)) = Failed err
        process _ _ _ = Failed "Unexpected error in parallel processing <|>"

    some = filterI (not . null) . many
    many f = Parser $ \ntok -> loop id (callParse f ntok)
      where
        loop acc (Done ctx ntp) = Yield (acc []) (Done ctx ntp)
        loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
        loop acc (Yield v np) = loop (\nxt -> acc (v : nxt)) np
        loop _ (Failed err) = Failed err

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
    case tp of
        (PartialResult ArrayBegin ntp) -> moreData (nextitem 0) ntp
        (PartialResult _ _) -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
        (TokMoreData ntok) -> MoreData (array' valparse, ntok)
        TokFailed -> Failed "Array - token failed"
  where
    nextitem !_ _ (ArrayEnd ctx) ntok = Done ctx ntok
    nextitem !i tok _ _ = arrcontent i (callParse (valparse i) tok)

    arrcontent !i (Done _ ntp) = moreData (nextitem (i + 1)) ntp
    arrcontent !i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent !i (Yield v np) = Yield v (arrcontent i np)
    arrcontent !_ (Failed err) = Failed err

-- | Match all items of an array.
arrayOf :: Parser a -> Parser a
arrayOf valparse = array' (const valparse)

{- | Generate start/end objects when an element is found, in between run a parser.
The inner parser is not run if an array is not found.
-}
elemFound :: Element -> a -> a -> Parser a -> Parser a
elemFound elsearch start end parser = Parser $ moreData handle
  where
    handle tok el _
        | el == elsearch = Yield start (parseAndAppend (callParse parser tok))
    handle tok _ _ = callParse ignoreVal tok

    parseAndAppend (Failed err) = Failed err
    parseAndAppend (Yield v np) = Yield v (parseAndAppend np)
    parseAndAppend (MoreData (Parser np, ntp)) = MoreData (Parser (parseAndAppend . np), ntp)
    parseAndAppend (Done ctx ntp) = Yield end (Done ctx ntp)

{- | Generate start/end values when an object is found, in between run a parser.
The inner parser is not run if an object is not found.
-}
objectFound :: a -> a -> Parser a -> Parser a
objectFound = elemFound ObjectBegin

{- | Generate start/end values when an array is found, in between run a parser.
The inner parser is not run if an array is not found.

>>> let test = "[[1,2,3],true,[],false,{\"key\":1}]" :: BS.ByteString
>>> parseByteString (arrayOf (arrayFound 10 20 (1 .! integer))) test :: [Int]
[10,2,20,10,20]
-}
arrayFound :: a -> a -> Parser a -> Parser a
arrayFound = elemFound ArrayBegin

-- | Match the item at a zero-based array index.
arrayWithIndexOf :: Int -> Parser a -> Parser a
arrayWithIndexOf idx valparse = array' itemFn
  where
    itemFn aidx
        | aidx == idx = valparse
        | otherwise = ignoreVal

-- | Match all items of an array, add index to output.
indexedArrayOf :: Parser a -> Parser (Int, a)
indexedArrayOf valparse = array' (\(!key) -> (key,) <$> valparse)

-- With once enabled, skip remaining fields after the first successful match.
object' :: Bool -> (T.Text -> Parser a) -> Parser a
object' once valparse = Parser $ \tp ->
    case tp of
        (PartialResult ObjectBegin ntp) -> moreData (nextitem False) ntp
        (PartialResult _ _) -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
        (TokMoreData ntok) -> MoreData (object' once valparse, ntok)
        TokFailed -> Failed "Object - token failed"
  where
    nextitem _ _ (ObjectEnd ctx) ntok = Done ctx ntok
    nextitem yielded _ (JValue (AE.String key)) ntok =
        objcontent yielded (callParse (valparse key) ntok)
    nextitem yielded _ (StringRaw bs True _) ntok =
        objcontent yielded (callParse (valparse (unsafeDecodeASCII bs)) ntok)
    nextitem yielded _ (StringRaw bs False _) ntok =
        case unescapeText bs of
            Right t -> objcontent yielded (callParse (valparse t) ntok)
            Left e -> Failed (show e)
    nextitem yielded _ (StringContent str) ntok =
        objcontent yielded $ moreData (getLongKey [str] (BS.length str)) ntok
    nextitem _ _ el _ = Failed $ "Object - unexpected item: " ++ show el

    -- If we already yielded and should yield once, ignore the rest of the object
    objcontent yielded (Done _ ntp)
        | once && yielded = callParse (ignoreVal' 1) ntp
        | otherwise = moreData (nextitem yielded) ntp -- Reset to next value
    objcontent yielded (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent yielded . np), ntok)
    objcontent _ (Yield v np) = Yield v (objcontent True np)
    objcontent _ (Failed err) = Failed err

    getLongKey acc !len _ el ntok =
        case el of
            StringEnd _
                | Right key <- unescapeText (BS.concat $ reverse acc) ->
                    callParse (valparse key) ntok
                | otherwise -> Failed "Error decoding UTF8"
            StringContent str
                | len > objectKeyStringLimit -> callParse (ignoreStrRestThen ignoreVal) ntok
                | otherwise -> moreData (getLongKey (str : acc) (len + BS.length str)) ntok
            _ -> Failed "Object longstr - lexer failed."

moreData :: (TokenResult -> Element -> TokenResult -> ParseResult v) -> TokenResult -> ParseResult v
moreData parser tok =
    case tok of
        PartialResult el ntok -> parser tok el ntok
        TokMoreData ntok -> MoreData (Parser (moreData parser), ntok)
        TokFailed -> Failed "More data - lexer failed."

{- | Match all key-value pairs of an object, return them as a tuple.
Repeated keys retain all yielded values.
-}
objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' False $ \(!key) -> (key,) <$> valparse

{- | Match all key-value pairs of an object, return only values.
Repeated keys retain all yielded values.
-}
objectValues :: Parser a -> Parser a
objectValues valparse = object' False (const valparse)

{- | Match all key-value pairs of an object, and parse the value based on the key.
Repeated keys retain all yielded values.
-}
objectKeyValues :: (T.Text -> Parser a) -> Parser a
objectKeyValues = object' False

-- | Yield all results from the first occurrence of this key that produces any result.
objectWithKey :: T.Text -> Parser a -> Parser a
objectWithKey name valparse = object' True itemFn
  where
    itemFn key
        | key == name = valparse
        | otherwise = ignoreVal

aeValue :: Parser AE.Value
aeValue = Parser $ moreData value'
  where
    tomap = AEK.fromList . map (first AEK.fromText)
    value' tok el ntok =
        case el of
            JValue val -> Yield val (Done "" ntok)
            JInteger val -> Yield (AE.Number $ fromIntegral val) (Done "" ntok)
            StringContent _ -> callParse (AE.String <$> longString Nothing) tok
            StringRaw bs True ctx -> Yield (AE.String (unsafeDecodeASCII bs)) (Done ctx ntok)
            StringRaw bs False ctx -> case unescapeText bs of
                Right t -> Yield (AE.String t) (Done ctx ntok)
                Left e -> Failed (show e)
            ArrayBegin -> AE.Array . Vec.fromList <$> callParse (many (arrayOf aeValue)) tok
            ObjectBegin -> AE.Object . tomap <$> callParse (manyReverse (objectItems aeValue)) tok
            _ -> Failed ("aeValue - unexpected token: " ++ show el)

-- | Collect yielded values in reverse input order.
manyReverse :: Parser a -> Parser [a]
manyReverse f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ctx ntp) = Yield acc (Done ctx ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v : acc) np
    loop _ (Failed err) = Failed err

-- Non-matching scalar types are skipped without applying the Aeson parser.
jvalue :: (AE.Value -> Maybe a) -> (CLong -> Maybe a) -> Parser a
jvalue convert cvtint = Parser (moreData value')
  where
    value' tok el ntok =
        case el of
            JValue val
                | Just convValue <- convert val -> Yield convValue (Done "" ntok)
                | otherwise -> Done "" ntok
            JInteger val
                | Just convValue <- cvtint val -> Yield convValue (Done "" ntok)
                | otherwise -> Done "" ntok
            _ -> callParse ignoreVal tok

longByteString :: Maybe Int -> Parser BS.ByteString
longByteString mbounds = Parser $ moreData (handle id 0)
  where
    handle acc !len tok el ntok =
        case el of
            JValue (AE.String _) -> Failed "INTERNAL ERROR! - got decoded JValue instead of string"
            StringRaw bs _ ctx -> Yield bs (Done ctx ntok)
            StringContent str
                | (Just bounds) <- mbounds
                , len > bounds -> -- If the string exceeds bounds, discard it
                    callParse (ignoreStrRestThen (Parser $ Done "")) ntok
                | otherwise -> moreData (handle (acc . (str :)) (len + BS.length str)) ntok
            StringEnd ctx -> Yield (BS.concat (acc [])) (Done ctx ntok)
            _ -> callParse ignoreVal tok

{- | Parse raw bytestring value (json string expected), skip parsing otherwise.
The returned value is not unescaped.
-}
byteString :: Parser BS.ByteString
byteString = longByteString Nothing

{- | Check accumulated escaped bytes only when another partial string piece arrives.
Complete strings bypass this inherited check. It is not a hard size bound.
-}
safeByteString :: Int -> Parser BS.ByteString
safeByteString limit = longByteString (Just limit)

longString :: Maybe Int -> Parser T.Text
longString mbounds = Parser $ moreData (handle id 0)
  where
    handle acc !len tok el ntok =
        case el of
            JValue (AE.String str) -> Yield str (Done "" ntok)
            StringRaw bs True ctx -> Yield (unsafeDecodeASCII bs) (Done ctx ntok)
            StringRaw bs False ctx ->
                case unescapeText bs of
                    Right t -> Yield t (Done ctx ntok)
                    Left e -> Failed (show e)
            StringContent str
                | (Just bounds) <- mbounds
                , len > bounds -> -- If the string exceeds bounds, discard it
                    callParse (ignoreStrRestThen (Parser $ Done "")) ntok
                | otherwise -> moreData (handle (acc . (str :)) (len + BS.length str)) ntok
            StringEnd ctx
                | Right val <- unescapeText (BS.concat (acc [])) ->
                    Yield val (Done ctx ntok)
                | otherwise -> Failed "Error decoding UTF8"
            _ -> callParse ignoreVal tok

-- | Parse string value, skip parsing otherwise.
string :: Parser T.Text
string = longString Nothing

{- | Check accumulated escaped bytes only when another partial string piece arrives.
Complete strings bypass this inherited check. It is not a hard size bound.
-}
safeString :: Int -> Parser T.Text
safeString limit = longString (Just limit)

-- | Parse number, return in scientific format.
number :: Parser Scientific
number = jvalue cvt (Just . fromIntegral)
  where
    cvt (AE.Number num) = Just num
    cvt _ = Nothing

{- | Parse to bounded integer type (not 'Integer').
If you are using integer numbers, use this parser.
-}
integer :: forall i. (Integral i, Bounded i) => Parser i
integer = jvalue cvt clongToBounded
  where
    clmax = toInteger (maxBound :: CLong)
    clmin = toInteger (minBound :: CLong)
    imax = toInteger (maxBound :: i)
    imin = toInteger (minBound :: i)
    -- Int is generally CLong, so we get this
    clongIsSmaller = clmax <= imax && clmin >= imin
    -- If partial, we have to convert to Integer to do the checking
    clongIsPartial = clmax < imax || clmin > imin

    inBounds num
        | clongIsPartial = toInteger num <= imax && toInteger num >= imin
        | otherwise = num <= fromIntegral (maxBound :: i) && num >= fromIntegral (minBound :: i)

    clongToBounded :: CLong -> Maybe i
    clongToBounded num
        | clongIsSmaller || inBounds num = Just (fromIntegral num)
        | otherwise = Nothing

    cvt (AE.Number num)
        | isInteger num = toBoundedInteger num
    cvt _ = Nothing

-- | Parse to float/double.
real :: (RealFloat a) => Parser a
real = jvalue cvt (Just . fromIntegral)
  where
    cvt (AE.Number num) = Just $ toRealFloat num
    cvt _ = Nothing

-- | Parse bool, skip if the type is not bool.
bool :: Parser Bool
bool = jvalue cvt (const Nothing)
  where
    cvt (AE.Bool b) = Just b
    cvt _ = Nothing

-- | Match a null value.
jNull :: Parser ()
jNull = jvalue cvt (const Nothing)
  where
    cvt AE.Null = Just ()
    cvt _ = Nothing

-- | Parses a field with a possible null value.
nullable :: Parser a -> Parser (Maybe a)
nullable valparse = Parser (moreData value')
  where
    value' _ (JValue AE.Null) ntok = Yield Nothing (Done "" ntok)
    value' tok _ _ = callParse (Just <$> valparse) tok

-- | Match values with a 'AE.Parser'.  Returns values for which the given parser succeeds.
valueWith :: (AE.Value -> AE.Parser a) -> Parser a
valueWith jparser = Parser $ \ntok -> loop (callParse aeValue ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np) =
        case AE.parse jparser v of
            AE.Error _ -> loop np
            AE.Success res -> Yield res (loop np)

{- | Match 'AE.FromJSON' value. Equivalent to @'valueWith' 'AE.parseJSON'@.

>>> let json = "[{\"key1\": [1,2], \"key2\": [5,6]}]"
>>> parseByteString (arrayOf value) json :: [AE.Value]
[Object (fromList [("key2",Array [Number 5.0,Number 6.0]),("key1",Array [Number 1.0,Number 2.0])])]
-}
value :: (AE.FromJSON a) => Parser a
value = valueWith AE.parseJSON

{- | Take maximum n matching items.

>>> parseByteString (takeI 3 $ arrayOf integer) "[1,2,3,4,5,6,7,8,9,0]" :: [Int]
[1,2,3]
-}
takeI :: Int -> Parser a -> Parser a
takeI num valparse = Parser $ \tok -> loop num (callParse valparse tok)
  where
    loop _ (Done ctx ntp) = Done ctx ntp
    loop _ (Failed err) = Failed err
    loop n (MoreData (Parser np, ntok)) = MoreData (Parser (loop n . np), ntok)
    loop 0 (Yield _ np) = loop 0 np
    loop n (Yield v np) = Yield v (loop (n - 1) np)

ignoreStrRestThen :: Parser a -> Parser a
ignoreStrRestThen next = Parser $ moreData handle
  where
    handle _ el ntok =
        case el of
            StringContent _ -> moreData handle ntok
            StringEnd _ -> callParse next ntok
            _ -> Failed "Unexpected result in ignoreStrRestPlusOne"

ignoreVal :: Parser a
ignoreVal = ignoreVal' 0

ignoreVal' :: Int -> Parser a
ignoreVal' stval = Parser $ moreData (handleTok stval)
  where
    handleLongString level _ (StringContent _) ntok = moreData (handleLongString level) ntok
    handleLongString 0 _ (StringEnd ctx) ntok = Done ctx ntok
    handleLongString level _ (StringEnd _) ntok = moreData (handleTok level) ntok
    handleLongString _ _ el _ = Failed $ "Unexpected element in handleLongStr: " ++ show el

    handleTok :: Int -> TokenResult -> Element -> TokenResult -> ParseResult a
    handleTok 0 _ (JValue _) ntok = Done "" ntok
    handleTok 0 _ (StringRaw _ _ ctx) ntok = Done ctx ntok
    handleTok 0 _ (JInteger _) ntok = Done "" ntok
    handleTok 0 _ (ArrayEnd _) _ = Failed "ArrayEnd in ignoreval on 0 level"
    handleTok 0 _ (ObjectEnd _) _ = Failed "ObjectEnd in ignoreval on 0 level"
    handleTok 1 _ (ArrayEnd ctx) ntok = Done ctx ntok
    handleTok 1 _ (ObjectEnd ctx) ntok = Done ctx ntok
    handleTok level _ el ntok =
        case el of
            JValue _ -> moreData (handleTok level) ntok
            JInteger _ -> moreData (handleTok level) ntok
            StringContent _ -> moreData (handleLongString level) ntok
            StringRaw{} -> moreData (handleTok level) ntok
            ArrayEnd _ -> moreData (handleTok (level - 1)) ntok
            ObjectEnd _ -> moreData (handleTok (level - 1)) ntok
            ArrayBegin -> moreData (handleTok (level + 1)) ntok
            ObjectBegin -> moreData (handleTok (level + 1)) ntok
            StringEnd _ -> Failed "Internal error - out of order StringEnd"

{- | Let only items matching a condition pass.

>>> parseByteString (filterI (>5) $ arrayOf integer) "[1,2,3,4,5,6,7,8,9,0]" :: [Int]
[6,7,8,9]
-}
filterI :: (a -> Bool) -> Parser a -> Parser a
filterI cond valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np)
        | cond v = Yield v (loop np)
        | otherwise = loop np

-- | Fold over values in stream
foldI :: (b -> a -> b) -> b -> Parser a -> Parser b
foldI mfold start f = Parser $ \ntok -> loop start (callParse f ntok)
  where
    loop !acc (Done ctx ntp) = Yield acc (Done ctx ntp)
    loop !acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop !acc (Yield v np) = loop (acc `mfold` v) np
    loop _ (Failed err) = Failed err

-- | Strict foldMap over values in stream
foldMapI :: (Monoid m) => (a -> m) -> Parser a -> Parser m
foldMapI f = foldI (\b a -> b <> f a) mempty

-- | Filter Nothing values out of a stream
catMaybeI :: Parser (Maybe a) -> Parser a
catMaybeI valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield (Just v) np) = Yield v (loop np)
    loop (Yield Nothing np) = loop np

-- | From a list of values generate single values
unFoldI :: Parser [a] -> Parser a
unFoldI valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield (v : rest) np) = Yield v (loop (Yield rest np))
    loop (Yield [] np) = loop np

-- | Convert a mapping failure into a parser failure.
mapWithFailure :: (a -> Either String b) -> Parser a -> Parser b
mapWithFailure mapping =
    updateParser
  where
    updateParser (Parser run) = Parser $ updateParseResult . run
    updateParseResult x = case x of
        MoreData (parser, continuation) -> MoreData (updateParser parser, continuation)
        Failed message -> Failed message
        Done a b -> Done a b
        Yield val parseResult -> case mapping val of
            Left message -> Failed message
            Right val' -> Yield val' (updateParseResult parseResult)

--- Convenience operators

class OnObject o a where
    {- | Synonym for 'objectWithKey'. The '.:' operators can be chained.

    >>> let json = "{\"key1\": {\"nested-key\": 3}}"
    >>> parseByteString ("key1" .: "nested-key" .: integer) json :: [Int]
    > [3]
    >>> let test = "[{\"name\": \"test1\", \"value\": 1}, {\"name\": \"test2\", \"value\": null}, {\"name\": \"test3\"}]"
    >>> let person = objectOf $ (,) <$> "name" .: string <*> "value" .: integer .| (-1)
    >>> let people = arrayOf person
    >>> parseByteString people test :: [(T.Text, Int)]
    [("test1",1),("test2",-1),("test3",-1)]
    -}
    (.:) :: T.Text -> Parser a -> o a

    {- | Returns 'Nothing' if value is null or does not exist or match. Otherwise returns 'Just' value.
    > key .:? val = optional (key .: val)
    -}
    (.:?) :: T.Text -> Parser a -> o (Maybe a)

    {- | Return default value if the parsers on the left hand didn't produce a result.
    > p .| defval = p <|> pure defval
    -}
    (.|) :: o a -> a -> o a

infixr 7 .:
infixr 7 .:?
infixl 6 .|

instance OnObject Parser a where
    (.:) = objectWithKey
    key .:? val = optional (key .: val)
    p .| defval = p <|> pure defval

instance OnObject Object a where
    (.:) = fastObjectWithKey
    (.:?) = fastObjectWithKeyMaybe
    (Object pmap initial out) .| defval = Object pmap initial altFunc
      where
        altFunc dmap = case out dmap of
            [] -> [defval]
            res -> res

{- | Synonym for 'arrayWithIndexOf'. Matches n-th item in array.

>>> parseByteString (arrayOf (1 .! bool)) "[ [1,true,null], [2,false], [3]]" :: [Bool]
[True,False]
-}
(.!) :: Int -> Parser a -> Parser a
(.!) = arrayWithIndexOf

infixr 7 .!

---

-- | Result of parsing. Contains continuations to continue parsing.
data ParseOutput a
    = -- | Returns a value from a parser.
      ParseYield ~a ~(ParseOutput a)
    | -- | Parser needs more data to continue parsing.
      ParseNeedData ~(BS.ByteString -> ParseOutput a)
    | -- | Parsing failed, error is reported.
      ParseFailed ~String
    | {- | Parsing finished, unparsed data is returned.
      Remaining data is not returned correctly if the parser parses directly numbers/booleans/nulls; JSON should be an array/object.
      -}
      ParseDone ~BS.ByteString

instance (Show a) => Show (ParseOutput a) where
    showsPrec d (ParseYield a next) = showParen True $ showString "ParseYield " . showsPrec d a . showString " " . showsPrec d next
    showsPrec _ (ParseNeedData _) = showString "ParseNeedData"
    showsPrec d (ParseFailed err) = showParen True $ showString "ParseFailed " . showsPrec d err
    showsPrec d (ParseDone rest) = showParen True $ showString "ParseDone " . showsPrec d rest

-- | Run streaming parser with initial input.
runParser' :: Parser a -> BS.ByteString -> ParseOutput a
runParser' parser startdata = parse $ callParse parser (tokenParser startdata)
  where
    parse (MoreData (np, ntok)) = ParseNeedData (parse . callParse np . ntok)
    parse (Failed err) = ParseFailed err
    parse (Yield v np) = ParseYield v (parse np)
    parse (Done ctx _) = ParseDone ctx

-- | Run streaming parser, immediately returns 'ParseNeedData'.
runParser :: Parser a -> ParseOutput a
runParser parser = runParser' parser BS.empty

{- | Parse a bytestring, generate lazy list of parsed values. If an error occurs, throws an exception.

>>> parseByteString (arrayOf integer) "[1,2,3,4]" :: [Int]
[1,2,3,4]
>>> parseByteString (arrayOf ("name" .: string)) "[{\"name\":\"KIWI\"}, {\"name\":\"BIRD\"}]"
["KIWI","BIRD"]
-}
parseByteString :: Parser a -> BS.ByteString -> [a]
parseByteString parser startdata = loop (runParser' parser startdata)
  where
    loop (ParseNeedData _) = parserException MissingInput
    loop (ParseDone _) = []
    loop (ParseFailed err) = parserException (InvalidInput err)
    loop (ParseYield v np) = v : loop np

-- | Parse a lazy bytestring, generate lazy list of parsed values. If an error occurs, throws an exception.
parseLazyByteString :: Parser a -> BL.ByteString -> [a]
parseLazyByteString parser input = loop input (runParser parser)
  where
    loop BL.Empty (ParseNeedData _) = parserException MissingInput
    loop (BL.Chunk dta rest) (ParseNeedData np) = loop rest (np dta)
    loop _ (ParseDone _) = []
    loop _ (ParseFailed err) = parserException (InvalidInput err)
    loop rest (ParseYield v np) = v : loop rest np

{- | Deserialise one JSON value, allowing only whitespace after it.
Return 'Nothing' for incomplete or invalid input.
-}
decode :: (AE.FromJSON a) => BL.ByteString -> Maybe a
decode bs =
    case eitherDecode bs of
        Right val -> Just val
        Left _ -> Nothing

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (AE.FromJSON a) => BL.ByteString -> Either String a
eitherDecode bs = loop bs (runParser value)
  where
    loop BL.Empty (ParseNeedData _) = Left "Not enough data."
    loop (BL.Chunk dta rest) (ParseNeedData np) = loop rest (np dta)
    loop _ (ParseDone _) = Left "Nothing parsed."
    loop _ (ParseFailed err) = Left err
    loop rest (ParseYield v next) = checkExit v next rest

    checkExit v (ParseDone srest) rest
        | BS.all isSpace srest && BL.all isSpace rest = Right v
        | otherwise = Left "Data followed by non-whitespace characters."
    checkExit _ (ParseYield _ _) _ = Left "Multiple value parses?"
    checkExit _ (ParseFailed err) _ = Left err
    checkExit _ (ParseNeedData _) BL.Empty = Left "Incomplete json structure."
    checkExit v (ParseNeedData cont) (BL.Chunk dta rest) = checkExit v (cont dta) rest

-- | Like 'decode', but on strict 'BS.ByteString'
decodeStrict :: (AE.FromJSON a) => BS.ByteString -> Maybe a
decodeStrict bs =
    case eitherDecodeStrict bs of
        Right val -> Just val
        Left _ -> Nothing

-- | Like 'eitherDecode', but on strict 'BS.ByteString'
eitherDecodeStrict :: (AE.FromJSON a) => BS.ByteString -> Either String a
eitherDecodeStrict bs =
    case runParser' value bs of
        ParseYield next v -> checkExit v next
        ParseNeedData _ -> Left "Incomplete json structure."
        ParseFailed err -> Left err
        ParseDone _ -> Left "No data found."
  where
    checkExit (ParseDone rest) v
        | BS.all isSpace rest = Right v
    checkExit _ _ = Left "Data folowed by non-whitespace characters."

-- | Applicative field selection with one pass over an object and typed private accumulators.
data Object a
    = forall state. Object
        ~(Map.Map T.Text (Parser (state -> state)))
        ~state
        ~(state -> [a])

instance Functor Object where
    fmap f (Object fields initial results) = Object fields initial (fmap f . results)

joinObjectFieldWith :: ([a] -> [b] -> [c]) -> Object a -> Object b -> Object c
joinObjectFieldWith combine (Object afields azero aresults) (Object bfields bzero bresults) =
    let fields = Map.unionWithKey duplicate (fmap (fmap updateLeft) afields) (fmap (fmap updateRight) bfields)
        duplicate key _ _ = parserException (DuplicateField key)
        updateLeft update (a, b) = let !next = update a in (next, b)
        updateRight update (a, b) = let !next = update b in (a, next)
     in fields `seq` Object fields (azero, bzero) (\(a, b) -> combine (aresults a) (bresults b))

instance Applicative Object where
    pure a = Object mempty () (const [a])
    (<*>) = joinObjectFieldWith (liftA2 ($))

instance Alternative Object where
    empty = Object mempty () (const [])
    (<|>) = joinObjectFieldWith choose
      where
        choose [] right = right
        choose left _ = left

instance Semigroup (Object a) where
    (<>) = joinObjectFieldWith (<>)

fastObjectWithKey :: T.Text -> Parser a -> Object a
fastObjectWithKey key parser = Object (Map.singleton key ((:) <$> parser)) [] reverse

fastObjectWithKeyMaybe :: T.Text -> Parser a -> Object (Maybe a)
fastObjectWithKeyMaybe key parser = Object (Map.singleton key ((:) <$> parser)) [] results
  where
    results [] = [Nothing]
    results values = Just <$> reverse values

{- | Parse all selected fields in one pass. Repeated field declarations throw 'ErrorCall'.
Repeated JSON keys retain their yielded values in input order.
-}
objectOf :: Object a -> Parser a
objectOf (Object fields initial results) =
    unFoldI $ results <$> foldI (flip ($)) initial (object' False parseKey)
  where
    parseKey key = fromMaybe ignoreVal (Map.lookup key fields)

data ParserFailure = MissingInput | InvalidInput ~String | DuplicateField ~T.Text

parserException :: ParserFailure -> a
parserException = throw . failureError

-- The throwing entry points retain ErrorCall for existing callers.
failureError :: ParserFailure -> ErrorCall
failureError failure = ErrorCall $ case failure of
    MissingInput -> "Not enough data."
    InvalidInput message -> message
    DuplicateField key -> "JStream Object - duplicate field access: " <> T.unpack key

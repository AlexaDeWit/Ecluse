{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Keep findIndex's loop counter unboxed under Cabal's default -O1.
{-# OPTIONS_GHC -fspec-constr #-}

-- | Incremental Haskell scanning with the vendored lexer's token and chunk semantics.
-- State owns immutable byte slices, with no foreign result records.
module Data.JsonStream.Lexer.Internal
    ( Cursor, Element (..), Scanned (..), start, next, feed, canFeed, stopped, remaining ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Scientific (scientific)
import Data.Word (Word8)
import Foreign.C.Types (CLong)

import Data.JsonStream.Number (numberDigitLimit, parseNumber)

-- | A lexical token independent of the public parser's leftover-input reporting.
data Element
    = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
    | StringContent !BS.ByteString | StringRaw !BS.ByteString !Bool | StringEnd
    | JValue !Aeson.Value | JInteger !CLong
    deriving (Eq, Show)

-- | The current piece and lexical state carried across pieces.
data Cursor = Cursor !BS.ByteString {-# UNPACK #-} !Int !Mode [BS.ByteString]

data Mode = Base | StringPart !Bool !Bool | NumberPart
    | LiteralPart !Literal {-# UNPACK #-} !Int | StringFinish | Broken

data Literal = TrueLiteral | FalseLiteral | NullLiteral

-- | One token, an input boundary, or a terminal failure.
data Scanned = Token !Element !Cursor | More !Cursor | Failed

-- | Begin a scan over one immutable input piece.
start :: BS.ByteString -> Cursor
start bytes = Cursor bytes 0 Base []

-- | A terminal failure with no retained input.
stopped :: Cursor
stopped = Cursor BS.empty 0 Broken []

-- | Materialise leftover input only for a caller that reports it.
remaining :: Cursor -> BS.ByteString
remaining (Cursor bytes offset _ _) = BS.drop offset bytes
{-# INLINE remaining #-}

-- | Whether input is exhausted without a pending split-string end.
canFeed :: Cursor -> Bool
canFeed (Cursor bytes offset mode _) = case mode of
    Broken -> False
    StringFinish -> False
    _ -> offset >= BS.length bytes

-- | Supply another piece. Premature input fails instead of dropping unread tokens.
feed :: Cursor -> BS.ByteString -> Cursor
feed cursor@(Cursor _ _ mode numbers) bytes
    | canFeed cursor = Cursor bytes 0 mode numbers
    | otherwise = stopped

-- | Scan one token without constructing its enclosing container.
next :: Cursor -> Scanned
next (Cursor bytes offset mode numbers) = case mode of
    Broken -> Failed
    StringFinish -> Token StringEnd (Cursor bytes offset Base numbers)
    _ | offset >= BS.length bytes -> More (Cursor bytes offset mode numbers)
    Base -> base bytes offset numbers
    StringPart continued escaped -> string bytes offset continued escaped numbers
    NumberPart -> number bytes offset True numbers
    LiteralPart literal matched -> identifier bytes offset literal matched numbers
{-# INLINE next #-}

base :: BS.ByteString -> Int -> [BS.ByteString] -> Scanned
base bytes = skipSpace
  where
    !size = BS.length bytes
    skipSpace !offset numbers
        | offset >= size = More (Cursor bytes offset Base numbers)
        | otherwise = dispatch offset (BS.index bytes offset) numbers
    dispatch offset byte numbers
        | emptyByte byte = skipSpace (offset + 1) numbers
        | byte == openBrace = emitted ObjectBegin
        | byte == closeBrace = emitted ObjectEnd
        | byte == openBracket = emitted ArrayBegin
        | byte == closeBracket = emitted ArrayEnd
        | byte == quote = next (Cursor bytes following (StringPart False False) numbers)
        | byte == trueInitial = identifierStart TrueLiteral
        | byte == falseInitial = identifierStart FalseLiteral
        | byte == nullInitial = identifierStart NullLiteral
        | numberByte byte = number bytes offset False numbers
        | otherwise = Failed
      where
        following = offset + 1
        emitted element = Token element (Cursor bytes following Base numbers)
        identifierStart literal = next (Cursor bytes following (LiteralPart literal 1) numbers)
{-# INLINE base #-}

identifier :: BS.ByteString -> Int -> Literal -> Int -> [BS.ByteString] -> Scanned
identifier bytes initial literal matched0 numbers = go initial matched0
  where
    !size = BS.length bytes
    word = literalBytes literal
    !wordSize = BS.length word
    go !offset !matched
        | offset >= size = More (Cursor bytes offset (LiteralPart literal matched) numbers)
        | matched >= wordSize =
            if emptyByte byte || byte == closeBracket || byte == closeBrace
                then Token (literalElement literal) (Cursor bytes offset Base numbers)
                else Failed
        | byte == BS.index word matched = go (offset + 1) (matched + 1)
        | otherwise = Failed
      where
        byte = BS.index bytes offset

string :: BS.ByteString -> Int -> Bool -> Bool -> [BS.ByteString] -> Scanned
string bytes initial continued escaped0 numbers = case stringStop bytes initial continued escaped0 of
    StringStop offset escapePending special
        | offset >= BS.length bytes -> Token (StringContent (piece offset)) (Cursor bytes offset (StringPart True escapePending) numbers)
        | continued -> Token (StringContent (piece offset)) (Cursor bytes (offset + 1) StringFinish numbers)
        | otherwise -> Token (StringRaw (piece offset) (not special)) (Cursor bytes (offset + 1) Base numbers)
  where
    piece offset = BS.take (offset - initial) (BS.drop initial bytes)

data StringStop = StringStop {-# UNPACK #-} !Int !Bool !Bool

stringStop :: BS.ByteString -> Int -> Bool -> Bool -> StringStop
stringStop bytes initial continued escaped0
    | escaped0 = escaped initial
    | continued = special initial
    | otherwise = plain initial
  where
    !size = BS.length bytes
    partial escapePending = StringStop size escapePending True
    plain !offset = case BS.findIndex specialByte (BS.drop offset bytes) of
        Nothing -> partial False
        Just distance ->
            let !position = offset + distance
                !byte = BS.index bytes position
             in if byte == quote
                then StringStop position False False
                else if byte == backslash
                    then escaped (position + 1)
                    else special (position + 1)
    special !offset = case BS.findIndex delimiterByte (BS.drop offset bytes) of
        Nothing -> partial False
        Just distance ->
            let !position = offset + distance
             in if BS.index bytes position == quote
                then StringStop position False True
                else escaped (position + 1)
    escaped !offset
        | offset >= size = partial True
        | otherwise = special (offset + 1)

specialByte :: Word8 -> Bool
specialByte byte = byte < printableLow || byte > printableHigh || delimiterByte byte
{-# INLINE specialByte #-}

delimiterByte :: Word8 -> Bool
delimiterByte byte = byte == quote || byte == backslash
{-# INLINE delimiterByte #-}

number :: BS.ByteString -> Int -> Bool -> [BS.ByteString] -> Scanned
number bytes initial continued numbers = go initial 0 0 False 0 continued 1
  where
    !size = BS.length bytes
    piece offset = BS.take (offset - initial) (BS.drop initial bytes)
    go !offset !computed !digits !dotted !fractionDigits !invalid !sign
        | offset >= size = partial (piece offset)
        | otherwise =
            let !byte = BS.index bytes offset
             in if not (numberByte byte)
                then complete (piece offset) offset computed fractionDigits invalid sign
                else if invalid
                    then go (offset + 1) computed digits dotted fractionDigits True sign
                    else if offset == initial && byte == minus
                        then go (offset + 1) computed digits dotted fractionDigits False (negate 1)
                        else if digitByte byte
                            then let !digits' = digits + 1
                                     !computed' = computed * decimalRadix + fromIntegral (byte - digitZero)
                                     !fractionDigits' = if dotted then fractionDigits + 1 else fractionDigits
                                  in go (offset + 1) computed' digits' dotted fractionDigits' (digits' > fastDigits) sign
                            else if byte == dot && not dotted
                                then go (offset + 1) computed digits True fractionDigits False sign
                                else go (offset + 1) computed digits dotted fractionDigits True sign
    partial part
        | not continued = More (Cursor bytes size NumberPart [part])
        | sum (map BS.length numbers) > numberDigitLimit = Failed
        | otherwise = More (Cursor bytes size NumberPart (part : numbers))
    complete part offset computed fractionDigits invalid sign
        | invalid =
            let whole = if continued then BS.concat (reverse (part : numbers)) else part
             in maybe Failed (emit . JValue . Aeson.Number) (parseNumber whole)
        | fractionDigits == 0 = emit (JInteger (sign * computed))
        | otherwise = emit (JValue (Aeson.Number (scientific (fromIntegral (sign * computed)) (negate fractionDigits))))
      where
        emit element = Token element (Cursor bytes offset Base numbers)

emptyByte :: Word8 -> Bool
emptyByte byte = byte == colon || byte == comma || byte == space || (byte >= whitespaceLow && byte <= whitespaceHigh)
{-# INLINE emptyByte #-}

digitByte :: Word8 -> Bool
digitByte byte = byte >= digitZero && byte <= digitNine
{-# INLINE digitByte #-}

numberByte :: Word8 -> Bool
numberByte byte = digitByte byte || byte == minus || byte == plus || byte == dot || byte == exponentLower || byte == exponentUpper
{-# INLINE numberByte #-}

literalBytes :: Literal -> BS.ByteString
literalBytes literal = case literal of
    TrueLiteral -> trueBytes
    FalseLiteral -> falseBytes
    NullLiteral -> nullBytes

literalElement :: Literal -> Element
literalElement literal = case literal of
    TrueLiteral -> trueElement
    FalseLiteral -> falseElement
    NullLiteral -> nullElement

trueBytes, falseBytes, nullBytes :: BS.ByteString
trueBytes = "true"
falseBytes = "false"
nullBytes = "null"

trueElement, falseElement, nullElement :: Element
trueElement = JValue (Aeson.Bool True)
falseElement = JValue (Aeson.Bool False)
nullElement = JValue Aeson.Null
{-# NOINLINE trueElement #-}
{-# NOINLINE falseElement #-}
{-# NOINLINE nullElement #-}

decimalRadix :: CLong
decimalRadix = 10

fastDigits :: Int
fastDigits = length (show (maxBound :: CLong)) - 1

openBrace, closeBrace, openBracket, closeBracket, quote, backslash, colon, comma, space :: Word8
openBrace = 123
closeBrace = 125
openBracket = 91
closeBracket = 93
quote = 34
backslash = 92
colon = 58
comma = 44
space = 32

trueInitial, falseInitial, nullInitial, printableLow, printableHigh, whitespaceLow, whitespaceHigh :: Word8
trueInitial = 116
falseInitial = 102
nullInitial = 110
printableLow = 32
printableHigh = 126
whitespaceLow = 9
whitespaceHigh = 13

digitZero, digitNine, minus, plus, dot, exponentLower, exponentUpper :: Word8
digitZero = 48
digitNine = 57
minus = 45
plus = 43
dot = 46
exponentLower = 101
exponentUpper = 69

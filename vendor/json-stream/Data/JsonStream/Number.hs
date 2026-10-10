{-# LANGUAGE BangPatterns #-}
-- | The vendored lexer's numeric fallback, including its accepted noncanonical forms.
module Data.JsonStream.Number (parseNumber, numberDigitLimit) where

import Control.Monad (when)
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8 as BS
import Data.Scientific (Scientific, scientific)
import Data.Word (Word8)

-- | Bound accumulated pieces before accepting another partial number.
numberDigitLimit :: Int
numberDigitLimit = 200000

decimalRadix :: Integer
decimalRadix = 10

digitZero, digitNine :: Word8
digitZero = 48
digitNine = 57

-- | Parse the lexer's number bytes with the existing exponent and fraction rules.
parseNumber :: BS.ByteString -> Maybe Scientific
parseNumber tnumber = do
    let
      (csign, r1) = parseSign tnumber :: (Int, BS.ByteString)
      ((num, numdigits), r2) = parseDecimal r1 :: ((Integer, Int), BS.ByteString)
      ((frac, frdigits), r3) = parseFract r2 :: ((Integer, Int), BS.ByteString)
      (texp, rest) = parseE r3
    when (numdigits == 0 || not (BS.null rest)) Nothing
    let dpart = fromIntegral csign * (num * (decimalRadix ^ frdigits) + fromIntegral frac) :: Integer
        e = texp - frdigits
    return $ scientific dpart e
  where
    parseFract txt
      | BS.null txt = ((0, 0), txt)
      | BS.head txt == '.' = parseDecimal (BS.tail txt)
      | otherwise = ((0,0), txt)

    parseE txt
      | BS.null txt = (0, txt)
      | firstc == 'e' || firstc == 'E' =
              let (sign, rest) = parseSign (BS.tail txt)
                  ((dnum, _), trest) = parseDecimal rest :: ((Int, Int), BS.ByteString)
              in (dnum * sign, trest)
      | otherwise = (0, txt)
      where
        firstc = BS.head txt

    parseSign txt
      | BS.null txt = (1, txt)
      | BS.head txt == '+' = (1, BS.tail txt)
      | BS.head txt == '-' = (-1, BS.tail txt)
      | otherwise = (1, txt)

    parseDecimal txt
      | BS.null txt = ((0, 0), txt)
      | otherwise = parseNum txt (0,0)

    parseNum txt (!start, !digits)
      | BS.null txt = ((start, digits), txt)
      | dchr >= digitZero && dchr <= digitNine = parseNum (BS.tail txt) (start * fromIntegral decimalRadix + fromIntegral (dchr - digitZero), digits + 1)
      | otherwise = ((start, digits), txt)
      where
        dchr = BSW.head txt

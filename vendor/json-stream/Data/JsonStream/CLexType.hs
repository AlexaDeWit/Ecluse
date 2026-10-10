{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Legacy token tags retained for source compatibility.
module Data.JsonStream.CLexType where

import Foreign.C.Types (CInt)
import Foreign.Storable (Storable)

newtype LexResultType = LexResultType CInt deriving (Show, Eq, Storable)

resNumber, resString, resTrue, resFalse, resNull, resOpenBrace, resCloseBrace,
    resOpenBracket, resCloseBracket, resStringPartial, resNumberPartial, resNumberSmall :: LexResultType
resNumber = LexResultType 0
resString = LexResultType 1
resTrue = LexResultType 2
resFalse = LexResultType 3
resNull = LexResultType 4
resOpenBrace = LexResultType 5
resCloseBrace = LexResultType 6
resOpenBracket = LexResultType 7
resCloseBracket = LexResultType 8
resStringPartial = LexResultType 9
resNumberPartial = LexResultType 10
resNumberSmall = LexResultType 12

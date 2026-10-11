-- SPDX-FileCopyrightText: 2015 Ondrej Palkovsky
-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE StrictData #-}

-- | Lazy token continuations preserve the public parser's incremental consumption.
module Data.JsonStream.TokenParser (
    Element (..),
    TokenResult (..),
) where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import Foreign.C.Types

data Element
    = ArrayBegin
    | ArrayEnd !BS.ByteString -- Rest of the source string for correct (ParseDone <rest>)
    | ObjectBegin
    | ObjectEnd !BS.ByteString -- Rest of the source string for correct (ParseDone <rest>)
    | StringContent !BS.ByteString
    | StringRaw !BS.ByteString !Bool !BS.ByteString -- Allow raw strings to go into parser as bytestring/ isAscii
    | StringEnd !BS.ByteString
    | JValue !AE.Value
    | JInteger !CLong
    deriving (Show, Eq)

-- | Public interface for parsing JSON tokens.
data TokenResult
    = TokMoreData ~(BS.ByteString -> TokenResult)
    | -- | The continuation stays lazy until its caller consumes it.
      PartialResult ~Element ~TokenResult
    | TokFailed

instance Show TokenResult where
    show (TokMoreData _) = "TokMoreData"
    show TokFailed = "TokFailed"
    show (PartialResult el next) = "(PartialResult (" ++ show el ++ ") " ++ show next ++ ")"

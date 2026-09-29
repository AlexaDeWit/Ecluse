-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pack a whole JSON value through the production reader, and render one packed value alone.
module Ecluse.Test.Registry.Packed (
    packValue,
    renderAlone,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS

import Data.JsonStream.TokenParser (TokenResult)
import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (ArrayPieces), RenderPlan (..), Replacement, docTable, renderPlan)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape (Generic), readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (newWriter, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (testTable, walkWritingChunks)

{- | Read a value's encoding with every key and string shared, and pack it with a hole at the path. A
trailing space ends a scalar that the lexer would otherwise wait on.
-}
packValue :: [Text] -> Value -> Maybe (DocTable, Packed)
packValue hole value = case walkWritingChunks (MetadataBodyLimit (BS.length body)) setup [body] of
    Right result -> join (rightToMaybe (streamValue result))
    Left _ -> Nothing
  where
    body = toStrict (encode value) <> " "
    setup :: ST st (TokenResult -> ST st (Steps (ST st) (Maybe (DocTable, Packed))))
    setup =
        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
            readShape writer (Generic 64) Share (testTable []) element rest $ \() table _ -> do
                packed <- sealValue writer hole
                pure (Finished (Just (docTable (tableTexts table), packed)))

-- | One packed value rendered alone, as the one item of an array under the key @k@.
renderAlone :: DocTable -> Packed -> Maybe Replacement -> ByteString
renderAlone table packed substitute = renderPlan (RenderPlan mempty "k" (ArrayPieces [Piece table packed substitute]))

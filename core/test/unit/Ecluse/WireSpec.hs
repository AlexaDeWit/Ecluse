-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WireSpec (spec) where

import Test.Hspec

import Ecluse.Core.Wire (WireVocab (..), parseWire)

{- | A throwaway enum standing in for the real wire vocabularies. Its 'wireTable' is in neither
constructor nor alphabetical order, so the assertions below are about the table, not the type.
-}
data Direction
    = North
    | South
    | East
    | West
    deriving stock (Eq, Show)

instance WireVocab Direction where
    wireKind = "direction"
    wireTable =
        (East, "east")
            :| [ (West, "west")
               , (North, "north")
               , (South, "south")
               ]

spec :: Spec
spec = describe "parseWire" $ do
    it "parses each name back to its value" $ do
        parseWire "north" `shouldBe` Right North
        parseWire "south" `shouldBe` Right South
        parseWire "east" `shouldBe` Right East
        parseWire "west" `shouldBe` Right West

    it "rejects an unknown name, naming the accepted set in table order" $
        -- east, west, north, south is table order, neither constructor order (north, south, east,
        -- west) nor alphabetical. The message follows the table.
        (parseWire "up" :: Either Text Direction)
            `shouldBe` Left "unknown direction \"up\" (expected one of: east, west, north, south)"

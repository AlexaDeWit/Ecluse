-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WireSpec (spec) where

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)
import Test.Hspec

import Ecluse.Core.Wire (WireVocab (..), parseWire, renderWire)
import Ecluse.Test.WireVocab (wireRoundTrips)

{- | A throwaway enum standing in for the real wire vocabularies. Its 'wireTable' is in neither
constructor nor alphabetical order, so the assertions below are about the table, not the type.
-}
data Direction
    = North
    | South
    | East
    | West
    deriving stock (Eq, Generic, Show)

instance Universe Direction where universe = universeGeneric

instance WireVocab Direction where
    wireKind = "direction"
    wireTable =
        (East, "east")
            :| [ (West, "west")
               , (North, "north")
               , (South, "south")
               ]
    wireAliases = [(North, "nord")]

spec :: Spec
spec = do
    wireRoundTrips @Direction

    describe "parseWire" $ do
        it "parses each name back to its value" $ do
            parseWire "north" `shouldBe` Right North
            parseWire "south" `shouldBe` Right South
            parseWire "east" `shouldBe` Right East
            parseWire "west" `shouldBe` Right West

        it "parses an alias to the value its canonical name names" $
            parseWire "nord" `shouldBe` Right North

        it "rejects an unknown name, naming the table's own set in table order" $
            -- east, west, north, south is table order, neither constructor order (north, south,
            -- east, west) nor alphabetical. The alias "nord" is accepted but never offered.
            (parseWire "up" :: Either Text Direction)
                `shouldBe` Left "unknown direction \"up\" (expected one of: east, west, north, south)"

    describe "renderWire" $
        it "emits the canonical name, never an alias" $
            renderWire North `shouldBe` "north"

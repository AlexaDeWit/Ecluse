-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WireSpec (spec) where

import Prelude hiding (universe)

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)
import Test.Hspec

import Ecluse.Core.Wire (WireVocab (..), parseWire, renderWire)

{- | A throwaway enum standing in for the real wire vocabularies, exercising the
class in isolation from any one call site. Its 'wireTable' lists every constructor
(the contract 'renderWire' assumes) and is intentionally in neither constructor nor
alphabetical order, so the order-sensitive assertions below are about the /table/,
not the type.
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

spec :: Spec
spec = do
    describe "renderWire" $ do
        it "renders each value to its name in the table" $ do
            renderWire North `shouldBe` "north"
            renderWire South `shouldBe` "south"
            renderWire East `shouldBe` "east"
            renderWire West `shouldBe` "west"

        it "falls back to the first entry's name for a value the table omits" $
            -- The contract: an instance that has fallen behind its type renders the
            -- missing value as the first entry's name rather than crashing. Real
            -- instances keep complete tables, so this case is unreachable there; the
            -- deliberately partial 'Partial' instance below makes the fallback
            -- observable.
            renderWire Two `shouldBe` "one"

    describe "parseWire" $ do
        it "parses each name back to its value" $ do
            parseWire "north" `shouldBe` Right North
            parseWire "south" `shouldBe` Right South
            parseWire "east" `shouldBe` Right East
            parseWire "west" `shouldBe` Right West

        it "rejects an unknown name, naming the accepted set in table order" $
            -- The accepted set is listed in table order (east, west, north, south),
            -- which is neither constructor order (north, south, east, west) nor
            -- alphabetical: the message follows the table, the property the
            -- byte-identical migration depends on.
            (parseWire "up" :: Either Text Direction)
                `shouldBe` Left "unknown direction \"up\" (expected one of: east, west, north, south)"

    describe "round trip" $
        it "parseWire . renderWire is the identity for every value" $
            for_ universe $ \d ->
                parseWire (renderWire d) `shouldBe` Right (d :: Direction)

{- | A deliberately incomplete vocabulary: 'Two' is absent from the table, so it
exercises 'renderWire's first-entry fallback for a value the table omits.
-}
data Partial
    = One
    | Two
    deriving stock (Eq, Show)

instance WireVocab Partial where
    wireKind = "partial"
    wireTable = (One, "one") :| []

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE AllowAmbiguousTypes #-}

{- | The exhaustiveness guard for a 'WireVocab' instance.

'Ecluse.Core.Wire.renderWire' reads a value's name out of the table rather than out of a
compiler-checked case, so a constructor the table omits would render as another value's
name. Every vocabulary calls 'wireRoundTrips' to close that gap at test time.
-}
module Ecluse.Test.WireVocab (wireRoundTrips) where

import Prelude hiding (universe)

import Data.Universe.Class (Universe (..))
import Test.Hspec

import Ecluse.Core.Wire (WireVocab (..), parseWire, renderWire)

{- | Assert the vocabulary names every inhabitant distinctly and parses each name back.
Call it as @wireRoundTrips \@MyType@.
-}
wireRoundTrips :: forall a. (WireVocab a, Universe a, Eq a, Show a) => Spec
wireRoundTrips = describe (toString (wireKind @a) <> " wire vocabulary") $ do
    it "renders every value to a name that parses back to it" $
        map (parseWire . renderWire) values `shouldBe` map Right values

    it "gives each value a distinct name" $ do
        let names = map renderWire values
        length (ordNub names) `shouldBe` length names
  where
    values = universe :: [a]

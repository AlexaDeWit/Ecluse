-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native selected reads and refused full reads bound retained state under repeated records.
module Ecluse.Core.Registry.Cargo.ReaderResidencySpec (spec) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Reader (CargoRead (..), readCargoIndex)
import Ecluse.Core.Registry.Json.WalkProbe (allowance, heldReading)
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Retention (boundedRetention)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion)
import Ecluse.Test.Registry.JsonStream (testTable)
import Ecluse.Test.Support (expectRight)

-- | Keep the shared repetition counts and allowance for both native reader paths.
spec :: Spec
spec = describe "Cargo reader live bytes" $ do
    it (caseLabel "retention/cargo/selected/other-releases" "drops other releases during selected reads") $
        boundedRetention allowance (heldFor (SelectedRead (cargoTestVersion "1.0.0")) defaultLimits)
    it (caseLabel "retention/cargo/full/past-limit" "releases state after the full read reaches its version limit") $
        boundedRetention allowance (heldFor FullRead defaultLimits{maxVersionCount = 2})

heldFor :: CargoRead -> Limits -> Int -> IO Integer
heldFor mode limits count = do
    discovery <- expectRight cargoTestDiscovery
    heldReading (\bound -> readCargoIndex discovery limits cargoTestName mode bound (testTable ["vers", "cksum"])) (body count)

body :: Int -> ByteString
body count = foldMap row [0 .. count]
  where
    row index = toStrict (encode (versioned ("1.0." <> show index))) <> "\n"
    versioned version = case cargoTestRecord "1.0.0" of
        Object fields -> Object (KeyMap.insert "vers" (String version) fields)
        value -> value

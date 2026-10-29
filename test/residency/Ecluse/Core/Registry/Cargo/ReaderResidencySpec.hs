-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native selected reads and refused full reads bound retained state under repeated records.
module Ecluse.Core.Registry.Cargo.ReaderResidencySpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult (..), CargoRead (..), readCargoIndexCharged)
import Ecluse.Core.Registry.Json.WalkProbe (allowance, heldReading)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes, maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Retention (boundedRetention)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion, readCargoIndexChunks)
import Ecluse.Test.Registry.JsonStream (testTable)
import Ecluse.Test.Support (expectRight)

-- | Preserve the shared retention law and bound growing native rows by their paid workspace.
spec :: Spec
spec = describe "Cargo reader live bytes" $ do
    it (caseLabel "retention/cargo/selected/other-releases" "drops other releases during selected reads") $ do
        discovery <- expectRight cargoTestDiscovery
        for_ [2000, 16000] $ \count -> do
            streamed <- expectRight (readCargoIndexChunks discovery defaultLimits cargoTestName selected (MetadataBodyLimit (maxMetadataBytes defaultLimits)) [body count])
            result <- expectRight (streamValue streamed)
            Map.keys (infoVersions (cargoIndexInfo result)) `shouldBe` ["1.0.0"]
            fmap (map fst . cargoRecords) (snd cargoCached (cargoIndexDocument result)) `shouldBe` Just [RecordEntry 0]
        boundedRetention allowance (heldFor selected defaultLimits)
    for_ ["1.0.0", "2.0.0"] $ \version ->
        it ("keeps a growing row within its selected reservation: " <> toString version) $
            for_ [8192, 256 * 1024, 2 * 1024 * 1024] $ \size -> do
                discovery <- expectRight cargoTestDiscovery
                peak <- newIORef (0 :: Int)
                let pay cost = modifyIORef' peak (max cost)
                    large = case cargoTestRecord version of
                        Object fields -> Object (KeyMap.insert "features" (Object (KeyMap.singleton "large" (Array (fromList [String (T.replicate size "x")])))) fields)
                        value -> value
                    input = toStrict (encode large) <> "\n"
                held <- heldReading (\bound -> readCargoIndexCharged pay discovery defaultLimits cargoTestName (SelectedRead (cargoTestVersion "1.0.0")) bound (testTable [])) input
                paid <- readIORef peak
                paid `shouldSatisfy` (>= 12 * size)
                held `shouldSatisfy` (<= toInteger paid + allowance)
    it (caseLabel "retention/cargo/full/past-limit" "releases state after the full read reaches its version limit") $
        boundedRetention allowance (heldFor FullRead defaultLimits{maxVersionCount = 2})

-- The release every body's first row carries.
selected :: CargoRead
selected = SelectedRead (cargoTestVersion "1.0.0")

heldFor :: CargoRead -> Limits -> Int -> IO Integer
heldFor mode limits count = do
    discovery <- expectRight cargoTestDiscovery
    peak <- newIORef (0 :: Int)
    heldReading (\bound -> readCargoIndexCharged (modifyIORef' peak . max) discovery limits cargoTestName mode bound (testTable ["vers", "cksum"])) (body count)

body :: Int -> ByteString
body count = foldMap row [0 .. count]
  where
    row index = toStrict (encode (versioned ("1.0." <> show index))) <> "\n"
    versioned version = case cargoTestRecord "1.0.0" of
        Object fields -> Object (KeyMap.insert "vers" (String version) fields)
        value -> value

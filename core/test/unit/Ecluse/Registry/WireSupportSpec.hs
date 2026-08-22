-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.WireSupportSpec (spec) where

import Data.Aeson (Value (Number, String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    InvalidEntry (invalidKey, invalidKind, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidVersionManifest),
    PackageName,
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Registry.WireSupport (
    NameAgreement (NameAgrees, NameDisagrees),
    checkNameAgreement,
    partitionLenient,
 )

{- | Direct tests for the cross-ecosystem wire-projection helpers the npm projection builds on.
"Ecluse.Registry.Npm.ProjectSpec" covers the npm projection end to end.
-}
spec :: Spec
spec = do
    partitionLenientSpec
    checkNameAgreementSpec

partitionLenientSpec :: Spec
partitionLenientSpec = describe "partitionLenient" $ do
    it "keeps the entries that decode" $
        fst (partitionLenient InvalidVersionManifest decodeInt mixed)
            `shouldBe` Map.fromList [("1.0.0", 1), ("3.0.0", 3)]

    it "drops the undecodable entry, recording its kind, key, and raw value" $ do
        let dropped = snd (partitionLenient InvalidVersionManifest decodeInt mixed)
        map invalidKind dropped `shouldBe` [InvalidVersionManifest]
        map invalidKey dropped `shouldBe` ["2.0.0"]
        map invalidValue dropped `shouldBe` [String "nope"]

    it "lists dropped entries in ascending key order, deterministically" $
        -- "bravo" decodes. "alpha" and "charlie" do not, and must surface in that order.
        map invalidKey (snd (partitionLenient InvalidDistTag decodeInt manyBad))
            `shouldBe` ["alpha", "charlie"]

checkNameAgreementSpec :: Spec
checkNameAgreementSpec = describe "checkNameAgreement" $ do
    it "agrees when the reported name matches the request" $
        checkNameAgreement (npmName "left-pad") (npmName "left-pad") `shouldBe` NameAgrees

    it "disagrees when the reported bare name differs, carrying the reported name" $
        checkNameAgreement (npmName "left-pad") (npmName "evil-pad")
            `shouldBe` NameDisagrees "evil-pad"

    it "disagrees on a differing scope even when the bare name matches" $
        -- Ecosystem-aware equality compares the whole name, scope included, so the same
        -- bare name under a different scope is the anti-shadowing disagreement.
        checkNameAgreement (scoped "one" "x") (scoped "two" "x")
            `shouldBe` NameDisagrees "@two/x"

-- | Decode a JSON value as an 'Int', the per-entry decode the partition drives.
decodeInt :: Value -> Either String Int
decodeInt = parseEither parseJSON

-- | A raw entry map with a healthy pair and one undecodable (string) entry between them.
mixed :: Map Text Value
mixed =
    Map.fromList
        [ ("1.0.0", Number 1)
        , ("2.0.0", String "nope")
        , ("3.0.0", Number 3)
        ]

-- | A raw entry map with two undecodable entries out of key order, to pin the drop order.
manyBad :: Map Text Value
manyBad =
    Map.fromList
        [ ("charlie", String "x")
        , ("alpha", String "y")
        , ("bravo", Number 2)
        ]

-- | An unscoped npm 'PackageName'.
npmName :: Text -> PackageName
npmName = mkPackageName Npm Nothing

-- | A scoped npm 'PackageName' @\@scope\/base@.
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))

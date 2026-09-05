-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Package.InvalidEntrySpec (spec) where

import Data.Aeson (Value (Array, Number, String), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Ecluse.Core.Package.InvalidEntry (
    InvalidEntry (invalidKey, invalidReason, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidIndexFile, InvalidVersionListing, InvalidVersionManifest),
    dropCountsByKind,
    mkInvalidEntry,
    renderInvalidEntryKind,
 )

{- | The one builder for a dropped entry. The record reaches an operator log line, so a
credential an upstream wrote into a URL must not survive into it.
-}
spec :: Spec
spec = do
    redactionSpec
    passthroughSpec
    bucketingSpec

redactionSpec :: Spec
redactionSpec = describe "mkInvalidEntry (URL redaction)" $ do
    it "reduces a credentialed URL nested in the value to its authority" $
        invalidValue (entryWith credentialedManifest)
            `shouldBe` object
                [ "version" .= String "1.0.0"
                , "dist" .= object ["tarball" .= String "registry.npmjs.org:443"]
                ]

    it "leaves neither userinfo nor a signed query string in the rendered entry" $ do
        let rendered = show (entryWith credentialedManifest) :: Text
        rendered `shouldSatisfy` (not . T.isInfixOf "hunter2")
        rendered `shouldSatisfy` (not . T.isInfixOf "deploy")
        rendered `shouldSatisfy` (not . T.isInfixOf "sig=abc")

    it "reduces a URL inside an array element" $
        invalidValue (entryWith (Array (V.fromList [String credentialedUrl])))
            `shouldBe` Array (V.fromList [String "registry.npmjs.org:443"])

    it "reduces a URL the caller passed as the key" $
        invalidKey (mkInvalidEntry InvalidVersionManifest credentialedUrl (Number 1) "reason")
            `shouldBe` "registry.npmjs.org:443"

passthroughSpec :: Spec
passthroughSpec = describe "mkInvalidEntry (non-URL values)" $ do
    it "records a value carrying no scheme separator verbatim" $
        invalidValue (entryWith (String "not-a-date")) `shouldBe` String "not-a-date"

    it "records a non-string value verbatim" $
        invalidValue (entryWith (Number 5)) `shouldBe` Number 5

    it "records the reason verbatim" $
        invalidReason (entryWith (Number 5)) `shouldBe` "expected an object"

{- | The bucketing the neutral serve path reads. It keys on the arm's label, so a second
ecosystem's kinds reach the operator log without a case in the shared path.
-}
bucketingSpec :: Spec
bucketingSpec = describe "dropCountsByKind" $ do
    it "counts each kind under its own label" $
        dropCountsByKind (map dropOf [InvalidVersionManifest, InvalidDistTag, InvalidVersionManifest])
            `shouldBe` Map.fromList [("dist-tag", 1), ("version-manifest", 2)]

    it "buckets a second ecosystem's kinds the same way, with no npm bucket" $
        dropCountsByKind (map dropOf [InvalidIndexFile, InvalidVersionListing, InvalidIndexFile])
            `shouldBe` Map.fromList [("index-file", 2), ("version-listing", 1)]

    it "reports no buckets for a document that dropped nothing" $
        dropCountsByKind [] `shouldBe` Map.empty

    it "gives every kind a distinct label" $
        let kinds = [InvalidVersionManifest, InvalidDistTag, InvalidIndexFile, InvalidVersionListing]
         in length (ordNub (map renderInvalidEntryKind kinds)) `shouldBe` length kinds

-- | Record a drop of the given kind, holding the key, value, and reason fixed.
dropOf :: InvalidEntryKind -> InvalidEntry
dropOf kind = mkInvalidEntry kind "2.0.0" (Number 1) "reason"

-- | Record a drop of the given value, holding the kind, key, and reason fixed.
entryWith :: Value -> InvalidEntry
entryWith value = mkInvalidEntry InvalidVersionManifest "2.0.0" value "expected an object"

{- | A dropped npm version manifest whose @dist.tarball@ carries a credential in its
userinfo and a signature in its query string, as a private index can serve.
-}
credentialedManifest :: Value
credentialedManifest =
    object
        [ "version" .= String "1.0.0"
        , "dist" .= object ["tarball" .= String credentialedUrl]
        ]

credentialedUrl :: Text
credentialedUrl = "https://deploy:hunter2@registry.npmjs.org/thing/-/thing-1.0.0.tgz?sig=abc"

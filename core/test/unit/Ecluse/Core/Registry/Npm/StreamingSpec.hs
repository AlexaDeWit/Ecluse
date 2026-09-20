-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Required npm metadata survives selective extraction while unknown fields never enter the result.
module Ecluse.Core.Registry.Npm.StreamingSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (VersionRead (vrVersion))
import Ecluse.Core.Registry.Npm.Metadata (selectNpmVersionDoc)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)

import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Registry.Npm.Streaming
import Ecluse.Core.Registry.Npm.StreamingProjection (collectField, emptyProjection, finishProjection)
import Ecluse.Core.Registry.Publish (PublishPlan (..))
import Ecluse.Core.Registry.WireSupport (Projection (Projected))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes), defaultLimits, maxNestingDepth)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Corpus (corpusPackages, cpPackage, cpPath)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue, parseVersionList)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "npmFields" $ do
    forM_ corpusPackages $ \package ->
        it ("preserves the policy projection of the complete capture " <> cpPath package) $ do
            bytes <- readFileBS (cpPath package)
            original <- expectRight (eitherDecodeStrict bytes)
            expected <- expectRight (parsePackageInfoFromValue (cpPackage package) original)
            let limits = defaultLimits{maxMetadataBytes = BS.length bytes}
            (actual, _) <- expectRight (projectNpmManifest limits (cpPackage package) bytes)
            Projected actual `shouldBe` expected

    it "preserves installation maps and skips unknown top-level, release and publisher fields" $ do
        (_, compact) <- expectRight (projectNpmManifest defaultLimits name body)
        lookupField "unknown" compact `shouldBe` Nothing
        let selected = lookupField "versions" compact >>= lookupField "1.0.0"
        fmap (lookupField "dependencies") selected `shouldBe` Just (Just (object ["dep" .= ("^2" :: Text)]))
        fmap (lookupField "typesVersions") selected `shouldBe` Just (Just typesVersions)
        fmap (lookupField "unknown") selected `shouldBe` Just Nothing
        (selected >>= lookupField "_npmUser" >>= lookupField "unknown") `shouldBe` Nothing
        (selected >>= lookupField "author") `shouldBe` Just (String "See https://registry.npmjs.org/thing")

    it "mirrors the same supported fields and source author pointer" $ do
        (_, compact) <- expectRight (projectNpmManifest defaultLimits name body)
        let version = mkVersion Npm "1.0.0"
        selected <- maybe (fail "missing selected metadata") pure (selectNpmVersionDoc version (fst npmCached compact))
        mirroredBytes <- expectRight (npmPublishDocument name (PublishPlan version version selected) "thing-1.0.0.tgz" Nothing Nothing "tarball bytes")
        mirrored <- expectRight (eitherDecodeStrict mirroredBytes)
        let releaseDoc = lookupField "versions" mirrored >>= lookupField "1.0.0"
        (releaseDoc >>= lookupField "author") `shouldBe` Just (String "See https://registry.npmjs.org/thing")
        (releaseDoc >>= lookupField "_hasShrinkwrap") `shouldBe` Just (Bool True)
        (releaseDoc >>= lookupField "dependencies") `shouldBe` Just (object ["dep" .= ("^2" :: Text)])
        (releaseDoc >>= lookupField "unknown") `shouldBe` Nothing

    it "joins timestamps and tags when they precede versions across one-byte chunks" $ do
        streamed <-
            expectRight
                ( parseJsonChunks
                    (MetadataBodyLimit (BS.length body))
                    (npmFields (maxNestingDepth defaultLimits) FullRead)
                    (collectField defaultLimits name)
                    emptyProjection
                    (map BS.singleton (BS.unpack body))
                )
        projected <- expectRight (streamValue streamed)
        (info, _) <- expectRight (finishProjection defaultLimits name "See source" projected)
        Map.keys (infoVersions info) `shouldBe` ["1.0.0"]

    it "retains no sibling release objects on selected reads" $ do
        streamed <-
            expectRight
                ( parseJsonChunks
                    (MetadataBodyLimit (BS.length body))
                    (npmFields (maxNestingDepth defaultLimits) (SelectedRead "absent"))
                    (collectField defaultLimits name)
                    emptyProjection
                    [body]
                )
        projected <- expectRight (streamValue streamed)
        (_, compact) <- expectRight (finishProjection defaultLimits name "See source" projected)
        lookupField "versions" compact `shouldBe` Just (Object mempty)

    forM_ [Array mempty, object ["install" .= ([] :: [Value])]] $ \scripts ->
        it ("drops invalid script containers in full, selected and inventory reads: " <> show scripts) $ do
            let malformed = case release of
                    Object fields -> Object (KeyMap.insert "scripts" scripts fields)
                    other -> other
                raw = toStrict (encode (object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= malformed]]))
            (info, _) <- expectRight (projectNpmManifest defaultLimits name raw)
            infoVersions info `shouldSatisfy` Map.null
            selected <- expectRight (projectNpmVersion defaultLimits name (mkVersion Npm "1.0.0") raw)
            vrVersion selected `shouldBe` Nothing
            parseVersionList (RegistryResponse 200 (BS.length raw) raw) `shouldBe` Right []

    it "excludes entries with unusable discriminators from version lists" $ do
        let versions =
                object
                    [ "1.0.0" .= release
                    , "2.0.0" .= object ["name" .= ("thing" :: Text)]
                    , "3.0.0" .= invalidScripts
                    ]
            raw = toStrict (encode (object ["versions" .= versions]))
        fmap (map renderVersion) (parseVersionList (RegistryResponse 200 (BS.length raw) raw)) `shouldBe` Right ["1.0.0"]

name :: PackageName
name = unscopedNpm "thing"

body :: ByteString
body =
    toStrict
        ( encode
            ( object
                [ "time" .= object ["1.0.0" .= ("2020-01-01T00:00:00Z" :: Text)]
                , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
                , "versions" .= object ["1.0.0" .= release]
                , "name" .= ("thing" :: Text)
                , "unknown" .= T.replicate 65536 "x"
                ]
            )
        )

release :: Value
release =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= ("1.0.0" :: Text)
        , "dist" .= object ["tarball" .= ("https://registry.npmjs.org/thing/-/thing-1.0.0.tgz" :: Text)]
        , "dependencies" .= object ["dep" .= ("^2" :: Text)]
        , "_hasShrinkwrap" .= Bool True
        , "acceptDependencies" .= object ["dep" .= ("^3" :: Text)]
        , "typesVersions" .= typesVersions
        , "_npmUser" .= object ["name" .= ("publisher" :: Text), "unknown" .= T.replicate 65536 "x"]
        , "unknown" .= T.replicate 65536 "x"
        ]

typesVersions :: Value
typesVersions = object [">=4" .= object ["*" .= (["ts4/*"] :: [Text])]]

invalidScripts :: Value
invalidScripts = case release of
    Object fields -> Object (KeyMap.insert "scripts" (object ["install" .= (3 :: Int)]) fields)
    other -> other

lookupField :: Text -> Value -> Maybe Value
lookupField key = \case
    Object fields -> KeyMap.lookup (Key.fromText key) fields
    _ -> Nothing

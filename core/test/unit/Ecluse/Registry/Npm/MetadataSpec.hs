-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Npm.MetadataSpec (spec) where

import Data.Aeson (Value (Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo (infoName, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested, TooManyVersions),
    Limits (maxNestingDepth, maxVersionCount),
    defaultLimits,
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unscopedNpm, validSha1, validSha512Sri)

{- | Pure-projection tests for the npm full-manifest read, pinning the 'MetadataError' each
failure maps to. 'Ecluse.Core.Registry.Npm.Metadata.fetchNpmManifest' enforces the body-size
bound over the HTTP body, so the data-plane tests cover that instead.
-}
spec :: Spec
spec = do
    projectNpmManifestSpec
    projectNpmVersionSpec

projectNpmManifestSpec :: Spec
projectNpmManifestSpec = describe "projectNpmManifest" $ do
    it "projects a well-formed packument into the manifest paired with its raw document" $
        case projectNpmManifest defaultLimits (unscopedNpm "is-odd") (manifestBytes "is-odd" ["3.0.1"]) of
            Right (info, raw) -> do
                renderPackageName (infoName info) `shouldBe` "is-odd"
                Map.keys (infoVersions info) `shouldBe` ["3.0.1"]
                -- The raw document is the decoded bytes, kept so the served surface
                -- stays coherent with the typed view it came from.
                raw `shouldSatisfy` isObject
            other -> expectationFailure ("expected a projection, got: " <> show other)

    it "reports an undecodable body" $
        projectNpmManifest defaultLimits (unscopedNpm "is-odd") "{not json"
            `shouldBe` Left MetadataUndecodable

    it "reports an absent top-level name as undecodable" $
        projectNpmManifest defaultLimits (unscopedNpm "is-odd") (BL.toStrict (encode (object ["versions" .= object []])))
            `shouldBe` Left MetadataUndecodable

    it "reports a self-reported different name as a name mismatch (the anti-shadowing distinction)" $
        projectNpmManifest defaultLimits (unscopedNpm "is-odd") (manifestBytes "is-even" ["1.0.0"])
            `shouldBe` Left (MetadataNameMismatch "is-even")

    it "reports a version-count breach as a bound breach" $
        projectNpmManifest (defaultLimits{maxVersionCount = 1}) (unscopedNpm "is-odd") (manifestBytes "is-odd" ["1.0.0", "2.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reports a nesting-depth breach as a bound breach" $
        projectNpmManifest (defaultLimits{maxNestingDepth = 2}) (unscopedNpm "is-odd") (manifestBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

{- | Parity tests for the selective single-version decode. 'projectNpmVersion' must yield the
identical 'PackageDetails' that 'projectNpmManifest' plus a version lookup yields, for every
version of a rich packument, and must pin the same 'MetadataError' taxonomy.
-}
projectNpmVersionSpec :: Spec
projectNpmVersionSpec = describe "projectNpmVersion" $ do
    it "matches the full projection over a real multi-version packument (express, 288 versions)" $ do
        body <- readFileBS "core/test/unit/fixtures/npm/express.full.json"
        case projectNpmManifest defaultLimits (unscopedNpm "express") body of
            Right (info, _raw) -> do
                let keys = Map.keys (infoVersions info)
                    n = length keys
                    -- A spread of versions across the packument (first, quartiles, last).
                    sample = mapMaybe (keys !!?) (ordNub [0, n `div` 4, n `div` 2, (3 * n) `div` 4, n - 1])
                length sample `shouldSatisfy` (> 0)
                forM_ sample $ \v ->
                    projectNpmVersion defaultLimits (unscopedNpm "express") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the express fixture did not project: " <> show err)

    it "yields the PackageDetails identical to a full projection + lookup, for every version" $ do
        let versions = ["1.0.0", "2.1.3", "10.0.0-beta.1"]
            body = richPackumentBytes "is-odd" versions
        case projectNpmManifest defaultLimits (unscopedNpm "is-odd") body of
            Right (info, _raw) ->
                forM_ versions $ \v ->
                    projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the rich fixture did not project: " <> show err)

    it "reports a version absent from a sound packument as a forwarded miss (Right Nothing)" $
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "9.9.9") (richPackumentBytes "is-odd" ["1.0.0"])
            `shouldBe` Right Nothing

    it "drops a malformed requested-version object as a forwarded miss (Right Nothing), as the full path would" $ do
        let body = BL.toStrict . encode $ object ["name" .= ("is-odd" :: Text), "versions" .= object ["1.0.0" .= object ["name" .= ("is-odd" :: Text)]]]
        -- The 1.0.0 manifest has no @dist@, so the projection drops it. That is a
        -- genuine absence both the full and the selective path reach.
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` Right Nothing

    it "reports an undecodable body" $
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") "{not json"
            `shouldBe` Left MetadataUndecodable

    it "reports trailing non-whitespace after the document as undecodable (the end-of-input check)" $
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0"] <> " trailing")
            `shouldBe` Left MetadataUndecodable

    it "reports an absent top-level name as undecodable" $
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (BL.toStrict (encode (object ["versions" .= object []])))
            `shouldBe` Left MetadataUndecodable

    it "reports a self-reported different name as a name mismatch (the anti-shadowing distinction)" $
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-even" ["1.0.0"])
            `shouldBe` Left (MetadataNameMismatch "is-even")

    it "reports a version-count breach as a bound breach" $
        projectNpmVersion (defaultLimits{maxVersionCount = 1}) (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0", "2.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reports a nesting-depth breach as a bound breach" $
        projectNpmVersion (defaultLimits{maxNestingDepth = 2}) (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

    duplicateKeyParity

{- | A hostile upstream can repeat a top-level key. The @aeson@ whole-document decode keeps the
first occurrence, so 'projectNpmVersion' must too: a last-wins walk would serve a later
duplicate's manifest, revive an absent version, or shadow the validated @name@.
-}
duplicateKeyParity :: Spec
duplicateKeyParity = describe "duplicate top-level keys resolve first-occurrence-wins, matching the whole-document decode" $ do
    it "counts only the first versions object, not the sum across duplicate versions keys" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    , ("versions", object ["2.0.0" .= versionObject "is-odd" "2.0.0", "3.0.0" .= versionObject "is-odd" "3.0.0"])
                    ]
            limits = defaultLimits{maxVersionCount = 1}
        projectNpmVersion limits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome limits (unscopedNpm "is-odd") "1.0.0" body

    it "serves the first versions object's manifest, not a later duplicate's" $ do
        let firstManifest = distTarballObject "is-odd" "1.0.0" "https://example.test/is-odd-1.0.0-first.tgz"
            secondManifest = distTarballObject "is-odd" "1.0.0" "https://example.test/is-odd-1.0.0-second.tgz"
            body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object ["1.0.0" .= firstManifest])
                    , ("versions", object ["1.0.0" .= secondManifest])
                    ]
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "is-odd") "1.0.0" body

    it "treats a version absent from the first versions object as absent, ignoring a later duplicate" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object [])
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    ]
        projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "is-odd") "1.0.0" body

    it "validates the first top-level name, not a later duplicate (anti-shadowing)" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("name", String "evil")
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    ]
        projectNpmVersion defaultLimits (unscopedNpm "evil") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "evil") "1.0.0" body

{- | A minimal packument body self-reporting @name@ and carrying each given version
with a @dist.tarball@ (the field a version must have to project).
-}
manifestBytes :: Text -> [Text] -> ByteString
manifestBytes name versions =
    BL.toStrict . encode $
        object
            [ "name" .= name
            , "dist-tags" .= object ["latest" .= latestOf versions]
            , "versions" .= object [Key.fromText v .= versionObject name v | v <- versions]
            ]

-- | The @latest@ dist-tag value: the first listed version, or a placeholder when none.
latestOf :: [Text] -> Text
latestOf = \case
    (v : _) -> v
    [] -> "0.0.0"

-- | A minimal version manifest carrying the @dist.tarball@ required to project.
versionObject :: Text -> Text -> Value
versionObject name v =
    object
        [ "name" .= name
        , "version" .= v
        , "dist" .= object ["tarball" .= ("https://example.test/" <> name <> "-" <> v <> ".tgz")]
        ]

isObject :: Value -> Bool
isObject = \case
    Object _ -> True
    _ -> False

{- | A rich multi-field packument, so the parity test exercises every 'PackageDetails' field.
The @time@ map carries a distinct stamp per version, which populates 'pkgPublishedAt'.
-}
richPackumentBytes :: Text -> [Text] -> ByteString
richPackumentBytes nm versions =
    BL.toStrict . encode $
        object
            [ "name" .= nm
            , "dist-tags" .= object ["latest" .= latestOf versions]
            , "versions" .= object [Key.fromText v .= richVersionObject nm v | v <- versions]
            , "time"
                .= object
                    ( ("created" .= ("2009-01-01T00:00:00.000Z" :: Text))
                        : ("modified" .= ("2030-01-01T00:00:00.000Z" :: Text))
                        : [Key.fromText v .= (stampFor i :: Text) | (i, v) <- zip [0 :: Int ..] versions]
                    )
            ]
  where
    stampFor :: Int -> Text
    stampFor i = "20" <> show (10 + i) <> "-03-14T15:09:26.000Z"

-- | A version manifest carrying every rule-\/serve-decisive field, for the parity test.
richVersionObject :: Text -> Text -> Value
richVersionObject nm v =
    object
        [ "name" .= nm
        , "version" .= v
        , "dist"
            .= object
                [ "tarball" .= ("https://example.test/" <> nm <> "-" <> v <> ".tgz")
                , "integrity" .= validSha512Sri
                , "shasum" .= validSha1
                ]
        , "dependencies" .= object ["left-pad" .= ("^1.0.0" :: Text), "lodash" .= ("^4.17.0" :: Text)]
        , "devDependencies" .= object ["jest" .= ("^29.0.0" :: Text)]
        , "scripts" .= object ["postinstall" .= ("node ./build.js" :: Text)]
        , "license" .= ("MIT" :: Text)
        , "maintainers" .= [object ["name" .= ("alice" :: Text), "email" .= ("alice@example.test" :: Text)]]
        , "_npmUser" .= object ["name" .= ("bob" :: Text)]
        ]

{- | A minimal version manifest with an explicit @dist.tarball@, so two manifests for the
same version can be told apart.
-}
distTarballObject :: Text -> Text -> Text -> Value
distTarballObject name v tarball =
    object
        [ "name" .= name
        , "version" .= v
        , "dist" .= object ["tarball" .= tarball]
        ]

{- | The outcome the whole-document path reaches for one version. 'projectNpmVersion' must match
it, duplicate-key documents included.
-}
fullVersionOutcome :: Limits -> PackageName -> Text -> ByteString -> Either MetadataError (Maybe PackageDetails)
fullVersionOutcome limits name v body =
    (\(info, _raw) -> Map.lookup v (infoVersions info)) <$> projectNpmManifest limits name body

{- | Serialise raw packument bytes from top-level members in order, preserving duplicate keys.
The @aeson@ 'encode' cannot emit a repeated key, because its @KeyMap@ de-duplicates.
-}
rawObject :: [(Text, Value)] -> ByteString
rawObject members =
    "{" <> BS.intercalate "," [BL.toStrict (encode k) <> ":" <> BL.toStrict (encode v) | (k, v) <- members] <> "}"

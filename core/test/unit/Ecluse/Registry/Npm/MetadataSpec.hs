module Ecluse.Registry.Npm.MetadataSpec (spec) where

import Data.Aeson (Value (Object), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artUrl),
    InvalidEntry (invalidKind),
    InvalidEntryKind (InvalidVersionManifest),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoInvalidEntries, infoName, infoVersions),
    PackageName,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)
import Ecluse.Core.Registry.Npm.Project (enforceTarballScheme)
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested, TooManyVersions),
    Limits (maxNestingDepth, maxVersionCount),
    defaultLimits,
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (validSha1, validSha512Sri)

{- | Pure-projection tests for the npm full-manifest read primitive. They pin the
'MetadataError' each failure maps to -- the distinctions the serve path renders as
distinct responses -- and that a well-formed packument projects to the typed manifest
paired with the raw document the serve path re-serializes. (The body-size bound is
enforced over the HTTP body in 'Ecluse.Core.Registry.Npm.Metadata.fetchNpmManifest',
not in this pure step, so it is exercised by the data-plane tests instead.)
-}
spec :: Spec
spec = do
    projectNpmManifestSpec
    projectNpmVersionSpec
    enforceTarballSchemeSpec

{- | The https-only @dist.tarball@ normalisation applied as a projection post-step: a
same-host @http@ tarball is upgraded to https, a foreign-host @http@ tarball is dropped
and recorded (the #486 drop-and-record contract), and a non-https (test/dev loopback)
upstream leaves tarballs untouched.
-}
enforceTarballSchemeSpec :: Spec
enforceTarballSchemeSpec = describe "enforceTarballScheme (https-only dist.tarball normalisation)" $ do
    let name = unscoped "thing"
        body tarball =
            BL.toStrict . encode $
                object
                    [ "name" .= ("thing" :: Text)
                    , "versions"
                        .= object
                            [ "1.0.0"
                                .= object
                                    [ "name" .= ("thing" :: Text)
                                    , "version" .= ("1.0.0" :: Text)
                                    , "dist" .= object ["tarball" .= (tarball :: Text)]
                                    ]
                            ]
                    ]
        projected upstream tarball = enforceTarballScheme upstream . fst <$> projectNpmManifest defaultLimits name (body tarball)
        tarballOf info = (\(art :| _) -> artUrl art) . pkgArtifacts <$> Map.lookup "1.0.0" (infoVersions info)

    it "upgrades a same-host http tarball to https (https upstream)" $
        case projected "https://registry.npmjs.org" "http://registry.npmjs.org/thing/-/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

    it "keeps an https tarball unchanged (https upstream)" $
        case projected "https://registry.npmjs.org" "https://cdn.example.net/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "https://cdn.example.net/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

    it "drops a foreign-host http tarball and records it (https upstream)" $
        case projected "https://registry.npmjs.org" "http://evil.example.test/thing-1.0.0.tgz" of
            Right info -> do
                Map.lookup "1.0.0" (infoVersions info) `shouldBe` Nothing
                map invalidKind (infoInvalidEntries info) `shouldBe` [InvalidVersionManifest]
            Left err -> expectationFailure ("did not project: " <> show err)

    it "leaves tarballs untouched for a non-https (loopback) upstream" $
        case projected "http://127.0.0.1:8080" "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

projectNpmManifestSpec :: Spec
projectNpmManifestSpec = describe "projectNpmManifest" $ do
    it "projects a well-formed packument into the manifest paired with its raw document" $
        case projectNpmManifest defaultLimits (unscoped "is-odd") (manifestBytes "is-odd" ["3.0.1"]) of
            Right (info, raw) -> do
                renderPackageName (infoName info) `shouldBe` "is-odd"
                Map.keys (infoVersions info) `shouldBe` ["3.0.1"]
                -- The raw document is the decoded bytes, kept so the served surface
                -- stays coherent with the typed view it was projected from.
                raw `shouldSatisfy` isObject
            other -> expectationFailure ("expected a projection, got: " <> show other)

    it "reports an undecodable body" $
        projectNpmManifest defaultLimits (unscoped "is-odd") "{not json"
            `shouldBe` Left MetadataUndecodable

    it "reports an absent top-level name as undecodable" $
        projectNpmManifest defaultLimits (unscoped "is-odd") (BL.toStrict (encode (object ["versions" .= object []])))
            `shouldBe` Left MetadataUndecodable

    it "reports a self-reported different name as a name mismatch (the anti-shadowing distinction)" $
        projectNpmManifest defaultLimits (unscoped "is-odd") (manifestBytes "is-even" ["1.0.0"])
            `shouldBe` Left (MetadataNameMismatch "is-even")

    it "reports a version-count breach as a bound breach" $
        projectNpmManifest (defaultLimits{maxVersionCount = 1}) (unscoped "is-odd") (manifestBytes "is-odd" ["1.0.0", "2.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reports a nesting-depth breach as a bound breach" $
        projectNpmManifest (defaultLimits{maxNestingDepth = 2}) (unscoped "is-odd") (manifestBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

{- | Parity and taxonomy tests for the selective single-version decode. The headline is
__parity__: 'projectNpmVersion' must yield the byte-for-byte identical 'PackageDetails' a
full 'projectNpmManifest' followed by a version lookup would -- over a rich, multi-field
packument (integrity digests, dependencies, install scripts, publish times), for /every/
version. The rest pin the 'MetadataError' taxonomy and the present\/absent contract.
-}
projectNpmVersionSpec :: Spec
projectNpmVersionSpec = describe "projectNpmVersion" $ do
    it "matches the full projection over a real multi-version packument (express, 288 versions)" $ do
        body <- readFileBS "core/test/unit/fixtures/npm/express.full.json"
        case projectNpmManifest defaultLimits (unscoped "express") body of
            Right (info, _raw) -> do
                let keys = Map.keys (infoVersions info)
                    n = length keys
                    -- A spread of versions across the packument (first, quartiles, last).
                    sample = mapMaybe (keys !!?) (ordNub [0, n `div` 4, n `div` 2, (3 * n) `div` 4, n - 1])
                length sample `shouldSatisfy` (> 0)
                forM_ sample $ \v ->
                    projectNpmVersion defaultLimits (unscoped "express") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the express fixture did not project: " <> show err)

    it "yields the PackageDetails identical to a full projection + lookup, for every version" $ do
        let versions = ["1.0.0", "2.1.3", "10.0.0-beta.1"]
            body = richPackumentBytes "is-odd" versions
        case projectNpmManifest defaultLimits (unscoped "is-odd") body of
            Right (info, _raw) ->
                forM_ versions $ \v ->
                    projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the rich fixture did not project: " <> show err)

    it "reports a version absent from a sound packument as a forwarded miss (Right Nothing)" $
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "9.9.9") (richPackumentBytes "is-odd" ["1.0.0"])
            `shouldBe` Right Nothing

    it "drops a malformed requested-version object as a forwarded miss (Right Nothing), as the full path would" $ do
        let body = BL.toStrict . encode $ object ["name" .= ("is-odd" :: Text), "versions" .= object ["1.0.0" .= object ["name" .= ("is-odd" :: Text)]]]
        -- The 1.0.0 manifest has no @dist@, so it is unprojectable and dropped -- a genuine
        -- absence both the full and the selective path reach.
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` Right Nothing

    it "reports an undecodable body" $
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "1.0.0") "{not json"
            `shouldBe` Left MetadataUndecodable

    it "reports trailing non-whitespace after the document as undecodable (the end-of-input check)" $
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0"] <> " trailing")
            `shouldBe` Left MetadataUndecodable

    it "reports an absent top-level name as undecodable" $
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "1.0.0") (BL.toStrict (encode (object ["versions" .= object []])))
            `shouldBe` Left MetadataUndecodable

    it "reports a self-reported different name as a name mismatch (the anti-shadowing distinction)" $
        projectNpmVersion defaultLimits (unscoped "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-even" ["1.0.0"])
            `shouldBe` Left (MetadataNameMismatch "is-even")

    it "reports a version-count breach as a bound breach" $
        projectNpmVersion (defaultLimits{maxVersionCount = 1}) (unscoped "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0", "2.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reports a nesting-depth breach as a bound breach" $
        projectNpmVersion (defaultLimits{maxNestingDepth = 2}) (unscoped "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

-- | An unscoped npm 'PackageName'.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

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

{- | A __rich__ multi-field packument body, so the selective\/full parity test exercises
every 'PackageDetails' field: each version carries both integrity digests, a dependency
set, an install script, maintainers and a per-version publisher, and the document carries a
@time@ map (a distinct publish stamp per version, plus the @created@\/@modified@
bookkeeping keys) so 'pkgPublishedAt' is populated and keyed correctly.
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

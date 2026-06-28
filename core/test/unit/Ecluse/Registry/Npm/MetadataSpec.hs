module Ecluse.Registry.Npm.MetadataSpec (spec) where

import Data.Aeson (Value (Object), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageInfo (infoName, infoVersions),
    PackageName,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested, TooManyVersions),
    Limits (maxNestingDepth, maxVersionCount),
    defaultLimits,
 )

{- | Pure-projection tests for the npm full-manifest read primitive. They pin the
'MetadataError' each failure maps to — the distinctions the serve path renders as
distinct responses — and that a well-formed packument projects to the typed manifest
paired with the raw document the serve path re-serializes. (The body-size bound is
enforced over the HTTP body in 'Ecluse.Core.Registry.Npm.Metadata.fetchNpmManifest',
not in this pure step, so it is exercised by the data-plane tests instead.)
-}
spec :: Spec
spec = describe "projectNpmManifest" $ do
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

-- ── fixtures ──────────────────────────────────────────────────────────────────

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

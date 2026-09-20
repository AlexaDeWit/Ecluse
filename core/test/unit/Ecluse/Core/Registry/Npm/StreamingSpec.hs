-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Required npm metadata survives selective extraction while unknown fields never enter the result.
module Ecluse.Core.Registry.Npm.StreamingSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Merge (Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw), VersionRead (vrVersion))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (selectNpmVersionDoc)
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Registry.Npm.Streaming
import Ecluse.Core.Registry.Npm.StreamingProjection (collectField, emptyProjection, finishProjection)
import Ecluse.Core.Registry.Publish (PublishPlan (..))
import Ecluse.Core.Registry.WireSupport (Projection (Projected))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes), defaultLimits, maxNestingDepth)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Corpus (corpusPackages, cpPackage, cpPath)
import Ecluse.Test.Json (fieldAt, withKeys)
import Ecluse.Test.Package (unscopedNpm, validSha1, validSha512Sri)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue, parseVersionList)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

-- | Verify the supported representation and policy projection against independent expectations.
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
        fieldAt "unknown" compact `shouldBe` Nothing
        let selected = fieldAt "versions" compact >>= fieldAt "1.0.0"
        fmap (fieldAt "dependencies") selected `shouldBe` Just (Just (object ["dep" .= ("^2" :: Text)]))
        fmap (fieldAt "typesVersions") selected `shouldBe` Just (Just typesVersions)
        fmap (fieldAt "unknown") selected `shouldBe` Just Nothing
        (selected >>= fieldAt "_npmUser" >>= fieldAt "unknown") `shouldBe` Nothing
        (selected >>= fieldAt "author") `shouldBe` Just (String "See https://registry.npmjs.org/thing")

    it "mirrors the same supported fields and source author pointer" $ do
        (_, compact) <- expectRight (projectNpmManifest defaultLimits name body)
        let version = mkVersion Npm "1.0.0"
        selected <- maybe (fail "missing selected metadata") pure (selectNpmVersionDoc version (fst npmCached compact))
        mirroredBytes <- expectRight (npmPublishDocument name (PublishPlan version version selected) "thing-1.0.0.tgz" Nothing Nothing "tarball bytes")
        mirrored <- expectRight (eitherDecodeStrict mirroredBytes)
        let releaseDoc = fieldAt "versions" mirrored >>= fieldAt "1.0.0"
        (releaseDoc >>= fieldAt "author") `shouldBe` Just (String "See https://registry.npmjs.org/thing")
        (releaseDoc >>= fieldAt "_hasShrinkwrap") `shouldBe` Just (Bool True)
        (releaseDoc >>= fieldAt "dependencies") `shouldBe` Just (object ["dep" .= ("^2" :: Text)])
        (releaseDoc >>= fieldAt "unknown") `shouldBe` Nothing

    it "preserves the independent installation contract in full, selected, served and mirrored documents" $ do
        let source = object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= contractSource]]
            bytes = toStrict (encode source)
            version = mkVersion Npm "1.0.0"
            digest = digestOf bytes
        (info, compact) <- expectRight (projectNpmManifest defaultLimits name bytes)
        (fieldAt "versions" compact >>= fieldAt "1.0.0") `shouldBe` Just contractExpected
        selected <- expectRight (projectNpmVersion defaultLimits name version bytes)
        selectedRaw <- maybe (fail "missing selected metadata") pure (vrVersion selected >>= vdRaw)
        snd npmCached selectedRaw `shouldBe` Just contractExpected
        plan <- maybe (fail "missing merge plan") pure (mergePackuments [(GatedSource, Snapshot digest info)])
        let served = assembleMergedPackument "https://proxy.example/npm" (Map.singleton 0 (Snapshot digest compact)) plan compact
            servedDist = withKeys [("tarball", String "https://proxy.example/npm/thing/-/thing-1.0.0.tgz")] contractDist
        (fieldAt "versions" served >>= fieldAt "1.0.0") `shouldBe` Just (withKeys [("dist", servedDist)] contractExpected)
        mirroredBytes <- expectRight (npmPublishDocument name (PublishPlan version version selectedRaw) "thing-1.0.0.tgz" (Just validSha512Sri) (Just validSha1) "tarball bytes")
        mirrored <- expectRight (eitherDecodeStrict mirroredBytes)
        let mirroredDist = object ["tarball" .= ("thing-1.0.0.tgz" :: Text), "integrity" .= validSha512Sri, "shasum" .= validSha1, "fileCount" .= (2 :: Int), "unpackedSize" .= (123 :: Int)]
            mirroredExpected = case withKeys [("dist", mirroredDist)] contractExpected of
                Object fields -> Object (KeyMap.delete "_npmUser" fields)
                other -> other
        (fieldAt "versions" mirrored >>= fieldAt "1.0.0") `shouldBe` Just mirroredExpected

    forM_ [String "cli.js", object ["thing" .= ("cli.js" :: Text)]] $ \bin ->
        it ("preserves both npm bin forms: " <> show bin) $ do
            let bytes = toStrict (encode (object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= withKeys [("bin", bin)] release]]))
            (_, compact) <- expectRight (projectNpmManifest defaultLimits name bytes)
            (fieldAt "versions" compact >>= fieldAt "1.0.0" >>= fieldAt "bin") `shouldBe` Just bin

    it "preserves array workspaces on the selected path" $ do
        let workspaces = toJSON (["packages/*", "tools/*"] :: [Text])
            bytes = toStrict (encode (object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= withKeys [("workspaces", workspaces)] release]]))
        selected <- expectRight (projectNpmVersion defaultLimits name (mkVersion Npm "1.0.0") bytes)
        (vrVersion selected >>= vdRaw >>= snd npmCached >>= fieldAt "workspaces") `shouldBe` Just workspaces

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
        fieldAt "versions" compact `shouldBe` Just (Object mempty)

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

contractExpected :: Value
contractExpected = object [(Key.fromText key, expected) | (key, _, expected) <- contractFields]

contractSource :: Value
contractSource =
    withKeys
        [("unknown", object ["nested" .= ("omit" :: Text)]), ("author", object ["name" .= ("original author" :: Text)])]
        (object [(Key.fromText key, source) | (key, source, _) <- contractFields])

contractFields :: [(Text, Value, Value)]
contractFields =
    [(key, value, value) | (key, value) <- plainFields]
        <> [(key, withKeys [("unknown", String "omit")] value, value) | (key, value) <- shapedFields]
        <> [
               ( "dist"
               , withKeys
                    [ ("unknown", String "omit")
                    , ("signatures", toJSON [withKeys [("unknown", String "omit")] signature])
                    , ("attestations", withKeys [("unknown", String "omit"), ("provenance", withKeys [("unknown", String "omit")] provenance)] attestations)
                    ]
                    contractDist
               , contractDist
               )
           , ("peerDependenciesMeta", object ["peer" .= withKeys [("unknown", String "omit")] optionalPeer], object ["peer" .= optionalPeer])
           ,
               ( "devEngines"
               , withKeys
                    [ ("unknown", String "omit")
                    , ("runtime", toJSON [withKeys [("unknown", String "omit")] engine])
                    , ("packageManager", withKeys [("unknown", String "omit")] packageManager)
                    ]
                    devEngines
               , devEngines
               )
           ]
  where
    plainFields :: [(Text, Value)]
    plainFields =
        [ ("name", String "thing")
        , ("version", String "1.0.0")
        , ("author", String "See https://registry.npmjs.org/thing")
        , ("dependencies", object ["dep" .= ("^2" :: Text)])
        , ("acceptDependencies", object ["dep" .= ("^3" :: Text)])
        , ("devDependencies", object ["test-tool" .= ("^1" :: Text)])
        , ("optionalDependencies", object ["optional" .= ("^1" :: Text)])
        , ("peerDependencies", object ["peer" .= ("^4" :: Text)])
        , ("bundleDependencies", toJSON (["dep"] :: [Text]))
        , ("bundledDependencies", Bool False)
        , ("_hasShrinkwrap", Bool True)
        , ("hasInstallScript", Bool True)
        , ("scripts", object ["install" .= ("node install.js" :: Text)])
        , ("deprecated", String "use a later release")
        , ("engines", object ["node" .= (">=20" :: Text)])
        , ("engineStrict", Bool True)
        , ("os", toJSON (["linux", "darwin"] :: [Text]))
        , ("cpu", toJSON (["x64", "arm64"] :: [Text]))
        , ("libc", toJSON (["glibc"] :: [Text]))
        , ("bin", object ["thing" .= ("cli.js" :: Text)])
        , ("man", toJSON (["thing.1"] :: [Text]))
        , ("main", String "index.cjs")
        , ("module", String "index.mjs")
        , ("browser", object ["./node.js" .= False, "./index.js" .= ("./browser.js" :: Text)])
        , ("exports", object ["." .= object ["import" .= ("./index.mjs" :: Text), "require" .= ("./index.cjs" :: Text)]])
        , ("imports", object ["#internal" .= ("./internal.js" :: Text)])
        , ("type", String "module")
        , ("types", String "index.d.ts")
        , ("typings", String "legacy.d.ts")
        , ("typesVersions", typesVersions)
        , ("files", toJSON (["lib", "cli.js"] :: [Text]))
        , ("gypfile", Bool True)
        , ("preferGlobal", Bool False)
        , ("config", object ["port" .= (8080 :: Int)])
        , ("packageManager", String "npm@11.0.0")
        , ("sideEffects", toJSON (["*.css"] :: [Text]))
        ]
    shapedFields :: [(Text, Value)]
    shapedFields =
        [ ("_npmUser", object ["name" .= ("publisher" :: Text), "email" .= ("publisher@example.test" :: Text), "url" .= ("https://example.test/publisher" :: Text)])
        , ("license", object ["type" .= ("MIT" :: Text), "url" .= ("https://example.test/license" :: Text)])
        , ("directories", object [Key.fromText key .= key | key <- ["lib", "bin", "man", "doc", "example", "test"]])
        ,
            ( "publishConfig"
            , object
                [ "registry" .= ("https://registry.example.test" :: Text)
                , "tag" .= ("next" :: Text)
                , "access" .= ("public" :: Text)
                , "provenance" .= True
                , "ignore-scripts" .= True
                , "directory" .= ("dist" :: Text)
                , "linkDirectory" .= False
                , "executableFiles" .= (["cli.js"] :: [Text])
                , "main" .= ("index.cjs" :: Text)
                , "module" .= ("index.mjs" :: Text)
                , "types" .= ("index.d.ts" :: Text)
                , "typings" .= ("legacy.d.ts" :: Text)
                , "exports" .= object ["." .= ("./index.js" :: Text)]
                , "imports" .= object ["#internal" .= ("./internal.js" :: Text)]
                , "bin" .= object ["thing" .= ("cli.js" :: Text)]
                , "browser" .= object ["./node.js" .= False]
                ]
            )
        , ("workspaces", object ["packages" .= (["packages/*"] :: [Text]), "nohoist" .= (["**/dep"] :: [Text])])
        ]
    optionalPeer :: Value
    optionalPeer = object ["optional" .= True]
    engine :: Value
    engine = object ["name" .= ("node" :: Text), "version" .= (">=20" :: Text), "onFail" .= ("error" :: Text)]
    packageManager :: Value
    packageManager = object ["name" .= ("npm" :: Text), "version" .= (">=11" :: Text), "onFail" .= ("warn" :: Text)]
    devEngines :: Value
    devEngines = object ["runtime" .= [engine], "packageManager" .= packageManager, "cpu" .= object ["name" .= ("x64" :: Text)], "os" .= [object ["name" .= ("linux" :: Text)]], "libc" .= object ["name" .= ("glibc" :: Text)]]

contractDist :: Value
contractDist = object ["tarball" .= ("https://registry.npmjs.org/thing/-/thing-1.0.0.tgz" :: Text), "shasum" .= validSha1, "integrity" .= validSha512Sri, "fileCount" .= (2 :: Int), "unpackedSize" .= (123 :: Int), "signatures" .= [signature], "attestations" .= attestations]

signature :: Value
signature = object ["keyid" .= ("SHA256:key" :: Text), "sig" .= ("signature" :: Text)]

attestations :: Value
attestations = object ["url" .= ("https://registry.npmjs.org/-/npm/v1/attestations/thing@1.0.0" :: Text), "provenance" .= provenance]

provenance :: Value
provenance = object ["predicateType" .= ("https://slsa.dev/provenance/v1" :: Text)]

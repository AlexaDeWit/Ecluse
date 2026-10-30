-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which npm dependency specs a client fetches from outside the registry, and which one a release reports.
module Ecluse.Core.Registry.Npm.DependencySourceSpec (spec) where

import Data.Aeson (Value (Null, Object, String), object, toJSON, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Pair)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Registry.Npm.DependencySource

spec :: Spec
spec = do
    classificationSpec
    releaseSpec

classificationSpec :: Spec
classificationSpec = describe "externalSource" $ do
    for_ gitSpecs $ \written ->
        it ("reads " <> show written <> " as a git source") $
            externalSource written `shouldBe` Just GitSource
    for_ urlSpecs $ \written ->
        it ("reads " <> show written <> " as a remote URL") $
            externalSource written `shouldBe` Just RemoteUrl
    for_ inRegistrySpecs $ \written ->
        it ("reads " <> show written <> " as fetching nothing from outside the registry") $
            externalSource written `shouldBe` Nothing
    it "reads a spec with neither a colon nor a slash as a registry range, tag or file name" $
        hedgehog $ do
            written <- forAll (Gen.text (Range.linear 0 40) (Gen.filter (\c -> c /= ':' && c /= '/') Gen.unicode))
            externalSource written === Nothing

releaseSpec :: Spec
releaseSpec = describe "externalDependencyOf" $ do
    it "reports the dependency, its field and its kind" $
        externalDependencyOf (release ["peerDependencies" .= object ["left-pad" .= git]])
            `shouldBe` Just (ExternalDependency PeerDependencies "left-pad" GitSource)
    it "reads dependencies, optionalDependencies and peerDependencies, never devDependencies" $ do
        for_ [minBound .. maxBound] $ \field ->
            fmap edField (externalDependencyOf (release [fromString (toString (installableFieldName field)) .= object ["left-pad" .= git]])) `shouldBe` Just field
        externalDependencyOf (release ["devDependencies" .= object ["left-pad" .= git]]) `shouldBe` Nothing
    it "picks the earliest field, then the smallest name, whatever the document order" $
        externalDependencyOf
            ( release
                [ "peerDependencies" .= object ["alpha" .= git]
                , "optionalDependencies" .= object ["zeta" .= git, "beta" .= ("https://example.test/beta.tgz" :: Text)]
                , "dependencies" .= object ["safe" .= ("^1.0.0" :: Text)]
                ]
            )
            `shouldBe` Just (ExternalDependency OptionalDependencies "beta" RemoteUrl)
    it "ignores a map that is not an object and a spec that is not a string" $
        for_ [toJSON [git], String git, Null, object ["left-pad" .= (1 :: Int)], object ["left-pad" .= [git]], object ["left-pad" .= object ["url" .= git]]] $ \dependencies ->
            externalDependencyOf (release ["dependencies" .= dependencies]) `shouldBe` Nothing
    it "agrees with earlierDependency on which of two dependencies a release reports" $
        hedgehog $ do
            first' <- forAll genDependency
            second' <- forAll genDependency
            unless (edField first' == edField second' && edName first' == edName second') $
                externalDependencyOf (release (asMembers [first', second'])) === Just (earlierDependency first' second')
    it "names the dependency, its field and its kind, and never the spec" $ do
        renderExternalDependency (ExternalDependency Dependencies "left-pad" GitSource) `shouldBe` "dependency 'left-pad' in dependencies names a git source"
        renderExternalDependency (ExternalDependency OptionalDependencies "@acme/thing" RemoteUrl) `shouldBe` "dependency '@acme/thing' in optionalDependencies names a remote URL"
  where
    git = "github:acme/left-pad" :: Text

release :: [Pair] -> KeyMap Value
release members = case object members of
    Object fields -> fields
    _ -> mempty

-- Each field's dependencies as one map. The property skips a pair that shares a field and a name.
asMembers :: [ExternalDependency] -> [Pair]
asMembers dependencies =
    [ (fromString (toString (installableFieldName field)), object [(fromString (toString (edName d)), String (spellingOf (edSource d))) | d <- dependencies, edField d == field])
    | field <- ordNub (map edField dependencies)
    ]
  where
    spellingOf = \case
        GitSource -> "github:acme/left-pad"
        RemoteUrl -> "https://example.test/left-pad.tgz"

genDependency :: Gen ExternalDependency
genDependency =
    ExternalDependency
        <$> Gen.enumBounded
        <*> Gen.element ["alpha", "beta", "left-pad", "@acme/thing", "zeta"]
        <*> Gen.element [GitSource, RemoteUrl]

gitSpecs :: [Text]
gitSpecs =
    [ "git+https://github.com/acme/left-pad.git"
    , "git://github.com/acme/left-pad.git"
    , "git+ssh://git@github.com:acme/left-pad.git"
    , "git+http://example.test/left-pad.git"
    , "git+file:///opt/left-pad"
    , "git+rsync://example.test/left-pad"
    , "ssh://git@github.com/acme/left-pad.git"
    , "git@github.com:acme/left-pad.git"
    , "git@github.com/acme/left-pad"
    , "deploy@git.example.test:left-pad.git"
    , "github:acme/left-pad"
    , "gitlab:acme/left-pad"
    , "bitbucket:acme/left-pad"
    , "gist:11081aaa"
    , "sourcehut:~acme/left-pad"
    , "GitHub:acme/left-pad"
    , "acme/left-pad"
    , "acme/left-pad#v1.0.0"
    , "acme/left-pad#semver:^1.0.0"
    , "GIT+HTTPS://github.com/acme/left-pad.git"
    , " github:acme/left-pad"
    , "\tgit+https://github.com/acme/left-pad.git "
    , "git\t+https://github.com/acme/left-pad.git"
    , "user:secret@github.com:acme/left-pad"
    , " .x@github.com:acme/left-pad"
    ]

urlSpecs :: [Text]
urlSpecs = ["https://example.test/left-pad-1.0.0.tgz", "http://example.test/left-pad-1.0.0.tgz", "HTTPS://example.test/left-pad.tgz", "https://github.com/acme/left-pad"]

inRegistrySpecs :: [Text]
inRegistrySpecs =
    [ "^1.2.3"
    , "1.x"
    , "latest"
    , "*"
    , ""
    , ">=1.0.0 <2.0.0"
    , "1.0.0 - 2.0.0 || ^3"
    , "npm:left-pad@^1"
    , "NPM:left-pad@^1"
    , "npm:@acme/left-pad@1"
    , "file:../left-pad"
    , "FILE:../left-pad"
    , "./left-pad"
    , "../left-pad"
    , "/opt/left-pad"
    , "~/left-pad"
    , "C:\\left-pad"
    , "c:/left-pad"
    , "\\\\share\\left-pad"
    , "link:../left-pad"
    , "workspace:*"
    , "workspace:left-pad@*"
    , "catalog:"
    , "jsr:@std/path@^1"
    , "a/b/c"
    , "acme/"
    , "acme/left pad"
    , "left-pad-1.0.0.tgz"
    ]

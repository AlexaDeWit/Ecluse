-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache keys: the pinned rendering of each store, and the identity that keeps entries apart.
module Ecluse.Core.Server.Cache.TypesSpec (spec) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Universe.Class qualified as Universe
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, pkgEcosystem)
import Ecluse.Core.Server.Cache.Types (CacheKey, Source (Source), assembledKey, cacheKeyIdentity, fullKey, renderCacheKey, versionKey)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Package (npmVersion, pypiVersion, scopedNpm, unscopedNpm, unscopedPyPI)

spec :: Spec
spec = do
    renderingSpec
    ecosystemSpec
    identitySpec
    instanceSpec

renderingSpec :: Spec
renderingSpec = describe "the rendering of each store" $ do
    for_ pinnedKeys $ \(label, key, framed, rendering) ->
        it ("pins " <> label) $ do
            cacheKeyIdentity key `shouldBe` framed
            renderCacheKey key `shouldBe` rendering

    it "renders every key of a store at one length, under that store's namespace" $
        hedgehog $ do
            subject <- forAll genSubject
            let rendering = SBS.fromShort (renderCacheKey (keyOf subject))
                (namespace, digest) = BS8.splitAt (BS8.length (namespaceOf subject)) rendering
            namespace === namespaceOf subject
            BS8.length digest === 64
            assert (BS8.all (`BS8.elem` "0123456789abcdef") digest)

-- One key per store and ecosystem: its label, the key, its identity, and its rendering.
pinnedKeys :: [(String, CacheKey, ShortByteString, ShortByteString)]
pinnedKeys =
    [
        ( "a full npm key"
        , fullKey npmRegistry (scopedNpm "babel" "core")
        , "26:https://registry.npmjs.org3:npm5:babel11:@babel/core"
        , "ecluse:0:full:0:8fb21f04f4b99a00a5e57a8e6aa7d2f3a14ad06e4bb41dfa208c808faf538412"
        )
    ,
        ( "a full PyPI key"
        , fullKey pypiIndex (unscopedPyPI "Flask_SQLAlchemy")
        , "23:https://pypi.org/simple4:pypi-16:flask-sqlalchemy"
        , "ecluse:0:full:0:eefd13c5b5a2320d11995a2ad6a06a0ef7ab365db6173744fb17e50ea99f0715"
        )
    ,
        ( "a selected-version npm key"
        , versionKey npmRegistry (unscopedNpm "thing") (npmVersion "1.0.0")
        , "26:https://registry.npmjs.org3:npm-5:thing5:1.0.0"
        , "ecluse:0:version:0:875791437c4d4a33a63dcbc77460ecd4f6a260ee78c2ce56c5e1c3d316b96e74"
        )
    ,
        ( "a selected-version PyPI key"
        , versionKey pypiIndex (unscopedPyPI "requests") (pypiVersion "2.32.3")
        , "23:https://pypi.org/simple4:pypi-8:requests6:2.32.3"
        , "ecluse:0:version:0:4bba7e4b966b904dc623f54dd04536ad9693210dcd01d646a933dfd568f853db"
        )
    ,
        ( "an assembled key"
        , assembledKey (validatorOf "fingerprint")
        , "66:\"44863b03e9909b7100e05b02526909a346fd7455183f6619e0fe6198c89981e0\""
        , "ecluse:0:assembled:0:8c247eb182e687b42c52b404807c98e98d4ee8eccdecba091b9715d972ea6f1f"
        )
    ]

ecosystemSpec :: Spec
ecosystemSpec = describe "the ecosystem component" $
    it "frames every constructor as its own wire name" $ do
        map fst ecosystemFrames `shouldBe` Universe.universe
        for_ ecosystemFrames $ \(eco, frame) ->
            cacheKeyIdentity (fullKey (Source "https://a.example") (mkPackageName eco Nothing "thing"))
                `shouldBe` ("17:https://a.example" <> frame <> "-5:thing")

ecosystemFrames :: [(Ecosystem, ShortByteString)]
ecosystemFrames = [(Npm, "3:npm"), (PyPI, "4:pypi"), (RubyGems, "8:rubygems")]

identitySpec :: Spec
identitySpec = describe "the identity" $ do
    it "keeps a component that holds a separator inside its own frame" $ do
        let split = versionKey npmRegistry (unscopedNpm "thing") (npmVersion "1\US2")
            joined = versionKey npmRegistry (unscopedNpm "thing\US1") (npmVersion "2")
        cacheKeyIdentity split `shouldNotBe` cacheKeyIdentity joined
        split `shouldNotBe` joined

    it "tells an unscoped name from the scoped name of the same canonical text" $ do
        fullKey npmRegistry (mkPackageName Npm Nothing "@s/x") `shouldNotBe` fullKey npmRegistry (scopedNpm "s" "x")
        fullKey npmRegistry (mkPackageName Npm Nothing "@/x") `shouldNotBe` fullKey npmRegistry (scopedNpm "" "x")

    it "tells a package's full key from its selected-version key with an empty version" $
        cacheKeyIdentity (fullKey npmRegistry (unscopedNpm "thing"))
            `shouldNotBe` cacheKeyIdentity (versionKey npmRegistry (unscopedNpm "thing") (npmVersion ""))

    it "gives one PyPI project one key under every spelling of its name" $
        fullKey pypiIndex (unscopedPyPI "Flask_SQLAlchemy") `shouldBe` fullKey pypiIndex (unscopedPyPI "flask-sqlalchemy")

    describe "properties" $ do
        it "gives two subjects one identity only when they are the same entry" $
            hedgehog $ do
                (a, b) <- forAll genSubjectPair
                (cacheKeyIdentity (keyOf a) == cacheKeyIdentity (keyOf b)) === sameEntry a b

        it "gives two subjects one rendering only when they are the same entry" $
            hedgehog $ do
                (a, b) <- forAll genSubjectPair
                (renderCacheKey (keyOf a) == renderCacheKey (keyOf b)) === sameEntry a b

instanceSpec :: Spec
instanceSpec = describe "equality, order and hashing" $ do
    it "equates two keys only when they are the same entry" $
        hedgehog $ do
            (a, b) <- forAll genSubjectPair
            (keyOf a == keyOf b) === sameEntry a b

    it "orders keys as their renderings order" $
        hedgehog $ do
            (a, b) <- forAll genSubjectPair
            compare (keyOf a) (keyOf b) === comparing (renderCacheKey . keyOf) a b

    it "hashes a key as its rendering hashes" $
        hedgehog $ do
            subject <- forAll genSubject
            hashWithSalt 17 (keyOf subject) === hashWithSalt 17 (renderCacheKey (keyOf subject))

-- What a key addresses, in the terms its builder takes.
data Subject
    = FullSubject Source PackageName
    | VersionSubject Source PackageName Version
    | AssembledSubject ETag
    deriving stock (Show)

keyOf :: Subject -> CacheKey
keyOf = \case
    FullSubject source name -> fullKey source name
    VersionSubject source name version -> versionKey source name version
    AssembledSubject validator -> assembledKey validator

namespaceOf :: Subject -> ByteString
namespaceOf = \case
    FullSubject{} -> "ecluse:0:full:0:"
    VersionSubject{} -> "ecluse:0:version:0:"
    AssembledSubject{} -> "ecluse:0:assembled:0:"

-- Whether two subjects are one entry, by the equality of the domain values and never by a key.
sameEntry :: Subject -> Subject -> Bool
sameEntry a b = case (a, b) of
    (FullSubject source name, FullSubject source' name') -> source == source' && name == name'
    (VersionSubject source name version, VersionSubject source' name' version') ->
        source == source' && name == name' && renderVersion version == renderVersion version'
    (AssembledSubject validator, AssembledSubject validator') -> validator == validator'
    _ -> False

-- A pair drawn from small pools, so the same entry and its near misses both occur.
genSubjectPair :: Gen (Subject, Subject)
genSubjectPair = (,) <$> genSubject <*> genSubject

genSubject :: Gen Subject
genSubject =
    Gen.choice
        [ FullSubject <$> genSource <*> genName
        , genVersionSubject
        , AssembledSubject . validatorOf <$> Gen.element ["", "a", "b"]
        ]

genVersionSubject :: Gen Subject
genVersionSubject = do
    name <- genName
    VersionSubject <$> genSource <*> pure name <*> (mkVersion (pkgEcosystem name) <$> Gen.element ["", "1", "1.0.0", "1\US2", "2"])

genSource :: Gen Source
genSource = Source <$> Gen.element ["", "https://a.example", "https://a.example/", "https://a.example\USnpm", "https://b.example"]

-- Names that differ by ecosystem, scope presence, spelling, or text that looks like framing.
genName :: Gen PackageName
genName =
    mkPackageName
        <$> Gen.element Universe.universe
        <*> Gen.element [Nothing, Just (mkScope ""), Just (mkScope "s")]
        <*> Gen.element ["", "x", "@/x", "@s/x", "Flask", "flask", "thing", "thing\US1", "5:thing", "-"]

validatorOf :: ByteString -> ETag
validatorOf = mkStrongETag . hashWith SHA256

npmRegistry, pypiIndex :: Source
npmRegistry = Source "https://registry.npmjs.org"
pypiIndex = Source "https://pypi.org/simple"

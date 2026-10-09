-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache keys: the pinned identity of each store's key, and the store and identity that keep entries apart.
module Ecluse.Core.Server.Cache.TypesSpec (spec) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.Universe.Class qualified as Universe
import Hedgehog (Gen, PropertyT, cover, forAll, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Server.Cache.Types (CacheKey, Source (Source), assembledKey, cacheKeyIdentity, cacheKeyStore, fullKey, versionKey)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag)
import Ecluse.Core.Telemetry.Metrics (CacheStore (..))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Package (npmVersion, pypiVersion, scopedNpm, unscopedNpm, unscopedPyPI)

spec :: Spec
spec = do
    pinnedSpec
    storeSpec
    ecosystemSpec
    identitySpec
    instanceSpec

pinnedSpec :: Spec
pinnedSpec = describe "the identity of each store's key" $
    for_ pinnedKeys $ \(label, key, store, framed) ->
        it ("pins " <> label) $ do
            cacheKeyStore key `shouldBe` store
            cacheKeyIdentity key `shouldBe` framed

-- One key per store and name shape: its label, the key, its store, and its identity.
pinnedKeys :: [(String, CacheKey, CacheStore, ShortByteString)]
pinnedKeys =
    [
        ( "a full key for an npm name"
        , fullKey npmRegistry (unscopedNpm "thing")
        , FullStore
        , "26:https://registry.npmjs.org3:npm-5:thing"
        )
    ,
        ( "a full key for a scoped npm name"
        , fullKey npmRegistry (scopedNpm "babel" "core")
        , FullStore
        , "26:https://registry.npmjs.org3:npm5:babel11:@babel/core"
        )
    ,
        ( "a full key for a PyPI name"
        , fullKey pypiIndex (unscopedPyPI "Flask_SQLAlchemy")
        , FullStore
        , "23:https://pypi.org/simple4:pypi-16:flask-sqlalchemy"
        )
    ,
        ( "a selected-version key for an npm name"
        , versionKey npmRegistry (unscopedNpm "thing") (npmVersion "1.0.0")
        , VersionStore
        , "26:https://registry.npmjs.org3:npm-5:thing5:1.0.0"
        )
    ,
        ( "a selected-version key for a scoped npm name"
        , versionKey npmRegistry (scopedNpm "babel" "core") (npmVersion "7.26.0")
        , VersionStore
        , "26:https://registry.npmjs.org3:npm5:babel11:@babel/core6:7.26.0"
        )
    ,
        ( "a selected-version key for a PyPI name"
        , versionKey pypiIndex (unscopedPyPI "requests") (pypiVersion "2.32.3")
        , VersionStore
        , "23:https://pypi.org/simple4:pypi-8:requests6:2.32.3"
        )
    ,
        ( "an assembled key"
        , assembledKey (validatorOf "fingerprint")
        , AssembledStore
        , "66:\"44863b03e9909b7100e05b02526909a346fd7455183f6619e0fe6198c89981e0\""
        )
    ]

storeSpec :: Spec
storeSpec = describe "the store" $
    it "tags the key of every store with that store" $
        for_ Universe.universe $ \store ->
            cacheKeyStore (keyOf sampleSubject{subjectStore = store}) `shouldBe` store

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

    describe "properties" $
        it "gives two subjects of one store one identity only when they are the same entry" $
            hedgehog $ do
                (a, b) <- forAllPairs
                when (subjectStore a == subjectStore b) $
                    (cacheKeyIdentity (keyOf a) == cacheKeyIdentity (keyOf b)) === sameEntry a b

instanceSpec :: Spec
instanceSpec = describe "equality and hashing" $
    describe "properties" $ do
        it "equates two keys only when they are the same entry" $
            hedgehog $ do
                (a, b) <- forAllPairs
                (keyOf a == keyOf b) === sameEntry a b

        it "never equates keys of different stores" $
            hedgehog $ do
                (a, b) <- forAllPairs
                when (subjectStore a /= subjectStore b) (keyOf a /== keyOf b)

        it "hashes two equal keys alike" $
            hedgehog $ do
                (a, b) <- forAllPairs
                when (keyOf a == keyOf b) (hashWithSalt 17 (keyOf a) === hashWithSalt 17 (keyOf b))

-- The raw components of one entry. A store's key reads its own fields and no others.
data Subject = Subject
    { subjectStore :: CacheStore
    , subjectSource :: Text
    , subjectEcosystem :: Ecosystem
    , subjectScope :: Maybe Text
    , subjectName :: Text
    , subjectVersion :: Text
    , subjectFingerprint :: ByteString
    }
    deriving stock (Eq, Show)

keyOf :: Subject -> CacheKey
keyOf subject = case subjectStore subject of
    FullStore -> fullKey (sourceOf subject) (nameOf subject)
    VersionStore -> versionKey (sourceOf subject) (nameOf subject) (versionOf subject)
    AssembledStore -> assembledKey (validatorOf (subjectFingerprint subject))

sourceOf :: Subject -> Source
sourceOf = Source . subjectSource

nameOf :: Subject -> PackageName
nameOf subject = mkPackageName (subjectEcosystem subject) (mkScope <$> subjectScope subject) (subjectName subject)

versionOf :: Subject -> Version
versionOf subject = mkVersion (subjectEcosystem subject) (subjectVersion subject)

sampleSubject :: Subject
sampleSubject = Subject FullStore "https://a.example" Npm Nothing "thing" "1.0.0" "fingerprint"

-- Whether two subjects are one entry, by the equality of the domain values and never by a key.
sameEntry :: Subject -> Subject -> Bool
sameEntry a b =
    subjectStore a == subjectStore b && case subjectStore a of
        FullStore -> samePackage
        VersionStore -> samePackage && renderVersion (versionOf a) == renderVersion (versionOf b)
        AssembledStore -> validatorOf (subjectFingerprint a) == validatorOf (subjectFingerprint b)
  where
    samePackage = sourceOf a == sourceOf b && nameOf a == nameOf b

componentsApart :: Subject -> Subject -> Int
componentsApart a b =
    length . filter not $
        [ alike subjectStore
        , alike subjectSource
        , alike subjectEcosystem
        , alike subjectScope
        , alike subjectName
        , alike subjectVersion
        , alike subjectFingerprint
        ]
  where
    alike :: (Eq component) => (Subject -> component) -> Bool
    alike component = component a == component b

-- A pair for a property. The run fails unless it meets equal entries, near misses, and two stores.
forAllPairs :: PropertyT IO (Subject, Subject)
forAllPairs = do
    (a, b) <- forAll genSubjectPair
    cover 10 "the same entry" (sameEntry a b)
    cover 10 "one component apart" (not (sameEntry a b) && componentsApart a b == 1)
    cover 10 "different stores" (subjectStore a /= subjectStore b)
    pure (a, b)

-- A subject beside itself, one component away, in another store, or beside an unrelated subject.
genSubjectPair :: Gen (Subject, Subject)
genSubjectPair = do
    subject <- genSubject
    other <- Gen.frequency [(2, pure subject), (3, genRedrawn subject), (2, genInOtherStore subject), (1, genSubject)]
    pure (subject, other)

-- The same components under another store.
genInOtherStore :: Subject -> Gen Subject
genInOtherStore subject = (\store -> subject{subjectStore = store}) <$> Gen.element (filter (/= subjectStore subject) Universe.universe)

genSubject :: Gen Subject
genSubject = Subject <$> genStore <*> genSource <*> genEcosystem <*> genScope <*> genName <*> genVersion <*> genFingerprint

-- The subject with one component its store's key reads drawn again.
genRedrawn :: Subject -> Gen Subject
genRedrawn subject =
    Gen.choice $ case subjectStore subject of
        FullStore -> package
        VersionStore -> ((\version -> subject{subjectVersion = version}) <$> genVersion) : package
        AssembledStore -> [(\fingerprint -> subject{subjectFingerprint = fingerprint}) <$> genFingerprint]
  where
    package =
        [ (\source -> subject{subjectSource = source}) <$> genSource
        , (\ecosystem -> subject{subjectEcosystem = ecosystem}) <$> genEcosystem
        , (\scope -> subject{subjectScope = scope}) <$> genScope
        , (\name -> subject{subjectName = name}) <$> genName
        ]

genStore :: Gen CacheStore
genStore = Gen.element Universe.universe

genSource :: Gen Text
genSource = Gen.element ["", "https://a.example", "https://a.example/", "https://a.example\USnpm", "https://b.example"]

genEcosystem :: Gen Ecosystem
genEcosystem = Gen.element Universe.universe

genScope :: Gen (Maybe Text)
genScope = Gen.element [Nothing, Just "", Just "s"]

-- Names that differ by spelling alone, by a scope written into the name, or by text that looks like framing.
genName :: Gen Text
genName = Gen.element ["", "x", "@/x", "@s/x", "Flask", "flask", "thing", "thing\US1", "5:thing", "-"]

genVersion :: Gen Text
genVersion = Gen.element ["", "1", "1.0.0", "1\US2", "2"]

genFingerprint :: Gen ByteString
genFingerprint = Gen.element ["", "a", "b"]

validatorOf :: ByteString -> ETag
validatorOf = mkStrongETag . hashWith SHA256

npmRegistry, pypiIndex :: Source
npmRegistry = Source "https://registry.npmjs.org"
pypiIndex = Source "https://pypi.org/simple"

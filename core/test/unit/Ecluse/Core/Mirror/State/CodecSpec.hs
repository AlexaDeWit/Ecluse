-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Stored records preserve admission evidence and reject contradictory membership.
module Ecluse.Core.Mirror.State.CodecSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Mirror.State (recordExpectedInventory)
import Ecluse.Core.Mirror.State.Codec
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (msfCompanions))
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Mirror.State.Weight (retainedRecordWeight)
import Ecluse.Core.Package (Artifact (..), Availability (..), CodeExecSignal (..), DependencySources (..), HashAlg (SHA256), PackageDetails (..), mkHash, mkPackageName)
import Ecluse.Core.Package.Entry (EntryKey (SingletonEntry))
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry.Cargo.Mirror (cargoStateFormat)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (NpmMirrorRead), inventoryDetails, inventoryFiles, inventoryReadData)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.Npm.Mirror (npmStateFormat)
import Ecluse.Core.Registry.PyPI.Mirror (pypiStateFormat)
import Ecluse.Core.Registry.RubyGems.Mirror (rubyGemsStateFormat)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (..), Limits (..), defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker.Managed (readMirrorRecord)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Test.Json (changeAt)
import Ecluse.Test.Mirror.RubyGems (RubyStateProbeFixture (..), rubyStateProbeFixture)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoIndexRecord, minimalCargoRelease)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable record codec" $ do
    formatSpec
    it "keeps the old inventory, update, worker and encoder charges" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        projectRecordWorkWeight defaultLimits record `shouldBe` Right 25698
        mirrorInventoryWeight inventory `shouldBe` 21074
        projectRecordWeight pypiStateFormat defaultLimits record `shouldSatisfy` either (const False) (< 25698)
        paid <- newIORef ([] :: [Int])
        let charge cost = modifyIORef' paid (cost :)
            store =
                MirrorState
                    { msReadProject = \_ _ _ -> pure (Right (StateLoaded revision record))
                    , msWriteProject = \_ _ _ _ -> fail "idempotent admission must not write"
                    , msRetainCompanion = \_ _ -> fail "unexpected companion write"
                    , msReadCompanion = \_ _ -> fail "unexpected companion read"
                    }
            budget = WorkerStateBudget charge charge charge charge charge charge charge
            binding = WorkerMirrorState store target defaultLimits (\action -> Just <$> action budget)
        void (recordExpectedInventory pypiStateFormat defaultLimits charge target store inventory >>= expectRight)
        readIORef paid `shouldReturn` [97640]
        writeIORef paid []
        void (readMirrorRecord budget binding mirrorPackage >>= expectRight)
        readIORef paid `shouldReturn` [77094]
        writeIORef paid []
        void (chargedEncodeProjectRecord pypiStateFormat charge defaultLimits record >>= expectRight)
        readIORef paid `shouldReturn` [135560]

    for_ [(NoCodeOnInstall, Available), (CodeExecUnknown, Deprecated), (RunsCodeOnInstall "source build", Yanked)] $ \(install, availability) ->
        it ("preserves admission facts through restart: " <> show (install, availability)) $ do
            original <- mirrorSample [1] False "https://first.test/" "original"
            let details = inventoryDetails original
            inventory <- expectRight (restoreMirrorInventory details{pkgInstallCode = install, pkgAvailability = availability} (inventoryReadData original))
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
            decode bytes `shouldReturn` Right record

    for_ [RegistryDependencies, FetchesOutsideRegistry "dependency 'left-pad' in dependencies names a git source", DependencySourcesUnknown] $ \dependencies ->
        it ("preserves the dependency fact through restart: " <> show dependencies) $ do
            original <- mirrorSample [1] False "https://first.test/" "original"
            let details = (inventoryDetails original){pkgDependencySources = dependencies}
            inventory <- expectRight (restoreMirrorInventory details (inventoryReadData original))
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
            decode bytes `shouldReturn` Right record

    for_ invalidFields $ \(label, path, replacement, reason) ->
        it label $ do
            (record, value) <- recordFixture
            bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
            decode bytes `shouldReturn` Right record
            changed <- changeAt path (const (pure replacement)) value
            decode (LBS.toStrict (encode changed)) >>= (`shouldSatisfy` invalidBecause reason)
            decode bytes `shouldReturn` Right record

    for_ duplicateFields $ \(label, path) ->
        it label $ do
            (record, value) <- recordFixture
            decode (LBS.toStrict (encode value)) `shouldReturn` Right record
            changed <- changeAt path duplicateArray value
            decode (LBS.toStrict (encode changed)) >>= (`shouldSatisfy` invalidBecause "duplicate state entry")

    it "counts retained companion bytes together with the encoded record" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        let file :| _ = inventoryFiles inventory
        verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        retained <- expectRight (retainCompanionReference mirrorVersion verified reference record)
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits retained)
        let cap = BS.length bytes + 8
            exact = defaultLimits{maxMetadataBytes = cap}
            small = exact{maxMetadataBytes = cap - 1}
            refused = StateBound (BodyTooLarge (MetadataBodyLimit (cap - 1)))
        encodeProjectRecord pypiStateFormat exact retained `shouldBe` Right bytes
        chargedDecodeProjectRecord pypiStateFormat (const pass) exact target mirrorPackage bytes `shouldReturn` Right retained
        encodeProjectRecord pypiStateFormat small retained `shouldBe` Left refused
        chargedDecodeProjectRecord pypiStateFormat (const pass) small target mirrorPackage bytes `shouldReturn` Left refused
        encodeProjectRecord pypiStateFormat defaultLimits{maxMetadataBytes = 7} retained
            `shouldBe` Left (StateBound (BodyTooLarge (MetadataBodyLimit 7)))

    it "rejects excessive dimensions before paying for domain restoration" $ do
        (record, _) <- recordFixture
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
        for_ [(defaultLimits{maxVersionCount = 0}, TooManyVersions 1 0), (defaultLimits{maxArtifactCount = 0}, TooManyArtifacts 1 0)] $ \(bounds, fault) -> do
            paid <- newIORef ([] :: [Int])
            chargedDecodeProjectRecord pypiStateFormat (\cost -> modifyIORef' paid (cost :)) bounds target mirrorPackage bytes
                `shouldReturn` Left (StateBound fault)
            length <$> readIORef paid `shouldReturn` 1

recordFixture :: IO (ProjectMirrorRecord, Value)
recordFixture = do
    inventory <- mirrorSample [1] True "https://first.test/" "original"
    record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
    bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
    value <- expectRight (eitherDecodeStrict bytes)
    pure (record, value)

decode :: ByteString -> IO (Either MirrorStateFault ProjectMirrorRecord)
decode = chargedDecodeProjectRecord pypiStateFormat (const pass) defaultLimits target mirrorPackage

invalidBecause :: Text -> Either MirrorStateFault a -> Bool
invalidBecause reason (Left (StateInvalid detail)) = reason `T.isSuffixOf` detail
invalidBecause _ _ = False

duplicateArray :: Value -> IO Value
duplicateArray (Array values) = pure (Array (values <> values))
duplicateArray _ = fail "expected fixture array"

invalidFields :: [(String, [Key], Value, Text)]
invalidFields =
    [ ("refuses an unsupported durable schema", ["schema"], Number 999, "unsupported mirror state schema")
    , ("refuses an unknown package ecosystem", ["package", "ecosystem"], String "unknown", "unknown ecosystem")
    , ("refuses another package in the project envelope", ["envelope", "name"], String "other", "invalid project envelope identity")
    , ("refuses a version key that disagrees with its fragment", ["versions", "version"], String "2", "fragment identity mismatch")
    , ("refuses another package in an admission fragment", details <> ["package", "name"], String "other", "fragment identity mismatch")
    , ("refuses a version without fragments", ["versions", "fragments"], toJSON ([] :: [Value]), "empty version fragments")
    , ("refuses a fragment without artifacts", details <> ["artifacts"], toJSON ([] :: [Value]), "empty source fragment")
    , ("refuses a negative artifact size", artifacts <> ["size"], Number (-1), "negative artifact size")
    , ("refuses an unknown install fact", details <> ["install", "kind"], String "invalid", "unknown install fact")
    , ("refuses an unknown availability fact", details <> ["availability"], String "invalid", "unknown availability fact")
    , ("refuses an unknown source shape", source <> ["kind"], String "invalid", "unknown source shape")
    , ("refuses a source entry with another filename", source <> ["entries", "value", "filename"], String "other-1.0.tar.gz", "invalid source fragment pairing")
    , ("refuses missing declared companion membership", ["versions", "companions"], toJSON ([] :: [Value]), "companion set does not match declared source companions")
    , ("refuses companion membership for another filename", ["versions", "companions", "filename"], String "other", "companion set does not match declared source companions")
    , ("refuses a verified digest without an expected file", ["versions", "verified"], toJSON [object ["filename" .= ("other" :: Text), "digest" .= digest "sha256" 64]], "verified digest has no expected file")
    , ("refuses an unsupported verified digest algorithm", ["versions", "verified"], toJSON [object ["filename" .= mirrorFilename 1, "digest" .= digest "sha512" 128]], "unsupported verified artifact digest")
    , ("refuses a negative retained companion size", ["versions", "companions", "retained"], object ["digest" .= digest "sha256" 64, "size" .= (-1 :: Int)], "invalid retained companion identity")
    , ("refuses a retained companion with another digest algorithm", ["versions", "companions", "retained"], object ["digest" .= digest "sha512" 128, "size" .= (8 :: Int)], "invalid retained companion identity")
    ]
  where
    details = ["versions", "fragments", "details"]
    artifacts = details <> ["artifacts"]
    source = ["versions", "fragments", "source"]
    digest algorithm size = object ["algorithm" .= (algorithm :: Text), "value" .= T.replicate size "a"]

duplicateFields :: [(String, [Key])]
duplicateFields =
    [ ("refuses duplicate versions", ["versions"])
    , ("refuses duplicate filenames across fragments", ["versions", "fragments"])
    , ("refuses duplicate companion entries", ["versions", "companions"])
    , ("refuses duplicate source coordinates", ["versions", "fragments", "source", "entries"])
    ]

target :: StateTarget
target = stateTarget "https://private.test"

revision :: StateRevision
revision = StateRevision target mirrorPackage "etag"

-- | The bytes, decode charges, weights and companion names one format's record keeps.
data FormatPin = FormatPin
    { pinDigest :: Text
    , pinLength :: Int
    , pinDecodeCharges :: [Int]
    , pinWeights :: (Integer, Either MirrorStateFault Int, Either MirrorStateFault Int)
    , pinCompanions :: [(Text, [Text])]
    }
    deriving stock (Eq, Show)

formatSpec :: Spec
formatSpec = describe "each durable format" $ do
    for_ formatSamples $ \(label, format, sample, expected) ->
        it ("keeps the " <> label <> " record bytes, decode charges, weights and companions") $ do
            (owner, record) <- sample
            bytes <- expectRight (encodeProjectRecord format defaultLimits record)
            paid <- newIORef ([] :: [Int])
            chargedDecodeProjectRecord format (\cost -> modifyIORef' paid (cost :)) defaultLimits owner (recordPackage record) bytes `shouldReturn` Right record
            charges <- reverse <$> readIORef paid
            FormatPin
                { pinDigest = sha256Hex bytes
                , pinLength = BS.length bytes
                , pinDecodeCharges = charges
                , pinWeights = (retainedRecordWeight format record, projectRecordWeight format defaultLimits record, projectRecordWorkWeight defaultLimits record)
                , pinCompanions = [(key, Map.keys (versionCompanions version)) | (key, version) <- Map.toList (recordVersions record)]
                }
                `shouldBe` expected

    for_ [("npm" :: Text, npmStateFormat, npmSample), ("Cargo", cargoStateFormat, cargoSample), ("RubyGems", rubyGemsStateFormat, rubyGemsSample)] $ \(label, format, sample) ->
        it ("refuses a project envelope on a " <> toString label <> " record") $ do
            (owner, record) <- sample
            bytes <- expectRight (encodeProjectRecord format defaultLimits record)
            value <- expectRight (eitherDecodeStrict bytes)
            changed <- changeAt ["envelope"] (const (pure (object ["name" .= ("other" :: Text)]))) value
            chargedDecodeProjectRecord format (const pass) defaultLimits owner (recordPackage record) (LBS.toStrict (encode changed))
                >>= (`shouldSatisfy` invalidBecause (label <> " record has an unexpected project envelope"))

    it "verifies a declared companion only where its artifact declares one" $ do
        declared <- mirrorSample [1] True "https://first.test/" "original"
        undeclared <- mirrorSample [1] False "https://first.test/" "original"
        let firstFile inventory = let file :| _ = inventoryFiles inventory in file
            checked inventory = verifySourceCompanionBytes defaultLimits (msfCompanions pypiStateFormat) (firstFile inventory) "metadata"
        checked declared `shouldSatisfy` isRight
        checked undeclared `shouldBe` Left (StateInvalid "companion was not declared")

formatSamples :: [(String, MirrorStateFormat, IO (StateTarget, ProjectMirrorRecord), FormatPin)]
formatSamples =
    [ ("PyPI", pypiStateFormat, pypiSample, FormatPin "4edb9decf39b48edd00e4671f8be9bd4e25afe0a586495eab05fc0747cedecdf" 2552 [362016, 444208] (27317, Right 27317, Right 51628) [("1", map mirrorFilename [1, 2, 3])])
    , ("npm", npmStateFormat, npmSample, FormatPin "f7bfbd2280c62b8d452b4e397ebef60c1d011c286e52377084521662f2126a9d" 801 [127500, 156292] (27341, Right 27341, Right 24718) [("1.3.0", [])])
    , ("Cargo", cargoStateFormat, cargoSample, FormatPin "07d491bd3513762a296600adda6b85fbcd73ff8568ff289eedf2c73473f89995" 2317 [342812, 403124] (56832, Right 56832, Right 50777) [("1.10.0", []), ("1.9.0", [])])
    , ("RubyGems", rubyGemsStateFormat, rubyGemsSample, FormatPin "49c2cec1533bfe2ea0c1995a660068afcb2201922e33e2bfa8d2da65649c8698" 4069 [389948, 491780] (75359, Right 75359, Right 60590) [("1.0.0", rubyGemsPlatforms)])
    ]
  where
    rubyGemsPlatforms = map (\platform -> "ecluse-fixture-platform-1.0.0" <> platform <> ".gem") ["-arm64-darwin", "-java", "-x86_64-linux-musl", ""]

-- Two fragments: the second admission strips the retained envelope from its owned source.
pypiSample :: IO (StateTarget, ProjectMirrorRecord)
pypiSample = do
    admitted <- mirrorSample [1, 2] True "https://first.test/" "original"
    grown <- mirrorSample [1, 2, 3] True "https://first.test/" "original"
    initial <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target admitted Nothing)
    (target,) <$> expectRight (extendProjectRecord pypiStateFormat defaultLimits target grown (Just initial))

npmSample :: IO (StateTarget, ProjectMirrorRecord)
npmSample = do
    digest <- expectRight (mkHash SHA256 (T.replicate 64 "b"))
    let name = mkPackageName Npm Nothing "left-pad"
        artifact = Artifact SingletonEntry "left-pad-1.3.0.tgz" "https://registry.test/left-pad/-/left-pad-1.3.0.tgz" [digest] Nothing (Just 7)
        details = PackageDetails name (mkVersion Npm "1.3.0") Nothing NoCodeOnInstall DependencySourcesUnknown Available (artifact :| [])
        fields = KeyMap.fromList [("name", String "left-pad"), ("version", String "1.3.0"), ("dist", object ["tarball" .= artUrl artifact])]
    inventory <- expectRight (restoreMirrorInventory details (NpmMirrorRead fields))
    (target,) <$> expectRight (extendProjectRecord npmStateFormat defaultLimits target inventory Nothing)

cargoSample :: IO (StateTarget, ProjectMirrorRecord)
cargoSample = do
    let release = minimalCargoRelease "thing"
    (target,) <$> cargoMirrorRecord target (mkPackageName Cargo Nothing "thing") (releaseArchive (release "1.9.0")) [(0, cargoIndexRecord (release "1.9.0")), (1, cargoIndexRecord (release "1.10.0"))]

rubyGemsSample :: IO (StateTarget, ProjectMirrorRecord)
rubyGemsSample = do
    fixture <- rubyStateProbeFixture
    record <- chargedDecodeProjectRecord rubyGemsStateFormat (const pass) defaultLimits (rubyStateTarget fixture) (rubyStatePackage fixture) (rubyStateEncodedRecord fixture) >>= expectRight
    pure (rubyStateTarget fixture, record)

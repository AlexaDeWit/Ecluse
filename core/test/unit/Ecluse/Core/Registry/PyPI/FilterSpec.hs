-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI assembly preserves admitted entries and refuses locations it cannot rebase.
module Ecluse.Core.Registry.PyPI.FilterSpec (spec) where

import Data.Aeson (Value (Array, Number, Object, String), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    HashAlg (SHA1),
    InvalidEntry (invalidKey, invalidKind),
    InvalidEntryKind (InvalidIndexFile),
    PackageInfo (infoInvalidEntries),
    PackageName,
    mkPackageName,
 )
import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (ArrayEntry))
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Package.Integrity (IntegrityFloor, mkMinTrustedIntegrity)
import Ecluse.Core.Package.Merge (MergePlan (..), Provenance (GatedSource, TrustedSource), SourceId, mergePackuments)
import Ecluse.Core.Registry.PyPI.Filter (assembleSimpleIndex)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Core.Security (defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Pipeline.Internal (admitByIntegrity)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    ServeDecision (Reject),
 )
import Ecluse.Core.Snapshot (Snapshot (..), digestOf)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity, validSha1, validSha256)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Snapshot (jsonSnapshot, projectJsonSnapshot)
import Ecluse.Test.Support (expectRight)

-- | Pin PyPI source selection, artifact rebasing, and sidecar removal.
spec :: Spec
spec = do
    relaySpec
    survivorSpec
    admissionSpec
    rebaseSpec
    sidecarSpec

relaySpec :: Spec
relaySpec = describe "what the assembly relays from the base document" $ do
    it "keeps meta, so a mirror still reads the serial it revalidates against" $
        field "meta" (assembleOne allFiles)
            `shouldBe` Just (object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (37059094 :: Int)])

    it "keeps the project name the winning document reported" $
        field "name" (assembleOne allFiles) `shouldBe` Just (String "requests")

    it "keeps a top-level key this build does not model" $
        field "tracks" (assembleOne allFiles) `shouldBe` Just (Array mempty)

    it "keeps every modelled key on a served file entry, verbatim" $ do
        let entry = servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl"
        (entry >>= KeyMap.lookup "upload-time") `shouldBe` Just (String "2026-05-14T19:25:26Z")
        (entry >>= KeyMap.lookup "requires-python") `shouldBe` Just (String ">=3.10")
        (entry >>= KeyMap.lookup "size") `shouldBe` Just (Number 73075)
        (entry >>= KeyMap.lookup "yanked") `shouldBe` Just (String "withdrawn")
        (entry >>= KeyMap.lookup "hashes") `shouldBe` Just (object ["sha256" .= validSha256])

    it "keeps an unmodelled key on a served file entry too" $
        (servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl" >>= KeyMap.lookup "provenance")
            `shouldBe` Just (String "https://pypi.org/integrity/x/provenance")

    it "yields an object even for a base document that is not one" $
        assembleSimpleIndex mountBase (Map.singleton 0 (jsonSnapshot (indexOf allFiles))) (planOver [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2.tar.gz"])]) (String "not an index")
            `shouldSatisfy` isObject

survivorSpec :: Spec
survivorSpec = describe "which releases and files the assembly serves" $ do
    it "names the surviving releases in the PEP 700 versions array, and no others" $
        field "versions" (assembleOne allFiles) `shouldBe` Just (Array (fromList [String "2.34.2"]))

    it "omits a release the plan did not keep, files and all" $
        servedNames (assembleOne allFiles) `shouldNotContain` ["requests-2.34.1.tar.gz"]

    it "omits a file the per-artifact partition dropped from a surviving release" $ do
        let served = assemble [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2-py3-none-any.whl"])]
        servedNames served `shouldBe` ["requests-2.34.2-py3-none-any.whl"]

    it "takes each release's files from the source that won it" $ do
        let served =
                assembleSources
                    [(0, indexNamed "requests" [privateFile]), (1, indexOf allFiles)]
                    [("2.34.2", 0)]
                    [("2.34.2", ["requests-2.34.2-private.tar.gz"])]
        servedNames served `shouldBe` ["requests-2.34.2-private.tar.gz"]

    it "drops a named file the winning source does not hold, never fabricating one" $
        servedNames (assemble [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2-absent.whl"])]) `shouldBe` []

    it "serves nothing at all for a plan with no survivors" $ do
        let served = assemble [] []
        field "versions" served `shouldBe` Just (Array mempty)
        servedNames served `shouldBe` []

admissionSpec :: Spec
admissionSpec = describe "replaying per-entry admission for duplicate filenames" $ do
    it "refuses an admitted position from another source snapshot" $ do
        source <- projectAdmitted defaultMinIntegrity [admittedDuplicate]
        plan <- expectRight (maybeToRight ("expected merge plan" :: Text) (mergePackuments [(GatedSource, fst <$> source)]))
        let rawSource = snd <$> source
            serve sources = servedFiles (assembleSimpleIndex mountBase sources plan (indexOf []))
        serve (Map.singleton 0 rawSource) `shouldBe` [rebasedDuplicate admittedDuplicate]
        serve (Map.singleton 1 rawSource) `shouldBe` []
        serve (Map.singleton 0 rawSource{snapshotDigest = digestOf "different upstream bytes"}) `shouldBe` []

    for_ [("refused first", id), ("refused last", reverse)] $ \(order, arrange) ->
        describe order $ do
            it "omits an authority-refused sibling after projection, admission, and merge" $ do
                source@(Snapshot _ (info, _)) <- projectAdmitted defaultMinIntegrity (arrange [foreignDuplicate, admittedDuplicate])
                map (\entry -> (invalidKind entry, invalidKey entry)) (infoInvalidEntries info)
                    `shouldBe` [(InvalidIndexFile, duplicateFilename)]
                (plan, served) <- assembleDuplicates [(GatedSource, source)]
                Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [])
                servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

            for_ [("no digest", Object mempty), ("SHA-1 only", object ["sha1" .= validSha1])] $ \(label, hashes) ->
                it ("omits a public sibling admitted by location but refused for " <> label) $ do
                    let refused = withFileKeys [("hashes", hashes)] otherDuplicate
                    source@(Snapshot _ (info, _)) <- projectAdmitted defaultMinIntegrity (arrange [refused, admittedDuplicate])
                    infoInvalidEntries info `shouldBe` []
                    (plan, served) <- assembleDuplicates [(GatedSource, source)]
                    Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [])
                    servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

            it "applies the default trusted floor to each same-name sibling" $ do
                source <- projectAdmitted defaultMinTrustedIntegrity (arrange [weakDuplicate, admittedDuplicate])
                (plan, served) <- assembleDuplicates [(TrustedSource, source)]
                Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [])
                servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

            it "keeps both valid siblings with their own unknown fields and their original order" $ do
                let files = arrange [otherDuplicate, admittedDuplicate]
                source <- projectAdmitted defaultMinIntegrity files
                (plan, served) <- assembleDuplicates [(GatedSource, source)]
                Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [duplicateFilename])
                servedFiles served `shouldBe` map rebasedDuplicate files

            it "keeps a weak sibling when the trusted floor permits its digest" $ do
                floorSpec <- expectRight (mkMinTrustedIntegrity SHA1)
                let files = arrange [weakDuplicate, admittedDuplicate]
                source <- projectAdmitted floorSpec files
                (_, served) <- assembleDuplicates [(TrustedSource, source)]
                servedFiles served `shouldBe` map rebasedDuplicate files

    it "preserves repeated identical valid entries" $ do
        source <- projectAdmitted defaultMinIntegrity [admittedDuplicate, admittedDuplicate]
        (_, served) <- assembleDuplicates [(GatedSource, source)]
        servedFiles served `shouldBe` replicate 2 (rebasedDuplicate admittedDuplicate)

    it "does not confuse raw entry positions after a malformed file" $ do
        source@(Snapshot _ (info, _)) <- projectAdmitted defaultMinIntegrity [Number 1, foreignDuplicate, admittedDuplicate]
        length (infoInvalidEntries info) `shouldBe` 2
        (_, served) <- assembleDuplicates [(GatedSource, source)]
        servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

    for_ [("trusted first", id), ("trusted last", reverse)] $ \(order, arrange) ->
        it ("takes duplicate entries only from the winning source: " <> order) $ do
            private <- projectAdmitted defaultMinTrustedIntegrity [admittedDuplicate, otherDuplicate]
            public <- projectAdmitted defaultMinIntegrity [withFileKeys [("source-marker", String "public")] admittedDuplicate]
            let contributions = arrange [(TrustedSource, private), (GatedSource, public)]
                expectedSource = fst <$> find ((== TrustedSource) . fst . snd) (zip [0 ..] contributions)
            (plan, served) <- assembleDuplicates contributions
            Map.lookup "1" (mpSurvivors plan) `shouldBe` expectedSource
            servedFiles served `shouldBe` map rebasedDuplicate [admittedDuplicate, otherDuplicate]

projectAdmitted :: (IntegrityFloor floor) => floor -> [Value] -> IO (Snapshot (PackageInfo, Value))
projectAdmitted floorSpec files = do
    Snapshot digest (info, raw) <- projectJsonSnapshot (projectPyPIIndex defaultLimits (mkPackageName PyPI Nothing "requests")) (indexOf files)
    let located = enforceArtifactLocations (ecosystemArtifactAuthorities ["https://files.pythonhosted.org"]) "https://pypi.org" info
        (admitted, _) =
            admitByIntegrity
                floorSpec
                (Reject (Rejection BelowIntegrityFloor "below floor"))
                (Reject (Rejection MissingIntegrity "missing digest"))
                located
    pure (Snapshot digest (admitted, raw))

assembleDuplicates :: [(Provenance, Snapshot (PackageInfo, Value))] -> IO (MergePlan, Value)
assembleDuplicates contributions = do
    plan <- expectRight (maybeToRight ("expected a merge plan" :: Text) (mergePackuments (map (second (fmap fst)) contributions)))
    let sources = Map.fromList [(sid, snd <$> source) | (sid, (_, source)) <- zip [0 ..] contributions]
    pure (plan, assembleSimpleIndex mountBase sources plan (indexOf []))

duplicateFilename :: Text
duplicateFilename = "requests-1.0.0.tar.gz"

admittedDuplicate :: Value
admittedDuplicate = withFileKeys [("source-marker", String "admitted")] (simpleFile duplicateFilename)

otherDuplicate :: Value
otherDuplicate =
    withFileKeys
        [ ("url", String ("https://files.pythonhosted.org/packages/b1/" <> duplicateFilename))
        , ("source-marker", String "other")
        ]
        admittedDuplicate

foreignDuplicate :: Value
foreignDuplicate = withFileKeys [("url", String ("https://evil.test/" <> duplicateFilename))] otherDuplicate

weakDuplicate :: Value
weakDuplicate = withFileKeys [("hashes", object ["sha1" .= validSha1])] otherDuplicate

rebasedDuplicate :: Value -> Value
rebasedDuplicate = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> duplicateFilename))]

rebaseSpec :: Spec
rebaseSpec = describe "where a served file points" $ do
    for_ [String "https://files.pythonhosted.org/a\\..\\x", String "https://files.pythonhosted.org/%2e%2e", String "https://files.pythonhosted.org/a%2fb", String "https://files.pythonhosted.org/.. ", Number 1] $ \url ->
        it ("omits a duplicate named entry whose URL cannot be rebased: " <> show url) $ do
            let filename = "requests-2.34.2.tar.gz"
                valid = simpleFile filename
                refused = withFileKeys [("url", url)] valid
                expected = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> filename))] valid
            servedFiles (assembleOne [refused, valid]) `shouldBe` [expected]
            servedFiles (assembleOne [valid, refused]) `shouldBe` [expected]

    it "omits a duplicate named entry with no URL" $ do
        let filename = "requests-2.34.2.tar.gz"
            missing = object ["filename" .= filename]
        servedFiles (assembleOne [missing, simpleFile filename])
            `shouldBe` servedFiles (assembleOne [simpleFile filename])

    it "rebases a file location onto this mount under the artifact route's own spelling" $
        (servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl" >>= KeyMap.lookup "url")
            `shouldBe` Just (String "https://ecluse.test/pypi/simple/requests/requests-2.34.2-py3-none-any.whl")

    it "rebases under the requested project, not the spelling the document reported" $ do
        let served =
                assembleSimpleIndex
                    mountBase
                    (Map.singleton 0 (jsonSnapshot zopeIndex))
                    (planFor zopeInterface [(0, zopeIndex)] [("7.2", 0)] [("7.2", [zopeFile])])
                    zopeIndex
        (servedEntry served zopeFile >>= KeyMap.lookup "url")
            `shouldBe` Just (String ("https://ecluse.test/pypi/simple/zope-interface/" <> zopeFile))

sidecarSpec :: Spec
sidecarSpec = describe "the PEP 658 sidecar keys" $
    it "drops both spellings, because Écluse serves no .metadata companion" $ do
        let entry = servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl"
        (entry >>= KeyMap.lookup "core-metadata") `shouldBe` Nothing
        (entry >>= KeyMap.lookup "data-dist-info-metadata") `shouldBe` Nothing

mountBase :: Text
mountBase = "https://ecluse.test/pypi"

assembleOne :: [Value] -> Value
assembleOne files =
    assembleSources [(0, indexOf files)] [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"])]

assemble :: [(Text, SourceId)] -> [(Text, [Text])] -> Value
assemble = assembleSources [(0, indexOf allFiles)]

assembleSources :: [(SourceId, Value)] -> [(Text, SourceId)] -> [(Text, [Text])] -> Value
assembleSources sources survivors kept =
    assembleSimpleIndex mountBase (Map.fromList (map (second jsonSnapshot) sources)) (planFor (mkPackageName PyPI Nothing "requests") sources survivors kept) (snd (headSource sources))
  where
    headSource = \case
        source : _ -> source
        [] -> (0, Object mempty)

planOver :: [(Text, SourceId)] -> [(Text, [Text])] -> MergePlan
planOver = planFor (mkPackageName PyPI Nothing "requests") [(0, indexOf allFiles)]

planFor :: PackageName -> [(SourceId, Value)] -> [(Text, SourceId)] -> [(Text, [Text])] -> MergePlan
planFor name sources survivors kept =
    MergePlan
        { mpName = name
        , mpSurvivors = Map.fromList survivors
        , mpArtifacts =
            Map.fromList
                [ (version, entries)
                | (version, names) <- kept
                , Just sid <- [lookup version survivors]
                , Just raw <- [lookup sid sources]
                , let digest = snapshotDigest (jsonSnapshot raw)
                , Just entries <-
                    [ nonEmpty
                        [ AdmittedEntry digest (ArrayEntry position) filename
                        | (position, entry) <- zip [0 ..] (servedFiles raw)
                        , Just (String filename) <- [field "filename" entry]
                        , filename `elem` names
                        ]
                    ]
                ]
        , mpDistTags = Map.empty
        , mpTime = Map.empty
        , mpDivergences = mempty
        }

indexOf :: [Value] -> Value
indexOf = indexNamed "requests"

indexNamed :: Text -> [Value] -> Value
indexNamed name files =
    object
        [ "name" .= name
        , "meta" .= object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (37059094 :: Int)]
        , "tracks" .= Array mempty
        , "files" .= files
        ]

allFiles :: [Value]
allFiles =
    [ fileNamed "requests-2.34.2.tar.gz"
    , fileNamed "requests-2.34.2-py3-none-any.whl"
    , fileNamed "requests-2.34.1.tar.gz"
    ]

privateFile :: Value
privateFile = fileNamed "requests-2.34.2-private.tar.gz"

zopeInterface :: PackageName
zopeInterface = mkPackageName PyPI Nothing "zope-interface"

zopeIndex :: Value
zopeIndex = indexNamed "Zope.Interface" [fileNamed zopeFile]

zopeFile :: Text
zopeFile = "zope_interface-7.2-py3-none-any.whl"

fileNamed :: Text -> Value
fileNamed filename =
    withFileKeys
        [ ("size", toJSON (73075 :: Int))
        , ("yanked", toJSON ("withdrawn" :: Text))
        , ("core-metadata", object ["sha256" .= sidecarDigest])
        , ("data-dist-info-metadata", object ["sha256" .= sidecarDigest])
        ]
        (simpleFile filename)

sidecarDigest :: Text
sidecarDigest = "8c384ba3"

field :: Text -> Value -> Maybe Value
field key = \case
    Object o -> KeyMap.lookup (fromString (toString key)) o
    _ -> Nothing

servedFiles :: Value -> [Value]
servedFiles served = case field "files" served of
    Just (Array files) -> toList files
    _ -> []

servedNames :: Value -> [Text]
servedNames = mapMaybe name . servedFiles
  where
    name = \case
        Object entry | Just (String filename) <- KeyMap.lookup "filename" entry -> Just filename
        _ -> Nothing

servedEntry :: Value -> Text -> Maybe (KeyMap.KeyMap Value)
servedEntry served filename = listToMaybe [entry | Object entry <- servedFiles served, KeyMap.lookup "filename" entry == Just (String filename)]

isObject :: Value -> Bool
isObject = \case
    Object _ -> True
    _ -> False

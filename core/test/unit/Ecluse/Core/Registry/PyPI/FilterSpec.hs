-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI assembly preserves admitted entries, refuses locations it cannot rebase, and keeps served bytes.
module Ecluse.Core.Registry.PyPI.FilterSpec (spec) where

import Data.Aeson (Value (Array, Bool, Number, Object, String), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    HashAlg (SHA1),
    InvalidEntry (invalidKey, invalidKind),
    InvalidEntryKind (InvalidIndexFile),
    PackageInfo (infoInvalidEntries, infoVersions),
    PackageName,
    mkPackageName,
 )
import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (ArrayEntry))
import Ecluse.Core.Package.Filter (enforceArtifactLocations, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (IntegrityFloor, mkMinTrustedIntegrity)
import Ecluse.Core.Package.Merge (MergePlan (..), Provenance (GatedSource, TrustedSource), SourceId, mergePackuments)
import Ecluse.Core.Registry.CachedDocument (npmCached, pypiPacked, pypiRendered, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple (..), SimpleDocument, simpleFiles)
import Ecluse.Core.Registry.PyPI.Filter qualified as Filter
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Registry.PyPI.Project (projectName)
import Ecluse.Core.Security (defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Pipeline.Internal (admitByIntegrity)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    ServeDecision (Reject),
 )
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (..))
import Ecluse.Test.Corpus (cpPackage, cpPath, pypiCorpusPackages)
import Ecluse.Test.Json (encodeStrict, fieldAt, isObject)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity, requestsName, validSha1, validSha256)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndexWith, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, projectPyPIIndex, projectPyPIPackedChunks, simpleValue)
import Ecluse.Test.Snapshot (digestOf, jsonSnapshot, projectJsonSnapshot)
import Ecluse.Test.Support (expectRight)

-- | Pin PyPI source selection, artifact rebasing, sidecar declarations, and served bytes.
spec :: Spec
spec = do
    fallbackAssemblySpec
    relaySpec
    survivorSpec
    admissionSpec
    rebaseSpec
    sidecarSpec
    serialiseSpec
    packedSerialiseSpec
    packedAdverseSpec

fallbackAssemblySpec :: Spec
fallbackAssemblySpec = describe "cached PyPI fallback assembly" $
    it "renders plain and mixed sources with absent or foreign bases using exact snapshot selection" $ do
        let filename = "requests-2.34.2.tar.gz"
            file = simpleFile filename
            raw = indexOf [file]
            foreignDoc = fst npmCached (object ["name" .= ("foreign" :: Text)])
            plan = planFor requestsName [(0, raw)] [("2.34.2", 0)] [("2.34.2", [filename])]
            expected files = object ["versions" .= ["2.34.2" :: Text], "files" .= files]
            expectedFile = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> filename))] file
        (_, tree) <- expectRight (projectPyPIIndex defaultLimits requestsName (encodeStrict raw))
        (_, packed) <- expectRight (projectPackedIndex (encodeStrict raw))
        let source = jsonSnapshot raw
            plainSources = Map.singleton 0 (fst pypiSimpleCached tree <$ source)
            mixedSources = Map.insert 2 (foreignDoc <$ source) (Map.insert 1 (fst pypiPacked packed <$ source) plainSources)
        for_ [plainSources, mixedSources] $ \sources ->
            for_ [Nothing, Just foreignDoc] $ \baseDocument -> do
                let render inputs = do
                        let served = Filter.assembleSimpleDocument mountBase inputs plan baseDocument
                        snd pypiRendered served `shouldSatisfy` isNothing
                        bytes <- expectRight (Filter.serialiseSimpleDocument served)
                        expectRight (eitherDecode bytes)
                render sources `shouldReturn` expected [expectedFile]
                render (Map.adjust (\snapshot -> snapshot{snapshotDigest = digestOf "different source"}) 0 sources)
                    `shouldReturn` expected ([] :: [Value])

packedAdverseSpec :: Spec
packedAdverseSpec = describe "packed adverse assembly" $ do
    for_ [("refused first", id), ("refused last", reverse)] $ \(order, arrange) -> describe order $ do
        it "omits an authority-refused sibling after projection, admission and merge" $ do
            source@(Snapshot _ (info, _)) <- projectPackedAdmitted defaultMinIntegrity (arrange [foreignDuplicate, admittedDuplicate])
            map (\entry -> (invalidKind entry, invalidKey entry)) (infoInvalidEntries info)
                `shouldBe` [(InvalidIndexFile, duplicateFilename)]
            (plan, served) <- assemblePackedDuplicates [(GatedSource, source)]
            Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [])
            servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

        for_ [("no digest", Object mempty), ("SHA-1 only", object ["sha1" .= validSha1])] $ \(label, hashes) ->
            it ("omits a sibling refused for " <> label) $ do
                let refused = withFileKeys [("hashes", hashes)] otherDuplicate
                source <- projectPackedAdmitted defaultMinIntegrity (arrange [refused, admittedDuplicate])
                (plan, served) <- assemblePackedDuplicates [(GatedSource, source)]
                Map.map (fmap admittedFilename) (mpArtifacts plan) `shouldBe` Map.singleton "1" (duplicateFilename :| [])
                servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

        it "applies the trusted floor to each same-name sibling" $ do
            source <- projectPackedAdmitted defaultMinTrustedIntegrity (arrange [weakDuplicate, admittedDuplicate])
            (_, served) <- assemblePackedDuplicates [(TrustedSource, source)]
            servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]

        it "keeps valid same-name siblings in source order" $ do
            let files = arrange [otherDuplicate, admittedDuplicate]
            source <- projectPackedAdmitted defaultMinIntegrity files
            (_, served) <- assemblePackedDuplicates [(GatedSource, source)]
            servedFiles served `shouldBe` map rebasedDuplicate files

    it "keeps repeated valid entries and gaps from malformed or refused siblings" $ do
        source <- projectPackedAdmitted defaultMinIntegrity [Number 1, foreignDuplicate, admittedDuplicate, admittedDuplicate]
        (_, served) <- assemblePackedDuplicates [(GatedSource, source)]
        servedFiles served `shouldBe` replicate 2 (rebasedDuplicate admittedDuplicate)

    for_ [("trusted first", id), ("trusted last", reverse)] $ \(order, arrange) ->
        it ("takes duplicate entries only from the winning source: " <> order) $ do
            private <- projectPackedAdmitted defaultMinTrustedIntegrity [admittedDuplicate, otherDuplicate]
            public <- projectPackedAdmitted defaultMinIntegrity [withFileKeys [("size", Number 33)] admittedDuplicate]
            (_, served) <- assemblePackedDuplicates (arrange [(TrustedSource, private), (GatedSource, public)])
            servedFiles served `shouldBe` map rebasedDuplicate [admittedDuplicate, otherDuplicate]

    it "refuses entries from a different source or digest" $ do
        source <- projectPackedAdmitted defaultMinIntegrity [admittedDuplicate]
        (plan, sources) <- duplicatePlan [(GatedSource, source)]
        served <- servePacked sources plan Nothing
        servedFiles served `shouldBe` [rebasedDuplicate admittedDuplicate]
        wrongSource <- servePacked (Map.mapKeys (+ 1) sources) plan Nothing
        servedFiles wrongSource `shouldBe` []
        wrongDigest <- servePacked (fmap (\snapshot -> snapshot{snapshotDigest = digestOf "different upstream bytes"}) sources) plan Nothing
        servedFiles wrongDigest `shouldBe` []

    it "refuses ambiguous source coordinates" $ do
        source <- projectPackedAdmitted defaultMinIntegrity [admittedDuplicate]
        (plan, sources) <- duplicatePlan [(GatedSource, source)]
        let repeated document = document{packedFiles = packedFiles document <> packedFiles document}
        served <- servePacked (fmap (fmap repeated) sources) plan Nothing
        servedFiles served `shouldBe` []

    for_ [String "https://files.pythonhosted.org/a\\..\\x", String "https://files.pythonhosted.org/%2e%2e", String "https://files.pythonhosted.org/a%2fb", String "https://files.pythonhosted.org/.. ", Number 1] $ \url ->
        it ("omits a duplicate named entry with an unrebasable URL: " <> show url) $ do
            let filename = "requests-2.34.2.tar.gz"
                valid = simpleFile filename
                refused = withFileKeys [("url", url)] valid
                expected = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> filename))] valid
            for_ [[refused, valid], [valid, refused]] $ \files -> do
                served <- assemblePackedOne files
                servedFiles served `shouldBe` [expected]

    it "omits a duplicate named entry with no URL" $ do
        let filename = "requests-2.34.2.tar.gz"
            valid = simpleFile filename
            missing = object ["filename" .= filename]
            expected = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> filename))] valid
        served <- assemblePackedOne [missing, valid]
        servedFiles served `shouldBe` [expected]

packedSerialiseSpec :: Spec
packedSerialiseSpec = describe "packed served bytes" $
    for_ pypiCorpusPackages $ \package ->
        it ("matches tree assembly for single and merged survivor sets of " <> cpPath package) $ do
            bytes <- readFileBS (cpPath package)
            let name = cpPackage package
                digest = digestOf bytes
            (info, tree) <- expectRight (projectPyPIIndex defaultLimits name bytes)
            (packedInfo, packed) <- expectRight (projectPyPIPackedChunks defaultLimits name [bytes] >>= projectPyPIPacked defaultLimits name)
            packedInfo `shouldBe` info
            let keys = map fst (packedFiles packed)
                positions = [position | ArrayEntry position <- keys]
            keys `shouldBe` map fst (simpleFiles tree)
            length positions `shouldBe` length keys
            zip (-1 : positions) positions `shouldSatisfy` all (uncurry (<))
            fingerprint <$> Filter.serialiseSimpleDocument (fst pypiPacked packed)
                `shouldBe` (fingerprint <$> Filter.serialiseSimpleDocument (fst pypiSimpleCached tree))
            let releases = Map.keysSet (infoVersions info)
                selections = mempty : releases : map Set.singleton (toList (Set.lookupMin releases) <> toList (Set.lookupMax releases))
            for_ [[GatedSource], [TrustedSource, GatedSource]] $ \provenances ->
                for_ selections $ \survivors -> do
                    plan <- expectRight (maybeToRight ("expected a merge plan" :: Text) (mergePackuments [(provenance, Snapshot digest (restrictToSurvivors survivors info)) | provenance <- provenances]))
                    let sources value = Map.fromList (zip [0 ..] [Snapshot digest value | _ <- provenances])
                        treeDoc = fst pypiSimpleCached tree
                        packedDoc = fst pypiPacked packed
                        assembled value = Filter.assembleSimpleDocument mountBase (sources value) plan (Just value)
                        actual = assembled packedDoc
                    snd pypiRendered actual `shouldSatisfy` isJust
                    fingerprint <$> Filter.serialiseSimpleDocument actual
                        `shouldBe` (fingerprint <$> Filter.serialiseSimpleDocument (assembled treeDoc))
                    let changed = Map.adjust (\snapshot -> snapshot{snapshotDigest = digestOf "another source"}) 0 (sources packedDoc)
                        changedTree = Map.adjust (\snapshot -> snapshot{snapshotDigest = digestOf "another source"}) 0 (sources treeDoc)
                    fingerprint <$> Filter.serialiseSimpleDocument (Filter.assembleSimpleDocument mountBase changed plan (Just packedDoc))
                        `shouldBe` (fingerprint <$> Filter.serialiseSimpleDocument (Filter.assembleSimpleDocument mountBase changedTree plan (Just treeDoc)))

relaySpec :: Spec
relaySpec = describe "what the assembly relays from the base document" $ do
    it "keeps meta, so a mirror still reads the serial it revalidates against" $
        fieldAt "meta" (assembleOne allFiles)
            `shouldBe` Just (object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (37059094 :: Int)])

    it "keeps the project name the winning document reported" $
        fieldAt "name" (assembleOne allFiles) `shouldBe` Just (String "requests")

    it "omits unsupported top-level fields" $
        fieldAt "tracks" (assembleOne allFiles) `shouldBe` Nothing

    it "keeps every modelled key on a served file entry, verbatim" $ do
        let entry = servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl"
        (entry >>= KeyMap.lookup "upload-time") `shouldBe` Just (String "2026-05-14T19:25:26Z")
        (entry >>= KeyMap.lookup "requires-python") `shouldBe` Just (String ">=3.10")
        (entry >>= KeyMap.lookup "size") `shouldBe` Just (Number 73075)
        (entry >>= KeyMap.lookup "yanked") `shouldBe` Just (String "withdrawn")
        (entry >>= KeyMap.lookup "hashes") `shouldBe` Just (object ["sha256" .= validSha256])

    it "keeps the provenance URL" $
        (servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl" >>= KeyMap.lookup "provenance")
            `shouldBe` Just (String "https://pypi.org/integrity/x/provenance")

    it "yields an object even for a base document that is not one" $
        assembleSimpleIndex mountBase (Map.singleton 0 (jsonSnapshot (indexOf allFiles))) (planOver [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2.tar.gz"])]) (String "not an index")
            `shouldSatisfy` isObject

survivorSpec :: Spec
survivorSpec = describe "which releases and files the assembly serves" $ do
    it "names the surviving releases in the PEP 700 versions array, and no others" $
        fieldAt "versions" (assembleOne allFiles) `shouldBe` Just (Array (fromList [String "2.34.2"]))

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
        fieldAt "versions" served `shouldBe` Just (Array mempty)
        servedNames served `shouldBe` []

admissionSpec :: Spec
admissionSpec = describe "replaying per-entry admission for duplicate filenames" $ do
    it "refuses an admitted position from another source snapshot" $ do
        source <- projectAdmitted defaultMinIntegrity [admittedDuplicate]
        plan <- expectRight (maybeToRight ("expected merge plan" :: Text) (mergePackuments [(GatedSource, fst <$> source)]))
        let rawSource = snd <$> source
            serve sources = servedFiles (simpleValue (Filter.assembleSimpleIndex mountBase sources plan (compactFixture (indexOf []))))
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

            it "keeps both valid siblings with their own sizes and their original order" $ do
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
            public <- projectAdmitted defaultMinIntegrity [withFileKeys [("size", Number 33)] admittedDuplicate]
            let contributions = arrange [(TrustedSource, private), (GatedSource, public)]
                expectedSource = fst <$> find ((== TrustedSource) . fst . snd) (zip [0 ..] contributions)
            (plan, served) <- assembleDuplicates contributions
            Map.lookup "1" (mpSurvivors plan) `shouldBe` expectedSource
            servedFiles served `shouldBe` map rebasedDuplicate [admittedDuplicate, otherDuplicate]

projectAdmitted :: (IntegrityFloor floor) => floor -> [Value] -> IO (Snapshot (PackageInfo, SimpleDocument))
projectAdmitted = projectAdmittedWith (projectPyPIIndex defaultLimits (mkPackageName PyPI Nothing "requests"))

projectPackedAdmitted :: (IntegrityFloor floor) => floor -> [Value] -> IO (Snapshot (PackageInfo, PackedSimple))
projectPackedAdmitted = projectAdmittedWith projectPackedIndex

projectPackedIndex :: ByteString -> Either MetadataError (PackageInfo, PackedSimple)
projectPackedIndex bytes = projectPyPIPackedChunks defaultLimits requestsName [bytes] >>= projectPyPIPacked defaultLimits requestsName

projectAdmittedWith :: (IntegrityFloor floor) => (ByteString -> Either MetadataError (PackageInfo, document)) -> floor -> [Value] -> IO (Snapshot (PackageInfo, document))
projectAdmittedWith projectIndex floorSpec files = do
    Snapshot digest (info, raw) <- projectJsonSnapshot projectIndex (indexOf files)
    let located = enforceArtifactLocations (ecosystemArtifactAuthorities ["https://files.pythonhosted.org"]) "https://pypi.org" info
        (admitted, _) =
            admitByIntegrity
                floorSpec
                (Reject (Rejection BelowIntegrityFloor "below floor"))
                (Reject (Rejection MissingIntegrity "missing digest"))
                located
    pure (Snapshot digest (admitted, raw))

assembleDuplicates :: [(Provenance, Snapshot (PackageInfo, SimpleDocument))] -> IO (MergePlan, Value)
assembleDuplicates contributions = do
    (plan, sources) <- duplicatePlan contributions
    pure (plan, simpleValue (Filter.assembleSimpleIndex mountBase sources plan (compactFixture (indexOf []))))

duplicatePlan :: [(Provenance, Snapshot (PackageInfo, document))] -> IO (MergePlan, Map SourceId (Snapshot document))
duplicatePlan contributions = do
    plan <- expectRight (maybeToRight ("expected a merge plan" :: Text) (mergePackuments (map (second (fmap fst)) contributions)))
    let sources = Map.fromList [(sid, snd <$> source) | (sid, (_, source)) <- zip [0 ..] contributions]
    pure (plan, sources)

assemblePackedDuplicates :: [(Provenance, Snapshot (PackageInfo, PackedSimple))] -> IO (MergePlan, Value)
assemblePackedDuplicates contributions = do
    (plan, sources) <- duplicatePlan contributions
    (_, base) <- expectRight (projectPackedIndex (encodeStrict (indexOf [])))
    (plan,) <$> servePacked sources plan (Just base)

assemblePackedOne :: [Value] -> IO Value
assemblePackedOne files = do
    let raw = indexOf files
        plan = planFor requestsName [(0, raw)] [("2.34.2", 0)] [("2.34.2", ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"])]
    (_, document) <- expectRight (projectPackedIndex (encodeStrict raw))
    servePacked (Map.singleton 0 (Snapshot (digestOf (encodeStrict raw)) document)) plan (Just document)

servePacked :: Map SourceId (Snapshot PackedSimple) -> MergePlan -> Maybe PackedSimple -> IO Value
servePacked sources plan base = do
    let document = Filter.assembleSimpleDocument mountBase (fmap (fmap (fst pypiPacked)) sources) plan (fst pypiPacked <$> base)
    snd pypiRendered document `shouldSatisfy` isJust
    body <- expectRight (Filter.serialiseSimpleDocument document)
    expectRight (eitherDecode body)

duplicateFilename :: Text
duplicateFilename = "requests-1.0.0.tar.gz"

admittedDuplicate :: Value
admittedDuplicate = withFileKeys [("size", Number 11)] (simpleFile duplicateFilename)

otherDuplicate :: Value
otherDuplicate =
    withFileKeys
        [ ("url", String ("https://files.pythonhosted.org/packages/b1/" <> duplicateFilename))
        , ("size", Number 22)
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
sidecarSpec = describe "the PEP 658 sidecar keys" $ do
    for_ ["dist-info-metadata", "data-dist-info-metadata"] $ \key ->
        for_ [Bool True, Bool False, object ["sha256" .= validSha256]] $ \metadata ->
            it ("drops " <> show key <> " with value " <> show metadata <> " and omits unknown fields") $ do
                let filename = "requests-2.34.2-py3-none-any.whl"
                    original = withFileKeys [("custom-metadata", object ["retained" .= True])] (simpleFile filename)
                    advertised = withFileKeys [(key, metadata)] original
                    expected = withFileKeys [("url", String (mountBase <> "/simple/requests/" <> filename))] (simpleFile filename)
                servedFiles (assembleOne [advertised]) `shouldBe` [expected]

    it "keeps the canonical declaration and drops legacy aliases" $ do
        let entry = servedEntry (assembleOne allFiles) "requests-2.34.2-py3-none-any.whl"
        (entry >>= KeyMap.lookup "core-metadata") `shouldBe` Just (object ["sha256" .= sidecarDigest])
        (entry >>= KeyMap.lookup "dist-info-metadata") `shouldBe` Nothing
        (entry >>= KeyMap.lookup "data-dist-info-metadata") `shouldBe` Nothing

serialiseSpec :: Spec
serialiseSpec = describe "the served bytes" $ do
    it "encodes another ecosystem's document as an empty object" $
        Filter.serialiseSimpleDocument (fst npmCached (object ["name" .= ("requests" :: Text)])) `shouldBe` Right "{}"

    for_ pypiCorpusPackages $ \package ->
        it ("encodes the full and single-release listings of " <> cpPath package <> " as the rendered JSON object") $ do
            bytes <- readFileBS (cpPath package)
            (info, document) <- expectRight (projectPyPIIndex defaultLimits (cpPackage package) bytes)
            let source = Snapshot (digestOf bytes) (fst pypiSimpleCached document)
                releases = Map.keysSet (infoVersions info)
                selections = releases : map Set.singleton (toList (Set.lookupMin releases) <> toList (Set.lookupMax releases))
            for_ selections $ \survivors -> do
                plan <- expectRight (maybeToRight ("expected a merge plan" :: Text) (mergePackuments [(GatedSource, restrictToSurvivors survivors info <$ source)]))
                let served = Filter.assembleSimpleDocument mountBase (Map.singleton 0 source) plan (Just (snapshotValue source))
                assembled <- expectRight (maybeToRight ("expected a PyPI document" :: Text) (snd pypiSimpleCached served))
                simpleFiles assembled `shouldSatisfy` (not . null)
                fingerprint <$> Filter.serialiseSimpleDocument served `shouldBe` Right (fingerprint (encode (simpleValue assembled)))

-- A digest keeps a multi-megabyte mismatch report readable.
fingerprint :: LByteString -> (Int64, ContentDigest)
fingerprint bytes = (BSL.length bytes, digestOf (toStrict bytes))

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
                        , Just (String filename) <- [fieldAt "filename" entry]
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
indexNamed name =
    simpleIndexWith
        name
        [ "meta" .= object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (37059094 :: Int)]
        , "tracks" .= Array mempty
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
        , ("dist-info-metadata", object ["sha256" .= sidecarDigest])
        , ("data-dist-info-metadata", object ["sha256" .= sidecarDigest])
        ]
        (simpleFile filename)

sidecarDigest :: Text
sidecarDigest = "8c384ba3"

servedFiles :: Value -> [Value]
servedFiles served = case fieldAt "files" served of
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

assembleSimpleIndex :: Text -> Map SourceId (Snapshot Value) -> MergePlan -> Value -> Value
assembleSimpleIndex mount sources plan base =
    simpleValue (Filter.assembleSimpleIndex mount (fmap (fmap compactFixture) sources) plan (compactFixture base))

compactFixture :: Value -> SimpleDocument
compactFixture raw =
    either (const (documentFromValue raw)) snd (projectPyPIIndex defaultLimits name (encodeStrict raw))
  where
    name = fromMaybe requestsName $ do
        String reported <- fieldAt "name" raw
        rightToMaybe (projectName reported)

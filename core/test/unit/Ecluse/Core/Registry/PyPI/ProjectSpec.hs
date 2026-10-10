-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI coordinate compatibility, projection,
and allocation growth regressions.
-}
module Ecluse.Core.Registry.PyPI.ProjectSpec (spec) where

import Data.Aeson (Value (Array, String), object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Conc (getAllocationCounter)
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import UnliftIO (evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    Artifact (..),
    Availability (Available, Yanked),
    CodeExecSignal (CodeExecUnknown, NoCodeOnInstall, RunsCodeOnInstall),
    Hash,
    HashAlg (SHA256),
    InvalidEntry (invalidKey, invalidKind, invalidValue),
    InvalidEntryKind (InvalidIndexFile),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashAlg,
    hashValue,
    renderPackageName,
 )
import Ecluse.Core.Registry.PyPI.Project (
    DistributionKind (Sdist, Wheel),
    FileCoordinate (..),
    FilenameMemo,
    fcVersionKey,
    fileCoordinate,
    filenameMemo,
    isCanonicalName,
    projectName,
    readCoordinate,
    readLatestCoordinate,
 )
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Core.Version.Token (withinVersionLength)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, pypiCorpusPackages)
import Ecluse.Test.Json (encodeStrict, fieldAt)
import Ecluse.Test.Package (azureStorageBlob, requestsName, unscopedPyPI, validSha256)
import Ecluse.Test.Registry.PyPI (separatorHeavySdist, simpleFile, simpleIndex, withFileKeys, yankedForms)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIVersion)
import Ecluse.Test.Registry.PyPI.Project (projectSimpleIndexFromValue, readThrough)
import Ecluse.Test.Support (decodeJsonOrFail, expectRight)
import Ecluse.Test.Version (genPyPI)

spec :: Spec
spec = do
    projectNameSpec
    canonicalNameSpec
    coordinateSpec
    memoSpec
    captureSpec
    allocationSpec
    projectionSpec
    versionFoldSpec

projectNameSpec :: Spec
projectNameSpec = describe "projectName" $ do
    it "parses a canonical project name" $
        renderPackageName <$> projectName "requests" `shouldBe` Right "requests"

    it "keeps the published spelling while matching on the canonical key" $ do
        parsed <- expectRight (projectName "Zope.Interface")
        renderPackageName parsed `shouldBe` "Zope.Interface"
        parsed `shouldBe` unscopedPyPI "zope-interface"

    it "refuses an empty name" $
        projectName "" `shouldSatisfy` isLeft

    it "refuses a non-ASCII name, which renders two projects as one" $
        projectName "requ\1077sts" `shouldSatisfy` isLeft

    it "refuses a name that is not a safe path component" $
        projectName "../etc" `shouldSatisfy` isLeft

    it "refuses a name that opens or closes on a separator" $ do
        projectName "-requests" `shouldSatisfy` isLeft
        projectName "requests." `shouldSatisfy` isLeft

    it "refuses a name carrying a character outside PEP 508's grammar" $
        projectName "req~uests" `shouldSatisfy` isLeft

    it "refuses a name over PyPI's own cap" $
        projectName (T.replicate 101 "a") `shouldSatisfy` isLeft

canonicalNameSpec :: Spec
canonicalNameSpec = describe "isCanonicalName" $ do
    it "admits a PEP 503 canonical name" $
        isCanonicalName "zope-interface" `shouldBe` True

    it "refuses a spelling the route would have to redirect" $ do
        isCanonicalName "Zope.Interface" `shouldBe` False
        isCanonicalName "typing_extensions" `shouldBe` False

coordinateSpec :: Spec
coordinateSpec = describe "fileCoordinate" $ do
    it "reads a wheel's release" $
        fileCoordinate requestsName "requests-2.34.2-py3-none-any.whl"
            `shouldBe` Just (coordinate "2.34.2" Wheel)

    it "reads a wheel carrying a build tag" $
        fileCoordinate requestsName "requests-2.34.2-1-py3-none-any.whl"
            `shouldBe` Just (coordinate "2.34.2" Wheel)

    it "cross-normalises a wheel's underscored name onto the PEP 503 canonical key" $
        fileCoordinate azureStorageBlob "azure_storage_blob-12.14.0-py3-none-any.whl"
            `shouldBe` Just (coordinate "12.14" Wheel)

    it "reads a source distribution's release" $
        fileCoordinate requestsName "requests-2.34.2.tar.gz"
            `shouldBe` Just (coordinate "2.34.2" Sdist)

    it "takes the longest name part, so a project whose name carries a separator resolves" $
        fileCoordinate azureStorageBlob "azure-storage-blob-12.14.0.tar.gz"
            `shouldBe` Just (coordinate "12.14" Sdist)

    it "reads a source distribution whose version carries a separator" $
        fileCoordinate requestsName "requests-2.34.2-1.tar.gz"
            `shouldBe` Just (coordinate "2.34.2.post1" Sdist)

    it "preserves mixed separators, ignored edge runs, and every supported archive" $
        forM_ [".tar.gz", ".tgz", ".zip", ".tar.bz2", ".tar.xz"] $ \suffix ->
            fileCoordinate azureStorageBlob ("__Azure..Storage_-Blob---12.14.0" <> suffix)
                `shouldBe` Just (coordinate "12.14" Sdist)

    it "rejects missing boundaries and incomplete project names" $
        forM_ ["azure-storage-blob.tar.gz", "azure-storage-.tar.gz", "azure-storage-other-1.tar.gz"] $ \file ->
            fileCoordinate azureStorageBlob file `shouldBe` Nothing

    it "retains the canonicaliser's empty-name behaviour for domain values outside the project grammar" $ do
        let emptyName = unscopedPyPI ""
        fileCoordinate emptyName "___1.tar.gz" `shouldBe` Just (coordinate "1" Sdist)
        fileCoordinate emptyName "1.tar.gz" `shouldBe` Nothing

    it "canonicalises the release, so two spellings of it key alike" $
        fileCoordinate requestsName "requests-2.34.tar.gz" `shouldBe` fileCoordinate requestsName "requests-2.34.0.tar.gz"

    it "refuses a file naming another project, which on the artifact route is path confusion" $
        fileCoordinate requestsName "urllib3-2.0.0.tar.gz" `shouldBe` Nothing

    it "refuses a file whose name only starts like this project's" $
        fileCoordinate requestsName "requests_toolbelt-1.0.0.tar.gz" `shouldBe` Nothing

    it "refuses a version that is not PEP 440, which no resolver could install" $
        fileCoordinate requestsName "requests-nightly.tar.gz" `shouldBe` Nothing

    it "refuses an archive form a Python index does not serve" $
        fileCoordinate requestsName "requests-2.34.2.egg" `shouldBe` Nothing

    it "refuses a wheel with too few tag parts to be one" $
        fileCoordinate requestsName "requests-2.34.2-py3.whl" `shouldBe` Nothing

memoSpec :: Spec
memoSpec = describe "filenameMemo" $
    for_ memoReads $ \(readerName, readName) -> describe readerName $ do
        it "reads each filename as fileCoordinate does, whatever the memo remembered before" $
            hedgehog $ do
                files <- forAll (Gen.list (Range.linear 0 60) genFilename)
                snd (readThrough readName (filenameMemo azureStorageBlob) files) === map (fileCoordinate azureStorageBlob) files

        it "keeps each file's distribution kind when two files share a version text" $ do
            let (wheel, memo) = readName (filenameMemo requestsName) "requests-2.34.2-py3-none-any.whl"
            wheel `shouldBe` Just (coordinate "2.34.2" Wheel)
            fst (readName memo "requests-2.34.2.tar.gz") `shouldBe` Just (coordinate "2.34.2" Sdist)

        describe "properties" $
            modifyMaxSuccess (const 5000) $
                it "holds for each file name the version that parsing its canonical key builds" $
                    hedgehog $ do
                        earlier <- forAll (Gen.list (Range.linear 0 6) genNamedFile)
                        (spelling, file) <- forAll (Gen.frequency ((2, genNamedFile) : [(1, Gen.element earlier) | not (null earlier)]))
                        let (memo, _) = readThrough readName (filenameMemo azureStorageBlob) (map snd earlier)
                            held = fst (readName memo file)
                            key = maybe "" fcVersionKey held
                            mainPart = T.takeWhile (/= '+') key
                        cover 15 "a wheel" (fmap fcKind held == Just Wheel)
                        cover 15 "a source distribution" (fmap fcKind held == Just Sdist)
                        cover 15 "a name with no coordinate" (isNothing held)
                        cover 10 "a canonical key that differs from the name's version text" (isJust held && key /= spelling)
                        cover 8 "a version text the read met earlier" (isJust held && spelling `elem` map fst earlier)
                        cover 3 "an epoch" (T.any (== '!') key)
                        cover 5 "a pre-release" (T.any (`elem` ['a', 'b', 'c']) mainPart)
                        cover 5 "a post-release" (".post" `T.isInfixOf` key)
                        cover 5 "a dev release" (".dev" `T.isInfixOf` key)
                        cover 5 "a local part" (T.any (== '+') key)
                        cover 1 "a canonical key past the length bound" (isJust held && not (withinVersionLength key))
                        cover 1 "a version text past the length bound" (not (withinVersionLength spelling))
                        mismatched [held] === []

captureSpec :: Spec
captureSpec = describe "on the PyPI captures" $
    for_ ((,) <$> memoReads <*> pypiCorpusPackages) $ \((readerName, readName), package) ->
        it (readerName <> " holds for every file name of " <> cpPath package <> " the version that parsing its canonical key builds") $ do
            document <- readFileBS (cpPath package) >>= decodeJsonOrFail
            let names = [name | Just (Array files) <- [fieldAt "files" document], file <- toList files, Just (String name) <- [fieldAt "filename" file]]
                held = snd (readThrough readName (filenameMemo (cpPackage package)) names)
            Just (length names, length (catMaybes held)) `shouldBe` Map.lookup (cpName package) captureNames
            take 5 (mismatched held) `shouldBe` []
            take 5 [name | (name, found) <- zip names held, found /= fileCoordinate (cpPackage package) name] `shouldBe` []

-- The file names each capture lists, and how many of them name a release of its project.
captureNames :: Map Text (Int, Int)
captureNames = Map.fromList [("boto3", (4242, 4242)), ("numpy", (4232, 4198)), ("requests", (244, 243))]

type MemoRead = FilenameMemo -> Text -> (Maybe FileCoordinate, FilenameMemo)

-- A full read holds every version text, and a selected read only the latest.
memoReads :: [(String, MemoRead)]
memoReads = [("readCoordinate", readCoordinate), ("readLatestCoordinate", readLatestCoordinate)]

-- The coordinates whose version is not the one 'mkVersion' builds from their canonical key.
mismatched :: [Maybe FileCoordinate] -> [(Version, Version)]
mismatched held = [(version, rebuilt) | Just found <- held, let version = fcVersion found, let rebuilt = mkVersion PyPI (fcVersionKey found), version /= rebuilt]

coordinate :: Text -> DistributionKind -> FileCoordinate
coordinate = FileCoordinate . mkVersion PyPI

genFilename :: Gen Text
genFilename = snd <$> genNamedFile

-- A version text and a file name around it. Repeated spellings make later files reuse a remembered text.
genNamedFile :: Gen (Text, Text)
genNamedFile = do
    project <- Gen.frequency [(9, Gen.element ["azure-storage-blob", "azure_storage_blob", "Azure.Storage.Blob"]), (1, Gen.element ["azure-storage", "requests", ""])]
    version <-
        Gen.frequency
            [ (2, Gen.element ["1.0", "1.0.0", "1.0RC1", "1.0rc1", "v1.0", "1.0-1", "2!1.0", "1.0+Local.7", "nightly", "", "1..0"])
            , (2, genPyPI)
            , (4, genSpelling)
            , (2, genSpellingAtBound)
            ]
    suffix <- Gen.frequency [(4, pure "-py3-none-any.whl"), (2, pure "-1-py3-none-any.whl"), (3, pure ".tar.gz"), (2, pure ".zip"), (1, pure ".egg")]
    pure (version, project <> "-" <> version <> suffix)

-- A version in spellings PEP 440 normalises: a prefix, an epoch, zeros, each label and separator.
genSpelling :: Gen Text
genSpelling = do
    prefix <- Gen.element ["", "", "", "v", "V"]
    epoch <- sometimes ((<> "!") <$> number)
    release <- T.intercalate "." <$> Gen.list (Range.linear 1 4) number
    pre <- sometimes (labelled ["a", "b", "c", "rc", "alpha", "beta", "pre", "preview", "RC", "Alpha"])
    post <- sometimes (Gen.choice [labelled ["post", "rev", "r", "POST"], ("-" <>) <$> number])
    dev <- sometimes (labelled ["dev", "DEV"])
    localPart <- sometimes (("+" <>) <$> (T.intercalate <$> Gen.element separators <*> Gen.list (Range.linear 1 3) (Gen.element ["ubuntu", "Local", "7", "07", "cp39"])))
    pure (prefix <> epoch <> release <> pre <> post <> dev <> localPart)
  where
    sometimes part = Gen.frequency [(2, pure ""), (1, part)]
    labelled labels = do
        leading <- Gen.element ("" : separators)
        label <- Gen.element labels
        trailing <- Gen.element ("" : separators)
        count <- Gen.frequency [(3, number), (1, pure "")]
        pure (leading <> label <> trailing <> count)
    number = Gen.element ["0", "1", "2", "10", "01", "007"]
    separators = [".", "-", "_"]

-- A version within a few characters of the 1024-character bound. Most suffixes grow in the canonical key.
genSpellingAtBound :: Gen Text
genSpellingAtBound = do
    suffix <- Gen.element ["a", "rc", "c1", ".post", "-1", "dev", "", ".dev0"]
    spare <- Gen.element [-1, 0, 0, 0, 1, 3]
    pure (T.replicate (1024 - T.length suffix - spare) "1" <> suffix)

allocationSpec :: Spec
allocationSpec = describe "filename allocation growth" $
    it "keeps malformed rejection below quadratic growth as separators double" $ do
        allocations <- forM [1000, 2000, 4000, 8000] $ \count -> do
            files <- evaluate (force [separatorHeavySdist "requests" count (show repetition) | repetition <- [1 :: Int .. 5]])
            allocationBefore <- getAllocationCounter
            rejected <- evaluate (length (filter (isNothing . fileCoordinate requestsName) files))
            allocationAfter <- getAllocationCounter
            rejected `shouldBe` length files
            pure (allocationBefore - allocationAfter)
        -- Each window has two reads accurate to about 4 KiB. Weight both windows by the 3x ratio.
        -- The resulting 32 KiB allowance covers counter granularity without admitting quadratic growth.
        forM_ (zip allocations (drop 1 allocations)) $ \(smaller, larger) ->
            larger `shouldSatisfy` (<= 3 * smaller + 32768)

projectionSpec :: Spec
projectionSpec = describe "projectSimpleIndexFromValue" $ do
    it "drops malformed separator-heavy upstream files while retaining normal releases" $
        forM_ [1000, 2000, 4000, 8000] $ \count -> do
            let filename = separatorHeavySdist "requests" count "projection"
            info <- shouldProject requestsName (indexOf [simpleFile filename, sdistFile "2.34.2", wheelFile "2.34.2"])
            artifactNames info "2.34.2" `shouldBe` Just ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"]
            map invalidKey (infoInvalidEntries info) `shouldBe` [filename]

    it "projects one release per canonical version, carrying every file of it" $ do
        info <- shouldProject requestsName (indexOf [sdistFile "2.34.2", wheelFile "2.34.2", wheelFile "2.34.1"])
        Map.keys (infoVersions info) `shouldBe` ["2.34.1", "2.34.2"]
        artifactNames info "2.34.2" `shouldBe` Just ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"]

    it "merges two spellings of one release into one entry" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34", wheelFile "2.34.0"])
        Map.keys (infoVersions info) `shouldBe` ["2.34"]

    it "projects sha256 through the shared hash vocabulary" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34.2"])
        map (\h -> (hashAlg h, hashValue h)) (artifactHashes info "2.34.2") `shouldBe` [(SHA256, validSha256)]

    it "drops a digest under an algorithm this build does not know" $ do
        info <- shouldProject requestsName (indexOf [withFileKeys [("hashes", object ["blake2b_256" .= ("ab" :: Text)])] (wheelFile "2.34.2")])
        artifactHashes info "2.34.2" `shouldBe` []

    it "drops a file whose requires-python or provenance is not text, though neither is kept" $ do
        let unshaped key = withFileKeys [(key, toJSON (5 :: Int))] (sdistFile "2.34.2")
            index = indexOf [unshaped "requires-python", unshaped "provenance", wheelFile "2.34.2"]
        info <- shouldProject requestsName index
        (streamed, _) <- expectRight (projectPyPIIndex defaultLimits requestsName (encodeStrict index))
        for_ [info, streamed] $ \projected -> do
            artifactNames projected "2.34.2" `shouldBe` Just ["requests-2.34.2-py3-none-any.whl"]
            map invalidKind (infoInvalidEntries projected) `shouldBe` [InvalidIndexFile, InvalidIndexFile]

    it "points latest at the highest release, preferring a final over a pre-release" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34.2", wheelFile "3.0.0rc1"])
        fmap renderVersion (Map.lookup "latest" (infoDistTags info)) `shouldBe` Just "2.34.2"

    it "drops a file naming no release of this project and records it" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34.2", simpleFile "urllib3-2.0.0.tar.gz"])
        Map.keys (infoVersions info) `shouldBe` ["2.34.2"]
        map invalidKind (infoInvalidEntries info) `shouldBe` [InvalidIndexFile]
        map invalidKey (infoInvalidEntries info) `shouldBe` ["urllib3-2.0.0.tar.gz"]

    it "reduces a dropped file's location to its authority, which reaches a log line" $ do
        info <- shouldProject requestsName (indexOf [simpleFile "urllib3-2.0.0.tar.gz"])
        map invalidValue (infoInvalidEntries info) `shouldBe` [toJSON ("files.pythonhosted.org:443" :: Text)]

    it "agrees the index's self-reported name with the request through the shared check" $ do
        info <- shouldProject requestsName (simpleIndex "Requests" [wheelFile "2.34.2"])
        renderPackageName (infoName info) `shouldBe` "Requests"
        artifactNames info "2.34.2" `shouldBe` Just ["requests-2.34.2-py3-none-any.whl"]

    it "refuses an index self-reporting another project, carrying the reported name" $
        projectSimpleIndexFromValue requestsName (simpleIndex "urllib3" [])
            `shouldBe` Right (NameMismatch "urllib3")

    it "refuses an index that reports no usable name at all" $
        projectSimpleIndexFromValue requestsName (simpleIndex "" []) `shouldSatisfy` isLeft

versionFoldSpec :: Spec
versionFoldSpec = describe "the version-level folds over a release's files" $ do
    it "runs code on install when any file is a source distribution" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34.2", sdistFile "2.34.2"])
        pkgInstallCode <$> Map.lookup "2.34.2" (infoVersions info) `shouldSatisfy` maybe False runsCode

    it "runs no code on install for a release offering wheels alone" $ do
        info <- shouldProject requestsName (indexOf [wheelFile "2.34.2"])
        pkgInstallCode <$> Map.lookup "2.34.2" (infoVersions info) `shouldBe` Just NoCodeOnInstall

    it "ages a release from its newest file, so a late wheel does not shorten the quarantine" $ do
        info <-
            shouldProject
                requestsName
                ( indexOf
                    [ withFileKeys [("upload-time", toJSON ("2026-01-01T00:00:00Z" :: Text))] (sdistFile "2.34.2")
                    , withFileKeys [("upload-time", toJSON ("2026-06-01T00:00:00Z" :: Text))] (wheelFile "2.34.2")
                    ]
                )
        fmap show (pkgPublishedAt =<< Map.lookup "2.34.2" (infoVersions info))
            `shouldBe` Just ("2026-06-01 00:00:00 UTC" :: Text)

    it "withdraws a release only when every file of it is yanked" $ do
        partly <- shouldProject requestsName (indexOf [yanked (sdistFile "2.34.2"), wheelFile "2.34.2"])
        pkgAvailability <$> Map.lookup "2.34.2" (infoVersions partly) `shouldBe` Just Available
        wholly <- shouldProject requestsName (indexOf [yanked (sdistFile "2.34.2"), yanked (wheelFile "2.34.2")])
        pkgAvailability <$> Map.lookup "2.34.2" (infoVersions wholly) `shouldBe` Just Yanked

    for_ yankedForms $ \(form, member, withdrawn) ->
        it ("reads " <> form <> " on every file, and on either file of two, alike on the full and the selected read") $ do
            let marked = withFileKeys member
                expected = if withdrawn then Yanked else Available
            bothReads (indexOf [marked (sdistFile "2.34.2"), marked (wheelFile "2.34.2")]) `shouldReturn` (Just expected, Just expected)
            bothReads (indexOf [marked (sdistFile "2.34.2"), wheelFile "2.34.2"]) `shouldReturn` (Just Available, Just Available)
            bothReads (indexOf [sdistFile "2.34.2", marked (wheelFile "2.34.2")]) `shouldReturn` (Just Available, Just Available)

-- The availability of release 2.34.2 on the production full read and on its selected read.
bothReads :: Value -> IO (Maybe Availability, Maybe Availability)
bothReads index = do
    (full, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
    selected <- expectRight (projectPyPIVersion defaultLimits requestsName (mkVersion PyPI "2.34.2") body)
    pure (pkgAvailability <$> Map.lookup "2.34.2" (infoVersions full), pkgAvailability <$> selected)
  where
    body = encodeStrict index

shouldProject :: PackageName -> Value -> IO PackageInfo
shouldProject name value = case projectSimpleIndexFromValue name value of
    Left err -> fail (show err)
    Right (NameMismatch reported) -> fail (toString ("index self-reported " <> reported))
    Right (Projected info) -> pure info

runsCode :: CodeExecSignal -> Bool
runsCode = \case
    RunsCodeOnInstall _ -> True
    NoCodeOnInstall -> False
    CodeExecUnknown -> False

artifactsOf :: PackageInfo -> Text -> [Artifact]
artifactsOf info version = maybe [] (toList . pkgArtifacts) (Map.lookup version (infoVersions info))

artifactNames :: PackageInfo -> Text -> Maybe [Text]
artifactNames info version = map artFilename . toList . pkgArtifacts <$> Map.lookup version (infoVersions info)

artifactHashes :: PackageInfo -> Text -> [Hash]
artifactHashes info version = concatMap artHashes (take 1 (artifactsOf info version))

indexOf :: [Value] -> Value
indexOf = simpleIndex "requests"

wheelFile :: Text -> Value
wheelFile version = simpleFile ("requests-" <> version <> "-py3-none-any.whl")

sdistFile :: Text -> Value
sdistFile version = simpleFile ("requests-" <> version <> ".tar.gz")

yanked :: Value -> Value
yanked = withFileKeys [("yanked", toJSON ("withdrawn" :: Text))]

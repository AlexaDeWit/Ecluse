-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Field and backing-allocation checks for selected releases.
Cache integration checks live in the parent cache spec.
-}
module Ecluse.Core.Server.Cache.VersionWeightSpec (spec) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw), VersionRead (vrUpstreamLatest, vrVersion))
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Version (mkVersion, versionKey)
import Ecluse.Test.Package (sampleArtifact, sampleDetails, thingName, unsafeHash, v1_0_0, validSha256)
import Ecluse.Test.Snapshot (untaggedRead)

spec :: Spec
spec = describe "selected-release accounting" $ do
    it "keeps a cached absence smaller than a present release" $ do
        weighVersion (untaggedRead Nothing) `shouldBe` 1024
        weighVersion (untaggedRead (Just baseline)) `shouldSatisfy` (> weighVersion (untaggedRead Nothing))

    it "charges a retained raw version object on top of the release" $
        weighVersion (carrying (toJSON (T.replicate 4096 "x"))) `shouldSatisfy` (> weighVersion (untaggedRead (Just baseline)) + 4096)

    it "weighs the raw object by one walk of its structure, never by encoding it" $ do
        -- Each added key costs exactly its structural allowance under the shared expansion, so
        -- the weight is a function of the tree's shape rather than of any rendered bytes.
        let withKeys n = carrying (object [Key.fromText ("dep-" <> show i) .= ("^1.0.0" :: Text) | i <- [100 .. 99 + n :: Int]])
            wireOf n = 2 + n * (4 + T.length "dep-100" + 2 + T.length "^1.0.0")
        weighVersion (withKeys 100) - weighVersion (withKeys 0) `shouldBe` expandWireBytes (wireOf 100) - expandWireBytes (wireOf 0)
        weighVersion (withKeys 1000) `shouldSatisfy` (> weighVersion (withKeys 100))
        weighVersion (withKeys 0) `shouldSatisfy` (> weighVersion (untaggedRead (Just baseline)))

    it "charges the retained upstream release tag on top of the release" $ do
        let tagged = (untaggedRead (Just baseline)){vrUpstreamLatest = Just (mkVersion Npm "1.0.0")}
        weighVersion tagged `shouldSatisfy` (> weighVersion (untaggedRead (Just baseline)))

    it "charges each artifact even when its fields share allocations" $ do
        let singleWeight = weight baseline{pkgArtifacts = oneArtifact :| []}
            repeatedWeight = weight baseline{pkgArtifacts = oneArtifact :| replicate 99 oneArtifact}
        repeatedWeight `shouldSatisfy` (> singleWeight + 99 * 256)

    it "charges a Text slice for the complete retained backing allocation" $ do
        let backing = T.replicate 65536 "x"
            sliced = T.take 1 backing
            copied = T.copy sliced
            withUrl url = baseline{pkgArtifacts = oneArtifact{artUrl = url} :| []}
        weight (withUrl sliced) `shouldSatisfy` (>= weight (withUrl copied) + 65535)

    it "charges UTF-8 bytes rather than character counts" $ do
        let withUrl url = baseline{pkgArtifacts = oneArtifact{artUrl = url} :| []}
        weight (withUrl (T.replicate 1024 "\x1f600"))
            `shouldSatisfy` (>= weight (withUrl (T.replicate 1024 "a")) + 3072)

    it "charges a keyed identity for its complete retained backing allocation" $ do
        let backing = T.replicate 65536 "x"
            sliced = T.take 1 backing
            withKey key = baseline{pkgArtifacts = oneArtifact{artEntryKey = ObjectEntry key} :| []}
        weight (withKey sliced) `shouldSatisfy` (>= weight (withKey (T.copy sliced)) + 65535)

    for_ retainedFields $ \(label, change) ->
        it ("charges retained " <> label) $
            weight (change baseline) `shouldSatisfy` (> weight baseline)

    for_ parsedVersions $ \(label, ecosystem, raw, minimumParsedGrowth) ->
        it ("reserves parsed representation bytes for " <> label) $ do
            -- Copy both inputs so backing growth equals their encoded length difference.
            let compact = T.copy "1.0.0"
                expanded = T.copy raw
                compactVersion = mkVersion ecosystem compact
                expandedVersion = mkVersion ecosystem expanded
                payloadGrowth = BS.length (encodeUtf8 expanded) - BS.length (encodeUtf8 compact)
                compactWeight = weight baseline{pkgVersion = compactVersion}
                expandedWeight = weight baseline{pkgVersion = expandedVersion}
            versionKey compactVersion `shouldSatisfy` isJust
            versionKey expandedVersion `shouldSatisfy` isJust
            expandedWeight - compactWeight - payloadGrowth `shouldSatisfy` (>= minimumParsedGrowth)

weight :: PackageDetails -> Int
weight = weighVersion . untaggedRead . Just

-- The baseline release read carrying the given raw version object.
carrying :: Value -> VersionRead
carrying raw = (untaggedRead (Just baseline)){vrVersion = (\doc -> doc{vdRaw = Just (fst npmCached raw)}) <$> vrVersion (untaggedRead (Just baseline))}

oneArtifact :: Artifact
oneArtifact = sampleArtifact{artHashes = [], artInterpreter = Nothing, artProvenance = Nothing}

baseline :: PackageDetails
baseline = (sampleDetails thingName v1_0_0){pkgArtifacts = oneArtifact :| [], pkgLicenses = [], pkgPublisher = Nothing, pkgTrust = Untrusted}

retainedFields :: [(String, PackageDetails -> PackageDetails)]
retainedFields =
    [ ("canonical, display, and base names", \p -> p{pkgName = mkPackageName Npm Nothing longText})
    , ("scope", \p -> p{pkgName = mkPackageName Npm (Just (mkScope longText)) "name"})
    , ("install reason", \p -> p{pkgInstallCode = RunsCodeOnInstall longText})
    , ("timestamp Integer payload", \p -> p{pkgPublishedAt = Just (UTCTime (ModifiedJulianDay (10 ^ (5000 :: Int))) 0)})
    , ("trust evidence text", \p -> p{pkgTrust = Trusted (OtherEvidence longText :| [])})
    , ("trust evidence nodes", \p -> p{pkgTrust = Trusted (Signed :| [Attested, MfaPublished])})
    , ("deprecation reason", \p -> p{pkgAvailability = Deprecated longText})
    , ("yank reason", \p -> p{pkgAvailability = Yanked (Just longText)})
    , ("licences", \p -> p{pkgLicenses = replicate 100 longText})
    , ("publisher name", \p -> p{pkgPublisher = Just (Person longText Nothing Nothing)})
    , ("publisher email", \p -> p{pkgPublisher = Just (Person "a" (Just longText) Nothing)})
    , ("publisher URL", \p -> p{pkgPublisher = Just (Person "a" Nothing (Just longText))})
    , ("entry coordinates", changeArtifact (\a -> a{artEntryKey = ObjectEntry longText}))
    , ("filenames", changeArtifact (\a -> a{artFilename = longText}))
    , ("artifact URLs", changeArtifact (\a -> a{artUrl = longText}))
    , ("wheel tags", changeArtifact (\a -> a{artKind = Wheel longText}))
    , ("gem platforms", changeArtifact (\a -> a{artKind = Gem longText}))
    , ("hash nodes and digest backing allocations", changeArtifact (\a -> a{artHashes = [unsafeHash SHA256 (T.take 64 (validSha256 <> longText))]}))
    , ("interpreter constraints", changeArtifact (\a -> a{artInterpreter = Just longText}))
    , ("provenance URLs", changeArtifact (\a -> a{artProvenance = Just longText}))
    ]

parsedVersions :: [(String, Ecosystem, Text, Int)]
parsedVersions =
    -- A retained list cell uses three eight-byte words on the supported 64-bit targets.
    -- A 900-digit Integer needs over 300 payload bytes, apart from the raw text.
    [ ("npm dense prerelease tokens", Npm, "1.0.0-" <> T.intercalate "." (replicate 200 "a"), 199 * 24)
    , ("PyPI dense release tokens", PyPI, T.intercalate "." (replicate 200 "1"), 199 * 24)
    , ("RubyGems dense numeric and text tokens", RubyGems, T.replicate 100 "1a", 199 * 24)
    , ("npm long numeric prerelease components", Npm, "1.0.0-" <> T.intercalate "." (replicate 50 (T.replicate 18 "9")), 49 * 24)
    , ("PyPI long numeric component", PyPI, T.replicate 900 "9", 300)
    , ("RubyGems long numeric component", RubyGems, T.replicate 900 "9", 300)
    ]

changeArtifact :: (Artifact -> Artifact) -> PackageDetails -> PackageDetails
changeArtifact change details = details{pkgArtifacts = change oneArtifact :| []}

longText :: Text
longText = T.replicate 4096 "x"

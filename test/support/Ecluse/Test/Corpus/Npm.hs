-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Synthetic npm metadata for complexity measurements.
Realistic measurements use the committed captures in "Ecluse.Test.Corpus".
-}
module Ecluse.Test.Corpus.Npm (
    benchPackageName,
    syntheticPackumentBytes,
) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BSL
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Test.Package (validSha1, validSha512Sri)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)

benchPackageText :: Text
benchPackageText = "bench-pkg"

-- | The generated document's npm package identity.
benchPackageName :: PackageName
benchPackageName = mkPackageName Npm Nothing benchPackageText

syntheticPackumentValue :: Int -> Value
syntheticPackumentValue versionCount =
    packumentValue
        benchPackageText
        (versionText (max 0 (versionCount - 1)))
        [(versionText i, versionObject i) | i <- indices]
        timeEntries
        ["maintainers" .= toJSON [object ["name" .= ("ecluse-bench" :: Text)]]]
  where
    indices :: [Int]
    indices = [0 .. versionCount - 1]

    versionKeyOf :: Int -> Key.Key
    versionKeyOf = Key.fromText . versionText

    timeEntries :: [(Key.Key, Value)]
    timeEntries =
        (Key.fromText "created", toJSON publishedAt)
            : (Key.fromText "modified", toJSON publishedAt)
            : [(versionKeyOf i, toJSON publishedAt) | i <- indices]

versionText :: Int -> Text
versionText i = "1.0." <> show i

publishedAt :: Text
publishedAt = "2020-01-01T00:00:00.000Z"

versionObject :: Int -> Value
versionObject i =
    versionValue
        ( (versionSpec benchPackageText (versionText i) (tarballUrl i))
            { vsIntegrity = Just validSha512Sri
            , vsShasum = Just validSha1
            , vsHasInstallScript = True
            , vsExtraPairs =
                [ "dependencies"
                    .= object
                        [ "left-pad" .= ("^1.0.0" :: Text)
                        , "lodash" .= ("^4.17.0" :: Text)
                        ]
                , "scripts" .= object ["postinstall" .= ("node ./build.js" :: Text)]
                ]
            }
        )

tarballUrl :: Int -> Text
tarballUrl i =
    "https://registry.npmjs.org/"
        <> benchPackageText
        <> "/-/"
        <> benchPackageText
        <> "-"
        <> versionText i
        <> ".tgz"

-- | Encode the positive release count as registry response bytes.
syntheticPackumentBytes :: Int -> ByteString
syntheticPackumentBytes = BSL.toStrict . Aeson.encode . syntheticPackumentValue

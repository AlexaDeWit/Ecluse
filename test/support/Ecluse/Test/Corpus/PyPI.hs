-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Synthetic PEP 691 metadata for complexity measurements.
Each release offers a wheel and a source distribution through the shipped file projection.
-}
module Ecluse.Test.Corpus.PyPI (
    syntheticIndexBytes,
    benchProject,
) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)

-- | The generated project's normalised PyPI identity.
benchProject :: PackageName
benchProject = mkPackageName PyPI Nothing "bench-pkg"

-- | Encode a positive count of releases with two distribution files per release.
syntheticIndexBytes :: Int -> ByteString
syntheticIndexBytes count =
    BSL.toStrict $
        encode $
            object
                [ "name" .= ("bench-pkg" :: Text)
                , "meta" .= object ["api-version" .= ("1.4" :: Text)]
                , "files" .= concatMap files [0 .. count - 1]
                ]
  where
    files index =
        [ withFileKeys ["upload-time" .= ("2020-01-01T00:00:00Z" :: Text)] (simpleFile ("bench_pkg-1.0." <> show index <> suffix))
        | suffix <- ["-py3-none-any.whl", ".tar.gz"]
        ]

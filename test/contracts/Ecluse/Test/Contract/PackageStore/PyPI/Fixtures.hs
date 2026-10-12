-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TemplateHaskell #-}

-- | Frozen independent Python archives keep contract discovery free of fixture acquisition.
module Ecluse.Test.Contract.PackageStore.PyPI.Fixtures (primaryRelease, siblingRelease) where

import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map
import Ecluse.Test.Contract.PackageStore

-- | Three distinct distributions and their independently pinned consumer fields.
primaryRelease :: PackageStoreRelease
primaryRelease =
    PackageStoreRelease
        "1.0.0"
        ( Map.fromList
            [ ("contract-package-1.0.0.tar.gz", ">=3.8")
            , ("contract_package-1.0.0-py2.py3-none-any.whl", ">=3.9")
            , ("contract_package-1.0.0-py3-none-any.whl", ">=3.10")
            ]
        )
        ( PackageStoreArtifact "contract-package-1.0.0.tar.gz" $(embedFile "test/fixtures/package-store/pypi/contract-package-1.0.0.tar.gz") "46487cf118491e8398bf60625279901261964bf60b359a04cf91871118097664"
            :| [ PackageStoreArtifact "contract_package-1.0.0-py2.py3-none-any.whl" $(embedFile "test/fixtures/package-store/pypi/contract_package-1.0.0-py2.py3-none-any.whl") "e616e282ddccc1c7d6ce3867daefa86ba731169d30f5fb399b92501184fc914f"
               , PackageStoreArtifact "contract_package-1.0.0-py3-none-any.whl" $(embedFile "test/fixtures/package-store/pypi/contract_package-1.0.0-py3-none-any.whl") "80f377a4e81e35375a55d74d2d9548c15c489e1ca9984c4a94070eb8ca59b008"
               ]
        )

-- | Three distinct distributions and their independently pinned consumer fields.
siblingRelease :: PackageStoreRelease
siblingRelease =
    PackageStoreRelease
        "2.0.0"
        ( Map.fromList
            [ ("contract-package-2.0.0.tar.gz", ">=3.11")
            , ("contract_package-2.0.0-py2.py3-none-any.whl", ">=3.12")
            , ("contract_package-2.0.0-py3-none-any.whl", ">=3.13")
            ]
        )
        ( PackageStoreArtifact "contract-package-2.0.0.tar.gz" $(embedFile "test/fixtures/package-store/pypi/contract-package-2.0.0.tar.gz") "6fdbf61c11c775813f7b265140f6192562226a97ef2bed567dbc2df0e13c8971"
            :| [ PackageStoreArtifact "contract_package-2.0.0-py2.py3-none-any.whl" $(embedFile "test/fixtures/package-store/pypi/contract_package-2.0.0-py2.py3-none-any.whl") "abe5675e018ede2b407e9397d72421c2ae07c2df707364e9a222118c8d9f4ecd"
               , PackageStoreArtifact "contract_package-2.0.0-py3-none-any.whl" $(embedFile "test/fixtures/package-store/pypi/contract_package-2.0.0-py3-none-any.whl") "94737c10fd025e83cd809e653f3018529e90f2ece7f1b551edf4c3388abce9c6"
               ]
        )

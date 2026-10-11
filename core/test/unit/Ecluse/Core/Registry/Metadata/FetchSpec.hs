-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native documents for each adapter running the shared metadata transport contract.
module Ecluse.Core.Registry.Metadata.FetchSpec (spec) where

import Data.Aeson ((.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Universe.Class (Universe (universe))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterEcosystem, adapterMetadata))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Test.Contract.Metadata.Fetch (ReadFixture (..), metadataFetchContract)
import Ecluse.Test.Registry.Npm (packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.PyPI (filesNamed, simpleIndex)
import Test.Hspec
import Prelude hiding (universe)

spec :: Spec
spec = do
    it "holds documents for every ecosystem with an adapter" $
        map rdEcosystem registered `shouldBe` filter (isJust . adapterFor) universe
    for_ registered $ \eco -> describe (show (rdEcosystem eco)) (metadataFetchContract eco)

registered :: [ReadFixture]
registered =
    [ ReadFixture
        { rdEcosystem = adapterEcosystem npmAdapter
        , rdRead = metadataRead (adapterMetadata npmAdapter)
        , rdDocument = \name padding -> "{\"name\":\"" <> name <> "\",\"versions\":{}" <> paddingMember padding
        , rdReleases = \base versions ->
            packumentValue
                "thing"
                "1.2.3"
                [(version, versionValue (versionSpec "thing" version (base <> "/thing/-/thing-" <> version <> ".tgz"))) | version <- versions]
                [Key.fromText version .= ("2020-01-01T00:00:00.000Z" :: Text) | version <- versions]
                []
        }
    , ReadFixture
        { rdEcosystem = adapterEcosystem pypiAdapter
        , rdRead = metadataRead (adapterMetadata pypiAdapter)
        , rdDocument = \name padding -> "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"" <> name <> "\",\"files\":[]" <> paddingMember padding
        , rdReleases = \_ versions -> simpleIndex "thing" (filesNamed ["thing-" <> version <> ".tar.gz" | version <- versions])
        }
    ]
  where
    paddingMember padding = ",\"_padding\":\"" <> BS.replicate padding 0x78 <> "\"}"

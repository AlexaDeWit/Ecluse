-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Policy timestamps and artifact coordinates stay joined to their source release key.
module Ecluse.Core.Registry.Npm.StreamingProjectionSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson (Value (String), object, (.=))
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions))
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmField (..))
import Ecluse.Core.Registry.Npm.StreamingProjection
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "finishProjection" $
    it "joins release timestamps independently of source map order" $ do
        let name = unscopedNpm "thing"
            release = object ["name" .= ("thing" :: Text), "version" .= ("1.0.0" :: Text), "dist" .= object ["tarball" .= ("https://source.example/one.tgz" :: Text)]]
            fields =
                [ [NameField (String "thing")]
                , [BeginContainer VersionsContainer, VersionField "1.0.0" (Just release), IgnoredField]
                , [BeginContainer TimeContainer, TimeField "1.0.0" (String "2020-01-01T00:00:00Z"), IgnoredField]
                ]
            project ordered = do
                collected <- expectRight (foldM (collectField defaultLimits name) emptyProjection ordered)
                fst <$> expectRight (finishProjection defaultLimits name "See source" collected)
        forward <- project (concat fields)
        backward <- project (concat (reverse fields))
        forward `shouldBe` backward
        (Map.lookup "1.0.0" (infoVersions forward) >>= pkgPublishedAt) `shouldSatisfy` isJust

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.EgressSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artUrl),
    InvalidEntry (invalidKind),
    InvalidEntryKind (InvalidVersionManifest),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoInvalidEntries, infoVersions),
    PackageName,
    mkPackageName,
 )
import Ecluse.Core.Registry.Egress (enforceArtifactScheme)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Security (defaultLimits)

{- | The https-only egress policy applied to a projected package's artifact URLs: a
same-host @http@ URL is upgraded to https, a foreign-host @http@ URL is dropped and
recorded (the #486 drop-and-record contract), and a non-https (test/dev loopback)
upstream leaves artifact URLs untouched.

'enforceArtifactScheme' is ecosystem-generic (it reasons over 'PackageInfo' alone), but
these cases reach it through the npm projector, which is simply the cheapest way to
build a populated 'PackageInfo'. npm is the fixture here, not the subject.
-}
spec :: Spec
spec = describe "enforceArtifactScheme (https-only artifact-URL normalisation)" $ do
    let name = unscoped "thing"
        body tarball =
            BL.toStrict . encode $
                object
                    [ "name" .= ("thing" :: Text)
                    , "versions"
                        .= object
                            [ "1.0.0"
                                .= object
                                    [ "name" .= ("thing" :: Text)
                                    , "version" .= ("1.0.0" :: Text)
                                    , "dist" .= object ["tarball" .= (tarball :: Text)]
                                    ]
                            ]
                    ]
        projected upstream tarball = enforceArtifactScheme upstream . fst <$> projectNpmManifest defaultLimits name (body tarball)
        tarballOf info = (\(art :| _) -> artUrl art) . pkgArtifacts <$> Map.lookup "1.0.0" (infoVersions info)

    it "upgrades a same-host http artifact URL to https (https upstream)" $
        case projected "https://registry.npmjs.org" "http://registry.npmjs.org/thing/-/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

    it "keeps an https artifact URL unchanged (https upstream)" $
        case projected "https://registry.npmjs.org" "https://cdn.example.net/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "https://cdn.example.net/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

    it "drops a foreign-host http artifact URL and records it (https upstream)" $
        case projected "https://registry.npmjs.org" "http://evil.example.test/thing-1.0.0.tgz" of
            Right info -> do
                Map.lookup "1.0.0" (infoVersions info) `shouldBe` Nothing
                map invalidKind (infoInvalidEntries info) `shouldBe` [InvalidVersionManifest]
            Left err -> expectationFailure ("did not project: " <> show err)

    it "leaves artifact URLs untouched for a non-https (loopback) upstream" $
        case projected "http://127.0.0.1:8080" "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz" of
            Right info -> tarballOf info `shouldBe` Just "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz"
            Left err -> expectationFailure ("did not project: " <> show err)

unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

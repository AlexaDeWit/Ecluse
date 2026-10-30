-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The bodies npm 11.17.0 sent to a publish route for @\@acme/widget@, against a stub serving Écluse's view, and hand-built variants.
module Ecluse.Test.Registry.Npm.Publish (
    npmReleaseDocument,
    npmTaggedRelease,
    npmRepeatedAttachments,
    npmMisfiledVersion,
    npmProvenanceRelease,
    npmDeprecateDocument,
    npmStarDocument,
    npmDependentRelease,
    npmExternalDependencyRefusal,
) where

import Data.Aeson (Value (Null), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)

-- | An @npm publish@ of version 2.0.0, with the given @_id@, top-level @name@, and version @name@.
npmReleaseDocument :: Text -> Text -> Text -> LByteString
npmReleaseDocument documentId documentName versionName = releaseDocument documentId documentName versionName latestTag []

-- | An @npm publish@ of @\@acme/widget@ 2.0.0 with the given @dist-tags@ member. @--tag beta@ sends @{"beta":"2.0.0"}@.
npmTaggedRelease :: Value -> LByteString
npmTaggedRelease tags = releaseDocument widget widget widget tags []

-- | A hand-built release with @_attachments@ twice, which a first-wins reader and a last-wins store read differently.
npmRepeatedAttachments :: LByteString
npmRepeatedAttachments =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"2.0.0\":{\"name\":\"@acme/widget\",\"version\":\"2.0.0\",\"deprecated\":\"gone\"}},\"_attachments\":{\"@acme/widget-2.0.0.tgz\":{\"data\":\"AA==\"}},\"_attachments\":{}}"

-- | A hand-built release filed under the key 9.9.9 whose own @version@ field says 1.0.0.
npmMisfiledVersion :: LByteString
npmMisfiledVersion =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"dist-tags\":{\"latest\":\"9.9.9\"},\"versions\":{\"9.9.9\":{\"name\":\"@acme/widget\",\"version\":\"1.0.0\"}},\"_attachments\":{\"@acme/widget-9.9.9.tgz\":{\"data\":\"AA==\"}}}"

-- | An @npm publish --provenance@ of @\@acme/widget@ 2.0.0, with libnpmpublish's bundle attachment around a placeholder bundle.
npmProvenanceRelease :: LByteString
npmProvenanceRelease = releaseDocument widget widget widget latestTag [Key.fromText (widget <> "-2.0.0.sigstore") .= provenanceAttachment]

-- | An @npm deprecate \@acme/widget\@1.0.0@ with the given message, which is empty for an undeprecate.
npmDeprecateDocument :: Text -> LByteString
npmDeprecateDocument message =
    encode . object $
        [ "name" .= widget
        , "versions"
            .= object
                [ "1.0.0" .= servedVersion "1.0.0" "sha512-AAAA" "0000000000000000000000000000000000000000" ["deprecated" .= message]
                , "1.1.0" .= servedVersion "1.1.0" "sha512-BBBB" "1111111111111111111111111111111111111111" []
                ]
        , "time" .= object ["1.0.0" .= ("2026-01-01T00:00:00.000Z" :: Text), "1.1.0" .= ("2026-02-01T00:00:00.000Z" :: Text)]
        , "dist-tags" .= object ["latest" .= ("1.1.0" :: Text)]
        ]

-- | An @npm star \@acme/widget@. The served view has no @_id@ or @_rev@, so npm sends only the users.
npmStarDocument :: LByteString
npmStarDocument = "{\"users\":{\"e2e-user\":true}}"

-- | An @npm publish@ of @\@acme/widget@ 2.0.0 whose version document also carries the given members.
npmDependentRelease :: [Pair] -> LByteString
npmDependentRelease extra = releaseDocumentWith (releaseManifestWith widget extra) widget widget latestTag []

-- | The @400@ reason for a release whose dependency in the field names the source kind, such as @a git source@.
npmExternalDependencyRefusal :: Text -> Text -> Text -> Text
npmExternalDependencyRefusal dependency field kind =
    "the publish body's dependency '" <> dependency <> "' in " <> field <> " names " <> kind <> ", which a client would fetch from outside this proxy: depend on a version this proxy serves"

widget :: Text
widget = "@acme/widget"

latestTag :: Value
latestTag = object ["latest" .= ("2.0.0" :: Text)]

releaseDocument :: Text -> Text -> Text -> Value -> [Pair] -> LByteString
releaseDocument documentId documentName versionName = releaseDocumentWith (releaseManifestWith versionName []) documentId documentName

releaseDocumentWith :: Value -> Text -> Text -> Value -> [Pair] -> LByteString
releaseDocumentWith manifest documentId documentName tags extraAttachments =
    encode . object $
        [ "_id" .= documentId
        , "name" .= documentName
        , "dist-tags" .= tags
        , "versions" .= object ["2.0.0" .= manifest]
        , "access" .= Null
        , "_attachments" .= object ((Key.fromText (documentName <> "-2.0.0.tgz") .= tarballAttachment) : extraAttachments)
        ]

releaseManifestWith :: Text -> [Pair] -> Value
releaseManifestWith versionName extra =
    object $
        [ "name" .= versionName
        , "version" .= ("2.0.0" :: Text)
        , "files" .= ["package.json" :: Text]
        , "readme" .= ("ERROR: No README data found!" :: Text)
        , "_id" .= (versionName <> "@2.0.0")
        , "_nodeVersion" .= ("24.19.0" :: Text)
        , "_npmVersion" .= ("11.17.0" :: Text)
        , "dist"
            .= object
                [ "integrity" .= ("sha512-qLQG8OhjGuVQCR6C64XFpmmcAOD9ZnRdD6Lj/ZKG+4pnQ2sp9E2UC7+zTYkjoVpMf5v26rwq4fWBMhqbVH+lvg==" :: Text)
                , "shasum" .= ("a155ea534e072f11ef0f89f7da5858aa89ec01d1" :: Text)
                , "tarball" .= ("http://127.0.0.1:48731/" <> versionName <> "/-/" <> versionName <> "-2.0.0.tgz")
                ]
        ]
            <> extra

tarballAttachment :: Value
tarballAttachment =
    object
        [ "content_type" .= ("application/octet-stream" :: Text)
        , "data" .= ("H4sIAAAAAAAC/ytITM5OTE/VL4DQelnF+XkMVAYGBgZmJiYK2MRBwNDAWIHB2NTMwNDQzMwAqA7IMDUxANIMdAClxSWJRUCnUMGTQKAAp4cIqFbKS8xNVbJSckhMzk3VL89MSU8tUdJRKkstKs7MzwNKGOkZ6BkARdIyc1KLlayilZCTilJsLRfDKBgFo2AUjIKhBwCjSYUvAAgAAA==" :: Text)
        , "length" .= (157 :: Int)
        ]

provenanceAttachment :: Value
provenanceAttachment =
    object
        [ "content_type" .= bundleType
        , "data" .= bundle
        , "length" .= length (toString bundle)
        ]
  where
    bundleType = "application/vnd.dev.sigstore.bundle.v0.3+json" :: Text
    bundle = "{\"mediaType\":\"" <> bundleType <> "\",\"verificationMaterial\":{},\"dsseEnvelope\":{}}"

servedVersion :: Text -> Text -> Text -> [Pair] -> Value
servedVersion version integrity shasum extra =
    object $
        [ "name" .= widget
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= ("http://127.0.0.1:48731/@acme/widget/-/widget-" <> version <> ".tgz")
                , "integrity" .= integrity
                , "shasum" .= shasum
                ]
        ]
            <> extra

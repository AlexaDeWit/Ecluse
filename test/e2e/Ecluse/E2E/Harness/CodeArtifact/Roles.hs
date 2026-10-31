-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Product role environments for Dredger contracts over the CodeArtifact fixture's mirror and cache.
module Ecluse.E2E.Harness.CodeArtifact.Roles (
    PublicationAuthority (..),
    codeArtifactSweepRoleEnv,
    codeArtifactProxyRoleEnv,
    refusedAwsIdentity,
    refusedIdentityReports,
) where

import Data.Text qualified as T
import Data.Universe.Class qualified as Universe

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Adapter (uploadEndpointFor)
import Ecluse.Core.Registry.Adapter.Capability (UploadEndpointRequirement (ExplicitUploadEndpoint))
import Ecluse.E2E.Harness.Advisories (advisoryBucket)
import Ecluse.E2E.Harness.CodeArtifact (codeArtifactRepositoryUrl)
import Ecluse.E2E.Harness.Docker (ministackAwsEnv)
import Ecluse.E2E.Harness.Stub (publicStubRoute, stubUrl)

-- | The upload path, publication credential and identities a serving or worker role adds.
data PublicationAuthority = PublicationAuthority
    { paUploadPath :: Text
    -- ^ Appended to the mirror repository URL for native uploads, where the protocol takes an @uploadUrl@.
    , paCredential :: [(Text, Text)]
    -- ^ Keys under the publication target's @CODE_ARTIFACT@ tag, in the order they are set.
    , paFirstParty :: Text
    , paServiceToken :: Text
    -- ^ The token the product's own edge requires.
    }

-- | A Dredger role over the fixture's mirror and cache repositories, with every other mount off.
codeArtifactSweepRoleEnv :: Ecosystem -> [(Text, Text)]
codeArtifactSweepRoleEnv ecosystem = roleEnv ecosystem (codeArtifactRepositoryUrl ecosystem "mirror")

-- Every role's mounts, with the mirror target uploading to the given address where the protocol takes one.
roleEnv :: Ecosystem -> Text -> [(Text, Text)]
roleEnv ecosystem mirrorUpload =
    [ ("ECLUSE_SERVER__PORT", "4873")
    , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873")
    ]
        <> [(mountKey other "ENABLED", if other == ecosystem then "true" else "false") | other <- Universe.universe]
        <> [ (mountKey ecosystem "PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl (publicStubRoute ecosystem))
           , (mountKey ecosystem "PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", codeArtifactRepositoryUrl ecosystem "cache")
           , (mountKey ecosystem "MIRROR_TARGET__CODE_ARTIFACT__URL", codeArtifactRepositoryUrl ecosystem "mirror")
           , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
           , ("SSL_CERT_FILE", "/certs/bundle.pem")
           ]
        <> [(mountKey ecosystem "MIRROR_TARGET__CODE_ARTIFACT__UPLOAD_URL", mirrorUpload) | separateUpload ecosystem]
        <> ministackAwsEnv

-- | Serving and worker roles keep mirror state remotely across restarts and publish to the mirror.
codeArtifactProxyRoleEnv :: Ecosystem -> PublicationAuthority -> [(Text, Text)]
codeArtifactProxyRoleEnv ecosystem authority =
    filter ((/= "ECLUSE_SERVER__PUBLIC_URL") . fst) (roleEnv ecosystem upload)
        <> [ ("ECLUSE_MIRROR_STATE__URL", "s3://" <> advisoryBucket <> "/" <> ecosystemName ecosystem <> "-dredger")
           , (publication "URL", mirror)
           ]
        <> [(publication "UPLOAD_URL", upload) | separateUpload ecosystem]
        <> [(publication key, value) | (key, value) <- paCredential authority]
        <> [ (mountKey ecosystem "FIRST_PARTY", paFirstParty authority)
           , ("ECLUSE_SERVER__AUTH_TOKEN", paServiceToken authority)
           , ("ECLUSE_CACHE__TTL", "1")
           ]
  where
    mirror = codeArtifactRepositoryUrl ecosystem "mirror"
    upload = mirror <> paUploadPath authority
    publication key = mountKey ecosystem ("PUBLICATION_TARGET__CODE_ARTIFACT__" <> key)

-- | Keys to append to a role's environment so it signs as an identity other than the fixture's @test@, which the control plane refuses.
refusedAwsIdentity :: [(Text, Text)]
refusedAwsIdentity = [("AWS_ACCESS_KEY_ID", "AKIAREFUSEDIDENTITY"), ("AWS_SECRET_ACCESS_KEY", "refused")]

-- | The boot refusals a Dredger signing as 'refusedAwsIdentity' reports: the token mint and the private upstream probe.
refusedIdentityReports :: Ecosystem -> [Text]
refusedIdentityReports ecosystem =
    [ "credential provider codeartifact for "
        <> mountKey ecosystem "PRIVATE_UPSTREAM"
        <> ", "
        <> mountKey ecosystem "MIRROR_TARGET"
        <> " failed to mint an initial token at boot: AWS request failed at codeartifact.us-east-1.amazonaws.com:443: HTTP 403 AccessDenied"
    , mountKey ecosystem "PRIVATE_UPSTREAM" <> " could not be read: this role's identity is refused codeartifact:DescribeRepository"
    ]

-- The load requires uploadUrl where the protocol uploads separately and refuses it elsewhere.
separateUpload :: Ecosystem -> Bool
separateUpload ecosystem = uploadEndpointFor ecosystem == Just ExplicitUploadEndpoint

mountKey :: Ecosystem -> Text -> Text
mountKey ecosystem key = "ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName ecosystem) <> "__" <> key

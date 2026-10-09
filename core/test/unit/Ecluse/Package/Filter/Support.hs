-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | References and hostile inputs for the artifact location check. The references re-derive the
upstream's authority and host for every artifact, lower-case whole URLs, and check the filename
before and after normalising, so a spec can hold the per-document check and its drop records to them.
-}
module Ecluse.Package.Filter.Support (
    -- * References
    referenceResolveArtifact,
    referenceEnforceArtifactLocations,

    -- * Hostile inputs
    upstreams,
    genHostileUrl,
    genHostileInfo,
) where

import Data.Aeson (Value (String))
import Data.Char (toUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    InvalidEntry,
    InvalidEntryKind (InvalidDistTag, InvalidIndexFile, InvalidVersionManifest),
    PackageDetails (pkgArtifacts),
    PackageInfo (..),
    mkInvalidEntry,
 )
import Ecluse.Core.Package.Filter.Internal (ArtifactRefusal (..))
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Security (AllowedHostPorts, artifactAuthorityHonoured, authorityLabel, hostAddress, hostPortAddress)
import Ecluse.Core.Text (urlFilename)
import Ecluse.Test.Package (npmVersion, sampleArtifact, sampleDetails, thingName)

-- | The reference for 'Ecluse.Core.Package.Filter.Internal.resolveArtifact', given the upstream base URL.
referenceResolveArtifact :: AllowedHostPorts -> Text -> Artifact -> Either ArtifactRefusal Artifact
referenceResolveArtifact ecosystemHosts upstreamBaseUrl art = do
    checkFilename art
    normalised <- normaliseScheme
    checkFilename normalised
    if artifactAuthorityHonoured ecosystemHosts originAuthority (hostPortAddress (artUrl normalised))
        then Right normalised
        else Left (refusal "artifact authority is neither the serving upstream nor a declared artifact host" (artUrl normalised))
  where
    originAuthority = hostPortAddress upstreamBaseUrl

    checkFilename candidate =
        when (isNothing (urlFilename (artUrl candidate))) $
            Left (refusal "artifact URL has no safe filename" (artUrl candidate))

    normaliseScheme = case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> Right art
        Just upstreamHost -> case referenceTarballUrl upstreamHost (artUrl art) of
            Right resolved -> Right art{artUrl = resolved}
            Left reason -> Left (refusal reason (artUrl art))

    refusal reason url = ArtifactRefusal{refusedFile = artFilename art, refusedReason = reason, refusedUrl = url}

httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | "https://" `T.isPrefixOf` T.toLower baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing

-- 'Ecluse.Core.Security.Egress.resolveTarballUrl' with whole-URL lower-casing, returning the text.
referenceTarballUrl :: Text -> Text -> Either Text Text
referenceTarballUrl upstreamHost url
    | "https://" `T.isPrefixOf` lowered = referenceRegistryUrl url
    | "http://" `T.isPrefixOf` lowered =
        if hostAddress url == upstreamHost
            then referenceRegistryUrl ("https://" <> T.drop 7 url)
            else Left ("dist.tarball is http on a host other than the upstream registry: " <> authorityLabel url)
    | otherwise = Left ("dist.tarball is not an https URL: " <> authorityLabel url)
  where
    lowered = T.toLower url

-- 'Ecluse.Core.Security.Egress.mkRegistryUrl' with whole-URL lower-casing, returning the text.
referenceRegistryUrl :: Text -> Either Text Text
referenceRegistryUrl raw
    | T.null trimmed = Left "expected a non-empty https URL"
    | "https://" `T.isPrefixOf` T.toLower trimmed = Right trimmed
    | otherwise = Left ("registry URL must use https (got " <> trimmed <> ")")
  where
    trimmed = T.strip raw

{- | The reference for 'Ecluse.Core.Package.Filter.enforceArtifactLocations': every artifact checked by
'referenceResolveArtifact', and the drop records in version-key order, then file order.
-}
referenceEnforceArtifactLocations :: AllowedHostPorts -> Text -> PackageInfo -> PackageInfo
referenceEnforceArtifactLocations ecosystemHosts upstreamBaseUrl info =
    info{infoVersions = kept, infoInvalidEntries = infoInvalidEntries info <> drops}
  where
    (kept, drops) = Map.foldrWithKey step (Map.empty, []) (infoVersions info)

    step rawVersion details (keptAcc, dropAcc) =
        case referencePartition ecosystemHosts upstreamBaseUrl rawVersion details of
            (Just survivors, fileDrops) -> (Map.insert rawVersion survivors keptAcc, fileDrops <> dropAcc)
            (Nothing, emptied) -> (keptAcc, emptied <> dropAcc)

referencePartition :: AllowedHostPorts -> Text -> Text -> PackageDetails -> (Maybe PackageDetails, [InvalidEntry])
referencePartition ecosystemHosts upstreamBaseUrl rawVersion details =
    case nonEmpty (rights resolved) of
        Just survivors -> (Just details{pkgArtifacts = survivors}, map fileDrop refusals)
        Nothing -> (Nothing, map versionDrop (take 1 refusals))
  where
    resolved = map (referenceResolveArtifact ecosystemHosts upstreamBaseUrl) (toList (pkgArtifacts details))
    refusals = lefts resolved
    fileDrop refusal = mkInvalidEntry InvalidIndexFile (refusedFile refusal) (String (authorityLabel (refusedUrl refusal))) (refusedReason refusal)
    versionDrop refusal = mkInvalidEntry InvalidVersionManifest rawVersion (String (authorityLabel (refusedUrl refusal))) (refusedReason refusal)

{- | Upstream base URLs, respelled and loopback included, each with its ecosystem's artifact hosts and
authority spellings its reads honour.
-}
upstreams :: [(Text, [Text], [Text])]
upstreams =
    [ ("https://registry.npmjs.org", npmArtifactHosts, ["registry.npmjs.org", "REGISTRY.npmjs.ORG", "registry.npmjs.org:443"])
    , ("HTTPS://Registry.NPMJS.org", npmArtifactHosts, ["registry.npmjs.org", "Registry.NPMJS.org:443"])
    , ("https://pypi.org/simple", pypiArtifactHosts, ["pypi.org", "files.pythonhosted.org", "Files.PythonHosted.org:443"])
    , ("https://pypi.org:443/simple", [], ["pypi.org", "PyPI.org:443"])
    , ("https://[::1]:8443", ["https://files.pythonhosted.org"], ["[::1]:8443", "files.pythonhosted.org"])
    , ("http://127.0.0.1:8080", [], ["127.0.0.1:8080"])
    ]

-- | A URL assembled from hostile spellings of each part, with an occasional stray character.
genHostileUrl :: [Text] -> Gen Text
genHostileUrl served = do
    leading <- Gen.frequency [(9, pure ""), (1, Gen.element whitespace)]
    scheme <- Gen.frequency [(5, respell "https://"), (3, respell "http://"), (1, Gen.element oddSchemes)]
    userinfo <- Gen.frequency [(6, pure ""), (1, Gen.element ["deploy:hunter2@", "user@", "a@b@", "@", "%40@"])]
    authority <- Gen.frequency [(3, Gen.element served), (1, (<>) <$> Gen.element otherHosts <*> Gen.element ports)]
    suffix <- Gen.frequency [(8, pure ""), (1, Gen.element authoritySuffixes)]
    directory <- Gen.frequency [(5, Gen.element directories), (1, Gen.element delimitedDirectories)]
    file <- Gen.frequency [(5, Gen.element plainFiles), (2, Gen.element hostileFiles)]
    trailing <- Gen.frequency [(9, pure ""), (1, Gen.element whitespace)]
    let url = leading <> scheme <> userinfo <> authority <> suffix <> directory <> file <> trailing
    Gen.frequency [(8, pure url), (1, strayChar url)]
  where
    respell = fmap toText . traverse (\c -> Gen.element (ordNub [c, toUpper c])) . T.unpack
    strayChar url = do
        at <- Gen.int (Range.linear 0 (T.length url))
        stray <- Gen.unicode
        pure (T.take at url <> T.singleton stray <> T.drop at url)
    whitespace = [" ", "\t", "\n", "\xA0", "\x3000"]
    oddSchemes = ["", "//", "ftp://", "https:/", "https:", "HTTPS//", "\x130https://", "\x212Ahttps://", "https\xFF1A//", "http\xFF1A//"]
    otherHosts = ["registry.npmjs.org", "pypi.org", "files.pythonhosted.org", "[::1]", "127.0.0.1", "cdn.example.net", "evil.test", "[2606:4700::1111]", "[::1", "b\xFC\&cher.example", "xn--bcher-kva.example", "", "169.254.169.254"]
    ports = ["", ":443", ":8443", ":8080", ":", ":0", ":0443", ":65536", ":\xFF18\xFF10"]
    authoritySuffixes = ["#@evil.test", "?@registry.npmjs.org", "@evil.test", ":443@evil.test", "\\@evil.test"]
    directories = ["", "/thing/-", "/packages/ab/cd", "/%2e%2e", "/a b"]
    delimitedDirectories = ["?/", "#/", "/a?b/", "?/registry.npmjs.org", "#/files.pythonhosted.org", "/a#b@evil.test/c"]
    plainFiles = ["/thing-1.0.0.tgz", "/numpy-2.0.0-cp312-cp312-manylinux_2_17_x86_64.whl", "/requests-2.32.3.tar.gz", "/%C3%BCber-1.0.tar.gz", "/\xFC\&ber-1.0.tar.gz"]
    hostileFiles = ["/", "", "/.", "/..", "/%2e%2e", "/a%2Fb.whl", "/pkg%00.tgz", "/%E2%80%AE.whl", "/%ff.whl", "/a\\b.tgz", "/x.tgz?sig=abc", "/x.tgz#frag", "/?q", "/ ", "/. ", "/.. ", "/x.tgz\t"]

{- | A document of one to three versions, each with one to three files at hostile URLs under distinct
names, and one earlier drop record.
-}
genHostileInfo :: [Text] -> Gen PackageInfo
genHostileInfo served = do
    count <- Gen.int (Range.constant 1 3)
    versions <- traverse (\key -> (key,) <$> genDetails key) (take count ["1.0.0", "1.1.0", "2.0.0"])
    pure
        PackageInfo
            { infoName = thingName
            , infoVersions = Map.fromList versions
            , infoDistTags = Map.empty
            , infoInvalidEntries = [mkInvalidEntry InvalidDistTag "next" (String "not-a-version") "earlier drop"]
            }
  where
    genDetails key = do
        firstUrl <- genHostileUrl served
        laterUrls <- Gen.list (Range.constant 0 2) (genHostileUrl served)
        pure (sampleDetails thingName (npmVersion key)){pkgArtifacts = artifactAt 0 firstUrl :| zipWith artifactAt [1 ..] laterUrls}
    artifactAt :: Int -> Text -> Artifact
    artifactAt position url = sampleArtifact{artFilename = "file-" <> show position <> ".tgz", artUrl = url}

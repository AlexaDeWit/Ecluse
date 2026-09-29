-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The per-artifact location check, held to a per-artifact reference on generated hostile URLs.
module Ecluse.Core.Package.Filter.InternalSpec (spec) where

import Data.Char (toUpper)
import Data.Text qualified as T
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (Artifact (artUrl))
import Ecluse.Core.Package.Filter.Internal (ArtifactRefusal (refusedReason, refusedUrl), artifactOrigin, resolveArtifact)
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Security (ecosystemArtifactAuthorities)
import Ecluse.Core.Text (afterFirst)
import Ecluse.Package.Filter.Support (referenceResolveArtifact)
import Ecluse.Test.Package (sampleArtifact)

-- | The check returns what the per-artifact reference returns.
spec :: Spec
spec = describe "resolveArtifact (against the per-artifact reference)" $
    modifyMaxSuccess (const 5000) $
        it "returns what the reference returns, on generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                url <- forAll (genHostileUrl served)
                let hosts = ecosystemArtifactAuthorities hostUrls
                    art = sampleArtifact{artUrl = url}
                    expected = referenceResolveArtifact hosts upstreamBaseUrl art
                    refusal = leftToMaybe expected
                    refusedFor reason = maybe False (T.isPrefixOf reason . refusedReason) refusal
                    refusedAsWritten = fmap refusedUrl refusal == Just url
                    afterAuthority = T.dropWhile (`notElem` ['/', '?', '#']) (afterFirst "://" url)
                cover 5 "kept as written" (fmap artUrl expected == Right url)
                cover 2 "kept with its scheme upgraded" (maybe False ((/= url) . artUrl) (rightToMaybe expected))
                cover 5 "refused for its filename as written" (refusedFor "artifact URL has no safe filename" && refusedAsWritten)
                cover 1 "refused for its filename once normalised" (refusedFor "artifact URL has no safe filename" && not refusedAsWritten)
                cover 5 "refused for its scheme" (refusedFor "dist.tarball is")
                cover 5 "refused for its authority" (refusedFor "artifact authority")
                cover 2 "a query or fragment ends the authority" (T.take 1 afterAuthority `elem` ["?", "#"])
                cover 2 "an @ follows the end of the authority" ("@" `T.isInfixOf` afterAuthority)
                resolveArtifact (artifactOrigin hosts upstreamBaseUrl) art === expected

{- Upstream base URLs, respelled and loopback included, each with its ecosystem's artifact hosts and
authority spellings its reads honour. -}
upstreams :: [(Text, [Text], [Text])]
upstreams =
    [ ("https://registry.npmjs.org", npmArtifactHosts, ["registry.npmjs.org", "REGISTRY.npmjs.ORG", "registry.npmjs.org:443"])
    , ("HTTPS://Registry.NPMJS.org", npmArtifactHosts, ["registry.npmjs.org", "Registry.NPMJS.org:443"])
    , ("https://pypi.org/simple", pypiArtifactHosts, ["pypi.org", "files.pythonhosted.org", "Files.PythonHosted.org:443"])
    , ("https://pypi.org:443/simple", [], ["pypi.org", "PyPI.org:443"])
    , ("https://[::1]:8443", ["https://files.pythonhosted.org"], ["[::1]:8443", "files.pythonhosted.org"])
    , ("http://127.0.0.1:8080", [], ["127.0.0.1:8080"])
    ]

-- A URL assembled from hostile spellings of each part, with an occasional stray character.
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

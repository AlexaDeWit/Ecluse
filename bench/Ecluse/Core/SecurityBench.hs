-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure response bounds and the artifact location check over each ecosystem's wire documents and
projected releases. Parsed documents enter the nesting guard without timing their decoding.
-}
module Ecluse.Core.SecurityBench (benchmarks) where

import Ecluse.Test.Security.Limits (checkVersionCount)

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryInfo, entryName, forcedTree, syntheticPackageInfo, unHeldTree)
import Ecluse.Core.Package (PackageInfo, artUrl, infoVersions, pkgArtifacts)
import Ecluse.Core.Package.Filter.Internal (
    ArtifactOrigin (..),
    AuthorityVerdict,
    artifactOrigin,
    locateArtifact,
    locateArtifacts,
    locateNextArtifact,
    noAuthorityVerdict,
 )
import Ecluse.Core.Security (
    AuthorityText,
    BodyLimit (MetadataBodyLimit),
    LimitError,
    artifactAuthorityHonoured,
    authorityText,
    boundedRead,
    defaultLimits,
    hostPortAddress,
    maxMetadataBytes,
 )
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)
import Ecluse.Core.Text (urlFilename)
import Ecluse.Test.Corpus (CaptureUpstream (..))
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Test.Tasty (withResource)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfAppIO, whnfIO)
import Test.Tasty.HUnit (assertBool, assertEqual, (@?=))

-- | Exercise bounded reads, both structural guards and the location check on real and synthetic inputs.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "security guards" $
        [env (pure bodyChunks) $ \chunks -> bench "boundedRead (8 MiB body, 64 KiB chunks)" (whnfIO (boundedReadDepth chunks))]
            <> [ bgroup
                    (entryName entry)
                    [ withResource (forcedTree ecosystem document) (const pass) $ \held ->
                        bench "checkNestingDepth" (whnfAppIO (fmap (ebNestingDepth ecosystem . unHeldTree)) held)
                    , bench "checkVersionCount" (whnf versionCountDepth (entryInfo entry))
                    , bench "artifact locations" (whnf (locatedArtifacts origin) (entryInfo entry))
                    , withResource (assertWalksAgree origin (entryInfo entry)) (const pass) $ \_ ->
                        bench "artifact locations (shared verdict)" (whnf (sharedLocatedArtifacts origin) (entryInfo entry))
                    , bgroup
                        "artifact locations by part"
                        [ bench "filename" (whnf (countUrls (isJust . urlFilename)) (entryInfo entry))
                        , bench "https normalisation" (whnf (countUrls (normalisesToItself origin)) (entryInfo entry))
                        , bench "authority text" (whnf sameAuthorityTexts (entryInfo entry))
                        , bench "authority host and port" (whnf (countUrls (isJust . hostPortAddress)) (entryInfo entry))
                        , bench "authority verdict" (whnf (countUrls (authorityHonoured origin)) (entryInfo entry))
                        ]
                    ]
               | entry@(_, _, _, document) <- ebCorpus ecosystem
               ]
            <> [ bench
                    "checkNestingDepth (synthetic / 100000)"
                    (whnf (either (const (-1)) (ebNestingDepth ecosystem)) (ebReadDocument ecosystem (ebSynthetic ecosystem 100000)))
               , bench
                    "checkVersionCount (synthetic / 2000)"
                    (whnf (either (const (-1)) versionCountDepth) (syntheticPackageInfo ecosystem 2000))
               ]
  where
    origin = artifactOrigin (upstreamAuthorities (ebUpstream ecosystem)) (upstreamOrigin (ebUpstream ecosystem))

boundedReadDepth :: [ByteString] -> IO Int
boundedReadDepth chunks = do
    cursor <- newIORef chunks
    result <- boundedRead (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (popChunk cursor)
    pure $! either limitErrorCode (BS.length . snd) result
  where
    popChunk cursor = atomicModifyIORef' cursor $ \case
        [] -> ([], BS.empty)
        (c : cs) -> (cs, c)

bodyChunks :: [ByteString]
bodyChunks = replicate 128 (BS.replicate 65536 0x61)

versionCountDepth :: PackageInfo -> Int
versionCountDepth info = either limitErrorCode (const 1) (checkVersionCount defaultLimits info)

limitErrorCode :: LimitError -> Int
limitErrorCode _ = -1

-- The projected artifacts whose URL passes the check against the capture's registry, each result forced.
locatedArtifacts :: ArtifactOrigin -> PackageInfo -> Int
locatedArtifacts origin info =
    length [() | details <- Map.elems (infoVersions info), art <- toList (pkgArtifacts details), Right !_ <- [locateArtifact origin (artUrl art)]]

data Walk = Walk AuthorityVerdict Int

-- The same count, with each authority's verdict carried to the URLs after it.
sharedLocatedArtifacts :: ArtifactOrigin -> PackageInfo -> Int
sharedLocatedArtifacts origin info = located
  where
    Walk _ located = foldUrls step (Walk noAuthorityVerdict 0) info
    step (Walk previous count) url = case locateNextArtifact origin previous url of
        (verdict, Right !_) -> Walk verdict (count + 1)
        (verdict, Left _) -> Walk verdict count

-- Fails a capture whose two walks differ on any URL, or whose URLs never repeat an authority.
assertWalksAgree :: ArtifactOrigin -> PackageInfo -> IO ()
assertWalksAgree origin info = do
    assertBool "no URL follows another on the same authority" (length (group (map authorityText urls)) < length urls)
    for_ (zip3 urls (map (locateArtifact origin) urls) (locateArtifacts origin urls)) $ \(url, perUrl, shared) ->
        assertEqual (toString url) perUrl shared
    sharedLocatedArtifacts origin info @?= locatedArtifacts origin info
  where
    urls = reverse (foldUrls (flip (:)) [] info)

-- A strict fold over the projected artifact URLs, in the order 'locatedArtifacts' checks them.
foldUrls :: (acc -> Text -> acc) -> acc -> PackageInfo -> acc
foldUrls step start info = foldl' step start [artUrl art | details <- Map.elems (infoVersions info), art <- toList (pkgArtifacts details)]
{-# INLINE foldUrls #-}

countUrls :: (Text -> Bool) -> PackageInfo -> Int
countUrls holds = foldUrls (\count url -> if holds url then count + 1 else count) 0
{-# INLINE countUrls #-}

-- The https normalisation and the comparison with the URL as written, which the check makes next.
normalisesToItself :: ArtifactOrigin -> Text -> Bool
normalisesToItself origin url = case originHttpsHost origin of
    Nothing -> True
    Just upstreamHost -> either (const False) ((== url) . registryUrlText) (resolveTarballUrl upstreamHost url)

data Run = Run AuthorityText Int

-- What a reused verdict costs: each URL's authority text, compared with the previous URL's.
sameAuthorityTexts :: PackageInfo -> Int
sameAuthorityTexts info = repeated
  where
    Run _ repeated = foldUrls step (Run (authorityText "") 0) info
    step (Run previous count) url =
        let authority = authorityText url
         in Run authority (if authority == previous then count + 1 else count)

-- The authority test as 'locateArtifact' runs it for every URL.
authorityHonoured :: ArtifactOrigin -> Text -> Bool
authorityHonoured origin = artifactAuthorityHonoured (originHosts origin) (originAuthority origin) . hostPortAddress

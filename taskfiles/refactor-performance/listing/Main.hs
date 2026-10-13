-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | One bounded package-list read with prepared bytes and an independent output oracle.
module Main (main) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import GHC.Stats (getRTSStatsEnabled)
import System.Info (arch)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes, streamValue))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Core.Server.MemoryModel.Probe (HeapSample (..), sample)
import Ecluse.Test.Contract.Manifest (digest)
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks)
import ListingAdapter (readListing)

main :: IO ()
main =
    getArgs >>= \case
        [path, oracle] -> run path oracle
        _ -> fail "expected listing input and expected names"

run :: FilePath -> FilePath -> IO ()
run path oracle = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "listing probe requires RTS -T")
    body <- BS.readFile path
    expected <- T.lines <$> readFileText oracle
    let chunks = captureChunks body
        size = BS.length body
    void (evaluate (sum (map BS.length chunks)))
    void (evaluate (sum (map T.length expected)))
    next <- heldChunks chunks
    before <- sample
    (held, successful, count, consumed, elapsed) <- measureListing expected size next
    released <- sample
    capabilities <- getNumCapabilities
    LBS.putStr . encode . object $
        [ "status" .= (if successful then "ok" else "invalid-output" :: Text)
        , "architecture" .= arch
        , "capabilities" .= capabilities
        , "sha256" .= digest body
        , "wireBytes" .= consumed
        , "packages" .= count
        , "oracleSha256" .= digest (encodeUtf8 (T.unlines expected))
        , "allocated" .= (sampleAllocated held - sampleAllocated before)
        , "retained" .= (toInteger (live held) - toInteger (live before))
        , "released" .= (toInteger (live released) - toInteger (live before))
        , "peak" .= samplePeakLive held
        , "read_project_ns" .= elapsed
        ]
    unless successful exitFailure

{-# NOINLINE measureListing #-}
measureListing :: [Text] -> Int -> IO ByteString -> IO (HeapSample, Bool, Int, Int, Word64)
measureListing expected size next = do
    started <- getMonotonicTimeNSec
    streamed <- readListing (MetadataBodyLimit size) next >>= either (fail . toString) pure
    names <- either (fail . toString . show) pure (streamValue streamed)
    void (evaluate (sum (map (T.length . renderPackageName) names)))
    finished <- getMonotonicTimeNSec
    (held, valid) <- bracket (newStablePtr names) freeStablePtr $ \root -> do
        retained <- sample
        actual <- deRefStablePtr root
        pure (retained, map renderPackageName actual == expected)
    successful <- evaluate (valid && streamBytes streamed == size && not (null names))
    count <- evaluate (length names)
    consumed <- evaluate (streamBytes streamed)
    elapsed <- evaluate (finished - started)
    pure (held, successful, count, consumed, elapsed)

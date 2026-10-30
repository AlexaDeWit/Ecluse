-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Tar framing and inventory bounds that hold whatever members an ecosystem selects.
module Ecluse.Core.Registry.Archive.TarSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Conduit (yieldMany)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Tar (scanTar, tarContents, tarRawNames)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Archive (patchFirstHeader, pax, paxRecord, regular, tarBytes)

spec :: Spec
spec = describe "scanTar" $ do
    it "accepts exact entry, path-depth, and inflated-byte ceilings" $ do
        bytes <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        let limits = defaultLimits{maxArtifactCount = 1, maxNestingDepth = 2, maxMirrorArtifactBytes = BS.length bytes}
        scan limits [bytes] `shouldReturn` Right selected
        scan limits{maxArtifactCount = 0} [bytes] `shouldReturn` Left (ParseError "archive entry count exceeds its bound")
        scan limits{maxNestingDepth = 1} [bytes] `shouldReturn` Left (ParseError "tar path depth exceeds its bound")
        scan limits{maxMirrorArtifactBytes = BS.length bytes - 1} [bytes] `shouldReturn` Left (ParseError "archive inflated bytes exceed their bound")
    it "bounds the path inventory independently of selected content and payload bytes" $ do
        single <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        double <- BSL.toStrict <$> tarBytes [("example-1/data", payload), ("example-1/extra", "x")]
        let limits = defaultLimits{maxMetadataBytes = 300}
        scan limits [single] `shouldReturn` Right selected
        scan limits [double] `shouldReturn` Left (ParseError "archive path inventory exceeds its bound")
        for_ ["data", "extra"] $ \kept ->
            scanSelecting (== ["example-1", kept]) limits [double] `shouldReturn` Left (ParseError "archive path inventory exceeds its bound")
    it "rejects a partial trailer and a truncated selected payload" $ do
        bytes <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        scan defaultLimits [bytes] `shouldReturn` Right selected
        scan defaultLimits [BS.take 512 bytes <> BS.take 1 payload] `shouldReturn` Left (ParseError "truncated tar payload")
        scan defaultLimits [BS.take 1536 bytes <> BS.replicate 511 0] `shouldReturn` Left (ParseError "invalid tar trailer")
        scan defaultLimits [BS.take 1024 bytes <> BS.replicate 511 0] `shouldReturn` Left (ParseError "tar archive has no complete trailer")
        scan defaultLimits [bytes <> "nonzero"] `shouldReturn` Left (ParseError "tar archive has trailing nonzero bytes")
    it "refuses a header checksum mismatch after a valid framing control" $ do
        bytes <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        scan defaultLimits [bytes] `shouldReturn` Right selected
        scan defaultLimits ["X" <> BS.drop 1 bytes] `shouldReturn` Left (ParseError "malformed tar header")
    it "records extended headers and name prefixes without changing the members it reads" $ do
        plain <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        evidence plain `shouldReturn` Right (selected, True)
        extended <-
            traverse
                sequence
                [ [pax 'x' (paxRecord "path=example-1/data\n"), regular "example-1/placeholder" payload]
                , [pax 'g' (paxRecord "comment=opaque\n"), regular "example-1/data" payload]
                , [regular "example-1/data" payload, pax 'g' (paxRecord "comment=opaque\n")]
                , [pure (Tar.longLinkEntry "example-1/data"), regular "example-1/placeholder" payload]
                , [pure (Tar.longSymLinkEntry "unused"), regular "example-1/data" payload]
                ]
        for_ extended $ \entries -> evidence (BSL.toStrict (Tar.write entries)) `shouldReturn` Right (selected, False)
        let long = replicate 120 'a' <> "/data"
        prefixed <- regular long payload
        evidence (BSL.toStrict (Tar.write [prefixed])) `shouldReturn` Right (Map.singleton (toText long) payload, False)
    it "records bytes after a NUL in a name or prefix field without changing the member it reads" $ do
        plain <- BSL.toStrict <$> tarBytes [("example-1/data", payload)]
        -- The name fills the first 14 bytes of its field, and the ustar prefix field starts at byte 345.
        evidence (patchFirstHeader 14 "\NUL\NUL" plain) `shouldReturn` Right (selected, True)
        for_ [14, 345] $ \offset ->
            evidence (patchFirstHeader offset "\NULx" plain) `shouldReturn` Right (selected, False)

-- | Select every regular file, so each bound is checked against retained content.
scan :: Limits -> [ByteString] -> IO (Either ParseError (Map Text ByteString))
scan limits chunks = fmap tarContents <$> scanTar limits (const pass) (const (Right True)) ($ yieldMany chunks)

-- | The selected members together with whether every member kept its raw header name.
evidence :: ByteString -> IO (Either ParseError (Map Text ByteString, Bool))
evidence bytes = fmap (\tar -> (tarContents tar, tarRawNames tar)) <$> scanTar defaultLimits (const pass) (const (Right True)) ($ yieldMany [bytes])

payload :: ByteString
payload = "Generic tar payload.\n"

selected :: Map Text ByteString
selected = Map.singleton "example-1/data" payload

-- | Select only the members whose path parts satisfy the predicate, so an unselected member still meets each bound.
scanSelecting :: ([Text] -> Bool) -> Limits -> [ByteString] -> IO (Either ParseError (Map Text ByteString))
scanSelecting keep limits chunks = fmap tarContents <$> scanTar limits (const pass) (Right . keep) ($ yieldMany chunks)

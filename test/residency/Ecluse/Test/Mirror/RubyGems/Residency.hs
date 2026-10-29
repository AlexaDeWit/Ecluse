-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native state fixtures reach isolated children only through authenticated record and object files.
module Ecluse.Test.Mirror.RubyGems.Residency (
    RubyStateShape (..),
    RubyStateFiles (..),
    withRubyStateFiles,
    readRubyStateIdentity,
    readRubyStateRecord,
) where

import Data.Aeson (eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Mirror.State (CompanionState (CompanionRetained), ProjectMirrorRecord, companionSize, recordVersions, versionCompanions)
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (StateTarget (StateTarget), targetKey)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Mirror.RubyGems
import Ecluse.Test.Support (expectRight)

-- | Large source fields and large external objects exercise separate memory ownership.
data RubyStateShape = CommittedState | HistoryState | PaddedSourceState | LargeObjectState
    deriving stock (Eq, Show)

-- | Only paths and independently constructed counts reach a probe parent.
data RubyStateFiles = RubyStateFiles
    { rsfDirectory :: FilePath
    , rsfRecordPath :: FilePath
    , rsfVersions :: Int
    , rsfObjects :: Int
    , rsfObjectBytes :: Integer
    }

-- | Generate owned packages before timing and retain original external object bytes separately.
withRubyStateFiles :: RubyStateShape -> (RubyStateFiles -> IO a) -> IO a
withRubyStateFiles shape use = withSystemTempDirectory "ecluse-ruby-state" $ \directory -> do
    fixture <- case shape of
        CommittedState -> rubyStateProbeFixture
        PaddedSourceState -> rubyStateProbeFixture >>= padRubyStateProbeSource (64 * 1024)
        _ -> do
            ruby <- fromMaybe "ruby" <$> lookupEnv "ECLTEST_RUBY"
            let generated = directory </> "generated"
                flags = if shape == HistoryState then ["--state-versions", "128"] else ["--state-versions", "1", "--metadata-bytes", "1048576"]
            (status, _, errors) <- readProcessWithExitCode ruby (["test/fixtures/rubygems/generate.rb", "--output", generated] <> flags) ""
            unless (status == ExitSuccess) (fail ("native state fixture generation failed: " <> errors))
            makeRubyStateProbeFixture generated "ecluse-fixture-platform"
    let path = directory </> "state.json"
        objects = rubyStateCompanionObjects fixture
    BS.writeFile path (rubyStateEncodedRecord fixture)
    LBS.writeFile (directory </> "identity.json") (encode (object ["target" .= targetKey (rubyStateTarget fixture), "package" .= renderPackageName (rubyStatePackage fixture)]))
    createDirectoryIfMissing True (directory </> "objects")
    for_ (Map.toList objects) $ \(digest, bytes) -> BS.writeFile (directory </> "objects" </> toString digest) bytes
    record <- readRubyStateRecord directory
    let references = [reference | version <- Map.elems (recordVersions record), CompanionRetained reference <- Map.elems (versionCompanions version)]
    unless (length references == Map.size objects) (fail "native state reference count differs from independent object membership")
    use (RubyStateFiles directory path (Map.size (recordVersions record)) (Map.size objects) (sum (map (toInteger . companionSize) references)))

-- | Decode expected identity outside the retained-record measurement interval.
readRubyStateIdentity :: FilePath -> IO (StateTarget, PackageName)
readRubyStateIdentity directory = do
    value <- BS.readFile (directory </> "identity.json") >>= either fail pure . eitherDecodeStrict
    (target, name) <- either fail pure (parseEither (withObject "native state identity" (\fields -> (,) <$> fields .: "target" <*> fields .: "package")) value)
    package <- expectRight (projectName name)
    pure (StateTarget target, package)

-- | Reopening the encoded record uses the production identity and native-source decoder.
readRubyStateRecord :: FilePath -> IO ProjectMirrorRecord
readRubyStateRecord directory = do
    (target, name) <- readRubyStateIdentity directory
    bytes <- BS.readFile (directory </> "state.json")
    chargedDecodeProjectRecord (const pass) defaultLimits target name bytes >>= expectRight

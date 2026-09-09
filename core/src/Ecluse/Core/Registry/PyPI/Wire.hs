-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Decode PEP 691 metadata with per-entry drops.
Raw array positions survive malformed entries so admission and assembly identify the same files.
-}
module Ecluse.Core.Registry.PyPI.Wire (
    -- * The media type this shape travels under
    simpleIndexMediaType,

    -- * The Simple index
    SimpleIndex (..),
    checkApiVersion,

    -- * One distribution file
    IndexFile (..),
    YankState (..),
    decodeIndexFiles,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    Value (Bool, Object, String),
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Json.Lenient (lenientOptional)
import Ecluse.Core.Package (
    InvalidEntry,
    InvalidEntryKind (InvalidIndexFile, InvalidVersionListing),
 )
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry.WireSupport (partitionLenientList)

-- | The PEP 691 media type used for index requests and responses.
simpleIndexMediaType :: ByteString
simpleIndexMediaType = "application/vnd.pypi.simple.v1+json"

-- | A project's index records malformed file and version entries as drops.
data SimpleIndex = SimpleIndex
    { siName :: Text
    -- ^ The project name the index reports, verbatim. Empty when the key is absent.
    , siFiles :: [IndexFile]
    -- ^ The offered distribution files, in the order the index listed them.
    , siInvalidEntries :: [InvalidEntry]
    -- ^ The malformed @files@ and @versions@ entries the decode dropped.
    }
    deriving stock (Eq, Show)

instance FromJSON SimpleIndex where
    parseJSON = withObject "PyPI Simple index" $ \o -> do
        checkApiVersion o
        name <- o .:? "name" .!= ""
        (files, fileDrops) <- lenientFiles o
        versionDrops <- lenientVersionListing o
        pure
            SimpleIndex
                { siName = name
                , siFiles = files
                , siInvalidEntries = fileDrops <> versionDrops
                }

-- | A distribution file encodes its release in 'ifFilename'.
data IndexFile = IndexFile
    { ifEntryKey :: EntryKey
    , ifFilename :: Text
    -- ^ The distribution file name, which encodes the project, the release, and a wheel's tags.
    , ifUrl :: Text
    -- ^ The file's absolute upstream location, on the ecosystem's files host or the index's own.
    , ifHashes :: Map Text Text
    -- ^ Unknown digest algorithms drop during projection.
    , ifRequiresPython :: Maybe Text
    -- ^ The PEP 440 interpreter specifier a client filters on, if the file declares one.
    , ifSize :: Maybe Int
    -- ^ The file's byte count, if reported. Advisory, so a hostile value reads as absent.
    , ifUploadTime :: Maybe UTCTime
    -- ^ The per-file publication instant used to compute release age.
    , ifYanked :: YankState
    -- ^ Whether PEP 592 withdraws this file from resolution, and why.
    , ifProvenance :: Maybe Text
    -- ^ The URL of a PEP 740 attestation bundle, if the index names one.
    }
    deriving stock (Eq, Show)

instance FromJSON IndexFile where
    parseJSON = withObject "PyPI index file" $ \o ->
        IndexFile SingletonEntry
            <$> o .: "filename"
            <*> o .: "url"
            <*> o .:? "hashes" .!= mempty
            <*> o .:? "requires-python"
            <*> lenientOptional o "size"
            <*> lenientOptional o "upload-time"
            <*> (yankState <$> o .:? "yanked")
            <*> o .:? "provenance"

-- | A PEP 592 yank withdraws a file from ranges while allowing exact pins.
data YankState
    = -- | The file resolves normally.
      FileOffered
    | -- | The file is withdrawn from resolution, with the reason the index gave.
      FileWithdrawn (Maybe Text)
    deriving stock (Eq, Show)

yankState :: Maybe Value -> YankState
yankState = \case
    Just (Bool True) -> FileWithdrawn Nothing
    Just (String reason) -> FileWithdrawn (Just reason)
    _ -> FileOffered

-- | Refuse malformed or unsupported API declarations. An absent declaration uses the supported API.
checkApiVersion :: Object -> Parser ()
checkApiVersion o = do
    meta <- o .:? "meta" .!= mempty
    declared <- meta .:? "api-version"
    case T.breakOn "." <$> declared of
        Just (major, _) | major /= supportedApiMajor -> fail ("unsupported PEP 691 api-version: " <> toString major)
        _ -> pure ()

supportedApiMajor :: Text
supportedApiMajor = "1"

lenientFiles :: Object -> Parser ([IndexFile], [InvalidEntry])
lenientFiles o = do
    raw <- o .:? "files" .!= []
    pure (decodeIndexFiles (zip [0 ..] raw))

-- | Decode indexed files without renumbering entries that survive lenient parsing.
decodeIndexFiles :: [(Int, Value)] -> ([IndexFile], [InvalidEntry])
decodeIndexFiles = foldMap decode
  where
    decode (position, value) =
        first (map snd) $
            partitionLenientList
                InvalidIndexFile
                (fmap (\file -> file{ifEntryKey = ArrayEntry position}) . parseEither parseJSON)
                [(fileKey position value, value)]

lenientVersionListing :: Object -> Parser [InvalidEntry]
lenientVersionListing o = do
    raw <- o .:? "versions" .!= []
    pure (snd (partitionLenientList InvalidVersionListing decodeVersion (zip (map show [0 :: Int ..]) raw)))
  where
    decodeVersion :: Value -> Either String Text
    decodeVersion = parseEither parseJSON

fileKey :: Int -> Value -> Text
fileKey position = \case
    Object file | Just (String name) <- KeyMap.lookup "filename" file -> name
    _ -> show position

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Bounded reading of the name, version, @:action@ and file name claims in the unchanged PyPI publication relay body.
module Ecluse.Core.Registry.PyPI.Publish.Multipart (uploadClaims, pypiFileUploadOnly, pypiSdistNameForm, pypiVersionForm) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Wai.Parse qualified as Wai
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..), PublishBodyFault (PublishMalformed, PublishUnsupported), parseRefusal)
import Ecluse.Core.Registry.PyPI.Project (builtFileCoordinate, canonicalName, fcVersionKey, projectName)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version (canonicalPep440, renderVersion)

-- | The agreed project name of one @file_upload@ whose file name names that project at the declared version.
uploadClaims :: Limits -> Maybe ByteString -> ByteString -> IO (Either PublishBodyFault [Text])
uploadClaims limits contentType body = case checkedFields limits contentType body of
    Left fault -> pure (Left (PublishMalformed fault))
    Right claims -> case first PublishMalformed (formValues ActionField claims) >>= uploadAction of
        Left fault -> pure (Left fault)
        Right () -> first PublishMalformed . (>>= releaseFile claims) <$> agreedName limits claims

-- | The @501@ reason for any other @:action@, such as @remove_pkg@ or @doc_upload@.
pypiFileUploadOnly :: Text
pypiFileUploadOnly = "this proxy relays new-release uploads (:action=file_upload) only: change or remove an existing release at the publication target"

-- | The @400@ reason for any upload that is not a wheel and does not name the declared release as PEP 625 names an sdist.
pypiSdistNameForm :: Text
pypiSdistNameForm = "multipart file name does not name the declared project and version: an sdist must be named <normalised_name>-<version>.tar.gz (PEP 625)"

-- | The @400@ reason for a declared version that is not a PEP 440 version, whatever the file is named.
pypiVersionForm :: Text
pypiVersionForm = "multipart version is not a PEP 440 version"

-- Uploaders send one file_upload. pypiserver deletes a release on remove_pkg under the same permission.
uploadAction :: [ByteString] -> Either PublishBodyFault ()
uploadAction = \case
    ["file_upload"] -> Right ()
    [_] -> Left (PublishUnsupported pypiFileUploadOnly)
    [] -> Left (PublishMalformed (ParseError "multipart upload has no :action"))
    _ -> Left (PublishMalformed (ParseError "multipart upload declares more than one :action"))

-- Conflicting or missing names refuse the relay, as does a name Wai would read differently.
agreedName :: Limits -> [Claim] -> IO (Either ParseError PackageName)
agreedName limits claims = case formValues NameField claims >>= traverse decodeName of
    Left fault -> pure (Left fault)
    Right names -> do
        projected <- projectNames limits (map (TE.encodeUtf8 . canonicalName) names)
        pure $
            projected >> case names of
                [] -> parseRefusal "multipart upload has no name"
                name : remaining
                    | all (== name) remaining -> Right name
                    | otherwise -> parseRefusal "multipart upload declares conflicting names"
  where
    decodeName bytes = first (const (ParseError "multipart name is not UTF-8")) (TE.decodeUtf8' bytes) >>= projectName

projectNames :: Limits -> [ByteString] -> IO (Either ParseError ())
projectNames limits values
    | projectedSize > toInteger (maxPublishRequestBytes limits) = pure (parseRefusal "multipart name projection exceeds its byte bound")
    | otherwise = do
        chunks <- newIORef (nameProjection values)
        let readChunk = atomicModifyIORef' chunks (\remaining -> let (chunk, rest) = BS.splitAt 32768 remaining in (rest, chunk))
        outcome <- tryAny (Wai.sinkRequestBodyEx options discardFile (Wai.Multipart projectionBoundary) readChunk)
        pure $ case outcome of
            Left _ -> parseRefusal "malformed or oversized multipart upload"
            Right (params, files) -> unless (null files && params == map ("name",) values) (parseRefusal "multipart name parsers disagree")
  where
    projectedSize = sum (map (\value -> toInteger (BS.length value) + 100) values) + 100
    options = Wai.setMaxRequestParmsSize (maxMetadataBytes limits) Wai.defaultParseRequestBodyOptions

-- A checked package name cannot contain a colon, so this delimiter cannot occur in projected values.
projectionBoundary :: ByteString
projectionBoundary = "ecluse:name:projection"

nameProjection :: [ByteString] -> ByteString
nameProjection values = BS.concat (map part values) <> "--" <> projectionBoundary <> "--\r\n"
  where
    part value = "--" <> projectionBoundary <> "\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\n" <> value <> "\r\n"

discardFile :: Wai.BackEnd ()
discardFile _ _ readChunk = drain
  where
    drain = readChunk >>= \chunk -> unless (BS.null chunk) drain

-- A store can key the write off the file name, so it must name the agreed project at the declared version.
releaseFile :: [Claim] -> PackageName -> Either ParseError [Text]
releaseFile claims name = do
    versions <- formValues VersionField claims
    case (versions, [file | ContentClaim file <- claims]) of
        ([version], [Just file])
            | Nothing <- declaredVersion version -> parseRefusal pypiVersionForm
            | Just declared <- declaredVersion version
            , Just coordinate <- builtFileCoordinate name . unFilename =<< distributionFilename =<< utf8 file
            , fcVersionKey coordinate == renderVersion declared ->
                Right [canonicalName name]
            | ".whl" `BS.isSuffixOf` file -> parseRefusal "multipart file name does not name the declared project and version"
            | otherwise -> parseRefusal pypiSdistNameForm
        ([], _) -> parseRefusal "multipart upload has no version"
        (_, []) -> parseRefusal "multipart upload has no content file"
        (_, [Nothing]) -> parseRefusal "multipart content part has no file name"
        _ -> parseRefusal "multipart upload declares more than one version or content file"
  where
    utf8 = rightToMaybe . TE.decodeUtf8'
    declaredVersion = canonicalPep440 <=< utf8

-- Wheel and sdist names hold only these characters, so the gate also refuses whitespace and percent signs.
distributionFilename :: Text -> Maybe Filename
distributionFilename raw = do
    guard (T.all (\c -> isAsciiAlphaNum c || c `elem` ("-_.!+" :: String)) raw)
    mkFilename raw

-- A guarded field sent as a file part is refused, because a store reads file parts apart from form fields.
formValues :: FormField -> [Claim] -> Either ParseError [ByteString]
formValues field claims = traverse plainValue [(file, value) | FormClaim found file value <- claims, found == field]
  where
    plainValue = \case
        (Nothing, value) -> Right value
        (Just _, _) -> parseRefusal "multipart name, version, or :action is a file part"

-- The parts the guard reads: a form field with any file name it carries, and the content part's file name, never its data.
data Claim = FormClaim FormField (Maybe ByteString) ByteString | ContentClaim (Maybe ByteString)

data FormField = NameField | ActionField | VersionField
    deriving stock (Eq)

checkedFields :: Limits -> Maybe ByteString -> ByteString -> Either ParseError [Claim]
checkedFields limits contentType body = do
    when (BS.length body > maxPublishRequestBytes limits) (parseRefusal "multipart upload exceeds its byte bound")
    header <- maybe (parseRefusal "missing multipart Content-Type") Right contentType
    when (BS.length header > 8192) (parseRefusal "multipart Content-Type exceeds its bound")
    let (mediaType, attributes) = Wai.parseContentType header
    unless (mediaType == "multipart/form-data") (parseRefusal "upload is not multipart/form-data")
    boundary <- case [value | ("boundary", value) <- attributes] of
        [value] | not (BS.null value), BS.length value <= 70, BS.all boundaryByte value -> Right value
        _ -> parseRefusal "missing, malformed, or ambiguous multipart boundary"
    unless (BS.isPrefixOf ("--" <> boundary <> "\r\n") body) (parseRefusal "multipart opening boundary is missing")
    checkParts limits boundary 0 (BS.drop (BS.length boundary + 4) body)

boundaryByte :: Word8 -> Bool
boundaryByte byte = byte >= 33 && byte <= 126 && byte /= 34 && byte /= 92

-- Wai ignores malformed part headers, so reject ambiguity before it selects a disposition claim.
checkParts :: Limits -> ByteString -> Int -> ByteString -> Either ParseError [Claim]
checkParts limits boundary count bytes = do
    when (count >= maxArtifactCount limits) (parseRefusal "multipart part count exceeds its bound")
    let (headers, headerSuffix) = BS.breakSubstring "\r\n\r\n" bytes
    when (BS.null headerSuffix || BS.length headers > 16384) (parseRefusal "malformed multipart headers")
    claim <- checkHeaders headers
    (value, suffix) <- splitPart boundary (BS.drop 4 headerSuffix)
    when (hidesDelimiter boundary (BS.take (BS.length headers + 4 + BS.length value) bytes)) (parseRefusal "multipart part hides a boundary delimiter")
    later <- case suffix of
        "--\r\n" -> Right []
        "--" -> Right []
        remaining | Just following <- BS.stripPrefix "\r\n" remaining -> checkParts limits boundary (count + 1) following
        _ -> parseRefusal "malformed or padded multipart boundary delimiter"
    pure (map ($ value) (maybeToList claim) <> later)

splitPart :: ByteString -> ByteString -> Either ParseError (ByteString, ByteString)
splitPart boundary bytes = case BS.breakSubstring ("\r\n--" <> boundary) bytes of
    (_, suffix) | BS.null suffix -> parseRefusal "multipart closing boundary is missing"
    (value, suffix) -> Right (value, BS.drop (BS.length boundary + 4) suffix)

-- Python's cgi and werkzeug also end a part at a delimiter after a bare CR or LF, so none may sit inside a part.
hidesDelimiter :: ByteString -> ByteString -> Bool
hidesDelimiter boundary part = BS.isPrefixOf delimiter part || any (`BS.isInfixOf` part) [BS.cons 10 delimiter, BS.cons 13 delimiter]
  where
    delimiter = "--" <> boundary

checkHeaders :: ByteString -> Either ParseError (Maybe (ByteString -> Claim))
checkHeaders bytes = do
    headers <- traverse parseHeader (BSC.lines bytes)
    disposition <- case [value | ("content-disposition", value) <- headers] of
        [value] -> Right value
        _ -> parseRefusal "missing or ambiguous multipart disposition"
    when (BS.elem 92 disposition) (parseRefusal "escaped multipart disposition attributes are unsupported")
    let (kind, attributes) = Wai.parseContentType disposition
    when (any (BS.elem 34 . snd) attributes) (parseRefusal "malformed quoted multipart disposition attribute")
    name <- case [value | ("name", value) <- attributes] of
        [value] | kind == "form-data", not (BS.null value) -> Right value
        _ -> parseRefusal "malformed multipart disposition name"
    filename <- case [value | ("filename", value) <- attributes] of
        [] -> Right Nothing
        [value] -> Right (Just value)
        _ -> parseRefusal "ambiguous multipart filename"
    when (any (\(key, _) -> key `notElem` ["name", "filename"]) attributes) (parseRefusal "unsupported multipart disposition attribute")
    pure (partClaim name filename)
  where
    parseHeader line = do
        let clean = fromMaybe line (BS.stripSuffix "\r" line)
            (key, rest) = BS.break (== 58) clean
        when (BS.null key || BS.null rest || BS.any (\c -> c <= 32 || c >= 127) key) (parseRefusal "malformed multipart header")
        pure (BSC.map toLower key, BSC.dropWhile (== ' ') (BS.drop 1 rest))

-- Every other part passes through unread. The content part's value is its file data, which the guard never reads.
partClaim :: ByteString -> Maybe ByteString -> Maybe (ByteString -> Claim)
partClaim name filename = case name of
    "content" -> Just (const (ContentClaim filename))
    "name" -> Just (FormClaim NameField filename)
    ":action" -> Just (FormClaim ActionField filename)
    "version" -> Just (FormClaim VersionField filename)
    _ -> Nothing

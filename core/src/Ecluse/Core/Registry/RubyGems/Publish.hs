-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native publication validates the archive and preserves its exact bytes.
module Ecluse.Core.Registry.RubyGems.Publish (
    rubyPublish,
    rubyPublishCodec,
    rubyPublishCodecWith,
    uploadClaims,
    classifyPublish,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types.Header (hContentType)
import UnliftIO.Exception (evaluate)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (hashValue, renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), ParseError (..), PublishBodyFault (PublishMalformed), PublishError (PublishError), PublishFault (..), UrlFormationError (UnparseableUrl), isSuccessStatus)
import Ecluse.Core.Registry.Adapter.Capability
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead), prepareRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (checkedMirrorUploadData)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullReadWorkspace, ocLimits, ocToken), originBaseUrl)
import Ecluse.Core.Registry.Publish (PublishCodec (..), relayPublishRequest)
import Ecluse.Core.Registry.Request (CredentialMapping, attachCredential, metadataGet, parseRequestEither)
import Ecluse.Core.Registry.RubyGems.Archive
import Ecluse.Core.Registry.RubyGems.CompactSpec (checkGemSpecification)
import Ecluse.Core.Registry.RubyGems.Credential
import Ecluse.Core.Registry.RubyGems.Document (GemRow (..))
import Ecluse.Core.Registry.RubyGems.Metadata (prepareGemRead)
import Ecluse.Core.Registry.RubyGems.Mirror (mirrorRow, rubyMirrorCompanion)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Request (gemInfoRequestUnder, gemInfoUrl)
import Ecluse.Core.Security.Limits (Limits, requestTimeoutSeconds)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))

-- | Caller publication uses the configured native key and checked OTP header.
rubyPublish :: ChargeFactors -> AdapterPublish
rubyPublish factors =
    AdapterPublish
        { publishRelay = \origin name body -> do
            claims <- uploadClaims (ocLimits origin) (ocChargeFullReadWorkspace origin) (originBaseUrl origin) body
            let request = do
                    names <- first (const (UnparseableUrl "RubyGems relay archive is invalid")) claims
                    unless (traverse projectName names == Right [name]) (Left (UnparseableUrl "RubyGems relay name differs from the authorised package"))
                    bodyRequest rubyPublishCredential (originBaseUrl origin) (ocToken origin) (publishBodyExtras body) (publishBodyBytes body)
            relayPublishRequest origin request
        , publishCredential = rubyPublishCredential
        , publishRequestExtras = rubyPublishExtras
        , publishUploadClaims = uploadClaims
        , publishCodec = rubyPublishCodec factors
        , publishMirrorRequirements = MirrorRequirements (NativeChargedWorkerRead prepareGemRead) DurableVersionState ReadRootUpload
        , publishIdentity = TokenIdentity
        }

-- | Ordinary private registries use Basic reads and literal native publication keys.
rubyPublishCodec :: ChargeFactors -> PublishCodec
rubyPublishCodec = rubyPublishCodecWith rubyReadCredential rubyPublishCredential

-- | Declared backend roles select read and write credential presentation independently.
rubyPublishCodecWith :: CredentialMapping -> CredentialMapping -> ChargeFactors -> PublishCodec
rubyPublishCodecWith readCredential writeCredential factors =
    PublishCodec
        { pcPrepareSource = \name version doc -> do
            source <- maybeToRight (PublishSourceUnavailable "RubyGems snapshot has no paired native source") (vdRaw doc >>= snd rubyGemsCached)
            inventory <- checkedMirrorInventory name version doc (RubyGemsMirrorRead source)
            pure (inventory, ())
        , pcPrepareUpload = \limits charge inventory () file verified -> do
            inspected <- inspectGemArchive limits charge (verifiedBytes verified)
            evaluate $ do
                archive <- first (PublishSourceUnavailable . parseErrorMessage) inspected
                row <- mirrorRow inventory file
                checked <- first (PublishSourceUnavailable . parseErrorMessage) (checkGemArchive (grCoordinate row) (hashValue (grChecksum row)) archive)
                _ <- first (PublishSourceUnavailable . parseErrorMessage) (checkGemSpecification (grCoordinate row) row (gemArchiveSpecification checked))
                checkedMirrorUploadData inventory file verified checked
        , pcMirrorCompanion = Just rubyMirrorCompanion
        , pcNativeComplete = \_ _ _ -> False
        , pcTargetRead = PrepareRead $ \tracing origin -> fmap (\native -> native{erRequest = \base credential name -> attachCredential readCredential credential <$> gemInfoRequestUnder base Nothing name}) <$> prepareRead prepareGemRead tracing origin
        , pcTargetReadPermille = cfFullReadPermille factors
        , pcProbeRequest = \target credential name -> metadataGet readCredential credential "text/plain" <$> (gemInfoUrl target name >>= parseRequestEither)
        , pcVersionListWalk = Nothing
        , pcPrepareTarget = \_ _ target -> pure (Right target)
        , pcPublishRequest = \target credential _ upload -> first (PublishFetch . FetchUrlUnformable) (bodyRequest writeCredential target credential NoPublishExtras (uploadBytes upload))
        , pcPublishOutcome = classifyPublish
        }

-- | Archive identity controls first-party ownership before the server forwards any bytes.
uploadClaims :: Limits -> (Int -> IO ()) -> Text -> PublishRequestBody -> IO (Either PublishBodyFault [Text])
uploadClaims limits charge _ body =
    fromMaybe (Left (PublishMalformed (ParseError "RubyGems upload inspection exceeded its deadline"))) <$> timeout (requestTimeoutSeconds * 1000000) inspect
  where
    inspect = do
        inspected <- inspectGemArchive limits charge (publishBodyBytes body)
        case inspected of
            Left fault -> pure (Left (PublishMalformed fault))
            Right archive -> do
                name <- evaluate (gemArchiveName archive)
                pure (Right [renderPackageName name])

bodyRequest :: CredentialMapping -> Text -> Maybe ClientCredential -> PublishExtras -> ByteString -> Either UrlFormationError Request
bodyRequest credentialMapping target credential extras bytes = do
    when (T.any (`elem` ['?', '#']) target) (Left (UnparseableUrl "RubyGems publication root contains a query or fragment"))
    base <- parseRequestEither (T.dropWhileEnd (== '/') target <> "/api/v1/gems")
    pure . attachCredential credentialMapping credential $
        base
            { method = "POST"
            , requestHeaders = (hContentType, "application/octet-stream") : rubyOtpHeaders extras
            , requestBody = RequestBodyBS bytes
            }

-- | A successful write still needs an independent whole-version inventory proof.
classifyPublish :: Int -> ByteString -> Either PublishFault ()
classifyPublish status _
    | isSuccessStatus status = Right ()
    | otherwise = Left (PublishRejected (PublishError ("RubyGems publication failed with HTTP status " <> show status)))

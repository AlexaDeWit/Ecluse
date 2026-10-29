-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems reads bind the actual origin and charged complete-response reader.
module Ecluse.Core.Registry.RubyGems.Metadata (rubyGemsChargeFactors, prepareGemRead, gemReadWith, finishGemFull, finishGemSelected) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, rubyGemsCached)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (..), originBaseUrl)
import Ecluse.Core.Registry.Request (baseRequestUrl)
import Ecluse.Core.Registry.RubyGems.Document (selectGemVersion)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (..), GemReadResult (..), readGemInfoCharged)
import Ecluse.Core.Registry.RubyGems.Request (gemInfoRequestUnder)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | ARM64 charges combine public captures with labelled synthetic listings.
rubyGemsChargeFactors :: ChargeFactors
rubyGemsChargeFactors =
    -- Public captures: https://github.com/AlexaDeWit/Ecluse/actions/runs/40559003401/job/122382799126
    -- Synthetic: https://github.com/AlexaDeWit/Ecluse/actions/runs/40575218918/job/122439883465
    ChargeFactors{cfFullReadPermille = 18000, cfKeptPermille = 17300, cfOutputPermille = 4600}

-- | Bind charging callbacks without performing discovery or metadata IO at boot.
prepareGemRead :: PrepareRead
prepareGemRead = PrepareRead (\_ origin -> pure (Right (gemReadWith (ocChargeFullReadWorkspace origin) (ocChargeSelectedRead origin) (originBaseUrl origin))))

-- | Keep full and selected reads under the same captured source authority.
gemReadWith :: (Int -> IO ()) -> (Int -> IO ()) -> Text -> EcosystemRead
gemReadWith fullPay selectedPay sourceBase =
    EcosystemRead
        { erRequest = \base credential name -> do
            unless (registryUrlText (baseRequestUrl base) == sourceBase) (Left (UnparseableUrl "RubyGems reader belongs to another origin"))
            gemInfoRequestUnder base credential name
        , erUniqueFields = []
        , erWalkFull = \limits name _ -> readGemInfoCharged fullPay limits name sourceBase FullRead
        , erFinishFull = \limits name base streamed -> do
            unless (base == sourceBase) (Left MetadataUndecodable)
            finishGemFull limits name base streamed
        , erWalkSelected = \limits name version -> readGemInfoCharged selectedPay limits name sourceBase (SelectedRead version)
        , erFinishSelected = \limits name base version streamed -> do
            unless (base == sourceBase) (Left MetadataUndecodable)
            finishGemSelected limits name base version streamed
        }

-- | Retain policy facts and native rows from one fully consumed named response.
finishGemFull :: Limits -> PackageName -> Text -> StreamResult GemReadResult -> Either MetadataError (PackageInfo, CachedDoc)
finishGemFull limits _ _ streamed = do
    result <- first (streamError limits) (streamValue streamed)
    pure (gemReadInfo result, fst rubyGemsCached (gemReadDocument result))

-- | Selected source state includes every supported platform for the exact raw version.
finishGemSelected :: Limits -> PackageName -> Text -> Version -> StreamResult GemReadResult -> Either MetadataError VersionRead
finishGemSelected limits _ _ version streamed = do
    result <- first (streamError limits) (streamValue streamed)
    let selected = do
            details <- Map.lookup (renderVersion version) (infoVersions (gemReadInfo result))
            document <- selectGemVersion version (gemReadDocument result)
            pure (VersionDoc details (Just (fst rubyGemsCached document)))
    pure (VersionRead selected (streamBytes streamed) Nothing)

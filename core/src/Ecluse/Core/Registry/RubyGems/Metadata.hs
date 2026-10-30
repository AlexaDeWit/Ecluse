-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems reads bind the actual origin and charged complete-response reader.
module Ecluse.Core.Registry.RubyGems.Metadata (rubyGemsChargeFactors, prepareGemRead, gemReadWith, finishGemFull) where

import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, rubyGemsCached)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), bindToOrigin)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (finishSelectedVersion, streamError)
import Ecluse.Core.Registry.Origin (OriginClient (..), originBaseUrl)
import Ecluse.Core.Registry.RubyGems.Document (selectGemVersion)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (..), GemReadResult (..), readGemInfoCharged)
import Ecluse.Core.Registry.RubyGems.Request (gemInfoRequestUnder)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))

-- | ARM64 charges combine public captures with labelled synthetic listings.
rubyGemsChargeFactors :: ChargeFactors
rubyGemsChargeFactors =
    -- Public captures: https://github.com/AlexaDeWit/Ecluse/actions/runs/40559003401/job/122382799126
    -- Synthetic: https://github.com/AlexaDeWit/Ecluse/actions/runs/40575218918/job/122439883465
    ChargeFactors{cfFullReadPermille = 18000, cfKeptPermille = 17300, cfOutputPermille = 4600}

-- | Bind charging callbacks without performing discovery or metadata IO at boot.
prepareGemRead :: PrepareRead
prepareGemRead = PrepareRead (\origin -> pure (Right (gemReadWith (ocChargeFullReadWorkspace origin) (ocChargeSelectedRead origin) (originBaseUrl origin))))

-- | Keep full and selected reads under the same captured source authority.
gemReadWith :: (Int -> IO ()) -> (Int -> IO ()) -> Text -> EcosystemRead
gemReadWith fullPay selectedPay sourceBase =
    bindToOrigin
        (UnparseableUrl "RubyGems reader belongs to another origin")
        sourceBase
        EcosystemRead
            { erRequest = gemInfoRequestUnder
            , erUniqueFields = []
            , erWalkFull = \limits name _ -> readGemInfoCharged fullPay limits name sourceBase FullRead
            , erFinishFull = finishGemFull
            , erRetainedEntries = const mempty
            , erWalkSelected = \limits name version -> readGemInfoCharged selectedPay limits name sourceBase (SelectedRead version)
            , erFinishSelected = \limits _ _ -> finishSelectedVersion gemReadInfo (\version result -> fst rubyGemsCached <$> selectGemVersion version (gemReadDocument result)) limits
            }

-- | Retain policy facts and native rows from one fully consumed named response.
finishGemFull :: Limits -> PackageName -> Text -> StreamResult GemReadResult -> Either MetadataError (PackageInfo, CachedDoc)
finishGemFull limits _ _ streamed = do
    result <- first (streamError limits) (streamValue streamed)
    pure (gemReadInfo result, fst rubyGemsCached (gemReadDocument result))

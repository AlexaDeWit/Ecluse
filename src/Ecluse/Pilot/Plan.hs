-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The decisions Pilot makes before it does anything: whether the scheduled loop exports at
all and how often, which upstreams one compile reads, whether a failed EPSS feed stops its
publication, and whether a one-shot run uploads.

Each is a pure function over the resolved configuration, so "Ecluse.Pilot" dispatches on their
results instead of branching inside @IO@ ("Ecluse.Composition.MirrorRole" is the same shape).
-}
module Ecluse.Pilot.Plan (
    -- * The scheduled export loop
    ExportLoopPlan (..),
    ExportTarget (..),
    exportLoopPlan,
    exportCadenceMicros,
    idleCadenceMicros,

    -- * The upstreams a compile reads
    configuredSources,
    quietTimeFor,
    epssAttemptLine,

    -- * The one-shot run
    PilotCompileOptions (..),
    compileEpssRequirement,
    unmountedCompileWarning,
    UploadPlan (..),
    uploadPlan,
    PilotUploadUnconfigured (..),
    uploadTarget,
) where

import Data.Map.Strict qualified as Map
import System.FilePath (takeFileName)

import Ecluse.Config (
    AdvisoriesSettings (advCompileInterval, advEpssFeedUrl, advEpssQuietTime, advOsvExportBaseUrl, advQuietTime, advUrl),
    AdvisoryStoreUrl,
    Mount,
    MountMap,
    advisoryObjectKey,
    advisoryStoreBucket,
    mountEpssRequirement,
    unUrl,
 )
import Ecluse.Core.Clock (secondsToMicros)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (CompileSources (..))
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem (osvExportDirectory))
import Ecluse.Core.Osv.Provenance (QuietTime (..), defaultQuietTime)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)

-- | What the scheduled export loop does with the advisory settings and the mounted ecosystems.
data ExportLoopPlan
    = -- | No advisory store is configured, so the loop idles and exports nothing.
      ExportIdle
    | -- | Compile and upload one artifact per target to this store, each on its own cadence.
      ExportTo AdvisoryStoreUrl (NonEmpty ExportTarget)
    deriving stock (Eq, Show)

-- | One mounted ecosystem the scheduled loop compiles, with what a failed EPSS feed means for it.
data ExportTarget = ExportTarget
    { etEcosystem :: Ecosystem
    , etEpss :: EpssRequirement
    -- ^ The mount's resolved requirement, the one its advisory consumers enforce.
    }
    deriving stock (Eq, Show)

-- | A configured store turns exporting on, one artifact per mounted ecosystem. 'Nothing' is a store with no ecosystem, which the boot refuses.
exportLoopPlan :: AdvisoriesSettings -> [ExportTarget] -> Maybe ExportLoopPlan
exportLoopPlan advisories targets = case advUrl advisories of
    Nothing -> Just ExportIdle
    Just store -> ExportTo store <$> nonEmpty targets

-- | The delay between export cycles. The decoder bounds @compileInterval@, so the conversion cannot wrap to a negative delay.
exportCadenceMicros :: AdvisoriesSettings -> Int
exportCadenceMicros = secondsToMicros . advCompileInterval

-- | The idle loop's sleep. An added store takes effect only on the next boot, so nothing observes the wakeup.
idleCadenceMicros :: Int
idleCadenceMicros = secondsToMicros (24 * 60 * 60)

-- | The upstreams a compile reads. Both are configured keys, so a moved or mirrored feed never needs a new binary.
configuredSources :: AdvisoriesSettings -> OsvEcosystem -> CompileSources
configuredSources advisories eco =
    CompileSources
        { csOsvExportUrl = osvExportUrl (unUrl (advOsvExportBaseUrl advisories)) (osvExportDirectory eco)
        , csEpssFeedUrl = toString (unUrl (advEpssFeedUrl advisories))
        }

-- | The egress every compile attempts whatever the rules, named by the host and port it dials.
epssAttemptLine :: AdvisoriesSettings -> Text
epssAttemptLine advisories =
    "pilot: every compile attempts the EPSS feed at "
        <> dialledAuthorityLabel (unUrl (advEpssFeedUrl advisories))
        <> ", whatever the rules, so its egress must be allowed"

-- | The quiet-time thresholds of one compile. An unconfigured ecosystem, or one this build does not serve, takes 'defaultQuietTime'.
quietTimeFor :: AdvisoriesSettings -> Maybe Ecosystem -> QuietTime
quietTimeFor advisories mEco =
    QuietTime
        { qtOsv = maybe defaultQuietTime configured mEco
        , qtEpss = advEpssQuietTime advisories
        }
  where
    configured eco = Map.findWithDefault defaultQuietTime eco (advQuietTime advisories)

-- | Options for the one-shot @ecluse pilot compile@ mode.
data PilotCompileOptions = PilotCompileOptions
    { pcoEcosystem :: Text
    , pcoOutDir :: FilePath
    , pcoUpload :: Bool
    -- ^ Upload the compiled artifact to the configured advisory store.
    }
    deriving stock (Eq, Show)

-- | A source-configured mount's own requirement. Without a mount or a source, the artifact may serve a policy that needs EPSS.
compileEpssRequirement :: AdvisoriesSettings -> MountMap -> PilotCompileOptions -> EpssRequirement
compileEpssRequirement advisories mounts opts = case (advUrl advisories, compiledMount mounts opts) of
    (Just _, Just mount) -> mountEpssRequirement mount
    _ -> EpssRequired

-- | The warning a one-shot compile logs for an ecosystem the loaded configuration does not mount.
unmountedCompileWarning :: MountMap -> PilotCompileOptions -> Maybe Text
unmountedCompileWarning mounts opts = case compiledMount mounts opts of
    Just _ -> Nothing
    Nothing ->
        Just
            ( "The loaded configuration mounts no "
                <> pcoEcosystem opts
                <> " ecosystem. This compile prepares its full dataset, so it requires EPSS enrichment and publishes nothing without it"
            )

compiledMount :: MountMap -> PilotCompileOptions -> Maybe Mount
compiledMount mounts opts = (`Map.lookup` mounts) =<< parseEcosystem (pcoEcosystem opts)

-- | An upload requested without an advisory store: a wiring fault, so it throws rather than return a value to re-raise.
data PilotUploadUnconfigured = PilotUploadUnconfigured
    deriving stock (Eq, Show)

instance Exception PilotUploadUnconfigured

-- | Whether a one-shot run uploads its artifact, and where.
data UploadPlan
    = -- | The run did not ask to upload, so the artifact stays local.
      UploadSkipped
    | -- | Upload the compiled artifact to this store.
      UploadTo AdvisoryStoreUrl
    deriving stock (Eq, Show)

-- | Plan a one-shot run's upload. One asked for with no store is refused, so a scripted run cannot succeed having published nothing.
uploadPlan :: PilotCompileOptions -> Maybe AdvisoryStoreUrl -> Either PilotUploadUnconfigured UploadPlan
uploadPlan opts mStore
    | not (pcoUpload opts) = Right UploadSkipped
    | otherwise = maybe (Left PilotUploadUnconfigured) (Right . UploadTo) mStore

-- | Where one compiled artifact lands: the store's bucket and its file's key under the prefix, which the proxy's sync reads.
uploadTarget :: AdvisoryStoreUrl -> FilePath -> (Text, Text)
uploadTarget store dbPath =
    (advisoryStoreBucket store, advisoryObjectKey store (takeFileName dbPath))

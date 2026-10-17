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

    -- * The publication guard
    shrinkPolicyOf,
    pilotPostureLines,

    -- * The one-shot run
    PilotCompileOptions (..),
    UploadRequest (..),
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
    AdvisoriesSettings (advCompileInterval, advEpssFeedUrl, advEpssLossCap, advEpssQuietTime, advMaxShrink, advOsvExportBaseUrl, advQuietTime, advSourcePathRecording, advUrl),
    AdvisoryStoreUrl,
    Mount,
    MountMap,
    advisoryObjectKey,
    advisoryStoreBucket,
    compileMountEpssRequirement,
    unUrl,
 )
import Ecluse.Config.AdvisoryStore (advisoryStoreCapability)
import Ecluse.Core.Clock (secondsToMicros)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (CompileSources, mkCompileSources)
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem (osvExportDirectory))
import Ecluse.Core.Osv.Provenance (QuietTime (..), defaultQuietTime)
import Ecluse.Core.Osv.Publication (Rebaseline, ShrinkPolicy (..), renderBaselineCapability, renderPercent)
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

-- | The delay between export cycles. The decoder bounds @compileInterval@ to @maxBound \`div\` 1000000@ seconds, so it cannot wrap negative.
exportCadenceMicros :: AdvisoriesSettings -> Int
exportCadenceMicros = secondsToMicros . advCompileInterval

-- | The idle loop's sleep. An added store takes effect only on the next boot, so nothing observes the wakeup.
idleCadenceMicros :: Int
idleCadenceMicros = secondsToMicros (24 * 60 * 60)

-- | The upstreams a compile reads, and how its artifact records them. A moved or mirrored feed never needs a new binary.
configuredSources :: AdvisoriesSettings -> OsvEcosystem -> CompileSources
configuredSources advisories eco =
    mkCompileSources
        (advSourcePathRecording advisories)
        (osvExportUrl (unUrl (advOsvExportBaseUrl advisories)) (osvExportDirectory eco))
        (toString (unUrl (advEpssFeedUrl advisories)))

-- | The egress every compile attempts whatever the rules, named by the host and port it dials.
epssAttemptLine :: AdvisoriesSettings -> Text
epssAttemptLine advisories =
    "pilot: every compile attempts the EPSS feed at "
        <> dialledAuthorityLabel (unUrl (advEpssFeedUrl advisories))
        <> ", whatever the rules, so its egress must be allowed"

-- | The shrink limits every publication is held to, scheduled or one-shot.
shrinkPolicyOf :: AdvisoriesSettings -> ShrinkPolicy
shrinkPolicyOf advisories = ShrinkPolicy{spMaxShrink = advMaxShrink advisories, spEpssLossCap = advEpssLossCap advisories}

-- | What every Pilot boot reports: the egress it attempts, and, with a store to publish to, the guard each publication passes.
pilotPostureLines :: AdvisoriesSettings -> [Text]
pilotPostureLines advisories = epssAttemptLine advisories : maybe [] (pure . guardLine . advisoryStoreCapability) (advUrl advisories)
  where
    guardLine capability =
        "pilot: a publication must not be older than the stored artifact, by source, nor shrink more than "
            <> renderPercent (advMaxShrink advisories)
            <> " under its "
            <> renderBaselineCapability capability
            <> ", with up to "
            <> renderPercent (advEpssLossCap advisories)
            <> " more after an optional EPSS outage"

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
    , pcoUpload :: UploadRequest
    }
    deriving stock (Eq, Show)

-- | Whether a one-shot run publishes its artifact. A rebaseline exists only beside an upload.
data UploadRequest
    = NoUpload
    | -- | Publish under the configured guard.
      Upload
    | -- | Publish once over the named revision, with this run's own shrink allowance, and restart the baseline.
      UploadRebaselined Rebaseline
    deriving stock (Eq, Show)

-- | The compiled mount's own requirement. A compile for no mount may serve a policy that needs EPSS.
compileEpssRequirement :: AdvisoriesSettings -> MountMap -> PilotCompileOptions -> EpssRequirement
compileEpssRequirement advisories mounts opts = maybe EpssRequired (compileMountEpssRequirement advisories) (compiledMount mounts opts)

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
    | -- | Upload the compiled artifact to this store, under an operator's rebaseline if one was given.
      UploadTo AdvisoryStoreUrl (Maybe Rebaseline)
    deriving stock (Eq, Show)

-- | Plan a one-shot run's upload. One asked for with no store is refused, so a scripted run cannot succeed having published nothing.
uploadPlan :: PilotCompileOptions -> Maybe AdvisoryStoreUrl -> Either PilotUploadUnconfigured UploadPlan
uploadPlan opts mStore = case pcoUpload opts of
    NoUpload -> Right UploadSkipped
    Upload -> uploadingTo Nothing
    UploadRebaselined approval -> uploadingTo (Just approval)
  where
    uploadingTo approval = maybe (Left PilotUploadUnconfigured) (\store -> Right (UploadTo store approval)) mStore

-- | Where one compiled artifact lands: the store's bucket and its file's key under the prefix, which the proxy's sync reads.
uploadTarget :: AdvisoryStoreUrl -> FilePath -> (Text, Text)
uploadTarget store dbPath =
    (advisoryStoreBucket store, advisoryObjectKey store (takeFileName dbPath))

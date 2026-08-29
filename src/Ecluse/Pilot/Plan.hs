-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The decisions Pilot makes before it does anything: whether the scheduled loop exports at
all and how often, which upstreams one compile reads, and whether a one-shot run uploads.

Each is a pure function over the resolved configuration, so "Ecluse.Pilot" dispatches on their
results instead of branching inside @IO@ ("Ecluse.Composition.MirrorRole" is the same shape).
-}
module Ecluse.Pilot.Plan (
    -- * The scheduled export loop
    ExportLoopPlan (..),
    exportLoopPlan,
    exportCadenceMicros,
    idleCadenceMicros,

    -- * The upstreams a compile reads
    configuredSources,
    compileSources,

    -- * The one-shot run
    PilotCompileOptions (..),
    UploadPlan (..),
    uploadPlan,
    PilotUploadUnconfigured (..),
    uploadTarget,
) where

import Data.Time (NominalDiffTime)
import System.FilePath (takeFileName)

import Ecluse.Config (
    AdvisoriesSettings (advCompileInterval, advEpssFeedUrl, advOsvExportBaseUrl, advUrl),
    AdvisoryStoreUrl,
    advisoryObjectKey,
    advisoryStoreBucket,
    unUrl,
 )
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (CompileSources (..))

-- | What the scheduled export loop does with the resolved advisory settings.
data ExportLoopPlan
    = -- | No advisory store is configured, so the loop idles and exports nothing.
      ExportIdle
    | -- | Compile and upload to this store, one cycle per cadence.
      ExportTo AdvisoryStoreUrl
    deriving stock (Eq, Show)

-- | A configured store is the only thing that turns exporting on, as it is for the proxy's sync.
exportLoopPlan :: AdvisoriesSettings -> ExportLoopPlan
exportLoopPlan = maybe ExportIdle ExportTo . advUrl

{- | The delay between export cycles. The config decoder bounds @compileInterval@ to
@maxBound \`div\` 1000000@ seconds, so this conversion cannot wrap to a negative delay.
-}
exportCadenceMicros :: AdvisoriesSettings -> Int
exportCadenceMicros = secondsToMicros . advCompileInterval

{- | How long the idle loop sleeps between wakeups. Nothing observes the wakeup, because an
added store only takes effect on the next boot, so the sleep is deliberately long.
-}
idleCadenceMicros :: Int
idleCadenceMicros = secondsToMicros (24 * 60 * 60)

secondsToMicros :: NominalDiffTime -> Int
secondsToMicros seconds = round seconds * 1000000

{- | The upstreams a scheduled cycle reads, both configured keys so a moved or mirrored feed
never needs a new binary.
-}
configuredSources :: AdvisoriesSettings -> Text -> CompileSources
configuredSources advisories ecosystem =
    CompileSources
        { csOsvExportUrl = osvExportUrl (unUrl (advOsvExportBaseUrl advisories)) ecosystem
        , csEpssFeedUrl = toString (unUrl (advEpssFeedUrl advisories))
        }

{- | The upstreams a one-shot run reads: its own overrides over 'configuredSources'. Each feed
overrides on its own, so pinning one leaves the other configured.
-}
compileSources :: AdvisoriesSettings -> PilotCompileOptions -> CompileSources
compileSources advisories opts =
    CompileSources
        { csOsvExportUrl = fromMaybe (csOsvExportUrl configured) (pcoSource opts)
        , csEpssFeedUrl = fromMaybe (csEpssFeedUrl configured) (pcoEpssSource opts)
        }
  where
    configured = configuredSources advisories (pcoEcosystem opts)

-- | Options for the one-shot @ecluse pilot compile@ mode.
data PilotCompileOptions = PilotCompileOptions
    { pcoEcosystem :: Text
    , pcoSource :: Maybe String
    {- ^ Overrides the export URL. 'Nothing' selects the configured export
    base for the ecosystem ('osvExportUrl' under @osvExportBaseUrl@).
    -}
    , pcoEpssSource :: Maybe String
    -- ^ Overrides the EPSS feed URL. 'Nothing' selects the configured @epssFeedUrl@.
    , pcoOutDir :: FilePath
    , pcoUpload :: Bool
    -- ^ Upload the compiled artifact to the configured advisory store.
    }
    deriving stock (Eq, Show)

{- | Requesting an upload without a configured advisory store. It is a wiring fault at the
composition root, so it throws rather than returning a value the caller could only re-raise.
-}
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

{- | Plan a one-shot run's upload. Asking for one with no store configured is refused, never
skipped, so a scripted run cannot exit successfully having published nothing.
-}
uploadPlan :: PilotCompileOptions -> Maybe AdvisoryStoreUrl -> Either PilotUploadUnconfigured UploadPlan
uploadPlan opts mStore
    | not (pcoUpload opts) = Right UploadSkipped
    | otherwise = maybe (Left PilotUploadUnconfigured) (Right . UploadTo) mStore

{- | Where one compiled artifact lands: the store's bucket, and the object key its own file
name takes under the store's prefix. The proxy's sync reads that same key.
-}
uploadTarget :: AdvisoryStoreUrl -> FilePath -> (Text, Text)
uploadTarget store dbPath =
    (advisoryStoreBucket store, advisoryObjectKey store (takeFileName dbPath))

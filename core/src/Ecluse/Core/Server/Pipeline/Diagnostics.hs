-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The operator-facing diagnostics the packument serve path emits:

* the per-origin metadata-fetch failure log
* the response-bound breach warning
* the cross-upstream integrity-divergence warning (threat #11)
* the dropped-entry warning a successful-but-degraded projection produces

Each one is a log line with its structured payload, or a metered warning. An operator
filters it, alarms on it, or reads it back during an incident. None of them changes what
is served. The serve path emits them once per real fetch or merge, inside the request's
@katip@ context, so every line carries the request's trace correlation. The per-condition
log helpers these dispatch to ('logDecodeFailure' and friends) live in
"Ecluse.Core.Server.Pipeline.Internal". This module owns the packument-specific
renderings.
-}
module Ecluse.Core.Server.Pipeline.Diagnostics (
    logMetadataFailure,
    logInvalidEntries,
    warnDivergences,
) where

import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)

import Ecluse.Core.Package (
    HashAlg,
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidPublishTime, InvalidVersionManifest),
    PackageName,
    renderHashAlg,
    renderPackageName,
 )
import Ecluse.Core.Package.Merge (
    Divergence (divLosing, divVersion, divWinning),
    IntegrityFingerprint,
    MergePlan (mpDivergences),
    integrityHashes,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable, MetadataUnreachable, MetadataUrlUnformable),
 )
import Ecluse.Core.Security (LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions), authorityLabel)
import Ecluse.Core.Server.Context (Handler)
import Ecluse.Core.Server.Pipeline.Internal (
    logDecodeFailure,
    logNameMismatch,
    logUpstreamUnformable,
    logUpstreamUnreachable,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | Log a per-origin metadata-fetch failure, dispatched by its cause:

* a response-bound breach names the ceiling crossed ('logBreach')
* an undecodable body takes the silent-guard decode log ('logDecodeFailure')
* a self-reported /different/ name takes the name-mismatch log ('logNameMismatch')
* an unformable configured base URL takes the config-fault log ('logUpstreamUnformable')
* an unreachable origin takes the outage log ('logUpstreamUnreachable')

Called once per real fetch, inside the single-flight leader, in the request's context.
-}
logMetadataFailure :: PackageName -> Text -> MetadataError -> Handler ()
logMetadataFailure name baseUrl = \case
    MetadataBoundExceeded err -> logBreach name err
    MetadataUndecodable -> logDecodeFailure name
    MetadataNameMismatch reported -> logNameMismatch name baseUrl reported
    MetadataUrlUnformable urlErr -> logUpstreamUnformable name baseUrl urlErr
    MetadataUnreachable fault -> logUpstreamUnreachable name baseUrl fault

{- Log a response-bound breach at 'WarningS' before the contribution degrades
fail-closed. An operator can then tell a bound breach from an ordinary parse failure
or an upstream outage. A breach means a hostile or oversized upstream, or a too-tight
cap. The structured payload names the package, the @bound@ crossed, and the observed
value against its @cap@. Those high-cardinality identifiers belong on the log line,
not on a metric label. The line rides the ambient @katip@ context (the request's, so
it carries the trace-correlation @dd@), under the @ecluse@ namespace the rest of the
stream uses. -}
logBreach :: (KatipContext m) => PackageName -> LimitError -> m ()
logBreach name err =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "bound" boundName
            <> sl "observed" observed
            <> sl "cap" cap

    message :: Text
    message = "refused an upstream metadata document: it exceeded the " <> boundName <> " response bound (observed " <> observed <> ", cap " <> cap <> ")"

    -- Pulled from the typed error, so the ceiling, the observed value, and the cap
    -- always agree with what was enforced.
    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge c -> ("body-size", "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

{- | Log a cross-upstream integrity divergence (threat #11) at 'WarningS' and meter it.
A public copy contradicts the trusted one on a shared integrity algorithm for a shared
version. The trusted copy still won the merge, and it is served, or withheld under
'FailClosed'. So this is the supply-chain signal an operator alarms on, never a silent
reconciliation. The structured payload names the package and the contradicting versions.
The @ecluse.registry.merge.divergence@ counter counts one per contradicting version. A
clean merge logs and meters nothing.
-}
warnDivergences :: (KatipContext m) => MetricsPort -> PackageName -> MergePlan -> m ()
warnDivergences metrics name plan =
    case toList (mpDivergences plan) of
        [] -> pass
        divs -> do
            liftIO (for_ divs (const (mpMergeDivergence metrics)))
            katipAddContext (payload divs) $ logFM WarningS (ls (message divs))
  where
    payload divs =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "versions" (T.intercalate "," (map divVersion divs))
    message divs =
        "cross-upstream integrity divergence: the trusted copy of "
            <> renderPackageName name
            <> " is served, but a public copy contradicts it on a shared integrity algorithm for "
            <> show (length divs)
            <> " version(s): "
            <> T.intercalate "; " (map renderDivergence divs)

renderDivergence :: Divergence -> Text
renderDivergence d =
    divVersion d
        <> " (trusted "
        <> renderFingerprint (divWinning d)
        <> " vs public "
        <> renderFingerprint (divLosing d)
        <> ")"

renderFingerprint :: IntegrityFingerprint -> Text
renderFingerprint fp = "{" <> T.intercalate ", " (map renderHash (integrityHashes fp)) <> "}"

renderHash :: (Text, Maybe HashAlg, Text) -> Text
renderHash (file, alg, body) = file <> " " <> maybe "none" renderHashAlg alg <> ":" <> body

{- | Log at 'WarningS' the malformed packument entries the projection dropped rather
than failing the whole document on. An operator sees that an upstream served a malformed
entry, which kind it was, and __the raw value it sent__. The kinds are a version
manifest, a dist-tag, and a per-version publish time. The structured payload names the
package, the upstream's authority, the per-kind drop counts, and a bounded sample of the
dropped entries. Each sample renders its raw 'Data.Aeson.Value', truncated if large and
capped to 'maxRenderedDrops' entries, so a flood of drops cannot bloat the line. The
dropped versions are still served minus those entries (graceful degradation), so this is
an observability signal, not a refusal. The serve path emits it once per real fetch,
inside the cache leader, so a coalesced follower never re-logs, and through the request's
@katip@ context. The caller guards on a non-empty list, so a clean document logs
nothing.
-}
logInvalidEntries :: (KatipContext m) => PackageName -> Text -> [InvalidEntry] -> m ()
logInvalidEntries name baseUrl entries =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "upstream" (authorityLabel baseUrl)
            <> sl "droppedVersionManifests" manifests
            <> sl "droppedDistTags" distTags
            <> sl "droppedPublishTimes" publishTimes
            <> sl "droppedEntries" (map renderDroppedEntry (take maxRenderedDrops entries))

    (manifests, distTags, publishTimes, entriesLen) =
        foldl'
            accumulateDropCounts
            (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
            entries

    accumulateDropCounts (m, d, p, l) e =
        case invalidKind e of
            InvalidVersionManifest -> (m + 1, d, p, l + 1)
            InvalidDistTag -> (m, d + 1, p, l + 1)
            InvalidPublishTime -> (m, d, p + 1, l + 1)

    message :: Text
    message =
        "dropped " <> show entriesLen <> " malformed entr" <> plural <> " from an upstream packument (the rest is served)"
    plural = if entriesLen == 1 then "y" else "ies"

-- One dropped entry for the operator, carrying the raw value the upstream sent
-- (truncated) so the offending bytes stay visible.
renderDroppedEntry :: InvalidEntry -> Text
renderDroppedEntry e =
    renderInvalidKind (invalidKind e)
        <> " "
        <> invalidKey e
        <> " = "
        <> truncatedValue (invalidValue e)
        <> " ("
        <> invalidReason e
        <> ")"

renderInvalidKind :: InvalidEntryKind -> Text
renderInvalidKind = \case
    InvalidVersionManifest -> "version-manifest"
    InvalidDistTag -> "dist-tag"
    InvalidPublishTime -> "publish-time"

-- The raw value as compact JSON, truncated to 'maxRenderedValueChars': only that many
-- characters are ever forced, so a huge value never balloons the log line.
truncatedValue :: Value -> Text
truncatedValue v =
    let rendered = TL.toStrict (TL.take (fromIntegral maxRenderedValueChars + 1) (encodeToLazyText v))
     in if T.compareLength rendered maxRenderedValueChars == GT
            then T.take maxRenderedValueChars rendered <> "…"
            else rendered

-- How many dropped entries the drop-tracking log renders in full, and how many
-- characters of each raw value. An unbounded flood of malformed entries, or one huge
-- value, therefore cannot bloat a single log line. The per-kind counts in the payload
-- still report the full totals.
maxRenderedDrops :: Int
maxRenderedDrops = 20

maxRenderedValueChars :: Int
maxRenderedValueChars = 200

-- The @module@ tag this module's breach log carries. It is the operator-facing log
-- filter key, held stable as this value rather than the source module path, so an
-- operator's saved filter keeps matching. The decode-failure log lives in
-- "Ecluse.Core.Server.Pipeline.Internal", tagged likewise.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

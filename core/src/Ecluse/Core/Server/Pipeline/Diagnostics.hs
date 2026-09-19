-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Operator diagnostics for metadata failures, dropped entries, and integrity divergence.
Access-refusal logs contain no upstream body, headers, or credential.

Two @module@ filter keys are emitted here. The bad-upstream warnings keep
'pipelineInternalModule', and everything else carries 'pipelineModule'. Both are held stable
as values rather than source module paths, so an operator's saved filter keeps matching.
-}
module Ecluse.Core.Server.Pipeline.Diagnostics (
    -- * Metadata-read failures
    logMetadataFailure,
    logDecodeFailure,
    logNameMismatch,
    logUpstreamUnformable,
    logUpstreamUnreachable,

    -- * Dropped entries and divergence
    logInvalidEntries,
    warnDivergences,
) where

import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Katip (KatipContext, Severity (ErrorS, WarningS), SimpleLogPayload, katipAddContext, logFM, ls, sl)

import Ecluse.Core.Fault (TransportFault (tfCause, tfDetail))
import Ecluse.Core.Fault.Http (isRetryableStatusCode)

import Ecluse.Core.Package (
    HashAlg,
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    PackageName,
    dropCountsByKind,
    renderHashAlg,
    renderInvalidEntryKind,
    renderPackageName,
 )
import Ecluse.Core.Package.Merge (
    Divergence (divLosing, divVersion, divWinning),
    IntegrityFingerprint,
    MergePlan (mpDivergences),
    integrityHashes,
 )
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    UrlFormationError,
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Security (
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyArtifacts, TooManyVersions),
    authorityLabel,
 )
import Ecluse.Core.Server.Pipeline.Internal (pipelineInternalModule)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Log once per real fetch, inside the single-flight leader's request context.
logMetadataFailure :: (KatipContext m) => PackageName -> Text -> MetadataError -> m ()
logMetadataFailure name baseUrl = \case
    MetadataAbsent -> logHttpFailure name baseUrl 404 "the upstream has no metadata for the requested package"
    MetadataHttpFailure code -> logHttpFailure name baseUrl code "the upstream refused the metadata read"
    MetadataAuthorisationFailure _ -> logFM WarningS "the upstream refused metadata access"
    MetadataBoundExceeded err -> logBreach name err
    MetadataUndecodable -> logDecodeFailure name
    MetadataNameMismatch reported -> logNameMismatch name baseUrl reported
    MetadataFetch (FetchBoundExceeded err) -> logBreach name err
    MetadataFetch (FetchUrlUnformable urlErr) -> logUpstreamUnformable name baseUrl urlErr
    MetadataFetch (FetchTransport fault) -> logUpstreamUnreachable name baseUrl fault

logHttpFailure :: (KatipContext m) => PackageName -> Text -> Int -> Text -> m ()
logHttpFailure name baseUrl code message =
    katipAddContext payload (logFM severity (ls message))
  where
    severity = if isRetryableStatusCode code then ErrorS else WarningS
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "upstream" (authorityLabel baseUrl)
            <> sl "status" code

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

    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge c -> ("body-size", "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooManyArtifacts seen c -> ("artifact-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

-- The fields every bad-upstream warning carries, before the caller's own. The response-bound
-- guards leave these conditions silent, so an operator would otherwise see nothing at all.
warnUpstream :: (KatipContext m) => PackageName -> SimpleLogPayload -> Text -> m ()
warnUpstream name extra message =
    katipAddContext (prefix <> extra) $ logFM WarningS (ls message)
  where
    prefix = sl "module" pipelineInternalModule <> sl "package" (renderPackageName name)

-- | Warn that an upstream body did not decode into a usable packument.
logDecodeFailure :: (KatipContext m) => PackageName -> m ()
logDecodeFailure name =
    warnUpstream name mempty "refused an upstream metadata document: it did not decode into a usable packument"

{- | Warn that an origin's packument self-reported a name for a different package, so an
operator can tell a misconfigured or hostile upstream from an ordinary outage.
-}
logNameMismatch :: (KatipContext m) => PackageName -> Text -> Text -> m ()
logNameMismatch requested origin reported =
    warnUpstream
        requested
        (sl "origin" (authorityLabel origin) <> sl "upstreamName" reported)
        "dropped an upstream contribution: its packument self-reported a name for a different package"

{- | Warn that this origin's configured base URL could not be formed into a request, so an
operator sees a misconfigured mount rather than an upstream that merely appears unreachable.
-}
logUpstreamUnformable :: (KatipContext m) => PackageName -> Text -> UrlFormationError -> m ()
logUpstreamUnformable name origin urlErr =
    warnUpstream
        name
        (sl "origin" (authorityLabel origin) <> sl "urlError" (renderUrlFormationError urlErr))
        "refused an upstream metadata fetch: the configured base URL could not be formed into a request"

{- | Warn that the transport failed before a usable body returned, so an operator can tell an
outage from a decode failure or a misconfigured mount.
-}
logUpstreamUnreachable :: (KatipContext m) => PackageName -> Text -> TransportFault -> m ()
logUpstreamUnreachable name origin fault =
    warnUpstream
        name
        ( sl "origin" (authorityLabel origin)
            <> sl "transportCause" (show (tfCause fault) :: Text)
            <> sl "transportDetail" (tfDetail fault)
        )
        "an upstream metadata fetch could not reach the origin; its contribution degrades this request"

-- | The malformed packument entries the projection dropped rather than failing the whole document.
logInvalidEntries :: (KatipContext m) => PackageName -> Text -> [InvalidEntry] -> m ()
logInvalidEntries name baseUrl entries =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "upstream" (authorityLabel baseUrl)
            <> sl "droppedByKind" (dropCountsByKind entries)
            <> sl "droppedEntries" (map renderDroppedEntry (take maxRenderedDrops entries))

    entriesLen :: Int
    entriesLen = length entries

    message :: Text
    message =
        "dropped " <> show entriesLen <> " malformed entr" <> plural <> " from an upstream packument (the rest is served)"
    plural = if entriesLen == 1 then "y" else "ies"

renderDroppedEntry :: InvalidEntry -> Text
renderDroppedEntry e =
    renderInvalidEntryKind (invalidKind e)
        <> " "
        <> invalidKey e
        <> " = "
        <> truncatedValue (invalidValue e)
        <> " ("
        <> invalidReason e
        <> ")"

-- Only 'maxRenderedValueChars' characters are ever forced, so a huge value never balloons
-- the log line.
truncatedValue :: Value -> Text
truncatedValue v =
    let rendered = TL.toStrict (TL.take (fromIntegral maxRenderedValueChars + 1) (encodeToLazyText v))
     in if T.compareLength rendered maxRenderedValueChars == GT
            then T.take maxRenderedValueChars rendered <> "…"
            else rendered

-- How many dropped entries the log renders in full, and how many characters of each raw value, so
-- a flood of drops or one huge value cannot bloat a log line. The per-kind counts stay complete.
maxRenderedDrops :: Int
maxRenderedDrops = 20

maxRenderedValueChars :: Int
maxRenderedValueChars = 200

-- | Warn and increment the divergence metric when shared digests disagree across origins.
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

-- The @module@ filter key for this module's own lines, held stable as this value rather than
-- the source module path, so an operator's saved filter keeps matching.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

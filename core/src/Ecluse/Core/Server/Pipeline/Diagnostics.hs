-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Operator diagnostics for metadata failures, dropped entries, and integrity divergence.
Access-refusal logs contain no upstream body, headers, or credential.

The bad-upstream warnings carry 'pipelineInternalModule' as their @module@ filter key and the other
payload-bearing lines carry 'pipelineModule', both held stable as values rather than source module
paths, so an operator's saved filter keeps matching.
-}
module Ecluse.Core.Server.Pipeline.Diagnostics (
    -- * Metadata-read failures
    logMetadataFailure,
    logDirectoryFailure,
    logDecodeFailure,
    logNameMismatch,
    logUpstreamUnformable,
    logUpstreamUnreachable,

    -- * Dropped entries and divergence
    logInvalidEntries,
    logDirectoryInvalidEntries,
    warnDivergences,
) where

import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Katip (KatipContext, Severity (DebugS, ErrorS, WarningS), SimpleLogPayload, katipAddContext, logFM, ls, sl)

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
    FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport, FetchUrlUnformable),
    UrlFormationError,
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Security (
    BodyLimit (..),
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyArtifacts, TooManyVersions),
    authorityLabel,
    bodyLimitBytes,
 )
import Ecluse.Core.Server.Pipeline.Internal (pipelineInternalModule)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Log once per real fetch, inside the single-flight leader's request context.
logMetadataFailure :: (KatipContext m) => PackageName -> Text -> MetadataError -> m ()
logMetadataFailure = logFailureOf . packageSubject

-- | 'logMetadataFailure' for a registry-wide directory read. Its lines name no package.
logDirectoryFailure :: (KatipContext m) => Text -> MetadataError -> m ()
logDirectoryFailure = logFailureOf mempty

-- The subject payload names what was read: one package, or nothing for a directory.
logFailureOf :: (KatipContext m) => SimpleLogPayload -> Text -> MetadataError -> m ()
logFailureOf subject baseUrl = \case
    MetadataAbsent -> logHttpFailure subject baseUrl 404 "the upstream has no metadata for the requested package"
    MetadataHttpFailure code -> logHttpFailure subject baseUrl code "the upstream refused the metadata read"
    MetadataAuthorisationFailure _ -> logFM WarningS "the upstream refused metadata access"
    MetadataBoundExceeded err -> logBreach subject err
    MetadataUndecodable -> warnDecodeFailure subject
    MetadataNameMismatch reported -> warnNameMismatch subject baseUrl reported
    MetadataFetch (FetchBoundExceeded err) -> logBreach subject err
    MetadataFetch (FetchUrlUnformable urlErr) -> warnUnformable subject baseUrl urlErr
    MetadataFetch (FetchTransport fault) -> warnUnreachable subject baseUrl fault
    MetadataFetch (FetchCooldown _) -> pass

packageSubject :: PackageName -> SimpleLogPayload
packageSubject name = sl "package" (renderPackageName name)

logHttpFailure :: (KatipContext m) => SimpleLogPayload -> Text -> Int -> Text -> m ()
logHttpFailure subject baseUrl code message =
    katipAddContext payload (logFM severity (ls message))
  where
    severity
        | code == 404 = DebugS
        | isRetryableStatusCode code = ErrorS
        | otherwise = WarningS
    payload =
        sl "module" pipelineModule
            <> subject
            <> sl "upstream" (authorityLabel baseUrl)
            <> sl "status" code

logBreach :: (KatipContext m) => SimpleLogPayload -> LimitError -> m ()
logBreach subject err =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> subject
            <> sl "bound" boundName
            <> sl "observed" observed
            <> sl "cap" cap

    message :: Text
    message = "refused an upstream metadata document: it exceeded the " <> boundName <> " response bound (observed " <> observed <> ", cap " <> cap <> ")"

    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge bound ->
            let c = bodyLimitBytes bound
                role = case bound of
                    MetadataBodyLimit _ -> "metadata-body-size"
                    PublishRequestBodyLimit _ -> "publish-request-body-size"
                    MirrorArtifactBodyLimit _ -> "mirror-artifact-body-size"
             in (role, "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooManyArtifacts seen c -> ("artifact-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

-- The fields every bad-upstream warning carries, before the caller's own. The response-bound
-- guards leave these conditions silent, so an operator would otherwise see nothing at all.
warnUpstream :: (KatipContext m) => SimpleLogPayload -> SimpleLogPayload -> Text -> m ()
warnUpstream subject extra message =
    katipAddContext (sl "module" pipelineInternalModule <> subject <> extra) $ logFM WarningS (ls message)

-- | Warn that an upstream body did not decode into a usable packument.
logDecodeFailure :: (KatipContext m) => PackageName -> m ()
logDecodeFailure = warnDecodeFailure . packageSubject

warnDecodeFailure :: (KatipContext m) => SimpleLogPayload -> m ()
warnDecodeFailure subject =
    warnUpstream subject mempty "refused an upstream metadata document: it did not decode into a usable packument"

-- | Warn when an upstream declares a different package identity.
logNameMismatch :: (KatipContext m) => PackageName -> Text -> Text -> m ()
logNameMismatch = warnNameMismatch . packageSubject

warnNameMismatch :: (KatipContext m) => SimpleLogPayload -> Text -> Text -> m ()
warnNameMismatch subject origin reported =
    warnUpstream
        subject
        (sl "origin" (authorityLabel origin) <> sl "upstreamName" reported)
        "dropped an upstream contribution: its packument self-reported a name for a different package"

-- | Warn when a configured origin cannot form a request URL.
logUpstreamUnformable :: (KatipContext m) => PackageName -> Text -> UrlFormationError -> m ()
logUpstreamUnformable = warnUnformable . packageSubject

warnUnformable :: (KatipContext m) => SimpleLogPayload -> Text -> UrlFormationError -> m ()
warnUnformable subject origin urlErr =
    warnUpstream
        subject
        (sl "origin" (authorityLabel origin) <> sl "urlError" (renderUrlFormationError urlErr))
        "refused an upstream metadata fetch: the configured base URL could not be formed into a request"

-- | Report transport failure separately from decoding and configuration failures.
logUpstreamUnreachable :: (KatipContext m) => PackageName -> Text -> TransportFault -> m ()
logUpstreamUnreachable = warnUnreachable . packageSubject

warnUnreachable :: (KatipContext m) => SimpleLogPayload -> Text -> TransportFault -> m ()
warnUnreachable subject origin fault =
    warnUpstream
        subject
        ( sl "origin" (authorityLabel origin)
            <> sl "transportCause" (show (tfCause fault) :: Text)
            <> sl "transportDetail" (tfDetail fault)
        )
        "an upstream metadata fetch could not reach the origin; its contribution degrades this request"

-- | The malformed packument entries the projection dropped rather than failing the whole document.
logInvalidEntries :: (KatipContext m) => PackageName -> Text -> [InvalidEntry] -> m ()
logInvalidEntries name = logDroppedEntries (packageSubject name) "packument"

-- | The malformed rows a registry-wide directory dropped. The line names no package.
logDirectoryInvalidEntries :: (KatipContext m) => Text -> [InvalidEntry] -> m ()
logDirectoryInvalidEntries = logDroppedEntries mempty "directory"

logDroppedEntries :: (KatipContext m) => SimpleLogPayload -> Text -> Text -> [InvalidEntry] -> m ()
logDroppedEntries subject document baseUrl entries =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> subject
            <> sl "upstream" (authorityLabel baseUrl)
            <> sl "droppedByKind" (dropCountsByKind entries)
            <> sl "droppedEntries" (map renderDroppedEntry (take maxRenderedDrops entries))

    entriesLen :: Int
    entriesLen = length entries

    message :: Text
    message =
        "dropped " <> show entriesLen <> " malformed entr" <> plural <> " from an upstream " <> document <> " (the rest is served)"
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

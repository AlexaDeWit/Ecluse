-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory lookup capability: answer CVE questions about a package
version from a local, already-synced @osv.db@ artifact, never the network.

The handle is deliberately dumb data access over one artifact file. Rule
semantics live in pure predicates over what it returns
('insideAffectedRange'), because SQLite's text collation cannot order
versions; only 'Ecluse.Core.Version.compareVersions' can. The one deliberate
exception is 'cveRemediationProbe': a fixed bound in the artifact is a single
canonical version string, so exact-fix matching is plain string equality and
rides the @(package_name, fixed_version)@ index in one traversal. A fix
published under a non-canonical version string misses the probe and simply
waits out the ordinary quarantine; the operator workaround is an explicit
'Ecluse.Core.Rules.Types.AllowByIdentity' rule.

An artifact is accepted or rejected at 'openCveDb' (epoch stamp, integrity,
strict-schema conformance, ecosystem), with rejection as a value: the caller
keeps its last known-good handle and alarms. See "Ecluse.Core.Cve.Internal"
for the hardening detail.

__Ownership is split at the type level__: 'openCveDb' yields a 'CveDb', the
owning resource whose holder alone may 'cveDbClose'; consumers are handed only
its 'CveLookup' view, so nothing evaluating rules can release a shared
connection. The owner (the background sync's shadow-swap, which retires an
artifact only when no evaluation still reads it) holds the 'CveDb' and closes it
explicitly.
-}
module Ecluse.Core.Cve (
    -- * The owning resource
    CveDb (..),
    openCveDb,

    -- * The consumer view
    CveLookup (..),

    -- * What a lookup returns
    AdvisoryRange (..),

    -- * Rejection
    CveDbRejected (..),

    -- * Query faults
    CveQueryFault (..),

    -- * Artifact identity
    DbEtag (..),

    -- * Pure range matching
    insideAffectedRange,
    severityAtLeast,
) where

import UnliftIO.Exception (catch, catchAny, onException, throwIO)

import Ecluse.Core.Cve.Internal (AdvisoryRange (..), CveDbRejected (..), advisoriesQuery, openHardenedConnection, probeQuery, provenanceQuery)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Version (compareVersions, mkVersion)

import Database.SQLite.Simple (Connection, SQLError, close)

{- | An artifact version marker: S3's ETag, opaque text compared for equality
only. Two objects with equal ETags carry equal bytes, so an unchanged ETag is
"nothing to do", a rejected artifact's remembered ETag is "still the same bad
artifact", and it names the exact advisory database a rule decision was
recorded against.
-}
newtype DbEtag = DbEtag Text
    deriving stock (Eq, Show)

{- | Advisory questions about one ecosystem's artifact -- the read-only view a
consumer (a rule evaluation) is handed. It deliberately cannot release the
underlying connection; that is the owning 'CveDb''s capability.

Names and versions are the artifact's own vocabulary: the OSV wire package
name (scope inline, e.g. @\@scope\/name@) and verbatim version text. Callers
render their domain values to that form at the boundary.
-}
data CveLookup = CveLookup
    { cveRemediationProbe :: Text -> Text -> IO Bool
    {- ^ Does any advisory for this package name this exact version string as
    a fixed bound? One indexed B-tree traversal. A query fault throws the
    confined 'CveQueryFault' (see its Haddock for the confinement contract).
    -}
    , cveAdvisoriesFor :: Text -> IO [AdvisoryRange]
    {- ^ Every advisory range recorded against a package name; rule predicates
    interpret them. A query fault throws the confined 'CveQueryFault'.
    -}
    }

{- | A query the accepted advisory database could not answer: the SQLite edge
threw mid-query (an I\/O error on the database file, a connection released out
from under a straggling reader). Carries which handle field was asked and the
rendered 'SQLError' for the log line; the artifact's __content__ can never
produce this ('openCveDb' acceptance made the row decodes total), so it marks an
infrastructural fault, not a data fault.

A __confined typed exception__, the same shape as
'Ecluse.Core.Credential.Refresh.CredentialError' at the breaker leaf: it is
thrown at the SQLite edge inside the handle and absorbed by the one boundary
every advisory query runs under -- the rules engine's resilience harness
('Ecluse.Core.Rules.runEffectfulRule'), which resolves it to an @Unavailable@
evaluation and advances the rule's circuit breaker. It never crosses that
boundary, so no caller above the rules engine sees it. The alternative (an
@Either@ on every 'CveLookup' field) would reshape every rule's evaluation type
for a fault only the harness ever handles.
-}
data CveQueryFault = CveQueryFault
    { cqfQuery :: Text
    -- ^ Which handle field was asked (@remediation-probe@ or @advisories-for@).
    , cqfDetail :: Text
    -- ^ The rendered 'SQLError', for the harness's log line. Never parsed.
    }
    deriving stock (Eq, Show)

instance Exception CveQueryFault

{- | One opened artifact: the consumer view plus the owner's close. Whoever
holds this owns the connection's lifetime; hand consumers 'cveDbLookup' only.
-}
data CveDb = CveDb
    { cveDbLookup :: CveLookup
    -- ^ The view consumers query through.
    , cveDbClose :: IO ()
    {- ^ Release the artifact's connection. Owner-only; the artifact must no
    longer be read through this handle's view afterwards. __Never throws__: a
    close fault is absorbed inside the handle, since the connection is being
    discarded either way and every close site (a swap-out drain, an exception
    unwind) wants the same disposition.
    -}
    , cveDbMeta :: [(Text, Text)]
    {- ^ The artifact's @meta@ provenance rows (Pilot version, ecosystem, build
    timestamp, source URL, row count), snapshotted at open, key-sorted. The
    audit surface that ties this handle's decisions to the exact database that
    produced them.
    -}
    }

{- | Open an @osv.db@ artifact and build the owning handle over it, or reject
the artifact ('CveDbRejected') with its connection already closed. Nothing an
artifact carries can make this throw: acceptance admits only a conformant
@STRICT@ schema whose stored values the integrity walk verified, so the
provenance decode below it is total. Faults below the artifact contract (an
unopenable file) still throw, and then too the connection is already closed:
an exception never leaks it.
-}
openCveDb :: Ecosystem -> FilePath -> IO (Either CveDbRejected CveDb)
openCveDb eco dbFile =
    openHardenedConnection eco dbFile >>= \case
        Left rejection -> pure (Left rejection)
        Right conn -> do
            -- Until the handle is handed over, this side owns the connection;
            -- the guard is the no-leak backstop for faults below the artifact
            -- contract (acceptance has made the decode itself total).
            meta <- provenanceQuery conn `onException` close conn
            pure (Right (mkCveDb conn meta))

mkCveDb :: Connection -> [(Text, Text)] -> CveDb
mkCveDb conn meta =
    CveDb
        { cveDbLookup =
            CveLookup
                { cveRemediationProbe = \name version -> taggedQuery "remediation-probe" (probeQuery conn name version)
                , cveAdvisoriesFor = taggedQuery "advisories-for" . advisoriesQuery conn
                }
        , -- Total by construction: the connection is being discarded, so a close
          -- fault has no better disposition anywhere than "absorb it here".
          cveDbClose = close conn `catchAny` const pass
        , cveDbMeta = meta
        }

-- The SQLite edge: re-raise a mid-query 'SQLError' as the handle's confined
-- 'CveQueryFault', tagged with which field was asked, so what escapes the handle
-- is this module's closed vocabulary rather than the driver's exception type.
taggedQuery :: Text -> IO a -> IO a
taggedQuery tag act = act `catch` \(err :: SQLError) -> throwIO (CveQueryFault tag (show err))

{- | Is this version inside the advisory segment's affected interval, under the
ecosystem's version ordering? The interval is @introduced <= v@ bounded above by
either @v < fixed@ (exclusive) or @v <= last_affected@ (inclusive), whichever the
segment carries, or unbounded when it carries neither. A point segment
(@introduced == last_affected@) is affected only at that exact version.

__Fail-closed.__ Both the remediation fast lane (a fix must not fast-track while
it sits inside another advisory's affected range) and the deny gate (an
unvettable version must not be admitted) want the same polarity, so every
unprovable comparison, an unparseable bound or version, counts as __inside__:
trust is only ever granted on evidence.
-}
insideAffectedRange :: Ecosystem -> Text -> AdvisoryRange -> Bool
insideAffectedRange eco versionText ar = atOrAboveIntroduced && withinUpperBound
  where
    v = mkVersion eco versionText

    atOrAboveIntroduced = case arIntroduced ar of
        -- No introduced bound: the range starts at the beginning.
        Nothing -> True
        Just i -> case compareVersions v (mkVersion eco i) of
            Just LT -> False
            Just _ -> True
            Nothing -> True

    withinUpperBound = case (arFixed ar, arLastAffected ar) of
        -- A fix is an exclusive upper bound: affected while v < fixed.
        (Just f, _) -> case compareVersions v (mkVersion eco f) of
            Just LT -> True
            Just _ -> False
            Nothing -> True
        -- last_affected is an inclusive upper bound: affected while v <= it.
        (Nothing, Just la) -> case compareVersions v (mkVersion eco la) of
            Just GT -> False
            Just _ -> True
            Nothing -> True
        -- No upper bound: the range never ends.
        (Nothing, Nothing) -> True

{- | Does this advisory segment's severity meet or exceed the threshold (a CVSS
base score, 0 to 10)? 'arSeverity' is the artifact's normalised numeric score
(Pilot reduces a CVSS vector or a qualitative label to a number at ingest), or
absent.

__Fail-closed for the deny direction.__ The threshold gates a deny, so a severity
that cannot be shown to fall below it counts as meeting it: an unscored advisory
('Nothing' -- most of the npm malware feed) returns 'True'. Only a score strictly
below the threshold returns 'False'.
-}
severityAtLeast :: Double -> Maybe Double -> Bool
severityAtLeast threshold = maybe True (>= threshold)

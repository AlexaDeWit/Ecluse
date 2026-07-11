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

An artifact is accepted or rejected at 'openCveDb' (epoch stamp, table shape,
ecosystem), with rejection as a value: the caller keeps its last known-good
handle and alarms. See "Ecluse.Core.Cve.Internal" for the hardening detail.

__Ownership is split at the type level__: 'openCveDb' yields a 'CveDb', the
owning resource whose holder alone may 'cveDbClose'; consumers are handed only
its 'CveLookup' view, so nothing evaluating rules can release a shared
connection. A lexically-scoped use (a test, a one-shot check) brackets with
'withCveDb'; a dynamically-scoped owner (the background sync's shadow-swap,
which retires an artifact only when no evaluation still reads it) holds the
'CveDb' and closes explicitly.
-}
module Ecluse.Core.Cve (
    -- * The owning resource
    CveDb (..),
    openCveDb,
    withCveDb,

    -- * The consumer view
    CveLookup (..),

    -- * What a lookup returns
    AdvisoryRange (..),

    -- * Rejection
    CveDbRejected (..),

    -- * Artifact identity
    DbEtag (..),

    -- * Pure range matching
    insideAffectedRange,
    severityAtLeast,
) where

import UnliftIO.Exception (finally, onException)

import Ecluse.Core.Cve.Internal (AdvisoryRange (..), CveDbRejected (..), advisoriesQuery, openHardenedConnection, probeQuery, provenanceQuery)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Version (compareVersions, mkVersion)

import Database.SQLite.Simple (Connection, close)

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
    a fixed bound? One indexed B-tree traversal.
    -}
    , cveAdvisoriesFor :: Text -> IO [AdvisoryRange]
    {- ^ Every advisory range recorded against a package name; rule predicates
    interpret them.
    -}
    }

{- | One opened artifact: the consumer view plus the owner's close. Whoever
holds this owns the connection's lifetime; hand consumers 'cveDbLookup' only.
-}
data CveDb = CveDb
    { cveDbLookup :: CveLookup
    -- ^ The view consumers query through.
    , cveDbClose :: IO ()
    {- ^ Release the artifact's connection. Owner-only; the artifact must no
    longer be read through this handle's view afterwards.
    -}
    , cveDbMeta :: [(Text, Text)]
    {- ^ The artifact's @meta@ provenance rows (Pilot version, ecosystem, build
    timestamp, source URL, row count), snapshotted at open, key-sorted. The
    audit surface that ties this handle's decisions to the exact database that
    produced them.
    -}
    }

{- | Open an @osv.db@ artifact and build the owning handle over it, or reject
the artifact ('CveDbRejected') with its connection already closed. Throws on
faults below the acceptance contract (an unreadable file), and then too the
connection is already closed: an exception never leaks it.
-}
openCveDb :: Ecosystem -> FilePath -> IO (Either CveDbRejected CveDb)
openCveDb eco dbFile =
    openHardenedConnection eco dbFile >>= \case
        Left rejection -> pure (Left rejection)
        Right conn -> do
            -- Until the handle is handed over, this side owns the connection:
            -- acceptance decodes only the ecosystem row, so a further meta row
            -- can still fail to decode here, and that failure must close the
            -- connection rather than leak it.
            res <- provenanceQuery conn `onException` close conn
            case res of
                Left rejection -> do
                    close conn
                    pure (Left rejection)
                Right meta -> pure (Right (mkCveDb conn meta))

mkCveDb :: Connection -> [(Text, Text)] -> CveDb
mkCveDb conn meta =
    CveDb
        { cveDbLookup =
            CveLookup
                { cveRemediationProbe = probeQuery conn
                , cveAdvisoriesFor = advisoriesQuery conn
                }
        , cveDbClose = close conn
        , cveDbMeta = meta
        }

{- | Bracket a lexically-scoped use of an artifact: open, hand the consumer
view to the action, and close on any exit. A rejected artifact short-circuits
('Left') without running the action; its connection is already closed.
-}
withCveDb :: Ecosystem -> FilePath -> (CveLookup -> IO a) -> IO (Either CveDbRejected a)
withCveDb eco dbFile use =
    openCveDb eco dbFile >>= \case
        Left rejection -> pure (Left rejection)
        Right db -> Right <$> (use (cveDbLookup db) `finally` cveDbClose db)

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

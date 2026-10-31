-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | How durable mirror state stores, weighs and rechecks one format's read data.
Each publishing adapter declares a 'MirrorStateFormat', and composition binds it to the mount's
state store, mirror view and worker, so mirror state names no format.
-}
module Ecluse.Core.Mirror.State.Format (
    MirrorStateFormat (..),
    ProjectEnvelope (..),
    noProjectEnvelope,
) where

import Data.Aeson (Object, Value)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Registry (CompanionRequirement)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData)

-- | One format's operations on its own 'MirrorReadData' constructor. Another format's read data yields the neutral result each field names.
data MirrorStateFormat = MirrorStateFormat
    { msfKind :: Text
    -- ^ The @kind@ a stored source carries. A record whose source names another kind does not decode.
    , msfEncodeSource :: MirrorReadData -> Maybe Value
    -- ^ The stored source, @kind@ included. 'Nothing' for another format's read data, which refuses the write.
    , msfDecodeSource :: Object -> Parser MirrorReadData
    -- ^ Restore a stored source of this format's @kind@.
    , msfDecodeCharge :: Value -> Integer
    -- ^ The workspace a stored source reserves before it is parsed.
    , msfForceSource :: MirrorReadData -> ()
    -- ^ Materialise a decoded source under the decode reservation.
    , msfEnvelope :: ProjectEnvelope
    -- ^ The project-wide fields a record keeps once.
    , msfFragmentShell :: PackageDetails -> Integer
    -- ^ The retained allowance for a fragment's admission facts and roots.
    , msfSourceWeight :: MirrorReadData -> Integer
    -- ^ The retained weight of a stored source. Zero for another format's read data.
    , msfCompanions :: CompanionRequirement
    -- ^ Which retained files need a metadata companion. Boot checks the codec's native preparation against it.
    }

-- | Source fields a record keeps once per project rather than in each fragment.
data ProjectEnvelope = ProjectEnvelope
    { envelopeSplit :: MirrorReadData -> (Object, MirrorReadData)
    -- ^ An admitted source's project fields, and the source its fragment keeps without them.
    , envelopeCheck :: Object -> PackageDetails -> MirrorReadData -> Either Text ()
    -- ^ Recheck a stored envelope against the record's first stored fragment.
    }

-- | A format without project-wide fields. A stored envelope refuses the record, naming the format.
noProjectEnvelope :: Text -> ProjectEnvelope
noProjectEnvelope label =
    ProjectEnvelope
        { envelopeSplit = (mempty,)
        , envelopeCheck = \envelope _ _ -> unless (KeyMap.null envelope) (Left (label <> " record has an unexpected project envelope"))
        }

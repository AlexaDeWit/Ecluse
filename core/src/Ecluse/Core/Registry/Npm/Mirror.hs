-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How mirror state records an npm version's retained source fields.
module Ecluse.Core.Registry.Npm.Mirror (
    npmStateFormat,
) where

import Data.Aeson (Value (Object), object, (.:), (.=))

import Ecluse.Core.Mirror.State.Codec (sourceEntriesCharge)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (..), noProjectEnvelope)
import Ecluse.Core.Mirror.State.Weight (fragmentShellAllowance, valueBackingExcess)
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (NpmMirrorRead))
import Ecluse.Core.Server.Cache.VersionWeight.Fields (retainedRawWeight)

-- | A version keeps its retained fields as one object. Native inventory proves completion, so only the worker's in-memory record uses it.
npmStateFormat :: MirrorStateFormat
npmStateFormat =
    MirrorStateFormat
        { msfKind = "npm"
        , msfEncodeSource = \case
            NpmMirrorRead fields -> Just (object ["kind" .= ("npm" :: Text), "fields" .= fields])
            _ -> Nothing
        , msfDecodeSource = \value -> NpmMirrorRead <$> value .: "fields"
        , msfDecodeCharge = sourceEntriesCharge
        , msfForceSource = \case
            NpmMirrorRead fields -> force fields `seq` ()
            _ -> ()
        , msfEnvelope = noProjectEnvelope "npm"
        , msfFragmentShell = const fragmentShellAllowance
        , msfSourceWeight = \case
            NpmMirrorRead fields -> retainedRawWeight (fst npmCached (Object fields)) + valueBackingExcess (Object fields)
            _ -> 0
        , msfCompanions = DeclaredCompanions
        }

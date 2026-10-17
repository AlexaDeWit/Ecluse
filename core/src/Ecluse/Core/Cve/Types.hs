-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-artifact identity the sync writes and every reader carries, and why a fetch of
it failed. "Ecluse.Core.Cve" opens the artifact and "Ecluse.Runtime.Cve.Sync" installs it. The
rule vocabulary, the serve pipeline, the sweep and the sync span only carry these, so they live
apart from the module that opens SQLite.
-}
module Ecluse.Core.Cve.Types (
    DbEtag (..),
    OsvDbFetchFault (..),
    renderOsvDbFetchFault,
) where

import Ecluse.Core.Fault (TransportFault, renderTransportFault)

-- | An artifact version marker: S3's ETag, compared for equality only. Equal ETags carry equal bytes, so an unchanged one means nothing to do.
newtype DbEtag = DbEtag Text
    deriving stock (Eq, Show)

-- | Why an artifact fetch did not yield usable bytes: a value on the sync's fetch channel, never an exception.
data OsvDbFetchFault
    = -- | The object exceeds the configured byte cap (carried, in bytes).
      OsvDbTooLarge Int
    | -- | The response carried no ETag, so there is nothing truthful to record.
      OsvDbNoEtag
    | -- | The transport could not deliver the object (carried, classified).
      OsvDbTransport TransportFault
    deriving stock (Eq, Show)

-- | A fetch fault as a report line or a span status reads it. It never names the object URL or the request.
renderOsvDbFetchFault :: OsvDbFetchFault -> Text
renderOsvDbFetchFault = \case
    OsvDbTooLarge cap -> "the object passed its " <> show cap <> "-byte cap"
    OsvDbNoEtag -> "the store answered without an ETag"
    OsvDbTransport fault -> renderTransportFault fault

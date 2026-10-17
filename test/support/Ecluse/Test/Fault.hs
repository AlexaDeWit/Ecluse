-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Readers over the fault vocabulary, so a spec checks a classification and not the client's wording.
module Ecluse.Test.Fault (
    httpTransportCause,
) where

import Ecluse.Core.Fault (HttpFault (HttpStatus, HttpTransport), TransportCause, TransportFault (tfCause))

-- | The transport cause a fault carries, when the transport failed.
httpTransportCause :: HttpFault -> Maybe TransportCause
httpTransportCause = \case
    HttpStatus _ -> Nothing
    HttpTransport fault -> Just (tfCause fault)

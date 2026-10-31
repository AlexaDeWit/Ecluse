-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Closed Ruby metadata replies preserve admission and transport failures.
module Ecluse.Core.Registry.RubyGems.Route.Response (
    RubyResponse,
    rubyContract,
    rubyReplies,
    rubyBadRequest,
) where

import Network.HTTP.Types (status200, status304, status400, status401, status403, status404, status406, status500, status502, status503)

import Ecluse.Core.Server.Contract (BodySchema (SchemaOpaque, SchemaText), ResponseChoice (..), ResponseContract, ResponseValue, chooseContract, emptyContract, mediaContract, responseValue)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), mapPackumentReplies)
import Ecluse.Core.Server.Pipeline.Packument.Response (MetadataResponse, metadataReplies)
import Ecluse.Core.Server.Response (renderRefusal)
import Ecluse.Core.Server.Response.Summary (detailedRejections)

-- | Native successes and local errors have distinct declared status codes.
type RubyResponse = ResponseChoice (ResponseValue LByteString) (MetadataResponse LByteString)

-- | Match every local metadata result, including malformed native requests.
rubyContract :: ByteString -> ResponseContract RubyResponse
rubyContract media =
    chooseContract (text status400 "The native request is malformed or exceeds its limits.") $
        chooseContract (mediaContract status200 "Admitted native metadata." (SchemaOpaque media)) $
            chooseContract (emptyContract status304 "The admitted representation is unchanged.") $
                chooseContract (text status401 "Authentication is required.") $
                    chooseContract (text status403 "Access or admission was refused.") $
                        chooseContract (text status404 "The requested gem or coordinate is absent, the gem lists no version, or no version of the gem passed policy and admission.") $
                            chooseContract (text status406 "The representation is unavailable.") $
                                chooseContract (text status500 "The registry could not decide.") $
                                    chooseContract (text status502 "An origin reply was invalid or unusable.") (text status503 "The registry is temporarily unavailable.")
  where
    text status description = mediaContract status description (SchemaText "text/plain")

-- | Preserve the shared failure classification, including a local 403 for a private 401 or 403, while encoding native bytes.
rubyReplies :: PackumentReplies RubyResponse
rubyReplies = mapPackumentReplies SecondResponse (metadataReplies detailedRejections (fromStrict . encodeUtf8 . renderRefusal))

-- | Refuse query syntax before admission or upstream work.
rubyBadRequest :: Text -> RubyResponse
rubyBadRequest = FirstResponse . responseValue [] . fromStrict . encodeUtf8

-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Closed metadata responses shared by native registry routes.
module Ecluse.Core.Server.Pipeline.Packument.Response (
    MetadataResponse,
    metadataReplies,
    keepPrivateUnauthorised,
) where

import Network.HTTP.Types (ResponseHeaders)

import Ecluse.Core.Server.Contract (
    ResponseChoice (FirstResponse, SecondResponse),
    ResponseValue,
    responseValue,
 )
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..))
import Ecluse.Core.Server.Response (Refusal, ServeDecision)

-- | Metadata replies ordered as 200, 304, 401, 403, 404, 406, 500, 502 and 503.
type MetadataResponse refusal =
    ResponseChoice
        (ResponseValue LByteString)
        ( ResponseChoice
            (ResponseValue ())
            ( ResponseChoice
                (ResponseValue refusal)
                ( ResponseChoice
                    (ResponseValue refusal)
                    ( ResponseChoice
                        (ResponseValue refusal)
                        ( ResponseChoice
                            (ResponseValue refusal)
                            ( ResponseChoice
                                (ResponseValue refusal)
                                (ResponseChoice (ResponseValue refusal) (ResponseValue refusal))
                            )
                        )
                    )
                )
            )
        )

-- | Keep native denial wording and refusal bodies within the shared response order. A listing that admitted nothing answers 404.
metadataReplies ::
    ([ServeDecision] -> Text) ->
    (Refusal -> refusal) ->
    PackumentReplies (MetadataResponse refusal)
metadataReplies deniedText refusalBody =
    PackumentReplies
        { packumentDeniedText = deniedText
        , packumentOk = \headers body -> FirstResponse (responseValue headers body)
        , packumentNotModified = \headers -> SecondResponse (FirstResponse (responseValue headers ()))
        , packumentUnauthorised = \headers message -> SecondResponse (SecondResponse (FirstResponse (refusalValue headers message)))
        , packumentPrivateRefused = const forbiddenReply
        , packumentForbidden = forbiddenReply
        , packumentDenied = notFoundReply
        , packumentNotFound = notFoundReply
        , packumentNotAcceptable = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (refusalValue headers message))))))
        , packumentInternal = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (refusalValue headers message)))))))
        , packumentBadGateway = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (refusalValue headers message))))))))
        , packumentUnavailable = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (refusalValue headers message))))))))
        }
  where
    forbiddenReply headers message = SecondResponse (SecondResponse (SecondResponse (FirstResponse (refusalValue headers message))))
    notFoundReply headers message = SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (refusalValue headers message)))))
    refusalValue headers message = responseValue headers (refusalBody message)

-- | Answer a private origin's 401 with @replies@' 401 and every other private refusal with its 403.
keepPrivateUnauthorised :: PackumentReplies response -> Int -> ResponseHeaders -> Refusal -> response
keepPrivateUnauthorised replies code
    | code == 401 = packumentUnauthorised replies
    | otherwise = packumentForbidden replies

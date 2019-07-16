module Endpoint.AuthEndpoint exposing (authErrorDecoder)

import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http.Detailed as HttpEx
import Json.Decode as Decode


authDecoder : Decode.Decoder ErrorResponse
authDecoder =
    Decode.map
        ErrorResponse
        (Decode.at [ "jrData" ] Decode.string)


authErrorDecoder : HttpEx.Error String -> List String
authErrorDecoder responseError =
    errorDecoder responseError authDecoder

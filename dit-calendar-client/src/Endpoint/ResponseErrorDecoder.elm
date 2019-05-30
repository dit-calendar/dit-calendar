module Endpoint.ResponseErrorDecoder exposing (registerErrorDecoder)

import Http.Detailed as HttpEx
import Json.Decode as Decode


registerErrorDecoder : HttpEx.Error String -> List String
registerErrorDecoder responseError =
    errorDecoder responseError registerDecoder


errorDecoder : HttpEx.Error String -> Decode.Decoder ErrorResponse -> List String
errorDecoder responseError responseDecoder =
    case responseError of
        HttpEx.BadStatus metadata body ->
            let
                decode =
                    Decode.decodeString responseDecoder body
            in
            case decode of
                Ok regResponse ->
                    [ regResponse.message ]

                Err error ->
                    --TODO beim decodieren des Fehlers ist was scheifgelaufen
                    [ "beim decodieren des Fehlers ist was scheifgelaufen" ]

        HttpEx.NetworkError ->
            -- TODO
            [ "keine Verbindung" ]

        _ ->
            --TODO
            [ "irgendein Fehler" ]


type alias ErrorResponse =
    { message : String
    }


registerDecoder : Decode.Decoder ErrorResponse
registerDecoder =
    Decode.map
        ErrorResponse
        (Decode.at [ "jrData" ] Decode.string)

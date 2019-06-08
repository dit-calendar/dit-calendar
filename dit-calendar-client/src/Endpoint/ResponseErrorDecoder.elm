module Endpoint.ResponseErrorDecoder exposing (calendarErrorDecoder, registerErrorDecoder)

import Http.Detailed as HttpEx
import Json.Decode as Decode


calendarErrorDecoder : HttpEx.Error String -> List String
calendarErrorDecoder responseError =
    errorDecoder responseError calendarDecoder


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
                    [ "beim decodieren des Fehlers ist was scheifgelaufen. "
                        ++ "StatusCode: "
                        ++ String.fromInt metadata.statusCode
                        ++ ", StatusCode: "
                        ++ metadata.statusText
                        ++ ", error message: "
                        ++ Decode.errorToString error
                    ]

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


calendarDecoder : Decode.Decoder ErrorResponse
calendarDecoder =
    Decode.map ErrorResponse Decode.string

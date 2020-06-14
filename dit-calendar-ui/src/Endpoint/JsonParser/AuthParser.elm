module Endpoint.JsonParser.AuthParser exposing (authErrorDecoder, loginEncoder, parseLoginResult, registerEncoder)

import Data.Login as Login
import Data.Register as Register
import Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode
import Json.Encode as Encode


loginEncoder : Login.Model -> Encode.Value
loginEncoder model =
    Encode.object
        [ ( "user", Encode.string model.name )
        , ( "password", Encode.string model.password )
        ]


authDecoder : Decode.Decoder ErrorResponse
authDecoder =
    Decode.map
        ErrorResponse
        (Decode.at [ "jrData" ] Decode.string)


authErrorDecoder : HttpEx.Error String -> List String
authErrorDecoder responseError =
    errorDecoder responseError authDecoder


registerEncoder : Register.Model -> Encode.Value
registerEncoder model =
    let
        naUser =
            Encode.object
                [ ( "email", Encode.string model.register.email )
                , ( "username", Encode.string model.register.name )
                , ( "userId", Encode.int 0 )
                ]
    in
    Encode.object
        [ ( "naUser", naUser )
        , ( "naPassword", Encode.string model.register.password )
        , ( "naPasswordConfirm", Encode.string model.register.passwordConfirm )
        ]


parseLoginResult : ( Http.Metadata, String ) -> String
parseLoginResult ( _, body ) =
    let
        decode =
            Decode.decodeString loginDecoder body
    in
    case decode of
        Ok token ->
            token

        Err _ ->
            "fehler beim decodieren des login response"


loginDecoder : Decode.Decoder String
loginDecoder =
    Decode.field "jrData" (Decode.field "token" Decode.string)

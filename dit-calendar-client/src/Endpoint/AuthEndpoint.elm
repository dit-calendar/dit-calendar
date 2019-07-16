module Endpoint.AuthEndpoint exposing (login, loginResponse, register, registerResponse)

import Data.Login as Login
import Data.Register as Register
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http exposing (Body, Expect)
import Http.Detailed as HttpEx
import Json.Decode as Decode
import Json.Encode as Encode


login : Login.Model -> Cmd Login.Msg
login model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.loginUrl
        , body = Http.jsonBody (loginEncoder model)
        , expect = HttpEx.expectString Login.HttpLogin
        , timeout = Nothing
        , tracker = Nothing
        }


loginResponse : HttpEx.Error String -> Login.Model -> Login.Model
loginResponse error model =
    { model | problems = authErrorDecoder error }


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


register : Register.Model -> Cmd Register.Msg
register model =
    Http.post
        { url = Server.registerUrl
        , body = Http.jsonBody (registerEncoder model)
        , expect = HttpEx.expectString Register.RegisterResult
        }


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


registerResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Register.Model -> Register.Model
registerResponse response model =
    case response of
        Ok value ->
            model

        Err error ->
            { model | problems = authErrorDecoder error }

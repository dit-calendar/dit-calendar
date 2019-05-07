module Page.Register exposing (Model, Msg(..), init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser
import Endpoint.ResponseErrorDecoder exposing (registerErrorDecoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Body, Expect)
import Http.Detailed as HttpEx
import Json.Encode as Encode


type alias Model =
    { name : String
    , email : String
    , password : String
    , passwordConfirm : String
    , problems : List String
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" "" [], Cmd.none )


type Msg
    = Name String
    | Email String
    | Password String
    | PasswordConfirm String
    | Register
    | HttpRegister (Result (HttpEx.Error String) ( Http.Metadata, String ))


type alias RegisterResponse =
    { jrStatus : String
    , jrData : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordConfirm password ->
            ( { model | passwordConfirm = password }, Cmd.none )

        Register ->
            ( model, register model )

        HttpRegister result ->
            ( registerResponse result model, Cmd.none )


registerResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
registerResponse response model =
    case response of
        Ok value ->
            model

        Err error ->
            { model | problems = List.append model.problems (registerErrorDecoder error) }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Email" model.email Email
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Repeat Password" model.passwordConfirm PasswordConfirm
        , div []
            [ button [ onClick Register ]
                [ text "Submit" ]
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


register : Model -> Cmd Msg
register model =
    Http.post
        { url = "https://localhost:8443/authenticate/authentication-methods/password/account"
        , body = Http.jsonBody (registerEncoder model)
        , expect = HttpEx.expectString HttpRegister
        }


registerEncoder : Model -> Encode.Value
registerEncoder model =
    let
        naUser =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "username", Encode.string model.name )
                , ( "userId", Encode.int 0 )
                ]
    in
    Encode.object
        [ ( "naUser", naUser )
        , ( "naPassword", Encode.string model.password )
        , ( "naPasswordConfirm", Encode.string model.passwordConfirm )
        ]

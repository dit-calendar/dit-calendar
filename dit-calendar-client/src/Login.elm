module Login exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Body, Expect, riskyRequest)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Encode as Encode


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { name : String
    , password : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


type Msg
    = Name String
    | Password String
    | Login
    | HttpLogin (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            (model, login model)

        HttpLogin result ->
                    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , div []
            [ button [ onClick Login ]
                [ text "Submit" ]
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



login : Model -> Cmd Msg
login model =
  Http.riskyRequest
    { method = "POST"
        , headers = []
        , url = "https://localhost:8443/authenticate/authentication-methods/password/token"
        , body = Http.jsonBody (loginEncoder model)
        , expect = Http.expectWhatever HttpLogin
        , timeout = Nothing
        , tracker = Nothing
        }


loginEncoder : Model -> Encode.Value
loginEncoder model =
    Encode.object
        [ ( "user", Encode.string model.name )
        , ( "password", Encode.string model.password )
        ]
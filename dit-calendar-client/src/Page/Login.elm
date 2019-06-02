module Page.Login exposing (Model, Msg(..), init, update, view, viewInput)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Body, Expect, riskyRequest)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Encode as Encode


type alias Model =
    { name : String
    , password : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "", Cmd.none )


type Msg
    = Name String
    | Password String
    | Login
    | HttpLogin (Result Http.Error ())


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update authUrl msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            (model, login authUrl model)

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



login : String -> Model -> Cmd Msg
login authUrl model =
  Http.riskyRequest
    { method = "POST"
        , headers = []
        , url = authUrl ++ "token"
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
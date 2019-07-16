module Page.Login exposing (Model, Msg(..), init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser.Navigation as Navigation
import Endpoint.AuthEndpoint exposing (authErrorDecoder)
import Env.Serverurl as Server
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Body, Expect)
import Http.Detailed as HttpEx
import Json.Encode as Encode


type alias Model =
    { name : String
    , password : String
    , problems : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" [], Cmd.none )


type Msg
    = Name String
    | Password String
    | Login
    | HttpLogin (Result (HttpEx.Error String) ( Http.Metadata, String ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( model, login model )

        HttpLogin result ->
            case result of
                Ok _ ->
                    ( model, Navigation.load "#calendar" )

                Err error ->
                    ( loginResponse error model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , div []
            [ button [ onClick Login ]
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


login : Model -> Cmd Msg
login model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.loginUrl
        , body = Http.jsonBody (loginEncoder model)
        , expect = HttpEx.expectString HttpLogin
        , timeout = Nothing
        , tracker = Nothing
        }


loginResponse : HttpEx.Error String -> Model -> Model
loginResponse error model =
    { model | problems = authErrorDecoder error }


loginEncoder : Model -> Encode.Value
loginEncoder model =
    Encode.object
        [ ( "user", Encode.string model.name )
        , ( "password", Encode.string model.password )
        ]

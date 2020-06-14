port module Page.Login exposing (init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser.Navigation as Navigation
import Data.Login exposing (Model, Msg(..))
import Endpoint.AuthEndpoint exposing (login, loginResponse)
import Endpoint.JsonParser.AuthParser exposing (parseLoginResult)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


port saveCookie : String -> Cmd msg


init : ( Model, Cmd Msg )
init =
    ( Model "" "" [], Cmd.none )


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
                Ok token ->
                    ( model, Cmd.batch [ Navigation.load "#calendar", saveCookie (parseLoginResult token) ] )

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

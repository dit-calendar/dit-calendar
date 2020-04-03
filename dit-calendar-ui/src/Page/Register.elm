module Page.Register exposing (init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser
import Data.Register exposing (Model, Msg(..), RegisterModel, RegisterMsg(..))
import Endpoint.AuthEndpoint exposing (register, registerResponse)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tuple exposing (mapFirst)


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
    ( Model (RegisterModel "" "" "" "") [], Cmd.none )


type alias RegisterResponse =
    { jrStatus : String
    , jrData : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterUserMsg rMsg ->
            mapFirst (\x -> { model | register = x }) (updateRegister rMsg model.register)

        PerformRegister ->
            ( model, register model )

        RegisterResult result ->
            ( registerResponse result model, Cmd.none )


updateRegister : RegisterMsg -> RegisterModel -> ( RegisterModel, Cmd Msg )
updateRegister msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordConfirm password ->
            ( { model | passwordConfirm = password }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.map RegisterUserMsg (viewRegister model.register)
        , div []
            [ button [ onClick PerformRegister ]
                [ text "Submit" ]
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewRegister : RegisterModel -> Html RegisterMsg
viewRegister model =
    div [ class "register-fields" ]
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Email" model.email Email
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Repeat Password" model.passwordConfirm PasswordConfirm
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

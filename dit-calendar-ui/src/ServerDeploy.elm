module ServerDeploy exposing (Model, Msg(..), init, initModel, update, view)

import Bootstrap.Alert as Alert
import Bytes exposing (Bytes)
import Env.Serverurl as Server
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Http.Detailed as HttpEx


type alias Model =
    { showLoading : Bool
    , error : Maybe String
    }


type Msg
    = CheckHealth
    | CheckHealthResult (Result (HttpEx.Error Bytes) ())
    | ServerStartUpResult (Result (HttpEx.Error Bytes) ())


initModel : Model
initModel =
    { showLoading = False, error = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckHealth ->
            ( model, healthCheck )

        CheckHealthResult response ->
            case response of
                Ok _ ->
                    init

                Err _ ->
                    ( { showLoading = True, error = Nothing }, serverStartUp )

        ServerStartUpResult response ->
            case response of
                Ok _ ->
                    init

                Err _ ->
                    ( { showLoading = False, error = Just "Server Problems" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ if model.showLoading then
            div [ class "server-startup-spinner" ] [ Alert.simplePrimary [] [ text "Server initialising" ] ]

          else
            case model.error of
                Nothing ->
                    div [] []

                Just error ->
                    div [ class "error-messages" ] [ Alert.simpleDanger [] [ text error ] ]
        ]


healthCheck =
    Http.get
        { url = Server.baseUrl
        , expect = HttpEx.expectWhatever CheckHealthResult
        }


serverStartUp =
    Http.get
        { url = Server.serverStartUpUrl
        , expect = HttpEx.expectWhatever ServerStartUpResult
        }

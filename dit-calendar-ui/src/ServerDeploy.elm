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
    | ServerStartDeployResult (Result (HttpEx.Error Bytes) ())
    | ServerAwakeResult (Result (HttpEx.Error Bytes) ())


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
            ( model, healthCheck CheckHealthResult )

        CheckHealthResult response ->
            case response of
                Ok _ ->
                    init

                Err _ ->
                    ( { showLoading = True, error = Nothing }, serverStartUp )

        ServerStartDeployResult _ ->
            ( model, healthCheck ServerAwakeResult )

        ServerAwakeResult response ->
            case response of
                Ok _ ->
                    init

                Err _ ->
                    ( { showLoading = False, error = Just "Server is not running. Re-try in a moment" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ if model.showLoading then
            div [ class "server-startup-spinner" ] [ Alert.simplePrimary [] [ text "Startup Server" ] ]

          else
            case model.error of
                Nothing ->
                    div [] []

                Just error ->
                    div [ class "error-messages" ] [ Alert.simpleDanger [] [ text error ] ]
        ]


healthCheck resultType =
    Http.get
        { url = Server.baseUrl
        , expect = HttpEx.expectWhatever resultType
        }


serverStartUp =
    Http.get
        { url = Server.serverStartUpUrl
        , expect = HttpEx.expectWhatever ServerStartDeployResult
        }

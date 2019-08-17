module Page.TaskDetail exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Browser
import Data.Task exposing (Messages(..), Model, Msg(..), Task, TaskMsg(..))
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
import Maybe exposing (withDefault)


main : Program () Model Msg
main =
    Browser.element
        { init = initMain
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initMain : () -> ( Model, Cmd Msg )
initMain _ =
    ( Model (Task (Just 1) 0 "" "" (Just "")) (Problems []), Cmd.none )


init : Task -> ( Model, Cmd Msg )
init task =
    ( Model task (Problems []), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TaskMsg taskMsg ->
            ( { model | task = updateTaskDetials taskMsg model.task }, Cmd.none )


updateTaskDetials : TaskMsg -> Task -> Task
updateTaskDetials msg model =
    case msg of
        Description des ->
            { model | description = des }

        StartTime startT ->
            { model | startTime = startT }

        EndTime endT ->
            { model | endTime = Just endT }


view : Model -> Html Msg
view model =
    let
        taskInfo =
            model.task
    in
    div []
        [ Form.form []
            [ h4 [] [ text "Task Eintrag" ]
            , Form.group []
                [ Form.label [] [ text "description" ]
                , Input.text [ Input.value taskInfo.description, Input.onInput (TaskMsg << Description) ]
                ]
            , Form.group []
                [ Form.label [] [ text "start date" ]
                , Input.text [ Input.value taskInfo.startTime, Input.onInput (TaskMsg << StartTime) ]
                ]
            , Form.group []
                [ Form.label [] [ text "end date" ]
                , Input.text [ Input.value (withDefault "" taskInfo.endTime), Input.onInput (TaskMsg << EndTime) ]
                ]
            ]
        , case model.messages of
            Problems errors ->
                div [ class "error-messages" ] (List.map viewProblem errors)

            SuccessUpdate ->
                div [ class "success-messages" ]
                    [ viewSuccess "Task erfolgreich aktualisiert" ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewSuccess : String -> Html msg
viewSuccess success =
    Alert.simpleSuccess [] [ text success ]

module Page.TaskDetail exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Browser
import Data.Task exposing (Model, Msg(..), Task, TaskMsg(..))
import Data.UIMessages exposing (Messages(..))
import Endpoint.TaskEndpoint exposing (createTask, taskResponse, updateTask)
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
    ( Model (Task Nothing Nothing 0 "" Nothing "" "" Nothing Nothing) (Problems []), Cmd.none )


init : Task -> ( Model, Cmd Msg )
init task =
    ( Model task (Problems []), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TaskMsg taskMsg ->
            ( { model | task = updateTaskDetials taskMsg model.task }, Cmd.none )

        SaveTask ->
            ( { model | messages = Problems [] }
            , if model.task.taskId /= Nothing then
                updateTask model.task

              else
                createTask model.task
            )

        CreateTaskResult result ->
            ( taskResponse result model, Cmd.none )


updateTaskDetials : TaskMsg -> Task -> Task
updateTaskDetials msg model =
    case msg of
        Title title ->
            { model | title = title }

        Description des ->
            { model | description = Just des }

        StartTime startT ->
            { model | startTime = startT }

        StartDate startD ->
            { model | startDate = startD }

        EndTime endT ->
            { model | endTime = Just endT }

        EndDate endD ->
            { model | endDate = Just endD }


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
                [ Form.label [] [ text "title" ]
                , Input.text [ Input.value taskInfo.title, Input.onInput (TaskMsg << Title) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "description" ]
                , Input.text [ Input.value (withDefault "" taskInfo.description), Input.onInput (TaskMsg << Description) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "start date" ]
                , Input.date [ Input.value taskInfo.startDate, Input.onInput (TaskMsg << StartDate) ]
                , Input.time [ Input.value taskInfo.startTime, Input.onInput (TaskMsg << StartTime) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "end date" ]
                , Input.date [ Input.value (withDefault "" taskInfo.endDate), Input.onInput (TaskMsg << EndDate) ]
                , Input.time [ Input.value (withDefault "" taskInfo.endTime), Input.onInput (TaskMsg << EndTime) ]
                ]
            ]
        , Button.button [ Button.primary, Button.onClick SaveTask ] [ text "Speichern" ]
        , case model.messages of
            Problems errors ->
                div [ class "error-messages" ] (List.map viewProblem errors)

            SuccessUpdate ->
                div [ class "success-messages" ]
                    [ viewSuccess "Task erfolgreich gespeichert" ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewSuccess : String -> Html msg
viewSuccess success =
    Alert.simpleSuccess [] [ text success ]

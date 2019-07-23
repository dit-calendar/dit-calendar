module Page.CalendarEntryDetails exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Model, Msg(..), Task(..))
import Endpoint.CalendarEntryEndpoint exposing (saveCalendarEntry)
import Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
import Maybe exposing (withDefault)


getBla : Task -> String
getBla (Task str) =
    str


initModel : CalendarEntry -> Model
initModel cal =
    { calendarEntry = cal, tasks = [], problems = [] }


init : CalendarEntry -> ( Model, Cmd Msg )
init cal =
    update (GetCalendarEntryTasks (withDefault 0 cal.entryId)) (initModel cal)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendarDetailMsg calendarDetialMsg ->
            ( { model | calendarEntry = updateCalendarDetials calendarDetialMsg model.calendarEntry }, Cmd.none )

        GetCalendarEntryTasks taskId ->
            ( model, loadCalendarEntryTasks taskId )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )

        SaveCalendar ->
            ( model, saveCalendarEntry model.calendarEntry )

        SaveCalendarResult result ->
            -- TODO Benachrichtigung "wurde gespeichert" und error behandlung
            ( model, Cmd.none )


updateCalendarDetials : CalendarDetailMsg -> CalendarEntry -> CalendarEntry
updateCalendarDetials msg model =
    case msg of
        Description des ->
            { model | description = des }

        StartDate startD ->
            { model | startDate = startD }

        EndDate endD ->
            { model | endDate = endD }


view : Model -> Html Msg
view model =
    let
        calendarInfo =
            model.calendarEntry

        tasks =
            model.tasks
    in
    div []
        [ Form.form []
            [ h4 [] [ text "Kalendar Eintrag" ]
            , Form.group []
                [ Form.label [] [ text "description" ]
                , Input.text [ Input.value calendarInfo.description, Input.onInput (CalendarDetailMsg << Description) ]
                ]
            , Form.group []
                [ Form.label [] [ text "start date" ]
                , Input.text [ Input.value calendarInfo.startDate, Input.onInput (CalendarDetailMsg << StartDate) ]
                ]
            , Form.group []
                [ Form.label [] [ text "end date" ]
                , Input.text [ Input.value calendarInfo.endDate, Input.onInput (CalendarDetailMsg << EndDate) ]
                ]
            ]
        , ListGroup.ul
            (List.map
                (\task ->
                    ListGroup.li [] [ text ("task: " ++ getBla task) ]
                )
                tasks
            )
        , Button.button [ Button.primary, onClick SaveCalendar ] [ text "Speichern" ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]

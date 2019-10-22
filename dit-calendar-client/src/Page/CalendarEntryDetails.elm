module Page.CalendarEntryDetails exposing (init, initEmptyModelForPageReload, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Model, Msg(..), emptyCalendarEntry)
import Data.Task exposing (emptyTask)
import Data.UIMessages exposing (Messages(..))
import Endpoint.CalendarEntryEndpoint exposing (createCalendarEntry, getCalendarEntryResponse, loadCalendarEntry, saveCalendarEntry, saveCalendarEntryResponse)
import Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
import Html.Events as HtmlEvent
import Maybe exposing (withDefault)


init : CalendarEntry -> ( Model, Cmd Msg )
init cal =
    ( { calendarEntry = cal, tasks = [], messages = Problems [] }, Cmd.none )


initEmptyModelForPageReload : Int -> Model
initEmptyModelForPageReload cId =
    let
        cEmptyCalendarEntry =
            { emptyCalendarEntry | entryId = Just cId }
    in
    { calendarEntry = cEmptyCalendarEntry, tasks = [], messages = Problems [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendarDetailEditMsg calendarDetialMsg ->
            ( { model | calendarEntry = updateCalendarDetails calendarDetialMsg model.calendarEntry }, Cmd.none )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )

        SaveCalendar ->
            ( { model | messages = Problems [] }
            , if not (model.calendarEntry.entryId == Nothing) then
                saveCalendarEntry model.calendarEntry

              else
                createCalendarEntry model.calendarEntry
            )

        SaveCalendarResult result ->
            ( saveCalendarEntryResponse result model, Cmd.none )

        OpenTaskDetailsView _ ->
            ( model, Cmd.none )

        GetCalendarEntryResult result ->
            let
                newCalendarModel =
                    getCalendarEntryResponse result model
            in
            ( newCalendarModel, loadCalendarEntryTasks (withDefault 0 newCalendarModel.calendarEntry.entryId) )

        GetCalendarEntry ->
            --TODO sobald id in url übergeben wird, werden tasks nachgeladen; selbst wenn calendar nicht existent
            ( model, loadCalendarEntry (withDefault 0 model.calendarEntry.entryId) )


updateCalendarDetails : CalendarDetailMsg -> CalendarEntry -> CalendarEntry
updateCalendarDetails msg model =
    case msg of
        Description des ->
            { model | description = des }

        StartDate startD ->
            { model | startDate = startD }

        StartTime startT ->
            { model | startTime = startT }

        EndDate endD ->
            { model | endDate = endD }

        EndTime endT ->
            { model | endTime = endT }


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
                , Input.text [ Input.value calendarInfo.description, Input.onInput (CalendarDetailEditMsg << Description) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "start date" ]
                , Input.date [ Input.value calendarInfo.startDate, Input.onInput (CalendarDetailEditMsg << StartDate) ]
                , Input.time [ Input.value calendarInfo.startTime, Input.onInput (CalendarDetailEditMsg << StartTime) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "end date" ]
                , Input.date [ Input.value calendarInfo.endDate, Input.onInput (CalendarDetailEditMsg << EndDate) ]
                , Input.time [ Input.value calendarInfo.endTime, Input.onInput (CalendarDetailEditMsg << EndTime) ]
                ]
            ]
        , ListGroup.custom
            (List.map
                (\task ->
                    ListGroup.button [ ListGroup.attrs [ HtmlEvent.onClick (OpenTaskDetailsView task) ] ] [ text ("task: " ++ task.description) ]
                )
                tasks
            )
        , Button.button [ Button.primary, onClick SaveCalendar ] [ text "Speichern" ]
        , Button.button
            [ Button.primary, Button.onClick (OpenTaskDetailsView (emptyTask model.calendarEntry.entryId)) ]
            [ text "Neuen Task Eintrag erstellen" ]
        , case model.messages of
            Problems errors ->
                div [ class "error-messages" ] (List.map viewProblem errors)

            SuccessUpdate ->
                div [ class "success-messages" ]
                    [ viewSuccess "Kalendereintrag erfolgreich gespeichert" ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewSuccess : String -> Html msg
viewSuccess success =
    Alert.simpleSuccess [] [ text success ]

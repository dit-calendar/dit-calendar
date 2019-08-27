module Page.CalendarEntryDetails exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Messages(..), Model, Msg(..))
import Data.Task exposing (emptyTask)
import Endpoint.CalendarEntryEndpoint exposing (calendarEntryResponse, createCalendarEntry, saveCalendarEntry)
import Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
import Html.Events as HtmlEvent
import Maybe exposing (withDefault)


initModel : CalendarEntry -> Model
initModel cal =
    { calendarEntry = cal, tasks = [], messages = Problems [] }


init : CalendarEntry -> ( Model, Cmd Msg )
init cal =
    update (GetCalendarEntryTasks (withDefault 0 cal.entryId)) (initModel cal)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendarDetailMsg calendarDetialMsg ->
            ( { model | calendarEntry = updateCalendarDetails calendarDetialMsg model.calendarEntry }, Cmd.none )

        GetCalendarEntryTasks taskId ->
            ( model
            , if not (taskId == 0) then
                loadCalendarEntryTasks taskId

              else
                Cmd.none
            )

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
            -- TODO Benachrichtigung "wurde gespeichert" und error behandlung
            ( calendarEntryResponse result model, Cmd.none )

        OpenTaskDetailsView _ ->
            --TODO rais logic error exception
            ( model, Cmd.none )

        CreateCalendarResult result ->
            ( calendarEntryResponse result model, Cmd.none )


updateCalendarDetails : CalendarDetailMsg -> CalendarEntry -> CalendarEntry
updateCalendarDetails msg model =
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
        , ListGroup.custom
            (List.map
                (\task ->
                    ListGroup.button [ ListGroup.attrs [ HtmlEvent.onClick (OpenTaskDetailsView task) ] ] [ text ("task: " ++ task.description) ]
                )
                tasks
            )
        , Button.button [ Button.primary, onClick SaveCalendar ] [ text "Speichern" ]
        , Button.button
            [ Button.primary, Button.block, Button.large, Button.onClick (OpenTaskDetailsView (emptyTask model.calendarEntry.entryId)) ]
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

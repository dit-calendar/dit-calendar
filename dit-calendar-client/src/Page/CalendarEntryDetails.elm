module Page.CalendarEntryDetails exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Messages(..), Model, Msg(..))
import Endpoint.CalendarEntryEndpoint exposing (calendarEntryResponse, saveCalendarEntry)
import Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
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
            ( { model | calendarEntry = updateCalendarDetials calendarDetialMsg model.calendarEntry }, Cmd.none )

        GetCalendarEntryTasks taskId ->
            ( model, loadCalendarEntryTasks taskId )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )

        SaveCalendar ->
            ( { model | messages = Problems [] }, saveCalendarEntry model.calendarEntry )

        SaveCalendarResult result ->
            -- TODO Benachrichtigung "wurde gespeichert" und error behandlung
            ( calendarEntryResponse result model, Cmd.none )


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
                    ListGroup.li [] [ text ("task: " ++ task.description) ]
                )
                tasks
            )
        , Button.button [ Button.primary, onClick SaveCalendar ] [ text "Speichern" ]
        , case model.messages of
            Problems errors ->
                div [ class "error-messages" ] (List.map viewProblem errors)

            SuccessUpdate ->
                div [ class "success-messages" ]
                    [ viewSuccess "Kalendereintrag erfolgreich aktualisiert" ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewSuccess : String -> Html msg
viewSuccess success =
    Alert.simpleSuccess [] [ text success ]

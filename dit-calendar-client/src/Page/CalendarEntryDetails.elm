module Page.CalendarEntryDetails exposing (init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Data.CalendarEntry exposing (CalendarDetialMsg(..), CalendarEntry, Model, Msg(..), Task(..))
import Endpoint.CalendarEntryEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Maybe exposing (withDefault)
import Tuple exposing (mapFirst)


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
        CalendarDetialMsg calendarDetialMsg ->
            mapFirst (\x -> { model | calendarEntry = x }) (updateCalendarDetials calendarDetialMsg model.calendarEntry)

        GetCalendarEntryTasks taskId ->
            ( model, loadCalendarEntryTasks taskId )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )


updateCalendarDetials : CalendarDetialMsg -> CalendarEntry -> ( CalendarEntry, Cmd Msg )
updateCalendarDetials msg model =
    case msg of
        Description des ->
            ( { model | description = des }, Cmd.none )

        StartDate startD ->
            ( { model | startDate = startD }, Cmd.none )

        EndDate endD ->
            ( { model | endDate = endD }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        calendarInfo =
            model.calendarEntry

        tasks =
            model.tasks
    in
    div []
        [ Grid.container []
            [ Grid.row []
                [ Grid.col [] [ text ("description: " ++ calendarInfo.description) ]
                , Grid.col [] [ text ("start date:" ++ calendarInfo.startDate) ]
                , Grid.col [] [ text ("end date:" ++ calendarInfo.endDate) ]
                ]
            , ListGroup.ul
                (List.map
                    (\task ->
                        ListGroup.li [] [ text ("task: " ++ getBla task) ]
                    )
                    tasks
                )
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]

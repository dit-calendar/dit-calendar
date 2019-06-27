module Page.CalendarEntryDetails exposing (Model, Msg(..), initModel, main, update, view)

import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Browser
import Endpoint.ResponseErrorDecoder exposing (calendarErrorDecoder)
import Html exposing (Html, text)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)
import Page.SimpleCalendar as SimpleCal


type Task
    = Task String -- Todo use Task.Model from Task.elm(page)


getBla : Task -> String
getBla (Task str) =
    str


type alias Model =
    { calendarEntry : SimpleCal.CalendarEntry
    , tasks : List Task
    , problems : List String
    }


initModel : SimpleCal.CalendarEntry -> Model
initModel cal =
    { calendarEntry = cal, tasks = [], problems = [] }


type Msg
    = GetCalendarEntryTasks
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : SimpleCal.CalendarEntry -> ( Model, Cmd Msg )
init cal =
    update GetCalendarEntryTasks (initModel cal)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCalendarEntryTasks ->
            ( model, loadCalendarEntryTasks )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )


loadCalendarEntryTasks : Cmd Msg
loadCalendarEntryTasks =
    --TODO pass id for url
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = "https://localhost:8443/calendarentries/2/tasks"
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntryTasksResult
        , timeout = Nothing
        , tracker = Nothing
        }


calendarEntryTasksResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
calendarEntryTasksResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    calendarEntryTasksDecoder value
            in
            case resp of
                Ok tasks ->
                    { model | tasks = tasks }

                Err error ->
                    { model | problems = [ error ] }

        Err error ->
            { model | problems = calendarErrorDecoder error }


calendarEntryTasksDecoder : ( Http.Metadata, String ) -> Result String (List Task)
calendarEntryTasksDecoder ( meta, body ) =
    let
        decode =
            Decode.decodeString (Decode.list tasksDecoder) body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren des calendars" ++ Decode.errorToString error)


tasksDecoder : Decode.Decoder Task
tasksDecoder =
    Decode.map
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.at [ "description" ] Decode.string)


view : Model -> Html Msg
view calendarEntryDetails =
    let
        calendarInfo =
            calendarEntryDetails.calendarEntry

        tasks =
            calendarEntryDetails.tasks
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ text ("description: " ++ calendarInfo.description) ]
            , Grid.col [] [ text ("date:" ++ calendarInfo.date) ]
            ]
        , ListGroup.ul
            (List.map
                (\task ->
                    ListGroup.li [] [ text ("task: " ++ getBla task) ]
                )
                tasks
            )
        ]

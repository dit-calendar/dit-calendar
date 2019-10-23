module Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)

import Data.CalendarEntry exposing (CalendarEntry, Model, Msg(..))
import Data.Task exposing (Task)
import Data.UIMessages exposing (Messages(..))
import Endpoint.JsonParser.TaskParser exposing (taskErrorsDecoder, tasksDecoder)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


loadCalendarEntryTasks : Int -> Cmd Msg
loadCalendarEntryTasks taskId =
    --TODO hier mit Maybe arbeiten und bei Nothing keine Calendar Task laden oder anderen weg suchen, ansonsten bei nicht vorhandener calendar id wird unnötig nach tasks gesucht
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarTask taskId
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
                    parseCalendarEntryTasksResult model.calendarEntry.entryId value
            in
            case resp of
                Ok tasks ->
                    { model | tasks = tasks }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (taskErrorsDecoder error) }


parseCalendarEntryTasksResult : Maybe Int -> ( Http.Metadata, String ) -> Result String (List Task)
parseCalendarEntryTasksResult calendarId ( meta, body ) =
    let
        decode =
            Decode.decodeString (Decode.list (tasksDecoder calendarId)) body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren der calendars-tasks " ++ Decode.errorToString error)

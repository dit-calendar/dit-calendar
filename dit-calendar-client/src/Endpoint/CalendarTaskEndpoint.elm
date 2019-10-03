module Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)

import Data.CalendarEntry as CalendarDetail
import Data.Task exposing (Task)
import Endpoint.JsonParser.TaskParser exposing (taskErrorsDecoder, tasksDecoder)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


loadCalendarEntryTasks : Int -> Cmd CalendarDetail.Msg
loadCalendarEntryTasks taskId =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarTask taskId
        , body = Http.emptyBody
        , expect = HttpEx.expectString CalendarDetail.GetCalendarEntryTasksResult
        , timeout = Nothing
        , tracker = Nothing
        }


calendarEntryTasksResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> CalendarDetail.Model -> CalendarDetail.Model
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
                    { model | messages = CalendarDetail.Problems [ error ] }

        Err error ->
            { model | messages = CalendarDetail.Problems (taskErrorsDecoder error) }


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

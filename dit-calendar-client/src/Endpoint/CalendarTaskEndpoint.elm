module Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)

import Data.CalendarEntry as CalendarDetail
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http
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


tasksDecoder : Decode.Decoder CalendarDetail.Task
tasksDecoder =
    Decode.map
        -- TODO reuse decoder from Tasks?
        CalendarDetail.Task
        (Decode.at [ "description" ] Decode.string)


calendarEntryTasksResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> CalendarDetail.Model -> CalendarDetail.Model
calendarEntryTasksResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntryTasksResult value
            in
            case resp of
                Ok tasks ->
                    { model | tasks = tasks }

                Err error ->
                    { model | messages = CalendarDetail.Problems [ error ] }

        Err error ->
            { model | messages = CalendarDetail.Problems (taskErrorsDecoder error) }


parseCalendarEntryTasksResult : ( Http.Metadata, String ) -> Result String (List CalendarDetail.Task)
parseCalendarEntryTasksResult ( meta, body ) =
    let
        decode =
            Decode.decodeString (Decode.list tasksDecoder) body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren der calendars taks" ++ Decode.errorToString error)


taskErrorsDecoder : HttpEx.Error String -> List String
taskErrorsDecoder responseError =
    errorDecoder responseError taskErrorDecoder


taskErrorDecoder : Decode.Decoder ErrorResponse
taskErrorDecoder =
    Decode.map ErrorResponse Decode.string

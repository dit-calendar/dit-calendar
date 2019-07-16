module Endpoint.CalendarEntryEndpoint exposing (calendarEntryTasksDecoder, calendarEntryTasksResponse, calendarErrorDecoder, loadCalendarEntryTasks, tasksDecoder)

import Data.CalendarEntry exposing (CalendarDetialMsg(..), CalendarEntry, Model, Msg(..), Task(..))
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


calendarErrorDecoder : HttpEx.Error String -> List String
calendarErrorDecoder responseError =
    errorDecoder responseError calendarDecoder


calendarDecoder : Decode.Decoder ErrorResponse
calendarDecoder =
    Decode.map ErrorResponse Decode.string


loadCalendarEntryTasks : Int -> Cmd Msg
loadCalendarEntryTasks taskId =
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
            Err ("fehler beim decodieren der calendars taks" ++ Decode.errorToString error)


tasksDecoder : Decode.Decoder Task
tasksDecoder =
    Decode.map
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.at [ "description" ] Decode.string)

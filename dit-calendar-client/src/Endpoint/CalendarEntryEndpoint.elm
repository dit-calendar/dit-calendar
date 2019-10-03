module Endpoint.CalendarEntryEndpoint exposing (calendarEntriesResponse, calendarEntryResponse, createCalendarEntry, loadCalendarEntries, saveCalendarEntry)

import Data.CalendarEntry as CalendarDetail
import Data.SimpleCalendarList as CalendarList
import Endpoint.JsonParser.CalendarEntryParser exposing (calendarEntryEncoder, calendarErrorsDecoder, parseCalendarEntriesResult, parseCalendarEntryResult)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Maybe exposing (withDefault)


saveCalendarEntry : CalendarDetail.CalendarEntry -> Cmd CalendarDetail.Msg
saveCalendarEntry model =
    Http.riskyRequest
        { method = "PUT"
        , headers = []
        , url = Server.calendarEntry (withDefault 0 model.entryId)
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString CalendarDetail.SaveCalendarResult
        , timeout = Nothing
        , tracker = Nothing
        }


createCalendarEntry : CalendarDetail.CalendarEntry -> Cmd CalendarDetail.Msg
createCalendarEntry model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarEntries
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString CalendarDetail.CreateCalendarResult
        , timeout = Nothing
        , tracker = Nothing
        }


loadCalendarEntries : Cmd CalendarList.Msg
loadCalendarEntries =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarEntries
        , body = Http.emptyBody
        , expect = HttpEx.expectString CalendarList.GetCalendarEntriesResult
        , timeout = Nothing
        , tracker = Nothing
        }


calendarEntriesResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> CalendarList.Model -> CalendarList.Model
calendarEntriesResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntriesResult value
            in
            case resp of
                Ok calendarEntries ->
                    { model | calendarEntries = calendarEntries }

                Err error ->
                    { model | problems = [ error ] }

        Err error ->
            { model | problems = calendarErrorsDecoder error }


calendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> CalendarDetail.Model -> CalendarDetail.Model
calendarEntryResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntryResult value
            in
            case resp of
                Ok calendarEntry ->
                    { model | calendarEntry = calendarEntry, messages = CalendarDetail.SuccessUpdate }

                Err error ->
                    { model | messages = CalendarDetail.Problems [ error ] }

        Err error ->
            { model | messages = CalendarDetail.Problems (calendarErrorsDecoder error) }

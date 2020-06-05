module Endpoint.CalendarEntryEndpoint exposing (calendarEntriesResponse, createCalendarEntry, getCalendarEntryResponse, loadCalendarEntries, loadCalendarEntry, saveCalendarEntry, saveCalendarEntryResponse)

import Data.CalendarEntry exposing (CalendarEntry, Model, Msg(..))
import Data.SimpleCalendarList as CalendarList
import Data.UIMessages exposing (Messages(..))
import Endpoint.JsonParser.CalendarEntryParser exposing (calendarEntryEncoder, calendarErrorsDecoder, parseCalendarEntriesResult, parseCalendarEntryResult)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Maybe exposing (withDefault)


saveCalendarEntry : CalendarEntry -> Cmd Msg
saveCalendarEntry model =
    Http.riskyRequest
        { method = "PUT"
        , headers = []
        , url = Server.calendarEntry (withDefault 0 model.entryId)
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString SaveCalendarResult
        , timeout = Nothing
        , tracker = Nothing
        }


createCalendarEntry : CalendarEntry -> Cmd Msg
createCalendarEntry model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarEntries
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString SaveCalendarResult
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


loadCalendarEntry : Int -> Cmd Msg
loadCalendarEntry cId =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarEntry cId
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntryResult
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


calendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Messages -> Model
calendarEntryResponse response model sucessMessage =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntryResult value
            in
            case resp of
                Ok calendarEntry ->
                    { model | calendarEntry = calendarEntry, messages = sucessMessage }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (calendarErrorsDecoder error) }


getCalendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
getCalendarEntryResponse response model =
    calendarEntryResponse response model (Problems [])


saveCalendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
saveCalendarEntryResponse response model =
    calendarEntryResponse response model SuccessUpdate

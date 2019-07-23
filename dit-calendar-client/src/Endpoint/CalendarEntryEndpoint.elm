module Endpoint.CalendarEntryEndpoint exposing (calendarEntriesResponse, loadCalendarEntries, saveCalendarEntry)

import Data.CalendarEntry as CalendarDetail
import Data.SimpleCalendarList as CalendarList
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
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


calendarEntryEncoder : CalendarDetail.CalendarEntry -> Encode.Value
calendarEntryEncoder model =
    Encode.object
        [ ( "version", Encode.int model.version )
        , ( "description", Encode.string model.description )
        , ( "startDate", Encode.string model.startDate )
        , ( "endDate", Encode.string model.endDate )
        ]


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


calendarEntriesDecoder : Decode.Decoder (List CalendarDetail.CalendarEntry)
calendarEntriesDecoder =
    Decode.list
        (Decode.map5
            CalendarDetail.CalendarEntry
            (Decode.nullable (Decode.field "entryId" Decode.int))
            (Decode.field "version" Decode.int)
            (Decode.at [ "description" ] Decode.string)
            (Decode.field "startDate" Decode.string)
            (Decode.field "endDate" Decode.string)
        )


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


parseCalendarEntriesResult : ( Http.Metadata, String ) -> Result String (List CalendarDetail.CalendarEntry)
parseCalendarEntriesResult ( meta, body ) =
    let
        decode =
            Decode.decodeString calendarEntriesDecoder body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren des calendars" ++ Decode.errorToString error)


calendarErrorsDecoder : HttpEx.Error String -> List String
calendarErrorsDecoder responseError =
    errorDecoder responseError calendarErrorDecoder


calendarErrorDecoder : Decode.Decoder ErrorResponse
calendarErrorDecoder =
    Decode.map ErrorResponse Decode.string

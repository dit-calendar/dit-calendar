module Endpoint.JsonParser.CalendarEntryParser exposing (calendarEntryEncoder, calendarErrorsDecoder, parseCalendarEntriesResult, parseCalendarEntryResult)

import Data.CalendarEntry exposing (CalendarEntry)
import Endpoint.JsonParser.DateTimeDecoder exposing (stringToDate, stringToDateTime)
import Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Json.Encode.Extra as Encode


calendarEntryEncoder : CalendarEntry -> Encode.Value
calendarEntryEncoder model =
    Encode.object
        [ ( "version", Encode.int model.version )
        , ( "title", Encode.string model.title )
        , ( "description", Encode.maybe Encode.string model.description )
        , ( "startDate", Encode.string (model.startDate ++ "T" ++ model.startTime ++ ":00.000000Z") )
        , ( "endDate", Encode.string (model.endDate ++ "T" ++ model.endTime ++ ":00.000000Z") )
        ]


calendarEntriesDecoder : Decode.Decoder (List CalendarEntry)
calendarEntriesDecoder =
    Decode.list calendarEntryDecoder


calendarEntryDecoder : Decode.Decoder CalendarEntry
calendarEntryDecoder =
    Decode.map8
        CalendarEntry
        (Decode.nullable (Decode.field "entryId" Decode.int))
        (Decode.field "version" Decode.int)
        (Decode.at [ "title" ] Decode.string)
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "startDate" stringToDate)
        (Decode.field "startDate" stringToDateTime)
        (Decode.field "endDate" stringToDate)
        (Decode.field "endDate" stringToDateTime)


parseCalendarEntriesResult : ( Http.Metadata, String ) -> Result String (List CalendarEntry)
parseCalendarEntriesResult ( meta, body ) =
    let
        decode =
            Decode.decodeString calendarEntriesDecoder body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren der calendar Einträge" ++ Decode.errorToString error)


parseCalendarEntryResult : ( Http.Metadata, String ) -> Result String CalendarEntry
parseCalendarEntryResult ( meta, body ) =
    let
        decode =
            Decode.decodeString calendarEntryDecoder body
    in
    case decode of
        Ok calendarEntry ->
            Ok calendarEntry

        Err error ->
            Err ("fehler beim decodieren des calendars: " ++ Decode.errorToString error)


calendarErrorsDecoder : HttpEx.Error String -> List String
calendarErrorsDecoder responseError =
    errorDecoder responseError calendarErrorDecoder


calendarErrorDecoder : Decode.Decoder ErrorResponse
calendarErrorDecoder =
    Decode.map ErrorResponse Decode.string

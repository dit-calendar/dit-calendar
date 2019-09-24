module Endpoint.CalendarEntryEndpoint exposing (calendarEntriesResponse, calendarEntryResponse, createCalendarEntry, loadCalendarEntries, saveCalendarEntry)

import Data.CalendarEntry as CalendarDetail
import Data.SimpleCalendarList as CalendarList
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Decoder, Value, andThen, succeed)
import Json.Encode as Encode
import Maybe exposing (withDefault)
import String exposing (left)
import String.Extra exposing (leftOf, rightOf)


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


calendarEntryEncoder : CalendarDetail.CalendarEntry -> Encode.Value
calendarEntryEncoder model =
    Encode.object
        [ ( "version", Encode.int model.version )
        , ( "description", Encode.string model.description )
        , ( "startDate", Encode.string (model.startDate ++ "T" ++ model.startTime ++ ":00.000000Z") )
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
    Decode.list calendarEntryDecoder


calendarEntryDecoder : Decode.Decoder CalendarDetail.CalendarEntry
calendarEntryDecoder =
    Decode.map6
        CalendarDetail.CalendarEntry
        (Decode.nullable (Decode.field "entryId" Decode.int))
        (Decode.field "version" Decode.int)
        (Decode.at [ "description" ] Decode.string)
        (Decode.field "startDate" stringToDate)
        (Decode.field "startDate" stringToDateTime)
        (Decode.field "endDate" Decode.string)


stringToDate : Decoder String
stringToDate =
    Decode.string
        |> andThen (\val -> succeed <| leftOf "T" val)

stringToDateTime : Decoder String
stringToDateTime =
    Decode.string
        |> andThen (\val -> succeed <| left 5 (rightOf "T" val))


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
            Err ("fehler beim decodieren der calendar Einträge" ++ Decode.errorToString error)


parseCalendarEntryResult : ( Http.Metadata, String ) -> Result String CalendarDetail.CalendarEntry
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

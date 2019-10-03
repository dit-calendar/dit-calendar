module Endpoint.JsonParser.TaskParser exposing (parseTaskResult, taskEncoder, taskErrorsDecoder, tasksDecoder)

import Data.Task exposing (Task)
import Endpoint.JsonParser.DateTimeDecoder exposing (stringToDate, stringToDateTime)
import Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http as Http
import Http.Detailed as HttpEx
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as Encode


taskEncoder : Task -> Encode.Value
taskEncoder model =
    Encode.object
        [ ( "version", Encode.int model.version )
        , ( "description", Encode.string model.description )
        , ( "startTime", Encode.string (model.startDate ++ "T" ++ model.startTime ++ ":00.000000Z") )
        , ( "belongingUsers", Encode.list Encode.int [] )
        , case model.endDate of
            Just d ->
                case model.endTime of
                    Just t ->
                        ( "endTime", Encode.string (d ++ "T" ++ t ++ ":00.000000Z") )

                    Nothing ->
                        ( "endTime", Encode.maybe Encode.string Nothing )

            Nothing ->
                ( "endTime", Encode.maybe Encode.string Nothing )
        ]


tasksDecoder : Maybe Int -> Decode.Decoder Task
tasksDecoder calendarId =
    Decode.map8
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.succeed calendarId)
        (Decode.nullable (Decode.field "taskId" Decode.int))
        (Decode.field "version" Decode.int)
        (Decode.at [ "description" ] Decode.string)
        (Decode.field "startTime" stringToDate)
        (Decode.field "startTime" stringToDateTime)
        (Decode.maybe (Decode.field "endTime" stringToDate))
        (Decode.maybe (Decode.field "endTime" stringToDateTime))


taskErrorsDecoder : HttpEx.Error String -> List String
taskErrorsDecoder responseError =
    errorDecoder responseError taskErrorDecoder


taskErrorDecoder : Decode.Decoder ErrorResponse
taskErrorDecoder =
    Decode.map ErrorResponse Decode.string


parseTaskResult : Maybe Int -> ( Http.Metadata, String ) -> Result String Task
parseTaskResult calendarId ( meta, body ) =
    let
        decode =
            Decode.decodeString (tasksDecoder calendarId) body
    in
    case decode of
        Ok task ->
            Ok task

        Err error ->
            Err ("fehler beim decodieren des calendars: " ++ Decode.errorToString error)

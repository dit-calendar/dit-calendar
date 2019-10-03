module Endpoint.TaskEndpoint exposing (createTask, taskErrorsDecoder, taskResponse, tasksDecoder, updateTask)

import Data.Task exposing (Messages(..), Model, Msg(..), Task)
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Endpoint.Service.DateTimeDecoder exposing (stringToDate, stringToDateTime)
import Env.Serverurl as Server
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Maybe exposing (withDefault)


updateTask : Task -> Cmd Msg
updateTask model =
    Http.riskyRequest
        { method = "PUT"
        , headers = []
        , url = Server.updateCalendarTask (withDefault 0 model.calendarEntryId) (withDefault 0 model.taskId)
        , body = Http.jsonBody (taskEncoder model)
        , expect = HttpEx.expectString CreateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


createTask : Task -> Cmd Msg
createTask model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarTask (withDefault 0 model.calendarEntryId)
        , body = Http.jsonBody (taskEncoder model)
        , expect = HttpEx.expectString CreateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


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


taskResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
taskResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseTaskResult model.task.calendarEntryId value
            in
            case resp of
                Ok task ->
                    { model | task = task, messages = SuccessUpdate }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (taskErrorsDecoder error) }


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

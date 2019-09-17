module Endpoint.TaskEndpoint exposing (createTask, taskErrorsDecoder, taskResponse, tasksDecoder, updateTask)

import Data.Task exposing (Messages(..), Model, Msg(..), Task)
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
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
        , ( "startTime", Encode.string model.startTime )
        , ( "belongingUsers", Encode.list Encode.int [] )
        , ( "endTime", Encode.maybe Encode.string model.endTime )
        ]


tasksDecoder : Maybe Int -> Decode.Decoder Task
tasksDecoder calendarId =
    Decode.map6
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.succeed calendarId)
        (Decode.nullable (Decode.field "taskId" Decode.int))
        (Decode.field "version" Decode.int)
        (Decode.at [ "description" ] Decode.string)
        (Decode.field "startTime" Decode.string)
        (Decode.maybe (Decode.field "endTime" Decode.string))


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

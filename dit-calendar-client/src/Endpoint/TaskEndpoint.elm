module Endpoint.TaskEndpoint exposing (createTask, taskErrorsDecoder, taskResponse, tasksDecoder)

import Data.Task exposing (Messages(..), Model, Msg(..), Task)
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Env.Serverurl as Server
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode


createTask : Int -> Task -> Cmd Msg
createTask calendarId model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarTask calendarId
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

        --, ( "endTime", Encode.string model.endTime )
        ]


tasksDecoder : Decode.Decoder Task
tasksDecoder =
    Decode.map5
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.nullable (Decode.field "taskId" Decode.int))
        (Decode.field "version" Decode.int)
        (Decode.at [ "description" ] Decode.string)
        --(Decode.at [ "belongingUsers" ] Decode.list)
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
                    parseTaskResult value
            in
            case resp of
                Ok task ->
                    { model | task = task, messages = SuccessUpdate }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (taskErrorsDecoder error) }


parseTaskResult : ( Http.Metadata, String ) -> Result String Task
parseTaskResult ( meta, body ) =
    let
        decode =
            Decode.decodeString tasksDecoder body
    in
    case decode of
        Ok calendarEntry ->
            Ok calendarEntry

        Err error ->
            Err ("fehler beim decodieren des calendars" ++ Decode.errorToString error)

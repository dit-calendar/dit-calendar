module Endpoint.TaskEndpoint exposing (taskErrorsDecoder, tasksDecoder)

import Data.Task exposing (Task)
import Endpoint.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


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

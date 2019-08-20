module Data.Task exposing (Messages(..), Model, Msg(..), Task, TaskMsg(..), emptyTask)

import Http
import Http.Detailed as HttpEx


type alias Task =
    { taskId : Maybe Int
    , version : Int
    , description : String

    -- TODO , belongingUsers : List User
    , startTime : String
    , endTime : Maybe String
    }


emptyTask : Task
emptyTask =
    { taskId = Nothing, version = 0, description = "", startTime = "", endTime = Nothing }


type alias Model =
    { task : Task
    , messages : Messages
    }


type TaskMsg
    = Description String
      --TODO | BelongingUsers (List User)
    | StartTime String
    | EndTime String


type Msg
    = TaskMsg TaskMsg
    | SaveTask
    | CreateTaskResult (Result (HttpEx.Error String) ( Http.Metadata, String ))



-- TODO rausziehen in eigenes module


type Messages
    = Problems (List String)
    | SuccessUpdate

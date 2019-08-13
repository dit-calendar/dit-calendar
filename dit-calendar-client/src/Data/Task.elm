module Data.Task exposing (Model, Msg(..), Task)


type alias Task =
    { taskId : Maybe Int
    , version : Int
    , description : String

    -- TODO , belongingUsers : List User
    , startTime : String
    , endTime : Maybe String
    }


type alias Model =
    { task : Task
    , problems : List String
    }


type TaskMsg
    = Description String
      --TODO | BelongingUsers (List User)
    | StartTime String
    | EndTime String


type Msg
    = TaskMsg TaskMsg

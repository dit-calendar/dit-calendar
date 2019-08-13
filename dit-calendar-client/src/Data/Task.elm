module Data.Task exposing (Messages(..), Model, Msg(..), Task, TaskMsg(..))


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
    , messages : Messages
    }


type TaskMsg
    = Description String
      --TODO | BelongingUsers (List User)
    | StartTime String
    | EndTime String


type Msg
    = TaskMsg TaskMsg



-- TODO rausziehen in eigenes module


type Messages
    = Problems (List String)
    | SuccessUpdate

module Data.Task exposing (Model, Msg(..), Task, TaskMsg(..), emptyTask)

import Data.UIMessages exposing (Messages)
import Http
import Http.Detailed as HttpEx


type alias Task =
    { calendarEntryId : Maybe Int
    , taskId : Maybe Int
    , version : Int
    , title : String
    , description : Maybe String

    -- TODO , assignedUsers : List User
    , startDate : String
    , startTime : String
    , endDate : Maybe String
    , endTime : Maybe String
    }


emptyTask : Int -> String -> Task
emptyTask calendarId startDate =
    { calendarEntryId = Just calendarId, taskId = Nothing, version = 0, title = "", description = Nothing, startDate = startDate, startTime = "", endDate = Nothing, endTime = Nothing }


type alias Model =
    { task : Task
    , messages : Messages
    }


type TaskMsg
    = Title String
    | Description String
      --TODO | assignedUsers (List User)
    | StartTime String
    | StartDate String
    | EndTime String
    | EndDate String


type Msg
    = TaskMsg TaskMsg
    | SaveTask
    | CreateTaskResult (Result (HttpEx.Error String) ( Http.Metadata, String ))

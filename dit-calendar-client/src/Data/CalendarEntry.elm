module Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Messages(..), Model, Msg(..), emptyCalendarEntry)

import Data.Task exposing (Task)
import Http
import Http.Detailed as HttpEx


type alias CalendarEntry =
    { entryId : Maybe Int
    , version : Int
    , description : String
    , startDate : String
    , startTime : String
    , endDate : String
    , endTime : String
    }

type Messages
    = Problems (List String)
    | SuccessUpdate


type alias Model =
    { calendarEntry : CalendarEntry
    , tasks : List Task
    , messages : Messages
    }

emptyCalendarEntry : CalendarEntry
emptyCalendarEntry =
    {entryId = Nothing, version = 0, description = "", startDate = "", startTime = "", endDate = "", endTime = "" }

type CalendarDetailMsg
    = Description String
    | StartDate String
    | StartTime String
    | EndDate String
    | EndTime String


type Msg
    = CalendarDetailMsg CalendarDetailMsg
    | GetCalendarEntryTasks Int
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | SaveCalendar
    | SaveCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenTaskDetailsView Task
    | CreateCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))


module Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Messages(..), Model, Msg(..), Task(..))

import Http
import Http.Detailed as HttpEx


type Task
    = Task String -- Todo use Task.Model from Task.elm(page)


type alias CalendarEntry =
    { entryId : Maybe Int
    , version : Int
    , description : String
    , startDate : String
    , endDate : String
    }

type Messages
    = Problems (List String)
    | SuccessUpdate


type alias Model =
    { calendarEntry : CalendarEntry
    , tasks : List Task
    , messages : Messages
    }


type CalendarDetailMsg
    = Description String
    | StartDate String
    | EndDate String


type Msg
    = CalendarDetailMsg CalendarDetailMsg
    | GetCalendarEntryTasks Int
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | SaveCalendar
    | SaveCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))

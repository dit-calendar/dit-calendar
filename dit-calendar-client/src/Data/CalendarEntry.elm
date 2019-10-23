module Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Model, Msg(..), emptyCalendarEntry)

import Data.Task exposing (Task)
import Data.UIMessages exposing (Messages)
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


type alias Model =
    { calendarEntry : CalendarEntry
    , tasks : List Task
    , messages : Messages
    }


emptyCalendarEntry : CalendarEntry
emptyCalendarEntry =
    { entryId = Nothing, version = 0, description = "", startDate = "", startTime = "", endDate = "", endTime = "" }


type CalendarDetailMsg
    = Description String
    | StartDate String
    | StartTime String
    | EndDate String
    | EndTime String


type Msg
    = CalendarDetailEditMsg CalendarDetailMsg
    | GetCalendarEntry
    | GetCalendarEntryResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | SaveCalendar
    | SaveCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenTaskDetailsView Task

module Data.SimpleCalendarList exposing (Model, Msg(..), emptyModel)

import Data.CalendarEntry exposing (CalendarEntry)
import Http
import Http.Detailed as HttpEx


type alias Model =
    { calendarEntries : List CalendarEntry
    , problems : List String
    }


emptyModel : Model
emptyModel =
    { calendarEntries = [], problems = [] }


type Msg
    = PerformGetCalendarEntries
    | GetCalendarEntriesResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenCalendarDetialsView CalendarEntry

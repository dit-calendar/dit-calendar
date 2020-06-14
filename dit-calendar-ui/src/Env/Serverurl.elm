module Env.Serverurl exposing (baseUrl, calendarEntries, calendarEntry, calendarTask, loginUrl, logoutUrl, registerUrl, serverStartUpUrl, updateCalendarTask)


authUrl : String
authUrl =
    baseUrl ++ "authenticate/authentication-methods/password/"


loginUrl : String
loginUrl =
    authUrl ++ "token"


logoutUrl : String
logoutUrl =
    baseUrl ++ "logout"


registerUrl : String
registerUrl =
    authUrl ++ "account"


calendarEntries : String
calendarEntries =
    baseUrl ++ "calendarentries/"


calendarTask : Int -> String
calendarTask calendarId =
    calendarEntries ++ String.fromInt calendarId ++ "/tasks"


updateCalendarTask : Int -> Int -> String
updateCalendarTask calendarId taskId =
    calendarTask calendarId ++ "/" ++ String.fromInt taskId


calendarEntry : Int -> String
calendarEntry entryId =
    calendarEntries ++ String.fromInt entryId


baseUrl : String
baseUrl =
    -- "http://localhost:8080/"
    "https://dit-calendar-server-vitab.cloud.okteto.net/"


serverStartUpUrl =
    "https://dit-calendar-okteto.herokuapp.com/"

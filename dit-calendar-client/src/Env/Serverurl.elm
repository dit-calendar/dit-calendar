module Env.Serverurl exposing (calendarEntries, calendarEntry, calendarTask, loginUrl, logoutUrl, registerUrl, updateCalendarTask)


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
    "https://localhost:8443/"

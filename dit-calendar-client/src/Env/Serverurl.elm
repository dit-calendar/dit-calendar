module Env.Serverurl exposing (calendarEntries, calendarTask, loginUrl, registerUrl)


authUrl : String
authUrl =
    baseUrl ++ "authenticate/authentication-methods/password/"


loginUrl : String
loginUrl =
    authUrl ++ "token"


registerUrl : String
registerUrl =
    authUrl ++ "account"


calendarEntries : String
calendarEntries =
    baseUrl ++ "calendarentries/"


calendarTask : Int -> String
calendarTask taskId =
    calendarEntries ++ String.fromInt taskId ++ "/tasks"


baseUrl : String
baseUrl =
    "https://localhost:8443/"

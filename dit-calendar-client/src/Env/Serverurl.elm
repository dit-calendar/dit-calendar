module Env.Serverurl exposing (loginUrl, registerUrl)

authUrl : String
authUrl = baseUrl ++ "authenticate/authentication-methods/password/"

loginUrl : String
loginUrl = authUrl ++ "token"

registerUrl : String
registerUrl = authUrl ++ "account"

baseUrl : String
baseUrl = "https://localhost:8443/"

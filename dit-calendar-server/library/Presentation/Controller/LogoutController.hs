module Presentation.Controller.LogoutController where

import           Happstack.Server (Response, ok, setHeader, toResponse)

import           AppContext       (App)

logout :: App Response
logout = do
    let res = toResponse ("" :: String)
    ok $ setHeader "Set-Cookie" "atc=logout; expires=Thu, 01 Jan 1970 00:00:00 GMT" res

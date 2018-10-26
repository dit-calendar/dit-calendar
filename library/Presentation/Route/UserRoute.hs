module Presentation.Route.UserRoute
    ( routeUser
    , routeDetailUser
    , routeUsers
    ) where

import           Data.Text                                  (pack)

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look,
                                                             mapServerPartT)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           Presentation.AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getHttpMethod)
import           Presentation.ResponseHelper                (notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController

routeUsers :: App Response
routeUsers = do
    m <- getHttpMethod
    case m of
        GET -> UserController.usersPage

routeUser :: UserId -> App Response
routeUser userId = do
    m <- getHttpMethod
    case m of
        GET -> UserController.userPage userId

routeDetailUser :: App Response
routeDetailUser = do
    m <- getHttpMethod
    case m of
        PUT -> do
            name <- look "name"
            callIfAuthorized (UserController.updateUser $ pack name)
        DELETE -> callIfAuthorized UserController.deleteUser
        -- curl -X POST -d "name=FooBar" http://localhost:8000/user
        POST -> do
            description <- look "description"
            newDate <- look "date"
            callIfAuthorized (CalendarController.createCalendarEntry newDate $ pack description)
        other -> notImplemented other

module Presentation.Route.UserRoute
    ( routeUser
    , routeDetailUser
    , routeUsers
    ) where

import           Data.Text                                  (pack)

import           Happstack.Foundation                       (lift)
import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             look,
                                                             mapServerPartT)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           Presentation.AcidHelper                    (CtrlV)
import           Presentation.HttpServerHelper              (getHttpMethod)
import           Presentation.ResponseHelper                (notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController

routeUsers :: CtrlV
routeUsers = do
    m <- getHttpMethod
    case m of
        GET -> lift UserController.usersPage

routeUser :: UserId -> CtrlV
routeUser userId = do
    m <- getHttpMethod
    case m of
        GET -> lift $ UserController.userPage userId

routeDetailUser :: CtrlV
routeDetailUser = do
    m <- getHttpMethod
    case m of
        PUT -> do
            name <- look "name"
            lift $ callIfAuthorized (UserController.updateUser $ pack name)
        DELETE -> lift $ callIfAuthorized UserController.deleteUser
        -- curl -X POST -d "name=FooBar" http://localhost:8000/user
        POST -> do
            description <- look "description"
            newDate <- look "date"
            lift $ callIfAuthorized (CalendarController.createCalendarEntry newDate $ pack description)
        other -> lift $ notImplemented other

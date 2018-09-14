module Presentation.Route.UserRoute (routeUser, routeDetailUser) where

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             look,
                                                             mapServerPartT)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           Happstack.Foundation                       (lift)
import           Presentation.AcidHelper                    (CtrlV)
import           Presentation.HttpServerHelper              (getHttpMethod)
import           Presentation.ResponseHelper                (notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController


routeUser :: UserId -> CtrlV
routeUser userId = do
  m <- getHttpMethod
  case m of
    GET -> lift $ UserController.userPage userId
    POST -> do
      description <- look "description"
      newDate <- look "date"
      lift $ callIfAuthorized (CalendarController.createCalendarEntry userId newDate description)

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
    PUT -> do
      name <- look "name"
      lift $ callIfAuthorized (UserController.updateUser name)
    DELETE -> lift $ callIfAuthorized UserController.deleteUser
    -- curl -X POST -d "name=FooBar" http://localhost:8000/user
    GET -> lift UserController.usersPage
    other -> lift $ notImplemented other

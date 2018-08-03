module Presentation.Route.UserRoute (routeUser, routeDetailUser) where

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           Presentation.AcidHelper                    (CtrlV)
import           Presentation.HttpServerHelper              (getHttpMethod)
import           Presentation.ResponseHelper                (notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController


routeUser :: UserId -> CtrlV
routeUser userId = do
  m <- getHttpMethod
  case m of
    GET ->
      UserController.userPage userId
    POST -> do
      description <- look "description"
      newDate <- look "date"
      callIfAuthorized (CalendarController.createCalendarEntry userId newDate description)
    PUT -> do
      name <- look "name"
      callIfAuthorized (UserController.updateUser userId name)

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
    DELETE -> callIfAuthorized UserController.deleteUser
    -- curl -X POST -d "name=FooBar" http://localhost:8000/user
    other -> notImplemented other

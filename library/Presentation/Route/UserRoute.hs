module Presentation.Route.UserRoute (routeUser, routeDetailUser) where

import           Data.Acid                                  (AcidState)
import           Happstack.Authenticate.Core                (AuthenticateState)
import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           Presentation.AcidHelper                    (CtrlV)
import           Presentation.HttpServerHelper              (getHttpMethod)
import           Presentation.ResponseHelper                (notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController


routeUser :: UserId -> AcidState AuthenticateState -> CtrlV
routeUser userId authState = do
  m <- getHttpMethod
  case m of
    GET ->
      UserController.userPage userId
    DELETE ->
      callIfAuthorized authState (UserController.deleteUser userId authState)
    POST -> do
      description <- look "description"
      newDate <- look "date"
      callIfAuthorized authState (CalendarController.createCalendarEntry userId newDate description)
    PUT -> do
      name <- look "name"
      callIfAuthorized authState (UserController.updateUser userId name)

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/user
    other -> notImplemented other

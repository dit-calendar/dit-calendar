module Presentation.Route.UserRoute
    ( routeUser
    , routeDetailUser
    , routeUsers
    ) where

import           Data.Aeson                                 (eitherDecode)
import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look,
                                                             mapServerPartT)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (UserId)
import           AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getHttpMethod, getBody)
import           Presentation.ResponseHelper                (badRequest, notImplemented)
import           Presentation.Dto.User                      as UserDto (User (..), validate)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.UserController     as UserController
import qualified Presentation.Dto.CalendarEntry as CalendarDto

routeUsers :: App Response
routeUsers = do
    m <- getHttpMethod
    case m of
        GET -> UserController.usersPage
        other -> notImplemented other

routeUser :: UserId -> App Response
routeUser userId = do
    m <- getHttpMethod
    case m of
        GET -> UserController.userPage userId
        other -> notImplemented other

routeDetailUser :: App Response
routeDetailUser = do
    m <- getHttpMethod
    case m of
        GET -> callIfAuthorized UserController.loggedUserPage
        PUT -> do
              body <- getBody
              case validate (eitherDecode body :: Either String UserDto.User) of
                  Right userDto -> callIfAuthorized (UserController.updateUser userDto)
                  Left errorMessage -> badRequest errorMessage
        DELETE -> callIfAuthorized UserController.deleteUser
        other -> notImplemented other

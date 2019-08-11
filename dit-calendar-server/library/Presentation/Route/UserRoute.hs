module Presentation.Route.UserRoute
    ( routeUser
    , routeDetailUser
    , routeUsers
    ) where

import           Data.Aeson                             (eitherDecode)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response)

import           AcidHelper                             (App)
import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (UserId)
import           Presentation.Dto.User                  as UserDto (User (..),
                                                                    validate)
import           Presentation.HttpServerHelper          (getBody, getHttpMethod)
import           Presentation.ResponseHelper            (badRequest,
                                                         handleResponse,
                                                         notImplemented)

import qualified Presentation.Controller.UserController as UserController

routeUsers :: App Response
routeUsers = do
    m <- getHttpMethod
    case m of
        GET   -> UserController.usersPage >>= handleResponse
        other -> notImplemented other

routeUser :: UserId -> App Response
routeUser userId = do
    m <- getHttpMethod
    case m of
        GET   -> UserController.userPage userId >>= handleResponse
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

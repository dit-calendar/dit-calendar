module Presentation.Route.UserRoute
    ( routeDetailUser ) where

import           Data.Aeson                             (eitherDecode)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response)

import           AppContext                             (App)
import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (UserId)
import           Presentation.Dto.User                  as UserDto (User (..),
                                                                    validate)
import           Server.HttpServerHelper                (getBody, getHttpMethod)
import           Server.ResponseBuilder                 (badRequest,
                                                         handleResponse,
                                                         notImplemented)

import qualified Presentation.Controller.UserController as UserController

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

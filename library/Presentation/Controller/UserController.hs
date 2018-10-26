{-# LANGUAGE ScopedTypeVariables #-}

module Presentation.Controller.UserController (createUser, updateUser, deleteUser, usersPage, userPage) where

import           Data.List                            (isInfixOf)
import           Data.Text                            (Text, unpack)
import           Data.Aeson                           (encode)

import           Happstack.Authenticate.Core          (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Core (NewAccountData (..))
import           Happstack.Foundation                 (HasAcidState (getAcidState),
                                                       query)
import           Happstack.Server                     (Method (GET), Response,
                                                       ServerPartT, method, ok,
                                                       rsBody, toResponse)
import           Web.Routes                           (RouteT, mapRouteT,
                                                       nestURL, unRouteT)

import           Data.Domain.Types                    (Description, UserId)
import           Data.Domain.User                     as DomainUser (User (..))
import           Data.Service.Authorization           as AuthService (deleteAuthUser)
import           Presentation.AcidHelper              (App)
import           Presentation.HttpServerHelper        (getBody,
                                                       mapServerPartTIO2App,
                                                       readAuthUserFromBodyAsList)
import           Presentation.ResponseHelper          (okResponse, okResponseJson, onUserExist)
import           Presentation.Route.PageEnum          (Sitemap)
import           Presentation.Dto.User                as UserDto (User (..), transform)

import qualified Data.Repository.Acid.User            as UserAcid
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Service.User                    as UserService
import qualified Happstack.Authenticate.Core          as AuthUser


--handler for userPage
userPage :: UserId -> App Response
userPage i = onUserExist i (okResponseJson . encode . transform)

--handler for userPage
usersPage :: App Response
usersPage =
    do  method GET
        userList <- query UserAcid.AllUsers
        okResponseJson $ encode $ map transform userList

createUser  :: AuthenticateURL -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> App Response
createUser authenticateURL routeAuthenticate = do
    body <- getBody
    let createUserBody = readAuthUserFromBodyAsList body
    case createUserBody of
        Just (NewAccountData naUser naPassword _) ->
            do
                let naUsername :: AuthUser.Username = AuthUser._username naUser
                let username = AuthUser._unUsername naUsername

                response <- leaveRouteT (mapRouteT mapServerPartTIO2App $ routeAuthenticate authenticateURL)
                let responseBody = rsBody response
                if isInfixOf "NotOk" $ show responseBody then
                    return response
                else
                    createDomainUser username

        -- if request body is not valid use response of auth library
        Nothing -> leaveRouteT (mapRouteT mapServerPartTIO2App $ routeAuthenticate authenticateURL)

leaveRouteT :: RouteT url m a-> m a
leaveRouteT r = unRouteT r (\ _ _ -> undefined)


createDomainUser :: Text -> App Response
createDomainUser name = do
    mUser <- UserRepo.createUser name
    okResponseJson $ encode $ transform mUser

updateUser :: UserDto.User -> DomainUser.User -> App Response
updateUser userDto loggedUser = updateUsr loggedUser
    where updateUsr user = do
              UserRepo.updateName user (UserDto.name userDto)
              okResponse $ "User with id:" ++ show (DomainUser.userId loggedUser) ++ "updated"


deleteUser :: DomainUser.User -> App Response
deleteUser loggedUser = do
    UserService.deleteUser loggedUser
    AuthService.deleteAuthUser loggedUser
    okResponse $ "User with id:" ++ show (DomainUser.userId loggedUser) ++ "deleted"

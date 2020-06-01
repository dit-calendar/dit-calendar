{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Presentation.Controller.UserController (createUser, updateUser, deleteUser, loggedUserPage) where

import           Data.Aeson                           (encode)
import           Data.Default                         (def)
import           Data.List                            (isInfixOf)
import           Data.Text                            (Text)

import           Happstack.Authenticate.Core          (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Core (NewAccountData (..))
import           Happstack.Server                     (Method (GET), Response,
                                                       ServerPartT, badRequest,
                                                       method, ok, rsBody,
                                                       toResponse)
import           Web.Routes                           (RouteT, mapRouteT,
                                                       nestURL, unRouteT)

import           AppContext                           (App)
import           Data.Domain.Types                    (EitherResult, UserId)
import           Data.Domain.User                     as DomainUser (User (..))
import           Data.Service.Authorization           as AuthService (deleteAuthUser)
import           Presentation.Dto.User                as UserDto
import           Presentation.Mapper.BaseMapper       (transformToDtoE,
                                                       transformToDtoList)
import           Presentation.Mapper.UserMapper       (transformFromDto,
                                                       transformToDto)
import           Presentation.Route.PageEnum          (Sitemap)
import           Server.HappstackHelper               (HasAcidState (getAcidState),
                                                       liftServerPartT2FoundationT,
                                                       query)
import           Server.HttpServerHelper              (getBody, readAuthUserFromBodyAsList)
import           Server.ResponseBuilder               (okResponseJson,
                                                       onUserExist)

import qualified Data.Repository.Acid.User            as UserAcid
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Service.User                    as UserService
import qualified Happstack.Authenticate.Core          as AuthUser


loggedUserPage :: DomainUser.User -> App (EitherResult UserDto.User)
loggedUserPage loggedUser = return $ Right $ transformToDto loggedUser

--TODO wrapper für die Auth-lib
createUser  :: AuthenticateURL -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> App Response
createUser authenticateURL routeAuthenticate = do
    body <- getBody
    let createUserBody = readAuthUserFromBodyAsList body
    case createUserBody of
        Just (NewAccountData naUser _ _) ->
            do
                let naUsername :: AuthUser.Username = AuthUser._username naUser
                let username = AuthUser._unUsername naUsername

                response <- leaveRouteT (mapRouteT liftServerPartT2FoundationT $ routeAuthenticate authenticateURL)
                let responseBody = rsBody response
                if isInfixOf "NotOk" $ show responseBody then
                    badRequest response
                else
                    createDomainUser username

        -- if request body is not valid use response of auth library
        Nothing -> leaveRouteT (mapRouteT liftServerPartT2FoundationT $ routeAuthenticate authenticateURL)

leaveRouteT :: RouteT url m a-> m a
leaveRouteT r = unRouteT r (\ _ _ -> undefined)

--TODO other creating concept, or change rest interface (and transform UserDto to NewAccoundData)
createDomainUser :: Text -> App Response
createDomainUser name = do
    mUser <- UserRepo.createUser user
    okResponseJson $ encode $ transformToDto mUser
    where user = def { DomainUser.loginName = name }

--TODO updating AuthenticateUser is missing
updateUser :: UserDto.User -> DomainUser.User -> App (EitherResult UserDto.User)
updateUser userDto loggedUser = do
              updatedUser <- UserRepo.updateUser $ transformFromDto userDto (Just loggedUser)
              return $ transformToDtoE updatedUser


deleteUser :: DomainUser.User -> App (EitherResult ())
deleteUser loggedUser = do
    UserService.deleteUser loggedUser
    AuthService.deleteAuthUser loggedUser
    return $ Right ()

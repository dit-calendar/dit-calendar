{-# LANGUAGE ScopedTypeVariables #-}

module Presentation.Controller.UserController (createUser, updateUser, deleteUser, usersPage, userPage) where

import           Data.List                            (isInfixOf)
import           Data.Text                            (pack, unpack)

import           Happstack.Authenticate.Core          (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Core (NewAccountData (..))
import           Happstack.Foundation                 (HasAcidState (getAcidState),
                                                       query)
import           Happstack.Server                     (Method (GET), Response,
                                                       ServerPartT, method, ok,
                                                       rsBody, toResponse)
import           Web.Routes                           (RouteT, mapRouteT,
                                                       nestURL, unRouteT)

import           Data.Service.Authorization           as AuthService (deleteAuthUser)
import           Data.Domain.Types                    (UserId)
import           Data.Domain.User                     as DomainUser (User (..))
import           Presentation.AcidHelper              (App)
import           Presentation.HttpServerHelper        (getBody,
                                                       mapServerPartTIO2App,
                                                       readAuthUserFromBodyAsList)
import           Presentation.ResponseHelper          (okResponse, onUserExist)
import           Presentation.Route.PageEnum          (Sitemap)

import qualified Data.Repository.Acid.User            as UserAcid
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Service.User                    as UserService
import qualified Happstack.Authenticate.Core          as AuthUser


--handler for userPage
userPage :: UserId -> App Response
userPage i = onUserExist i (\u -> okResponse $ "peeked at the name and saw: " ++ show u)

--handler for userPage
usersPage :: App Response
usersPage =
    let temp = "Anzeige aller User \n" in
    do  method GET
        userList <- query UserAcid.AllUsers
        case userList of
            [] ->
                ok $ toResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                ok $ toResponse $ temp ++ printUsersList (x:xs)

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
                    createDomainUser (unpack username)

        -- if request body is not valid use response of auth library
        Nothing -> leaveRouteT (mapRouteT mapServerPartTIO2App $ routeAuthenticate authenticateURL)

leaveRouteT :: RouteT url m a-> m a
leaveRouteT r = unRouteT r (\ _ _ -> undefined)


createDomainUser :: String -> App Response
createDomainUser name = do
    mUser <- UserRepo.createUser name
    okResponse $ "User created: " ++ show mUser

updateUser :: String -> DomainUser.User -> App Response
updateUser name loggedUser = updateUsr loggedUser
    where updateUsr user = do
              UserRepo.updateName user name
              okResponse $ "User with id:" ++ show (DomainUser.userId loggedUser) ++ "updated"


deleteUser :: DomainUser.User -> App Response
deleteUser loggedUser = do
    UserService.deleteUser loggedUser
    AuthService.deleteAuthUser loggedUser
    okResponse $ "User with id:" ++ show (DomainUser.userId loggedUser) ++ "deleted"


printUsersList :: [DomainUser.User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ DomainUser.name x ++ "mit Id: "++ show (DomainUser.userId x))
        ++ "\n" ++ printUsersList xs



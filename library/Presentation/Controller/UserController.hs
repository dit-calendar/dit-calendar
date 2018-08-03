{-# LANGUAGE ScopedTypeVariables #-}

module Presentation.Controller.UserController (createUser, updateUser, deleteUser, usersPage, userPage) where

import           Data.List                            (isInfixOf)
import           Data.Maybe                           (fromJust)
import           Data.Text                            (pack)

import           Happstack.Authenticate.Core          (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Core (NewAccountData (..))
import           Happstack.Foundation                 (HasAcidState (getAcidState),
                                                       query, update)
import           Happstack.Server                     (Method (GET), Response,
                                                       ServerPartT, method,
                                                       rsBody)
import           Web.Routes                           (RouteT, mapRouteT,
                                                       nestURL)

import           Data.Domain.Types                    (UserId)
import           Data.Domain.User                     as DomainUser (User (..))
import           Presentation.AcidHelper              (CtrlV)
import           Presentation.HttpServerHelper        (getBody,
                                                       mapServerPartTIO2App,
                                                       readAuthUserFromBodyAsList)
import           Presentation.ResponseHelper          (okResponse, onUserExist)
import           Presentation.Route.PageEnum          (Sitemap (Authenticate))

import qualified Data.Repository.Acid.User            as UserAcid
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Service.User                    as UserService
import qualified Happstack.Authenticate.Core          as AuthUser


--handler for userPage
userPage :: UserId -> CtrlV
userPage i = onUserExist i (\u -> okResponse $ "peeked at the name and saw: " ++ show u)

--handler for userPage
usersPage :: CtrlV
usersPage =
    let temp = "Anzeige aller User \n" in
    do  method GET
        userList <- query UserAcid.AllUsers
        case userList of
            [] ->
                okResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                okResponse $ temp ++ printUsersList (x:xs)

createUser  :: AuthenticateURL -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> CtrlV
createUser authenticateURL routeAuthenticate = do
    body <- getBody
    let createUserBody = readAuthUserFromBodyAsList body
    case createUserBody of
        Just (NewAccountData naUser naPassword _) ->
            do
                let naUsername :: AuthUser.Username = AuthUser._username naUser
                let username = AuthUser._unUsername naUsername

                response <- mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
                let responseBody = rsBody response
                if isInfixOf "NotOk" $ show responseBody then
                    return response
                else
                    createDomainUser (show username)

        -- if request body is not valid use response of auth library
        Nothing -> mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL

createDomainUser :: String -> CtrlV
createDomainUser name = do
    mUser <- UserRepo.createUser name
    okResponse $ "User created: " ++ show mUser

updateUser :: UserId -> String -> DomainUser.User -> CtrlV
updateUser id name loggedUser = onUserExist id updateUsr
    where updateUsr user = do
              UserRepo.updateName user name
              okResponse $ "User with id:" ++ show id ++ "updated"


deleteUser :: DomainUser.User -> CtrlV
deleteUser loggedUser = do
    deleteUsr loggedUser
    deleteAuthUser loggedUser
        where deleteUsr user = do
                  UserService.deleteUser user
                  okResponse $ "User with id:" ++ show (DomainUser.userId loggedUser) ++ "deleted"

deleteAuthUser ::DomainUser.User -> CtrlV
deleteAuthUser loggedUser =  do
    let userId = DomainUser.userId loggedUser
    mUser <- query (AuthUser.GetUserByUsername autUserName)
    update $ AuthUser.DeleteUser (AuthUser._userId $ fromJust mUser)
    okResponse $ "User with id:" ++ show userId ++ "deleted"
        where autUserName = AuthUser.Username {AuthUser._unUsername = pack $ name loggedUser}


printUsersList :: [DomainUser.User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ DomainUser.name x ++ "mit Id: "++ show (DomainUser.userId x))
        ++ "\n" ++ printUsersList xs



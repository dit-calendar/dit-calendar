{-# LANGUAGE ScopedTypeVariables #-}

module Route.Routing ( authOrRoute ) where

import Happstack.Server            ( ServerPartT, Response, Method(GET, POST, DELETE, PUT)
                                   , BodyPolicy(..), decodeBody, defaultBodyPolicy, look, mapServerPartT )
import Happstack.Foundation        ( lift )
import Web.Routes                  ( RouteT, nestURL, mapRouteT )
import Happstack.Authenticate.Core ( AuthenticateURL(..), AuthenticateState, Username(..), User(_username) )
import Happstack.Authenticate.Password.Core  ( NewAccountData(..) )
import Data.Acid                   ( AcidState )

import Data.Domain.Types           ( UserId, EntryId, TaskId )
import Route.HttpService           ( getBody, readAuthUserFromBodyAsList, getHttpMethod )
import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV, App )
import Controller.ResponseHelper   ( okResponse )
import Auth.Authorization          ( routheIfAuthorized )

import qualified Data.Domain.User               as DomainUser
import qualified Controller.UserController      as UserController
import qualified Controller.HomeController      as HomeController
import qualified Controller.CalendarController  as CalendarController
import qualified Controller.TaskController      as TaskController


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

mapServerPartTIO2App :: (ServerPartT IO) Response -> App Response
mapServerPartTIO2App = mapServerPartT lift

--authenticate or route
authOrRoute :: AcidState AuthenticateState
        -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
        -> Sitemap -> CtrlV
authOrRoute authenticateState routeAuthenticate url =
    do  decodeBody myPolicy
        case url of
            Authenticate authenticateURL ->
                if show authenticateURL ==  "AuthenticationMethods (Just (AuthenticationMethod {_unAuthenticationMethod = \"password\"},[\"account\"]))"
                then
                    do
                        body <- getBody
                        let createUserBody = readAuthUserFromBodyAsList body
                        case createUserBody of
                            Just (NewAccountData naUser naPassword _) ->
                                do
                                    let naUsername :: Happstack.Authenticate.Core.Username = _username naUser
                                    let username = _unUsername naUsername
                                    UserController.createUser (show username)
                                    --TODO delete User if creating Auth.User failed
                                    mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
                                    --response <- mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
                                    --if (rsCode response == 200) then
                                        -- userName <- look "naUser.username"
                                    --mUser <- query' authenticateState (GetUserByUsername Username{_unUsername = pack (show body)})
                                    --case mUser of
                                    --    Nothing -> return response
                                    --    (Just user) -> UserController.createUser (show body)
                                    --else response
                            -- if request body is not valid use response of auth library
                            Nothing -> mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
                else mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
            other -> routheIfAuthorized authenticateState route other

-- | the route mapping function
route :: Sitemap -> DomainUser.User -> CtrlV
route url loggedUser =
    do  decodeBody myPolicy
        case url of
            Home                 -> HomeController.homePage
            Userdetail           -> routeDetailUser
            User i               -> routeUser i
            CalendarEntry i      -> routeCalendarEntry i
            Task i               -> routeTask i
            TaskWithCalendar e u -> routeTaskWithCalendar e u
            TaskWithUser t u     -> routeTaskWithUser t u

routeUser :: UserId -> CtrlV
routeUser userId = do
  m <- getHttpMethod
  case m of
    GET ->
      UserController.userPage userId
    DELETE ->
      UserController.deleteUser userId
    POST -> do
      description <- look "description"
      newDate <- look "date"
      CalendarController.createCalendarEntry userId newDate description
    PUT -> do
      name <- look "name"
      UserController.updateUser userId name

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/user
    other -> notImplemented other

routeTask :: TaskId -> CtrlV
routeTask taskId = do
  m <- getHttpMethod
  case m of
    GET  ->
      TaskController.taskPage taskId
    PUT -> do
      description <- look "description"
      TaskController.updateTask taskId description
    other -> notImplemented other

routeTaskWithCalendar :: TaskId -> EntryId -> CtrlV
routeTaskWithCalendar taskId entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      TaskController.deleteTask entryId taskId
    other -> notImplemented other

routeTaskWithUser :: TaskId -> UserId -> CtrlV
routeTaskWithUser taskId userId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      TaskController.removeUserFromTask taskId userId
    PUT ->
      TaskController.addUserToTask taskId userId
    other -> notImplemented other

routeCalendarEntry :: EntryId -> CtrlV
routeCalendarEntry entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      CalendarController.deleteCalendarEntry entryId
    GET ->
      CalendarController.entryPage entryId
    POST -> do
      description <- look "description"
      TaskController.createTask entryId description
    PUT -> do
      description <- look "description"
      CalendarController.updateCalendarEntry entryId description

notImplemented :: Method -> CtrlV
notImplemented httpMethod = okResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")

{-# LANGUAGE ScopedTypeVariables #-}

module Route.Routing ( authOrRoute ) where

import Data.List (isInfixOf)
import Happstack.Server            ( ServerPartT, Response, Method(GET, POST, DELETE, PUT), rsCode, rsBody
                                   , BodyPolicy(..), decodeBody, defaultBodyPolicy, look, mapServerPartT )
import Happstack.Foundation        ( lift )
import Web.Routes                  ( RouteT, nestURL, mapRouteT )
import Happstack.Authenticate.Core ( AuthenticateURL(..), AuthenticateState, Username(..), User(_username) )
import Happstack.Authenticate.Password.Core  ( NewAccountData(..) )
import Data.Acid                   ( AcidState )

import Data.Domain.Types           ( UserId, EntryId, TaskId )
import Route.HttpServerHelper      ( getBody, readAuthUserFromBodyAsList, getHttpMethod )
import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV, App )
import Controller.ResponseHelper   ( okResponse )
import Auth.Authorization          ( callIfAuthorized )

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
authOrRoute authenticateState routeAuthenticate url = case url of
    Authenticate authenticateURL ->
        if show authenticateURL ==  "AuthenticationMethods (Just (AuthenticationMethod {_unAuthenticationMethod = \"password\"},[\"account\"]))"
        then createUser authenticateURL routeAuthenticate
        else mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
    other -> route other authenticateState

createUser  :: AuthenticateURL -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> CtrlV
createUser authenticateURL routeAuthenticate = do
    body <- getBody
    let createUserBody = readAuthUserFromBodyAsList body
    case createUserBody of
        Just (NewAccountData naUser naPassword _) ->
            do
                let naUsername :: Happstack.Authenticate.Core.Username = _username naUser
                let username = _unUsername naUsername

                response <- mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
                let responseBody = rsBody response
                if (isInfixOf "NotOk" (show responseBody)) then
                    return response
                else
                    UserController.createUser (show username)

        -- if request body is not valid use response of auth library
        Nothing -> mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL

-- | the route mapping function
route :: Sitemap -> AcidState AuthenticateState -> CtrlV
route url authState =
    do  decodeBody myPolicy
        case url of
            Home                 -> HomeController.homePage
            Userdetail           -> routeDetailUser
            User i               -> routeUser i authState
            CalendarEntry i      -> routeCalendarEntry i authState
            Task i               -> routeTask i authState
            TaskWithCalendar e u -> routeTaskWithCalendar e u authState
            TaskWithUser t u     -> routeTaskWithUser t u authState

routeUser :: UserId -> AcidState AuthenticateState -> CtrlV
routeUser userId authState = do
  m <- getHttpMethod
  case m of
    GET ->
      UserController.userPage userId
    DELETE ->
      callIfAuthorized authState (UserController.deleteUser userId)
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

routeTask :: TaskId -> AcidState AuthenticateState -> CtrlV
routeTask taskId authState = do
  m <- getHttpMethod
  case m of
    GET  ->
      TaskController.taskPage taskId
    PUT -> do
      description <- look "description"
      callIfAuthorized authState (TaskController.updateTask taskId description)
    other -> notImplemented other

routeTaskWithCalendar :: TaskId -> EntryId -> AcidState AuthenticateState -> CtrlV
routeTaskWithCalendar taskId entryId authState = do
  m <- getHttpMethod
  case m of
    DELETE ->
      callIfAuthorized authState (TaskController.deleteTask entryId taskId)
    other -> notImplemented other

routeTaskWithUser :: TaskId -> UserId -> AcidState AuthenticateState -> CtrlV
routeTaskWithUser taskId userId authState = do
  m <- getHttpMethod
  case m of
    DELETE ->
      callIfAuthorized authState (TaskController.removeUserFromTask taskId userId)
    PUT ->
      callIfAuthorized authState (TaskController.addUserToTask taskId userId)
    other -> notImplemented other

routeCalendarEntry :: EntryId -> AcidState AuthenticateState -> CtrlV
routeCalendarEntry entryId authState = do
  m <- getHttpMethod
  case m of
    DELETE ->
      callIfAuthorized authState (CalendarController.deleteCalendarEntry entryId)
    GET ->
      CalendarController.entryPage entryId
    POST -> do
      description <- look "description"
      TaskController.createTask entryId description
    PUT -> do
      description <- look "description"
      callIfAuthorized authState (CalendarController.updateCalendarEntry entryId description)

notImplemented :: Method -> CtrlV
notImplemented httpMethod = okResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")

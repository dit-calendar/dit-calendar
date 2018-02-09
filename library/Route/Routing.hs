module Route.Routing ( route ) where

import Happstack.Server          ( ServerPartT(..), Response, ok, Method(GET, POST, DELETE, PUT), nullDir
                                 , Request(rqMethod), askRq , BodyPolicy(..), unauthorized, getHeaderM
                                 , decodeBody, defaultBodyPolicy, look, mapServerPartT, toResponse )
import Web.Routes                  ( RouteT(..), nestURL, mapRouteT )
import Happstack.Authenticate.Core ( AuthenticateURL(..), AuthenticateConfig(..), AuthenticateState, decodeAndVerifyToken )
import Data.Acid                   ( AcidState )
import Control.Monad.IO.Class      ( liftIO )
import Data.Time                   ( getCurrentTime )
import Happstack.Foundation        ( lift, runReaderT, ReaderT, UnWebT )

import Data.Domain.Types           ( UserId, EntryId, TaskId )
import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV, App(..) )
import Controller.ControllerHelper ( okResponse )

import qualified Controller.UserController      as UserController
import qualified Controller.HomeController      as HomeController
import qualified Controller.CalendarController  as CalendarController
import qualified Controller.TaskController      as TaskController


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- | the route mapping function
route :: Sitemap -> CtrlV
route url =
    do  decodeBody myPolicy
        case url of
            Home                 -> HomeController.homePage
            Userdetail           -> routeDetailUser
            User i               -> routeUser i
            CalendarEntry i      -> routeCalendarEntry i
            Task i               -> routeTask i
            TaskWithCalendar e u -> routeTaskWithCalendar e u
            TaskWithUser t u     -> routeTaskWithUser t u

getHttpMethod = do
  nullDir
  g <- rqMethod <$> askRq
  ok g

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
      CalendarController.createCalendarEntry userId description
    PUT -> do
      name <- look "name"
      UserController.updateUser userId name

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/user
    POST -> do
      name <- look "name"
      UserController.createUser name
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

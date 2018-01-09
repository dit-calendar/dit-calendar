module Route.Routing ( route ) where

import Happstack.Server          ( ServerPartT(..), Response, ok, Method(GET, POST, DELETE, PUT), nullDir
                                 , Request(rqMethod), askRq , BodyPolicy(..)
                                 , decodeBody, defaultBodyPolicy, look, mapServerPartT )
import Web.Routes                  ( RouteT(..), nestURL, mapRouteT )
import Happstack.Server         ( ok, toResponse, unauthorized, getHeaderM )
import Happstack.Authenticate.Core ( AuthenticateURL(..), AuthenticateConfig(..), AuthenticateState, decodeAndVerifyToken )
import Data.Acid                   ( AcidState )
import Control.Monad.IO.Class      ( liftIO )
import Data.Time                   ( getCurrentTime )
import Happstack.Foundation        ( lift, runReaderT, ReaderT, UnWebT )

import Data.Domain.Types           ( UserId, EntryId, TaskId )
import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV, App(..), Acid(..) )
import Controller.ControllerHelper ( okResponse )

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Controller.UserController      as UserController
import qualified Controller.HomeController      as HomeController
import qualified Controller.CalendarController  as CalendarController
import qualified Controller.TaskController      as TaskController


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- | the route mapping function
route :: AcidState AuthenticateState
         -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
         -> Sitemap -> CtrlV
route authenticateState routeAuthenticate url =
  do  decodeBody myPolicy
      case url of
        Home                 -> HomeController.homePage
        Restricted ->  api authenticateState
        -- Authenticate authenticateURL -> (test authenticateURL routeAuthenticate)
        Authenticate authenticateURL -> mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
        Userdetail           -> routeDetailUser
        User i               -> routeUser i
        CalendarEntry i      -> routeCalendarEntry i
        Task i               -> routeTask i
        TaskWithCalendar e u -> routeTaskWithCalendar e u
        TaskWithUser t u     -> routeTaskWithUser t u

mapServerPartTIO2App :: (ServerPartT IO) Response -> App Response
mapServerPartTIO2App f = App{unApp = mapServerPartT mapIO2ReaderTAcid f}

mapIO2ReaderTAcid :: UnWebT IO a -> UnWebT (ReaderT Acid IO) a
mapIO2ReaderTAcid a = lift a

api :: AcidState AuthenticateState -> CtrlV
api authenticateState =
  do mAuth <- getHeaderM "Authorization"
     case mAuth of
       Nothing -> unauthorized $ toResponse "You are not authorized."
       (Just auth') ->
         do let auth = B.drop 7 auth'
            now <- liftIO getCurrentTime
            mToken <- decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)
            case mToken of
              Nothing -> unauthorized $ toResponse "You are not authorized."
              (Just (_, jwt)) -> ok $ toResponse $ "You are now authorized"

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
  -- curl -X POST -d "name=FooBar" http://localhost:8000/userdetail
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

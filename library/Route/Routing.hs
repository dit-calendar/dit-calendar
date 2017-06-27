module Route.Routing where

import Happstack.Server          ( ok, toResponse, Method(GET, POST, DELETE, PUT), nullDir
                                 , Request(rqMethod), askRq , BodyPolicy(..)
                                 , decodeBody, defaultBodyPolicy, look)

import Controller.AcidHelper     ( CtrlV )
import Controller.UserController      as UserController
import Controller.HomeController      as HomeController
import Controller.CalendarController  as CalendarController
import Controller.TaskController      as TaskController
import Route.PageEnum            ( SiteMap(..) )
import Data.Domain.Types         ( UserId, EntryId, TaskId )


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
  do  decodeBody myPolicy
      case url of
        Home                 -> HomeController.homePage
        Userdetail           -> routeDetailUser
        Taskdetail           -> routeDetailUser
        (User i)             -> routeUser i
        (CalendarEntry i)    -> routeCalendarEntry i
        (Task i)             -> routeTask i
        (TaskWithUser taskId userId) -> routeTaskWithUser taskId userId

getHttpMethod = do
  nullDir
  g <- greet
  ok g
    where
  greet =
    rqMethod <$> askRq

routeUser :: UserId -> CtrlV
routeUser userId = do
  m <- getHttpMethod
  case m of
    GET  ->
      UserController.userPage userId
    DELETE ->
      UserController.deleteUser userId
    POST -> do
      description <- look "description"
      CalendarController.createCalendarEntry userId description
    PUT -> do
      name <- look "name"
      UserController.updateUser userId name

routeTask :: TaskId -> CtrlV
routeTask taskId = do
  m <- getHttpMethod
  case m of
    GET  ->
      TaskController.taskPage taskId
    DELETE ->
      TaskController.deleteTask taskId
    PUT -> do
      description <- look "description"
      TaskController.updateTask taskId description

routeDetailTask :: CtrlV
routeDetailTask = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/taskdetail
    POST -> TaskController.taskPage 1

routeTaskWithUser :: TaskId -> UserId -> CtrlV
routeTaskWithUser taskId userId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      TaskController.removeUserFromTask taskId userId
    PUT ->
      TaskController.addUserToTask taskId userId

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/userdetail
    POST -> do
      name <- look "name"
      UserController.createUser name

routeCalendarEntry :: EntryId -> CtrlV
routeCalendarEntry entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      CalendarController.deleteCalendarEntry entryId
    GET  ->
      CalendarController.entryPage entryId
    POST -> do
      description <- look "description"
      TaskController.createTask entryId description

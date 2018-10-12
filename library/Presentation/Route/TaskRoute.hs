module Presentation.Route.TaskRoute (routeTask, routeTaskWithCalendar, routeTaskWithUser) where

import           Data.Text                              (pack)

import           Happstack.Foundation                   (lift)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         look)

import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         UserId)
import           Presentation.AcidHelper                (CtrlV)
import           Presentation.HttpServerHelper          (getHttpMethod)
import           Presentation.ResponseHelper            (notImplemented)

import qualified Presentation.Controller.TaskController as TaskController

routeTask :: TaskId -> CtrlV
routeTask taskId = do
  m <- getHttpMethod
  case m of
    GET  -> lift $ TaskController.taskPage taskId
    PUT -> do
      description <- look "description"
      lift $ callIfAuthorized (TaskController.updateTask taskId $ pack description)
    other -> lift $ notImplemented other

routeTaskWithCalendar :: TaskId -> EntryId -> CtrlV
routeTaskWithCalendar taskId entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      lift $ callIfAuthorized (TaskController.deleteTask entryId taskId)
    other -> lift $ notImplemented other

routeTaskWithUser :: TaskId -> UserId -> CtrlV
routeTaskWithUser taskId userId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      lift $ callIfAuthorized (TaskController.removeUserFromTask taskId userId)
    PUT ->
      lift $ callIfAuthorized (TaskController.addUserToTask taskId userId)
    other -> lift $ notImplemented other

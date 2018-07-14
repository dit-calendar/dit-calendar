module Presentation.Route.TaskRoute (routeTask, routeTaskWithCalendar, routeTaskWithUser) where

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
    GET  ->
      TaskController.taskPage taskId
    PUT -> do
      description <- look "description"
      callIfAuthorized (TaskController.updateTask taskId description)
    other -> notImplemented other

routeTaskWithCalendar :: TaskId -> EntryId -> CtrlV
routeTaskWithCalendar taskId entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      callIfAuthorized (TaskController.deleteTask entryId taskId)
    other -> notImplemented other

routeTaskWithUser :: TaskId -> UserId -> CtrlV
routeTaskWithUser taskId userId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      callIfAuthorized (TaskController.removeUserFromTask taskId userId)
    PUT ->
      callIfAuthorized (TaskController.addUserToTask taskId userId)
    other -> notImplemented other

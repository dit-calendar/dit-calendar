module Presentation.Route.TaskRoute (routeTask, routeTaskWithCalendar, routeTaskWithUser) where

import           Data.Acid                              (AcidState)
import           Happstack.Authenticate.Core            (AuthenticateState)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         look)

import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         UserId)
import           Presentation.AcidHelper                (CtrlV)
import           Presentation.HttpServerHelper          (getHttpMethod)
import           Presentation.ResponseHelper            (notImplemented)

import qualified Presentation.Controller.TaskController as TaskController

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

module Presentation.Route.TaskRoute
    ( routeTask
    , routeTaskWithCalendar
    , routeTaskWithUser
    ) where

import           Data.Text                              (pack)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response, look)

import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         UserId)
import           Presentation.AcidHelper                (App)
import           Presentation.HttpServerHelper          (getHttpMethod)
import           Presentation.ResponseHelper            (notImplemented)

import qualified Presentation.Controller.TaskController as TaskController

routeTask :: TaskId -> App Response
routeTask taskId = do
    m <- getHttpMethod
    case m of
        GET -> TaskController.taskPage taskId
        PUT -> do
            description <- look "description"
            callIfAuthorized (TaskController.updateTask taskId $ pack description)
        other -> notImplemented other

routeTaskWithCalendar :: TaskId -> EntryId -> App Response
routeTaskWithCalendar taskId entryId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (TaskController.deleteTask entryId taskId)
        other  -> notImplemented other

routeTaskWithUser :: TaskId -> UserId -> App Response
routeTaskWithUser taskId userId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (TaskController.removeUserFromTask taskId userId)
        PUT -> callIfAuthorized (TaskController.addUserToTask taskId userId)
        other -> notImplemented other

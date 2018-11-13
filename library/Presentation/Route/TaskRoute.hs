module Presentation.Route.TaskRoute
    ( routeTask
    , routeTaskWithCalendar
    , routeTaskWithUser
    ) where

import           Data.Aeson                             (eitherDecode)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response, look)

import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         UserId)
import           Presentation.AcidHelper                (App)
import           Presentation.HttpServerHelper          (getHttpMethod, getBody)
import           Presentation.ResponseHelper            (badRequest, notImplemented)
import           Presentation.Dto.Task                  as TaskDto (Task (..))

import qualified Presentation.Controller.TaskController as TaskController

routeTask :: TaskId -> App Response
routeTask taskId = do
    m <- getHttpMethod
    case m of
        GET -> TaskController.taskPage taskId
        PUT -> do
            body <- getBody
            case eitherDecode body :: Either String TaskDto.Task of
                  Right taskDto ->
                       callIfAuthorized (TaskController.updateTask taskId taskDto)
                  Left errorMessage -> badRequest errorMessage
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

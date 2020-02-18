module Presentation.Route.TaskRoute
    ( routeTask
    , routeTaskDetail
    , routeTaskWithTelegramLink
    ) where

import           Data.Aeson                             (eitherDecode)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response)

import           AppContext                             (App)
import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         TelegramChatId)
import           Presentation.Dto.Task                  as TaskDto (Task (..),
                                                                    validate)
import           Presentation.Dto.TelegramUserLink      as TelegramDto (TelegramUserLink (..),
                                                                        validate)
import           Server.HttpServerHelper                (getBody, getHttpMethod)
import           Server.ResponseBuilder                 (badRequest,
                                                         handleResponse,
                                                         notImplemented)

import qualified Presentation.Controller.TaskController as TaskController


routeTask :: EntryId -> App Response
routeTask entryId = do
    m <- getHttpMethod
    case m of
        POST -> do
            body <- getBody
            case TaskDto.validate (eitherDecode body :: Either String TaskDto.Task) of
                 Right taskDto -> TaskController.createTask entryId taskDto >>= handleResponse
                 Left errorMessage -> badRequest errorMessage
        GET -> callIfAuthorized (TaskController.calendarTasks entryId)
        other -> notImplemented other

routeTaskDetail :: EntryId ->  TaskId -> App Response
routeTaskDetail entryId taskId = do
    m <- getHttpMethod
    case m of
        GET -> TaskController.taskPage taskId >>= handleResponse
        PUT -> do
            body <- getBody
            case TaskDto.validate (eitherDecode body :: Either String TaskDto.Task) of
                  Right taskDto ->
                       callIfAuthorized (TaskController.updateTask taskId taskDto)
                  Left errorMessage -> badRequest errorMessage
        DELETE -> callIfAuthorized (TaskController.deleteTask entryId taskId)
        other  -> notImplemented other

routeTaskWithTelegramLink :: TaskId -> App Response
routeTaskWithTelegramLink taskId = do
    m <- getHttpMethod
    body <- getBody
    case TelegramDto.validate (eitherDecode body :: Either String TelegramDto.TelegramUserLink) of
          Right telegramDto ->
               case m of
                   DELETE -> callIfAuthorized (TaskController.removeTelegramLinkFromTask telegramDto taskId)
                   PUT    -> callIfAuthorized (TaskController.addTelegramLinkToTask taskId telegramDto)
                   other  -> notImplemented other
          Left errorMessage -> badRequest errorMessage

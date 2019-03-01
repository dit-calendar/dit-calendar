{-# LANGUAGE OverloadedStrings #-}

module Presentation.Controller.TaskController where

import           Data.Aeson                     (encode)
import           Data.Text                      as C (pack)
import           Happstack.Server               (Response)

import           AcidHelper                     (App)
import           Data.Domain.Task               as Task
import           Data.Domain.Types              (Description, EntryId, TaskId,
                                                 UserId)
import           Data.Domain.User               as DomainUser (User (..))
import           Presentation.Dto.Task          as TaskDto (Task (..))
import           Presentation.Mapper.TaskMapper (transformFromDto,
                                                 transformToDto)
import           Presentation.ResponseHelper    (EitherResponse, okResponse,
                                                 okResponseJson, onEntryExist,
                                                 onTaskExist, onUserExist,
                                                 preconditionFailedResponse)

import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Repository.TaskRepo       as TaskRepo
import qualified Data.Service.Task              as TaskService


--handler for taskPage
taskPage :: TaskId -> App Response
taskPage i = onTaskExist i (return . Left . transformToDto)

createTask :: EntryId -> TaskDto.Task -> App Response
createTask calendarId taskDto =
    onEntryExist calendarId
        (\e -> do
        result <- TaskService.createTaskInCalendar e (transformFromDto taskDto Nothing)
        return $ Left $ transformToDto result)

updateTask :: TaskId -> TaskDto.Task -> DomainUser.User -> App Response
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        result <- TaskService.updateTaskInCalendar $ transformFromDto taskDto (Just t)
        return $ either Right (Left . transformToDto) result)

addUserToTask :: TaskId -> DomainUser.User-> App Response
addUserToTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- TaskService.addUserToTask t loggedUser
        return $ either Right (Left . transformToDto) result)

removeUserFromTask :: TaskId -> DomainUser.User -> App Response
removeUserFromTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- TaskService.removeUserFromTask t loggedUser
        return $ either Right (Left . transformToDto) result)

deleteTask :: EntryId -> TaskId -> DomainUser.User -> App Response
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e -> do
        mTask <- TaskRepo.findTaskById taskId
        case mTask of
            Nothing -> return $ Right $ pack $ "Could not find a task with id " ++ show taskId
            Just task -> do
                CalendarRepo.deleteTaskFromCalendarEntry e taskId
                TaskService.deleteTaskAndCascadeUsersImpl task
                return $ Left (""::String))

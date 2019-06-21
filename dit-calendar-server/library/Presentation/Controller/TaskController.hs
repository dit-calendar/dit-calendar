{-# LANGUAGE OverloadedStrings #-}

module Presentation.Controller.TaskController where

import           Data.Aeson                     (encode)
import           Data.Text                      as C (pack)
import           Happstack.Server               (Response, notFound, toResponse)

import           AcidHelper                     (App)
import           Data.Domain.Task               as Task
import           Data.Domain.Types              (Description, EntryId, TaskId,
                                                 UserId)
import           Data.Domain.User               as DomainUser (User (..))
import           Presentation.Dto.Task          as TaskDto (Task (..))
import           Presentation.Mapper.BaseMapper (transformToDtoE)
import           Presentation.Mapper.TaskMapper (transformFromDto,
                                                 transformToDto)
import           Presentation.ResponseHelper    (onEntryExist, onTaskExist,
                                                 onUserExist)

import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Repository.TaskRepo       as TaskRepo
import qualified Data.Service.CalendarEntry     as CalendarService
import qualified Data.Service.Task              as TaskService


--handler for taskPage
taskPage :: TaskId -> App Response
taskPage i = onTaskExist i (return . Right . transformToDto)

calendarTasks :: EntryId -> DomainUser.User -> App Response
calendarTasks entryId user = onEntryExist entryId getTasks
    where
        getTasks cEntry = do
              result <- CalendarService.getCalendarTasks cEntry
              return $ Right (map transformToDto result)

createTask :: EntryId -> TaskDto.Task -> App Response
createTask calendarId taskDto =
    onEntryExist calendarId
        (\e -> do
        result <- TaskService.createTaskInCalendar e (transformFromDto taskDto Nothing)
        return $ Right $ transformToDto result)

updateTask :: TaskId -> TaskDto.Task -> DomainUser.User -> App Response
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        result <- TaskService.updateTaskInCalendar $ transformFromDto taskDto (Just t)
        return $ transformToDtoE result)

addUserToTask :: TaskId -> DomainUser.User-> App Response
addUserToTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- TaskService.addUserToTask t loggedUser
        return $ transformToDtoE result)

removeUserFromTask :: TaskId -> DomainUser.User -> App Response
removeUserFromTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- TaskService.removeUserFromTask t loggedUser
        return $ transformToDtoE result)

deleteTask :: EntryId -> TaskId -> DomainUser.User -> App Response
deleteTask entryId taskId loggedUser = do
    mEntry <- CalendarRepo.findCalendarById entryId
    case mEntry of
        Nothing ->  notFound $ toResponse $ "Could not find a calendar entry with id " ++ show entryId
        Just entry -> onTaskExist taskId (\t -> do
            TaskService.deleteTaskAndCascadeImpl entry t
            return $ Right ())

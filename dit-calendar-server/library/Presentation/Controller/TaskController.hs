module Presentation.Controller.TaskController where

import           AcidHelper                     (App)
import           Data.Domain.Types              (EitherResult, EntryId,
                                                 ResultError (EntryNotFound),
                                                 TaskId)
import           Data.Domain.User               as DomainUser (User (..))
import           Presentation.Dto.Task          as TaskDto (Task (..))
import           Presentation.Mapper.BaseMapper (transformToDtoE,
                                                 transformToDtoList)
import           Presentation.Mapper.TaskMapper (transformFromDto,
                                                 transformToDto)
import           Presentation.ResponseHelper    (onEntryExist, onTaskExist)

import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Service.CalendarEntry     as CalendarService
import qualified Data.Service.CalendarTasks     as CalendarTasks
import qualified Data.Service.Task              as TaskService
import qualified Data.Service.UserTasks         as UserTasksService


--handler for taskPage
taskPage :: TaskId -> App (EitherResult TaskDto.Task)
taskPage i = onTaskExist i (return . Right . transformToDto)

calendarTasks :: EntryId -> DomainUser.User -> App (EitherResult [TaskDto.Task])
calendarTasks entryId user = onEntryExist entryId getTasks
    where
        getTasks cEntry = do
              result <- CalendarTasks.getCalendarTasks cEntry
              return $ Right (transformToDtoList result)

createTask :: EntryId -> TaskDto.Task -> App (EitherResult TaskDto.Task)
createTask calendarId taskDto =
    onEntryExist calendarId
        (\e -> do
        result <- TaskService.createTaskInCalendar e (transformFromDto taskDto Nothing)
        return $ Right $ transformToDto result)

updateTask :: TaskId -> TaskDto.Task -> DomainUser.User -> App (EitherResult TaskDto.Task)
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        result <- TaskService.updateTaskInCalendar $ transformFromDto taskDto (Just t)
        return $ transformToDtoE result)

addUserToTask :: TaskId -> DomainUser.User-> App (EitherResult TaskDto.Task)
addUserToTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- UserTasksService.addUserToTask t loggedUser
        return $ transformToDtoE result)

removeUserFromTask :: TaskId -> DomainUser.User -> App (EitherResult TaskDto.Task)
removeUserFromTask taskId loggedUser =
    onTaskExist taskId (\t -> do
        result <- UserTasksService.removeUserFromTask t loggedUser
        return $ transformToDtoE result)

deleteTask :: EntryId -> TaskId -> DomainUser.User -> App (EitherResult ())
deleteTask entryId taskId loggedUser = do
    mEntry <- CalendarRepo.findCalendarById entryId
    case mEntry of
        Nothing ->  return $ Left (EntryNotFound entryId)
        Just entry -> onTaskExist taskId (\t -> do
            TaskService.deleteTaskAndCascade entry t
            return $ Right ())

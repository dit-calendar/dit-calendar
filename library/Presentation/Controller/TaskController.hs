module Presentation.Controller.TaskController where

import           Data.Aeson                   (encode)
import           Happstack.Server             (Response)

import           Data.Domain.Task             as Task
import           Data.Domain.Types            (Description, EntryId, TaskId,
                                               UserId)
import           Data.Domain.User             (User)
import           AcidHelper      (App)
import           Presentation.Dto.Task        as TaskDto (Task (..), transformToDto, transformFromDto)
import           Presentation.ResponseHelper  (okResponse, okResponseJson,
                                               onEntryExist, onTaskExist,
                                               onUserExist,
                                               preconditionFailedResponse)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Service.Task            as TaskService


--handler for taskPage
taskPage :: TaskId -> App Response
taskPage i = onTaskExist i (okResponseJson . encode . transformToDto)

createTask :: EntryId -> TaskDto.Task -> App Response
createTask calendarId taskDto =
    onEntryExist calendarId (\e -> do
        t <- TaskService.createTaskInCalendar e (transformFromDto taskDto Nothing)
        okResponseJson $ encode $ transformToDto t)

updateTask :: TaskId -> TaskDto.Task -> User -> App Response
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        result <- TaskService.updateTaskInCalendar $ transformFromDto taskDto (Just t)
        case result of
            Left errorMessage -> preconditionFailedResponse errorMessage
            Right updatedTask -> okResponseJson $ encode $ transformToDto updatedTask)

addUserToTask :: UserId -> TaskId -> User-> App Response
addUserToTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            result <- TaskService.addUserToTask t userId
            case result of
                Left errorMessage -> preconditionFailedResponse errorMessage
                Right updatedTask -> okResponseJson $ encode $ transformToDto updatedTask))

removeUserFromTask :: UserId -> TaskId -> User -> App Response
removeUserFromTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            result <- TaskService.removeUserFromTask t userId
            case result of
                Left errorMessage -> preconditionFailedResponse errorMessage
                Right updatedTask -> okResponseJson $ encode $ transformToDto updatedTask))

deleteTask :: EntryId -> TaskId -> User -> App Response
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e ->
        onTaskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskService.deleteTaskAndCascadeUsersImpl t
            okResponse $ "Task with id:" ++ show taskId ++ "deleted"))

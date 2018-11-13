module Presentation.Controller.TaskController where

import           Data.Aeson                   (encode)
import           Happstack.Server             (Response)

import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EntryId, TaskId, UserId, Description)
import           Data.Domain.User             (User)
import           Presentation.AcidHelper      (App)
import           Presentation.ResponseHelper  (onEntryExist, onTaskExist,
                                               onUserExist, okResponse, okResponseJson)
import           Presentation.Dto.Task        as TaskDto (Task (..), transform)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Service.Task            as TaskService


--handler for taskPage
taskPage :: TaskId -> App Response
taskPage i = onTaskExist i (okResponseJson . encode . transform)

createTask :: EntryId -> TaskDto.Task -> App Response
createTask calendarId taskDto =
    onEntryExist calendarId (\e -> do
        t <- TaskService.createTaskInCalendar e (TaskDto.description taskDto)
        okResponseJson $ encode $ transform t)

updateTask :: TaskId -> TaskDto.Task -> User -> App Response
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        TaskRepo.updateDescription t (TaskDto.description taskDto)
        okResponse $ "Task with id:" ++ show id ++ "updated")

addUserToTask :: UserId -> TaskId -> User-> App Response
addUserToTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.addUserToTask t userId
            okResponse $ "User added to task: " ++ show userId))

removeUserFromTask :: UserId -> TaskId -> User -> App Response
removeUserFromTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.removeUserFromTask t userId
            okResponse $ "User removed from task" ++ show userId))

deleteTask :: EntryId -> TaskId -> User -> App Response
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e ->
        onTaskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskService.deleteTaskAndCascadeUsersImpl t
            okResponse $ "Task with id:" ++ show taskId ++ "deleted"))

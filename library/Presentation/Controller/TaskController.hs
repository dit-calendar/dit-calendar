module Presentation.Controller.TaskController where

import           Happstack.Server             (Response, ok, toResponse)

import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EntryId, TaskId, UserId)
import           Data.Domain.User             (User)
import           Presentation.AcidHelper      (App)
import           Presentation.ResponseHelper  (onEntryExist, onTaskExist,
                                               onUserExist)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Service.Task            as TaskService


--handler for taskPage
taskPage :: TaskId -> App Response
taskPage i = onTaskExist i (\t -> ok $ toResponse $ "peeked at the task and saw: " ++ show t)

createTask :: EntryId -> String -> App Response
createTask calendarId description =
    onEntryExist calendarId (\e -> do
        t <- TaskService.createTaskInCalendar e description
        ok $ toResponse $ "Task created: " ++ show (Task.taskId t) ++ "to CalendarEntry: " ++ show calendarId)

updateTask :: TaskId -> String -> User -> App Response
updateTask id description loggedUser =
    onTaskExist id (\t -> do
        TaskRepo.updateDescription t description
        ok $ toResponse $ "Task with id:" ++ show id ++ "updated")

addUserToTask :: UserId -> TaskId -> User-> App Response
addUserToTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.addUserToTask t userId
            ok $ toResponse $ "User added to task: " ++ show userId))

removeUserFromTask :: UserId -> TaskId -> User -> App Response
removeUserFromTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.removeUserFromTask t userId
            ok $ toResponse $ "User removed from task" ++ show userId))

deleteTask :: EntryId -> TaskId -> User -> App Response
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e ->
        onTaskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskService.deleteTaskAndCascadeUsersImpl t
            ok $ toResponse $ "Task with id:" ++ show taskId ++ "deleted"))

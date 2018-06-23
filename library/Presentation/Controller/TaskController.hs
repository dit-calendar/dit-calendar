module Presentation.Controller.TaskController where

import           Happstack.Server             (ok, toResponse)

import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EntryId, TaskId, UserId)
import           Data.Domain.User             (User)
import           Presentation.AcidHelper      (CtrlV)
import           Presentation.ResponseHelper  (onEntryExist, onTaskExist,
                                               onUserExist)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Service.Task            as TaskService


--handler for taskPage
taskPage :: TaskId -> CtrlV
taskPage i = onTaskExist i (\t -> ok $ toResponse $ "peeked at the task and saw: " ++ show t)

createTask :: EntryId -> String -> CtrlV
createTask calendarId description =
    onEntryExist calendarId (\e -> do
        t <- TaskService.createTask e description
        ok $ toResponse $ "Task created: " ++ show (Task.taskId t) ++ "to CalendarEntry: " ++ show calendarId)

updateTask :: TaskId -> String -> User -> CtrlV
updateTask id description loggedUser =
    onTaskExist id (\t -> do
        TaskRepo.updateDescription t description
        ok $ toResponse $ "Task with id:" ++ show id ++ "updated")

addUserToTask :: UserId -> TaskId -> User-> CtrlV
addUserToTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.addUserToTask t userId
            ok $ toResponse $ "User added to task: " ++ show userId))

removeUserFromTask :: UserId -> TaskId -> User -> CtrlV
removeUserFromTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.removeUserFromTask t userId
            ok $ toResponse $ "User removed from task" ++ show userId))

deleteTask :: EntryId -> TaskId -> User -> CtrlV
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e ->
        onTaskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskService.deleteTask t
            ok $ toResponse $ "Task with id:" ++ show taskId ++ "deleted"))

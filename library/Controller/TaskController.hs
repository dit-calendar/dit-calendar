module Controller.TaskController where

import Happstack.Server         ( ok, toResponse )
import Happstack.Foundation     ( query )

import Data.Domain.Task                  as Task
import Data.Domain.Types           ( TaskId, EntryId, UserId )
import Controller.AcidHelper       ( CtrlV )
import Controller.ResponseHelper ( onUserExist, onEntryExist, onTaskExist )

import qualified Data.Repository.Acid.Task                as TaskAcid
import qualified Data.Repository.Acid.CalendarEntry       as CalendarEntryAcid
import qualified Data.Repository.Acid.User                as UserAcid
import qualified Data.Repository.TaskRepo                 as TaskRepo
import qualified Data.Repository.CalendarRepo             as CalendarRepo
import qualified Data.Service.Task                        as TaskService
import qualified Data.Domain.User                         as DomainUser


--handler for taskPage
taskPage :: TaskId -> CtrlV
taskPage i = onTaskExist i (\t -> ok $ toResponse $ "peeked at the task and saw: " ++ show t)

createTask :: EntryId -> String -> CtrlV
createTask calendarId description =
    onEntryExist calendarId (\e -> do
        t <- TaskService.createTask e description
        ok $ toResponse $ "Task created: " ++ show (Task.taskId t) ++ "to CalendarEntry: " ++ show calendarId)

updateTask :: TaskId -> String -> DomainUser.User -> CtrlV
updateTask id description loggedUser =
    onTaskExist id (\t -> do
        TaskRepo.updateDescription t description
        ok $ toResponse $ "Task with id:" ++ show id ++ "updated")

addUserToTask :: UserId -> TaskId -> DomainUser.User-> CtrlV
addUserToTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.addUserToTask t userId
            ok $ toResponse $ "User added to task: " ++ show userId))

removeUserFromTask :: UserId -> TaskId -> DomainUser.User -> CtrlV
removeUserFromTask userId taskId loggedUser =
    onUserExist userId (\ _ ->
        onTaskExist taskId (\t -> do
            TaskService.removeUserFromTask t userId
            ok $ toResponse $ "User removed from task" ++ show userId))

deleteTask :: EntryId -> TaskId -> DomainUser.User -> CtrlV
deleteTask entryId taskId loggedUser =
    onEntryExist entryId (\e ->
        onTaskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskService.deleteTask t
            ok $ toResponse $ "Task with id:" ++ show taskId ++ "deleted"))

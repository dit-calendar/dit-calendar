module Controller.TaskController where

import Happstack.Server         ( ok, toResponse )
import Happstack.Foundation     ( query )

import Data.Domain.Task                  as Task
import Data.Domain.Types           ( TaskId, EntryId, UserId )
import Controller.AcidHelper       ( CtrlV )
import Controller.ControllerHelper ( userExist, entryExist, taskExist )

import qualified Data.Repository.Acid.Task                as TaskAcid
import qualified Data.Repository.Acid.CalendarEntry       as CalendarEntryAcid
import qualified Data.Repository.Acid.User                as UserAcid
import qualified Data.Repository.TaskRepo                 as TaskRepo
import qualified Data.Repository.CalendarRepo             as CalendarRepo
import qualified Data.Repository.TaskRepoHelper           as TaskRepoHelper


--handler for taskPage
taskPage :: TaskId -> CtrlV
taskPage i = do
    mTask <- query (TaskAcid.TaskById i)
    taskExist i (\t -> ok $ toResponse $ "peeked at the task and saw: " ++ show t) mTask

createTask :: EntryId -> String -> CtrlV
createTask calendarId description = do
    mCalendarEntry <- query (CalendarEntryAcid.EntryById calendarId)
    entryExist calendarId (\e -> do
        t <- TaskRepoHelper.createTask e description
        ok $ toResponse $ "Task created: " ++ show (Task.taskId t) ++ "to CalendarEntry: " ++ show calendarId) mCalendarEntry

updateTask :: TaskId -> String -> CtrlV
updateTask id description = do
    mTask <- query (TaskAcid.TaskById id)
    taskExist id (\t -> do
        TaskRepo.updateDescription t description
        ok $ toResponse $ "Task with id:" ++ show id ++ "updated") mTask

addUserToTask :: UserId -> TaskId -> CtrlV
addUserToTask userId taskId = do
    mUser <- query (UserAcid.UserById userId)
    userExist userId (\ _ -> do
        mTask <- query (TaskAcid.TaskById taskId)
        taskExist taskId (\t -> do
            TaskRepoHelper.addUserToTask t userId
            ok $ toResponse $ "User added to task: " ++ show userId) mTask) mUser

removeUserFromTask :: UserId -> TaskId -> CtrlV
removeUserFromTask userId taskId = do
    mUser <- query (UserAcid.UserById userId)
    userExist userId (\ _ -> do
        mTask <- query (TaskAcid.TaskById taskId)
        taskExist taskId (\t -> do
            TaskRepoHelper.removeUserFromTask t userId
            ok $ toResponse $ "User removed from task" ++ show userId) mTask) mUser

deleteTask :: EntryId -> TaskId -> CtrlV
deleteTask entryId taskId = do
    mEntry <- query (CalendarEntryAcid.EntryById entryId)
    entryExist entryId (\e -> do
        mTask <- query (TaskAcid.TaskById taskId)
        taskExist taskId (\t -> do
            CalendarRepo.deleteTaskFromCalendarEntry e taskId
            TaskRepoHelper.deleteTask t
            ok $ toResponse $ "Task with id:" ++ show taskId ++ "deleted") mTask) mEntry

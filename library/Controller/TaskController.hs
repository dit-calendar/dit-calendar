module Controller.TaskController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Data.Domain.Task                  as Task      ( Task(..))
import Data.Domain.Types        ( TaskId, EntryId, UserId )
import Data.Domain.CalendarEntry         as CalendarEntry
import Data.Repository.Acid.TaskAcid     as TaskAcid
import Data.Repository.Acid.CalendarAcid as CalendarAcid
import Data.Repository.Acid.UserAcid     as UserAcid
import Data.Repository.TaskRepo          as TaskRepo
import Data.Repository.UserTaskRepo      as UserTaskRepo
import Data.Repository.CalendarRepo      as CalendarRepo
import Data.Repository.CalendarTaskRepo  as CalendarTaskRepo
import Controller.AcidHelper     ( CtrlV )

--handler for taskPage
taskPage :: TaskId -> CtrlV
taskPage i =
    do
       mTask <- query (TaskAcid.TaskById i)
       case mTask of
            Nothing ->
                ok $ toResponse $ "Could not find a task with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the task and saw: " ++ show u

createTask :: EntryId -> String -> CtrlV
createTask calendarId description =
    do
        mCalendarEntry <- query (CalendarAcid.EntryById calendarId)
        case mCalendarEntry of
            Nothing ->
                ok $ toResponse $ "Could not find a calendarEntry with id " ++ show calendarId
            (Just u) ->
                do
                    t <- CalendarTaskRepo.createTask u description
                    ok $ toResponse $ "Task created: " ++ show (Task.taskId t) ++ "to CalendarEntry: " ++ show calendarId


updateTask :: TaskId -> String -> CtrlV
updateTask id description =
    do
       mTask <- query (TaskAcid.TaskById id)
       case mTask of
            Nothing ->
                ok $ toResponse $ "Could not find a task with id " ++ show id
            (Just t) -> do
                 TaskRepo.updateTask t description
                 ok $ toResponse $ "Task with id:" ++ show id ++ "updated"

addUserToTask :: UserId -> TaskId -> CtrlV
addUserToTask userId taskId =
    do
       mUser <- query (UserAcid.UserById userId)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show userId
            (Just _) -> do
                 mTask <- query (TaskAcid.TaskById taskId)
                 case mTask of
                     Nothing ->
                        ok $ toResponse $ "Could not find a task with id " ++ show taskId
                     (Just t) -> do
                        UserTaskRepo.addUserToTask t userId
                        ok $ toResponse $ "User added to task: " ++ show userId

removeUserFromTask :: UserId -> TaskId -> CtrlV
removeUserFromTask userId taskId =
    do
       mUser <- query (UserAcid.UserById userId)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show userId
            (Just u) -> do
                 mTask <- query (TaskAcid.TaskById taskId)
                 case mTask of
                     Nothing ->
                        ok $ toResponse $ "Could not find a task with id " ++ show taskId
                     (Just t) -> do
                        UserTaskRepo.removeUserFromTask t userId
                        ok $ toResponse $ "User removed from task" ++ show userId

deleteTask :: EntryId -> TaskId -> CtrlV
deleteTask entryId taskId = do
    mEntry <- query (CalendarAcid.EntryById entryId)
    case mEntry of
        Nothing ->
            ok $ toResponse $ "Could not find a entry with id " ++ show entryId
        (Just e) -> do
            mTask <- query (TaskAcid.TaskById taskId)
            case mTask of
                Nothing ->
                    ok $ toResponse $ "Could not find a task with id " ++ show taskId
                (Just t) -> do
                    CalendarRepo.deleteTaskFromCalendarEntry taskId e
                    TaskRepo.deleteTask t
                    ok $ toResponse $ "Task with id:" ++ show taskId ++ "deleted"

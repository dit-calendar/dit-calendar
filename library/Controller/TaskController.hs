module Controller.TaskController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Data.Domain.Task              as Task      ( Task(..))
import Data.Domain.Types             ( TaskId, UserId )
import Data.Repository.Acid.UserAcid as UserAcid
import Data.Repository.Acid.TaskAcid as TaskAcid
import Data.Repository.UserRepo      as UserRepo
import Controller.AcidHelper    ( CtrlV )

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

createTask :: String -> CtrlV
createTask description =
    do
        mTask <- update (TaskAcid.NewTask description)
        ok $ toResponse $ "Task created: " ++ show mTask

updateTask :: TaskId -> String -> CtrlV
updateTask id description =
    do
       mTask <- query (TaskAcid.TaskById id)
       case mTask of
            Nothing ->
                ok $ toResponse $ "Could not find a task with id " ++ show id
            (Just u) -> do
                 update (TaskAcid.UpdateTask u)
                 ok $ toResponse $ "Task with id:" ++ show id ++ "updated"

addUserToTask :: UserId -> TaskId -> CtrlV
addUserToTask userId taskId =
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
                        UserRepo.addUserToTask t userId
                        ok $ toResponse $ "Task added to userId: " ++ show userId

deleteTask :: TaskId -> CtrlV
deleteTask i = do
    mTask <- query (TaskAcid.TaskById i)
    case mTask of
        Nothing ->
            ok $ toResponse $ "Could not find a task with id " ++ show i
        (Just u) -> do
            update (TaskAcid.DeleteTask i)
            ok $ toResponse $ "Task with id:" ++ show i ++ "deleted"

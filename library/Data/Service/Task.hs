{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.Task
    ( deleteTaskAndCascadeUsersImpl, createTaskInCalendarImpl, updateTaskInCalendarImpl, addUserToTaskImpl, removeUserFromTaskImpl, TaskService(..) ) where

import           Control.Monad.IO.Class
import           Data.Generics.Aliases        (orElse)
import           Data.List                    (delete)

import           AcidHelper                   (App)
import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (Description, UserId)
import           Data.Repository.Acid.Types   (UpdateReturn)

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as TaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo


deleteTaskAndCascadeUsersImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadIO m) =>
            Task -> m ()
deleteTaskAndCascadeUsersImpl task = do
    deleteTaskFromAllUsers task
    TaskRepo.deleteTask $ Task.taskId task

createTaskInCalendarImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> Description -> m Task
createTaskInCalendarImpl calendarEntry description = do
    mTask <- TaskRepo.createTask description
    MonadDBCalendarRepo.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

updateTaskInCalendarImpl :: (MonadDBTaskRepo m, MonadDBCalendarRepo m) => Task -> Task -> m (UpdateReturn Task)
updateTaskInCalendarImpl dbTask updateTask = TaskRepo.updateTask dbTask {
            Task.description = Task.description updateTask
            --, belongingUsers = belongingUsers dbTask
            , startTime     = startTime updateTask `orElse` startTime dbTask
            , endTime = endTime updateTask `orElse` endTime dbTask
        }

deleteTaskFromAllUsers :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserRepo.findUserById x
        MonadDBUserRepo.deleteTaskFromUser user x ))
    (return ()) $ Task.belongingUsers task

addUserToTaskImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> UserId -> m (UpdateReturn Task)
addUserToTaskImpl task userId = do
    user <- MonadDBUserRepo.findUserById userId
    MonadDBUserRepo.addTaskToUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTaskImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> UserId -> m (UpdateReturn Task)
removeUserFromTaskImpl task userId = do
    user <- MonadDBUserRepo.findUserById userId
    MonadDBUserRepo.deleteTaskFromUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = delete userId (belongingUsers task)}

class TaskService m where
    deleteTaskAndCascadeUsers :: Task -> m ()
    createTaskInCalendar :: CalendarEntry -> Description -> m Task
    updateTaskInCalendar :: Task -> Task -> m (UpdateReturn Task)
    addUserToTask :: Task -> UserId -> m (UpdateReturn Task)
    removeUserFromTask :: Task -> UserId -> m (UpdateReturn Task)

instance (MonadDBTaskRepo App, MonadDBUserRepo App, MonadDBCalendarRepo App)
            => TaskService App where
    deleteTaskAndCascadeUsers = deleteTaskAndCascadeUsersImpl
    createTaskInCalendar = createTaskInCalendarImpl
    updateTaskInCalendar = updateTaskInCalendarImpl
    addUserToTask = addUserToTaskImpl
    removeUserFromTask = removeUserFromTaskImpl

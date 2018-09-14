{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.Task
    ( deleteTaskAndCascadeUsersImpl, createTaskInCalendarImpl, addUserToTaskImpl, removeUserFromTaskImpl, TaskService(..) ) where

import           Control.Monad.IO.Class
import           Data.List                    (delete)

import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (UserId)
import           Presentation.AcidHelper      (App)

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as MonadDBTaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo


deleteTaskAndCascadeUsersImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadIO m) =>
            Task -> m ()
deleteTaskAndCascadeUsersImpl task = do
    deleteTaskFromAllUsers task
    MonadDBTaskRepo.deleteTask task

createTaskInCalendarImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> String -> m Task
createTaskInCalendarImpl calendarEntry description = do
    mTask <- MonadDBTaskRepo.createTask description
    MonadDBCalendarRepo.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

deleteTaskFromAllUsers :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserRepo.getUser x
        MonadDBUserRepo.deleteTaskFromUser user x ))
    (return ()) $ Task.belongingUsers task

addUserToTaskImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> UserId -> m ()
addUserToTaskImpl task userId = do
    user <- MonadDBUserRepo.getUser userId
    MonadDBUserRepo.addTaskToUser user (taskId task)
    MonadDBTaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTaskImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> UserId -> m ()
removeUserFromTaskImpl task userId = do
    user <- MonadDBUserRepo.getUser userId
    MonadDBUserRepo.deleteTaskFromUser user (taskId task)
    MonadDBTaskRepo.updateTask task {belongingUsers = delete userId (belongingUsers task)}

class TaskService m where
    deleteTaskAndCascadeUsers :: Task -> m ()
    createTaskInCalendar :: CalendarEntry -> String -> m Task
    addUserToTask :: Task -> UserId -> m ()
    removeUserFromTask :: Task -> UserId -> m ()

instance (MonadDBTaskRepo App, MonadDBUserRepo App, MonadDBCalendarRepo App)
            => TaskService App where
    deleteTaskAndCascadeUsers = deleteTaskAndCascadeUsersImpl
    createTaskInCalendar = createTaskInCalendarImpl
    addUserToTask = addUserToTaskImpl
    removeUserFromTask = removeUserFromTaskImpl

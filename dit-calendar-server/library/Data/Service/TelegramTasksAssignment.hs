{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.TelegramTasksAssignment (deleteTaskFromAllUsersImpl, addUserToTaskImpl, removeUserFromTaskImpl, removeUserFromTasksImpl, UserTasksService(..) ) where

import           Control.Monad.IO.Class
import           Data.Generics.Aliases        (orElse)
import           Data.List                    (delete)
import           Data.Maybe                   (fromJust)

import           AppContext                   (App)
import           Data.Domain.Task             as Task
import           Data.Domain.Types            ( EitherResult)
import           Data.Domain.TelegramLink             as TelegramLink

import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as TaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo



deleteTaskFromAllUsersImpl :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsersImpl task = undefined
--    foldr (\ x ->
--      (>>) (do
--        user <- MonadDBUserRepo.findUserById x
--        MonadDBUserRepo.deleteTaskFromUser (fromJust user) task ))
--    (return ()) $ Task.assignedUsers task

addUserToTaskImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> TelegramLink -> m (EitherResult Task)
addUserToTaskImpl task user =
    if taskId task `elem` assignedUsers task
        then return (Right task) -- do nothing and return same task
        elseUserToTas do
            MonadDBUserRepo.addTaskToUser user (taskId task)
            TaskRepo.updateTask task {assignedUsers = User.userId user : assignedUsers task}

removeUserFromTaskImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> TelegramLink -> m (EitherResult Task)
removeUserFromTaskImpl task user = do
    MonadDBUserRepo.deleteTaskFromUser user task
    TaskRepo.updateTask task {assignedUsers = delete (User.userId user) (assignedUsers task)}

class Monad m => TelegramTasksAssignmentService m where
    deleteTaskFromAllUsers :: Task -> m ()
    addUserToTask :: Task -> TelegramLink -> m (EitherResult Task)
    removeUserFromTask :: Task -> TelegramLink -> m (EitherResult Task)

instance (MonadDBTaskRepo App, MonadDBUserRepo App)
            => TelegramTasksAssignmentService App where
    deleteTaskFromAllUsers = deleteTaskFromAllUsersImpl
    addUserToTask = addUserToTaskImpl
    removeUserFromTask = removeUserFromTaskImpl
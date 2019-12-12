{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.UserTasks (deleteTaskFromAllUsersImpl, addUserToTaskImpl, removeUserFromTaskImpl, removeUserFromTasksImpl, UserTasksService(..) ) where

import           Control.Monad.IO.Class
import           Data.Generics.Aliases        (orElse)
import           Data.List                    (delete)
import           Data.Maybe                   (fromJust)

import           AppContext                   (App)
import           Data.Domain.Task             as Task
import           Data.Domain.Types            ( EitherResult)
import           Data.Domain.User             as User

import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as TaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo



deleteTaskFromAllUsersImpl :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsersImpl task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserRepo.findUserById x
        MonadDBUserRepo.deleteTaskFromUser (fromJust user) task ))
    (return ()) $ Task.assignedUsers task

addUserToTaskImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> User -> m (EitherResult Task)
addUserToTaskImpl task user =
    if taskId task `elem` assignedUsers task
        then return (Right task) -- do nothing and return same task
        else do
            MonadDBUserRepo.addTaskToUser user (taskId task)
            TaskRepo.updateTask task {assignedUsers = User.userId user : assignedUsers task}

removeUserFromTaskImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> User -> m (EitherResult Task)
removeUserFromTaskImpl task user = do
    MonadDBUserRepo.deleteTaskFromUser user task
    TaskRepo.updateTask task {assignedUsers = delete (User.userId user) (assignedUsers task)}


removeUserFromTasksImpl ::(MonadDBTaskRepo m, MonadDBUserRepo m) => User -> m ()
removeUserFromTasksImpl user = foldr (\ taskId ->
    (>>) (do
        task <- TaskRepo.findTaskById taskId
        removeUserFromTaskImpl (fromJust task) user))
    (return ()) (assignedToTasks user)


class Monad m => UserTasksService m where
    deleteTaskFromAllUsers :: Task -> m ()
    addUserToTask :: Task -> User -> m (EitherResult Task)
    removeUserFromTask :: Task -> User -> m (EitherResult Task)
    removeUserFromTasks :: User -> m ()

instance (MonadDBTaskRepo App, MonadDBUserRepo App)
            => UserTasksService App where
    deleteTaskFromAllUsers = deleteTaskFromAllUsersImpl
    addUserToTask = addUserToTaskImpl
    removeUserFromTask = removeUserFromTaskImpl
    removeUserFromTasks = removeUserFromTasksImpl
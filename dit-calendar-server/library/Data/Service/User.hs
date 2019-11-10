{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.User ( deleteUserImpl, UserService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)

import           Data.Domain.User             as User
import           AcidHelper      (App)

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as MonadDBTaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo
import           Data.Service.Task            (TaskService)
import qualified Data.Service.Task            as TaskService


deleteUserImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, TaskService m) =>
            User -> m ()
deleteUserImpl user = let calendarToDelete = ownerOfCalendarEntries user in
    do
        foldr ((>>) . MonadDBCalendarRepo.deleteCalendarEntry)
            (return ()) (ownerOfCalendarEntries user)
        removeUserFromTasks user
        MonadDBUserRepo.deleteUser $ User.userId user

removeUserFromTasks ::(MonadDBTaskRepo m, TaskService m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- MonadDBTaskRepo.findTaskById taskId
        TaskService.removeUserFromTask (fromJust task) user))
    (return ()) (ownerOfTasks user)

class Monad m => UserService m where
    deleteUser :: User -> m ()

instance (MonadDBUserRepo App, MonadDBTaskRepo App, MonadDBCalendarRepo App, TaskService App)
            => UserService App where
    deleteUser = deleteUserImpl

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.User ( deleteUserImpl, UserService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)

import           AcidHelper                   (App)
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo
import           Data.Service.UserTasks       (UserTasksService)
import qualified Data.Service.UserTasks       as UserTasksService


deleteUserImpl :: (MonadDBUserRepo m, MonadDBCalendarRepo m, UserTasksService m) =>
            User -> m ()
deleteUserImpl user = do
    UserTasksService.removeUserFromTasks user
    foldr ((>>) . MonadDBCalendarRepo.deleteCalendarEntryById) -- TODO delete all Tasks of Calendar?
        (return ()) (ownerOfCalendarEntries user)
    MonadDBUserRepo.deleteUser user

class Monad m => UserService m where
    deleteUser :: User -> m ()

instance (MonadDBUserRepo App, MonadDBCalendarRepo App, UserTasksService App)
            => UserService App where
    deleteUser = deleteUserImpl

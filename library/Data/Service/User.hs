module Data.Service.User ( deleteUser ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.User                     as User

import qualified Data.Repository.UserRepo            as MonadDBUserRepo
import Data.Repository.UserRepo                      ( MonadDBUserRepo )
import qualified Data.Repository.TaskRepo            as MonadDBTaskRepo
import Data.Repository.TaskRepo                      ( MonadDBTaskRepo )
import qualified Data.Repository.CalendarRepo        as MonadDBCalendarRepo
import Data.Repository.CalendarRepo                  ( MonadDBCalendarRepo )
import qualified Data.Service.Task                    as TaskService


deleteUser :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m) =>
            User -> m ()
deleteUser user = let calendarToDelete = calendarEntries user in
    do
        foldr ((>>) . MonadDBCalendarRepo.deleteCalendarEntry)
            (return ()) (calendarEntries user)
        removeUserFromTasks user
        MonadDBUserRepo.deleteUser user

removeUserFromTasks ::(MonadDBTaskRepo m, MonadDBUserRepo m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- MonadDBTaskRepo.getTask taskId
        TaskService.removeUserFromTask task (userId user)))
    (return ()) (belongingTasks user)
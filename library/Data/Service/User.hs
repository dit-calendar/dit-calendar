module Data.Service.User ( deleteUser ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.User                     as User

import qualified Data.Service.MonadDB.User            as MonadDBUserService
import Data.Service.MonadDB.User                      ( MonadDBUserService )
import qualified Data.Service.MonadDB.Task            as MonadDBTaskService
import Data.Service.MonadDB.Task                      ( MonadDBTaskService )
import qualified Data.Service.MonadDB.Calendar        as MonadDBCalendarService
import Data.Service.MonadDB.Calendar                  ( MonadDBCalendarService )
import qualified Data.Service.Task                    as TaskService


deleteUser :: (MonadDBUserService m, MonadDBTaskService m, MonadDBCalendarService m) =>
            User -> m ()
deleteUser user = let calendarToDelete = calendarEntries user in
    do
        foldr ((>>) . MonadDBCalendarService.deleteCalendarEntry)
            (return ()) (calendarEntries user)
        removeUserFromTasks user
        MonadDBUserService.deleteUser user

removeUserFromTasks ::(MonadDBTaskService m, MonadDBUserService m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- MonadDBTaskService.getTask taskId
        TaskService.removeUserFromTask task (userId user)))
    (return ()) (belongingTasks user)
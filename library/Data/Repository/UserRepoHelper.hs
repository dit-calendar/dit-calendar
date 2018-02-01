module Data.Repository.UserRepoHelper ( deleteUser ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.User                     as User

import qualified Data.Repository.MonadDB.UserRepo     as UserRepo
import qualified Data.Repository.MonadDB.TaskRepo     as TaskRepo
import qualified Data.Repository.MonadDB.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepoHelper       as TaskRepoHelper


deleteUser :: (UserRepo.MonadDBUserHelper m, TaskRepo.MonadDBTaskRepo m, CalendarRepo.MonadDBCalendarRepo m) =>
            User -> m ()
deleteUser user = let calendarToDelete = calendarEntries user in
    do
        foldr ((>>) . CalendarRepo.deleteCalendar)
            (return ()) (calendarEntries user)
        removeUserFromTasks user
        UserRepo.deleteUser user

removeUserFromTasks ::(TaskRepo.MonadDBTaskRepo m, UserRepo.MonadDBUserHelper m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- TaskRepo.getTask taskId
        TaskRepoHelper.removeUserFromTask task (userId user)))
    (return ()) (belongingTasks user)
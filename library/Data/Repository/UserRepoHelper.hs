module Data.Repository.UserRepoHelper ( deleteUser ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.User                     as User
import Data.Domain.Types                    ( TaskId )
import Data.Repository.MonadDB.Calendar     ( MonadDBCalendar )
import Data.Repository.MonadDB.Task         ( MonadDBTask )
import Data.Repository.MonadDB.User         ( MonadDBUser )

import qualified Data.Repository.MonadDB.UserRepo     as UserRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.TaskRepoHelper       as TaskRepoHelper


deleteUser :: (UserRepo.MonadDBUserHelper m, MonadDBUser m, MonadDBTask m, MonadDBCalendar m, MonadIO m) =>
            User -> m ()
deleteUser user = let calendarToDelete = calendarEntries user in
    do
        foldr ((>>) . CalendarRepo.deleteCalendar)
            (return ()) (calendarEntries user)
        removeUserFromTasks user
        UserRepo.deleteUser user

removeUserFromTasks ::(MonadDBUser m, MonadDBTask m, MonadIO m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- TaskRepo.getTask taskId
        TaskRepoHelper.removeUserFromTask task (userId user)))
    (return ()) (belongingTasks user)
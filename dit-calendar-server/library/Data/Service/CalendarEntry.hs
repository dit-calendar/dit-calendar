{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarEntry ( createEntryImpl, removeCalendarImpl, CalendarEntryService(..), getCalendarTasksIml ) where

import           Control.Monad.IO.Class
import           Data.Maybe                         (fromJust)

import           AppContext                         (App)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.Task                   as Task
import           Data.Domain.Types                  (Description, TaskId)
import           Data.Domain.User                   as User
import           Data.Repository.Acid.CalendarEntry (CalendarDAO)
import           Data.Repository.Acid.Task          (TaskDAO)
import           Data.Repository.Acid.User          (UserDAO)
import           Data.Repository.CalendarRepo       (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo       as MonadDBCalendarRepo
import           Data.Repository.TaskRepo           (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo           as MonadDBTaskRepo
import           Data.Repository.UserRepo           (MonadDBUserRepo)
import qualified Data.Repository.UserRepo           as MonadDBUserRepo


createEntryImpl :: (MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> User -> m CalendarEntry
createEntryImpl newCalendar user = do
    calendarEntry <- MonadDBCalendarRepo.createCalendarEntry newCalendarWithUser
    MonadDBUserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry
    where newCalendarWithUser = newCalendar {CalendarEntry.owner = User.userId user }


removeCalendarImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendarImpl calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- MonadDBUserRepo.findUserById (CalendarEntry.owner calendarEntry)
       MonadDBUserRepo.deleteCalendarEntryFromUser (fromJust user) cEntryId
       deleteCalendarsTasks calendarEntry
       MonadDBCalendarRepo.deleteCalendarEntry calendarEntry

deleteCalendarsTasks :: (MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m)
                => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.findTaskById x
        MonadDBTaskRepo.deleteTask (fromJust task) ))
    (return ()) $ CalendarEntry.tasks calendar

-- https://en.wikibooks.org/wiki/Haskell/do_notation#The_fail_method
getTaskWithFail :: (MonadDBTaskRepo m) => TaskId -> m Task
getTaskWithFail taskId = do
    Just task <- MonadDBTaskRepo.findTaskById taskId
    return task

getCalendarTasksIml :: (MonadDBTaskRepo m, MonadIO m)
                                    => CalendarEntry -> m [Task]
getCalendarTasksIml calendar = mapM getTaskWithFail (CalendarEntry.tasks calendar)

class Monad m => CalendarEntryService m where
    createEntry :: CalendarEntry -> User -> m CalendarEntry
    removeCalendar :: CalendarEntry -> m ()
    getCalendarTasks :: CalendarEntry -> m [Task]

instance (MonadDBUserRepo App, MonadDBTaskRepo App, MonadDBCalendarRepo App)
            => CalendarEntryService App where
    createEntry = createEntryImpl
    removeCalendar = removeCalendarImpl
    getCalendarTasks = getCalendarTasksIml

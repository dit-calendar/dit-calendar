{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarEntry ( createEntryImpl, removeCalendarImpl, CalendarEntryService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                         (fromJust)

import           AcidHelper                         (App)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.Task                   as Task
import           Data.Domain.Types                  (Description)
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
    where newCalendarWithUser = newCalendar {CalendarEntry.userId = User.userId user }


removeCalendarImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendarImpl calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- MonadDBUserRepo.findUserById (CalendarEntry.userId calendarEntry)
       MonadDBUserRepo.deleteCalendarEntryFromUser (fromJust user) cEntryId
       deleteCalendarsTasks calendarEntry
       MonadDBCalendarRepo.deleteCalendarEntry cEntryId

deleteCalendarsTasks :: (MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m)
                => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.findTaskById x
        MonadDBTaskRepo.deleteTask $ Task.taskId (fromJust task) ))
    (return ()) $ CalendarEntry.tasks calendar

class CalendarEntryService m where
    createEntry :: CalendarEntry -> User -> m CalendarEntry
    removeCalendar :: CalendarEntry -> m ()

instance (MonadDBUserRepo App, MonadDBTaskRepo App, MonadDBCalendarRepo App)
            => CalendarEntryService App where
    createEntry = createEntryImpl
    removeCalendar = removeCalendarImpl

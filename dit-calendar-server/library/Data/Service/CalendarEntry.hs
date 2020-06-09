{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarEntry ( createEntryImpl, removeCalendarImpl, CalendarEntryService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)

import           AppContext                   (App)
import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.User             as User
import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo
import           Data.Service.CalendarTasks   (CalendarTasksService)
import qualified Data.Service.CalendarTasks   as CalendarTasks


createEntryImpl :: (MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> User -> m CalendarEntry
createEntryImpl newCalendar user = do
    calendarEntry <- MonadDBCalendarRepo.createCalendarEntry newCalendarWithUser
    MonadDBUserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry
    where newCalendarWithUser = newCalendar {CalendarEntry.owner = User.userId user }


removeCalendarImpl :: (MonadDBUserRepo m, CalendarTasksService m, MonadDBCalendarRepo m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendarImpl calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- MonadDBUserRepo.findUserById (CalendarEntry.owner calendarEntry)
       MonadDBUserRepo.deleteCalendarEntryFromUser (fromJust user) cEntryId
       CalendarTasks.deleteCalendarsTasks calendarEntry
       MonadDBCalendarRepo.deleteCalendarEntry calendarEntry

class Monad m => CalendarEntryService m where
    createEntry :: CalendarEntry -> User -> m CalendarEntry
    removeCalendar :: CalendarEntry -> m ()

instance (MonadDBUserRepo App, MonadDBCalendarRepo App)
            => CalendarEntryService App where
    createEntry = createEntryImpl
    removeCalendar = removeCalendarImpl

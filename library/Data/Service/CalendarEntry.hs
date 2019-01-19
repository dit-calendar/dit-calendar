{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarEntry ( createEntryImpl, removeCalendarImpl, CalendarEntryService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                         (fromJust)

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
import           Presentation.AcidHelper            (App)
import qualified Presentation.Dto.CalendarEntry     as CalendarDto


createEntryImpl :: (MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarDto.CalendarEntry -> User -> m CalendarEntry
createEntryImpl calendarDto user =
    let newDate = CalendarDto.date calendarDto in
    let description = fromJust (CalendarDto.description calendarDto) in
        do
            calendarEntry <- MonadDBCalendarRepo.createCalendarEntry newDate description user
            MonadDBUserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
            return calendarEntry

removeCalendarImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendarImpl calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- MonadDBUserRepo.findUserById (CalendarEntry.userId calendarEntry)
       MonadDBUserRepo.deleteCalendarEntryFromUser user cEntryId
       deleteCalendarsTasks calendarEntry
       MonadDBCalendarRepo.deleteCalendarEntry cEntryId

deleteCalendarsTasks :: (MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m)
                => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.findTaskById x
        MonadDBTaskRepo.deleteTask $ Task.taskId task ))
    (return ()) $ CalendarEntry.tasks calendar

class CalendarEntryService m where
    createEntry :: CalendarDto.CalendarEntry -> User -> m CalendarEntry
    removeCalendar :: CalendarEntry -> m ()

instance (MonadDBUserRepo App, MonadDBTaskRepo App, MonadDBCalendarRepo App)
            => CalendarEntryService App where
    createEntry = createEntryImpl
    removeCalendar = removeCalendarImpl

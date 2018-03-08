module Data.Service.CalendarEntry ( createEntry, removeCalendar ) where

import Control.Monad.IO.Class

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Repository.Acid.CalendarEntry         ( MonadDBCalendar )
import Data.Repository.Acid.Task                  ( MonadDBTask )
import Data.Repository.Acid.User                  ( MonadDBUser )

import qualified Data.Repository.UserRepo            as MonadDBUserRepo
import Data.Repository.UserRepo                      ( MonadDBUserRepo )
import qualified Data.Repository.TaskRepo            as MonadDBTaskRepo
import Data.Repository.TaskRepo                      ( MonadDBTaskRepo )
import qualified Data.Repository.CalendarRepo        as MonadDBCalendarRepo
import Data.Repository.CalendarRepo                  ( MonadDBCalendarRepo )


createEntry :: (MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            String -> String -> User -> m CalendarEntry
createEntry newDate description user = do
    calendarEntry <- MonadDBCalendarRepo.newCalendarEntry newDate description user
    MonadDBUserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry

removeCalendar :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- MonadDBUserRepo.getUser (CalendarEntry.userId calendarEntry)
       MonadDBUserRepo.deleteCalendarEntryFromUser user cEntryId
       deleteCalendarsTasks calendarEntry
       MonadDBCalendarRepo.deleteCalendarEntry cEntryId

deleteCalendarsTasks :: (MonadDBTaskRepo m, MonadDBCalendarRepo m, MonadIO m)
                => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.getTask x
        MonadDBTaskRepo.deleteTask task ))
    (return ()) $ CalendarEntry.tasks calendar

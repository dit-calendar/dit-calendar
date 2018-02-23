module Data.Service.CalendarEntry ( createEntry, removeCalendar ) where

import Control.Monad.IO.Class

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Repository.Acid.CalendarEntry         ( MonadDBCalendar )
import Data.Repository.Acid.Task                  ( MonadDBTask )
import Data.Repository.Acid.User                  ( MonadDBUser )

import qualified Data.Repository.MonadDB.User            as MonadDBUserRepo
import Data.Repository.MonadDB.User                      ( MonadDBUserRepo )
import qualified Data.Repository.MonadDB.Task            as MonadDBTaskRepo
import Data.Repository.MonadDB.Task                      ( MonadDBTaskRepo )
import qualified Data.Repository.MonadDB.Calendar        as MonadDBCalendarRepo
import Data.Repository.MonadDB.Calendar                  ( MonadDBCalendarRepo )


createEntry :: (MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            String -> User -> m CalendarEntry
createEntry description user = do
    calendarEntry <- MonadDBCalendarRepo.newCalendarEntry description user
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

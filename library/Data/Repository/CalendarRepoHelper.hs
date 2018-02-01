module Data.Repository.CalendarRepoHelper ( createEntry, removeCalendar ) where

import Control.Monad.IO.Class

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Repository.CalendarRepo      ( MonadDBCalendar )
import Data.Repository.TaskRepo          ( MonadDBTask )
import Data.Repository.UserRepo          ( MonadDBUser )

import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.UserRepo             as UserRepo


createEntry :: (MonadDBUser m, MonadDBCalendar m) =>
            String -> User -> m CalendarEntry
createEntry description user = do
    calendarEntry <- CalendarRepo.createEntry description user
    UserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry

removeCalendar :: (MonadDBUser m, MonadDBTask m, MonadDBCalendar m, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- UserRepo.getUser (CalendarEntry.userId calendarEntry)
       UserRepo.deleteCalendarEntryFromUser user cEntryId
       deleteCalendarsTasks calendarEntry
       CalendarRepo.deleteCalendar cEntryId

deleteCalendarsTasks :: (MonadDBTask m, MonadDBCalendar m, MonadIO m)
                => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- TaskRepo.getTask x
        TaskRepo.deleteTask task ))
    (return ()) $ CalendarEntry.tasks calendar

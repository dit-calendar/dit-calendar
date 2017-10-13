{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.CalendarRepoHelper ( createEntry, removeCalendar ) where

import Happstack.Foundation     ( HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Repository.MonadDB.Calendar  ( MonadDBCalendar )
import Data.Repository.MonadDB.Task      ( MonadDBTask )
import Data.Repository.MonadDB.User      ( MonadDBUser )

import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid
import qualified Data.Repository.Acid.TaskAcid        as TaskAcid

createEntry :: (MonadDBUser m, MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList,
    HasAcidState m UserAcid.UserList) =>
            String -> User -> m CalendarEntry
createEntry description user = do
    calendarEntry <- CalendarRepo.createEntry description user
    UserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry

removeCalendar :: (MonadDBUser m, MonadDBTask m, MonadDBCalendar m,
    HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList,
    HasAcidState m TaskAcid.TaskList, MonadIO m) =>
                CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- UserRepo.getUser (CalendarEntry.userId calendarEntry)
       UserRepo.deleteCalendarEntryFromUser user cEntryId
       deleteCalendarsTasks calendarEntry
       CalendarRepo.deleteCalendar [cEntryId]

deleteCalendarsTasks :: (MonadDBTask m, MonadDBCalendar m, HasAcidState m TaskAcid.TaskList, MonadIO m)
    => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- TaskRepo.getTask x
        TaskRepo.deleteTask task ))
    (return ()) $ CalendarEntry.tasks calendar

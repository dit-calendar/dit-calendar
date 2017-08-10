{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.CalendarRepoHelper ( createEntry, removeCalendar ) where

import Happstack.Foundation     ( HasAcidState )
import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class

import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid
import qualified Data.Repository.Acid.TaskAcid        as TaskAcid

createEntry :: (HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList,
            MonadIO m) => String -> User -> m CalendarEntry
createEntry description user = do
    calendarEntry <- CalendarRepo.createEntry description user
    UserRepo.addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry

removeCalendar :: (HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList,
      HasAcidState m TaskAcid.TaskList, MonadIO m) => CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       user <- UserRepo.getUser (CalendarEntry.userId calendarEntry)
       UserRepo.deleteCalendarEntryFromUser user cEntryId
       deleteCalendarsTasks calendarEntry
       CalendarRepo.deleteCalendar [cEntryId]

deleteCalendarsTasks :: (HasAcidState m TaskAcid.TaskList, MonadIO m)
    => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        task <- TaskRepo.getTask x
        TaskRepo.deleteTask task ))
    (return ()) $ CalendarEntry.calendarTasks calendar
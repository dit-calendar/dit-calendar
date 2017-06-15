{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo where

import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.User                     as User
import Data.Repository.Acid.UserAcid        as UserAcid
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.Types             ( UserId, EntryId )
import Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: (HasAcidState m EntryList, HasAcidState m UserAcid.UserList,
            MonadIO m) => String -> User -> m ()
createEntry description user =
    do
        calendarEntry <- update (CalendarAcid.NewEntry description $ User.userId user)
        addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry

addCalendarEntryToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    let updatedUser = user {calendarEntrys = calendarEntrys user ++ [entryId]} in
        update $ UserAcid.UpdateUser updatedUser

deleteCalendar :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                   [EntryId] -> m ()
deleteCalendar = foldr (\ x -> (>>) (update $ CalendarAcid.DeleteEntry x))
        (return ())

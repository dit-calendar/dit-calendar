{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.UserRepoHelper ( deleteUser ) where

import Happstack.Foundation     ( query, HasAcidState )
import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.User                     as User
import Data.Domain.Task                     as Task
import Data.Domain.Types                    ( TaskId )

import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.TaskAcid        as TaskAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

import qualified Data.Repository.TaskRepoHelper       as TaskRepoHelper


deleteUser :: (MonadIO m, HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList, HasAcidState m TaskAcid.TaskList) =>
     User -> m ()
deleteUser user =
     let calendarToDelete = calendarEntries user in
        do
            CalendarRepo.deleteCalendar (calendarEntries user)
            removeUserFromTasks user
            UserRepo.deleteUser user

removeUserFromTasks ::(HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
       (>>) (do
            mTask <- query (TaskAcid.TaskById taskId)
            (TaskRepoHelper.removeUserFromTask (fromJust mTask) (userId user))))
        (return ()) (belongingTasks user)
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.User ( deleteUserImpl, UserService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)

import           AppContext                   (App)
import           Data.Domain.Types            (EntryId)
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo
import           Data.Service.CalendarEntry   (CalendarEntryService)
import qualified Data.Service.CalendarEntry   as CalendarService
import Data.Foldable (forM_)

deleteUserImpl :: (CalendarEntryService m, MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            User -> m ()
deleteUserImpl user = do
    foldr ((>>) . removeCalendarEntryFromUser)
        (return ()) (ownerOfCalendarEntries user)
    MonadDBUserRepo.deleteUser user

removeCalendarEntryFromUser :: ( CalendarEntryService m, MonadDBCalendarRepo m) => EntryId -> m ()
removeCalendarEntryFromUser entryId = do
    mEntry <- MonadDBCalendarRepo.findCalendarById entryId
    forM_ mEntry CalendarService.removeCalendar

class Monad m => UserService m where
    deleteUser :: User -> m ()

instance (MonadDBUserRepo App, MonadDBCalendarRepo App)
            => UserService App where
    deleteUser = deleteUserImpl

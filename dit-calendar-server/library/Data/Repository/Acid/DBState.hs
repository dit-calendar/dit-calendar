module Data.Repository.Acid.DBState (Acid(..)) where

import           Data.Acid                          (AcidState)
import           Happstack.Authenticate.Core        (AuthenticateState)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.User          as UserAcid


data Acid = Acid
   {
     acidUserListState    :: AcidState UserAcid.UserList
     , acidEntryListState :: AcidState CalendarEntryAcid.EntryList
     , acidTaskListState  :: AcidState TaskAcid.TaskList
     , acidAuthState      :: AcidState AuthenticateState
   }

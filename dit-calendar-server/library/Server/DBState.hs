module Server.DBState (Acid(..)) where

import           Data.Acid                          (AcidState)
import           Happstack.Authenticate.Core        (AuthenticateState)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.TelegramLink  as TelegramAcid
import qualified Data.Repository.Acid.User          as UserAcid


data Acid = Acid
   {
     acidUserListState       :: AcidState UserAcid.UserList
     , acidEntryListState    :: AcidState CalendarEntryAcid.EntryList
     , acidTaskListState     :: AcidState TaskAcid.TaskList
     , acidTelegramLinkState :: AcidState TelegramAcid.TelegramLinkList
     , acidAuthState         :: AcidState AuthenticateState
   }

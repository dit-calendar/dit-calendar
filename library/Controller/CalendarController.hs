module Controller.CalendarController where

import Happstack.Foundation     ( query )

import Data.Domain.CalendarEntry                as CalendarEntry
import Data.Domain.Types           ( EntryId, UserId )
import Controller.AcidHelper       ( CtrlV )
import Controller.ResponseHelper ( onUserExist, onEntryExist, okResponse )

import qualified Data.Repository.Acid.CalendarEntry       as CalendarEntryAcid
import qualified Data.Repository.Acid.User                as UserAcid
import qualified Data.Repository.CalendarRepo             as CalendarRepo
import qualified Data.Service.CalendarEntry               as CalendarService
import qualified Data.Domain.User                         as DomainUser


--handler for entryPage
entryPage :: EntryId -> CtrlV
entryPage i = onEntryExist i (\e -> okResponse $ "peeked at the description and saw: " ++ show e)

createCalendarEntry :: UserId -> String -> String -> DomainUser.User -> CtrlV
createCalendarEntry userId newDate description loggedUser =
    onUserExist userId (\u -> do
            entry <- CalendarService.createEntry newDate description u
            okResponse $ "Add Entry: " ++ show (CalendarEntry.entryId entry) ++ "to User: " ++ show userId)

deleteCalendarEntry :: EntryId -> DomainUser.User -> CtrlV
deleteCalendarEntry i loggedUser =
    onEntryExist i (\e -> do
            CalendarService.removeCalendar e
            okResponse $ "CalendarEntry with id:" ++ show i ++ "deleted")

updateCalendarEntry :: EntryId -> String -> DomainUser.User -> CtrlV
updateCalendarEntry id description loggedUser =
    onEntryExist id (\e -> do
        CalendarRepo.updateDescription e description
        okResponse $ "CalendarEntry with id:" ++ show id ++ "updated")

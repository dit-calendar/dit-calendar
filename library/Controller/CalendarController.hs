module Controller.CalendarController where

import Happstack.Foundation     ( query )

import Data.Domain.CalendarEntry                as CalendarEntry
import Data.Domain.Types           ( EntryId, UserId )
import Controller.AcidHelper       ( CtrlV )
import Controller.ResponseHelper ( userExist, entryExist, okResponse )

import qualified Data.Repository.Acid.CalendarEntry       as CalendarEntryAcid
import qualified Data.Repository.Acid.User                as UserAcid
import qualified Data.Repository.CalendarRepo             as CalendarRepo
import qualified Data.Service.CalendarEntry               as CalendarService


--handler for entryPage
entryPage :: EntryId -> CtrlV
entryPage i = do
    mEntry <- query (CalendarEntryAcid.EntryById i)
    entryExist i (\e -> okResponse $ "peeked at the description and saw: " ++ show e) mEntry

createCalendarEntry :: UserId -> String -> String -> CtrlV
createCalendarEntry userId newDate description = do
    mUser <- query (UserAcid.UserById userId)
    userExist userId (\u -> do
            entry <- CalendarService.createEntry newDate description u
            okResponse $ "Add Entry: " ++ show (CalendarEntry.entryId entry) ++ "to User: " ++ show userId) mUser

deleteCalendarEntry :: EntryId -> CtrlV
deleteCalendarEntry i = do
    mEntry <- query (CalendarEntryAcid.EntryById i)
    entryExist i (\e -> do
            CalendarService.removeCalendar e
            okResponse $ "CalendarEntry with id:" ++ show i ++ "deleted") mEntry

updateCalendarEntry :: EntryId -> String -> CtrlV
updateCalendarEntry id description = do
    mEntry <- query (CalendarEntryAcid.EntryById id)
    entryExist id (\e -> do
        CalendarRepo.updateDescription e description
        okResponse $ "CalendarEntry with id:" ++ show id ++ "updated") mEntry

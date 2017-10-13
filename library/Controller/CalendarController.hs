module Controller.CalendarController where

import Happstack.Foundation     ( query )

import Data.Domain.CalendarEntry                as CalendarEntry
import Data.Domain.Types           ( EntryId, UserId )
import Controller.AcidHelper       ( CtrlV )
import Controller.ControllerHelper ( userExist, entryExist, okResponse )

import qualified Data.Repository.Acid.CalendarAcid        as CalendarAcid
import qualified Data.Repository.Acid.UserAcid            as UserAcid
import qualified Data.Repository.CalendarRepo             as CalendarRepo
import qualified Data.Repository.CalendarRepoHelper       as CalendarRepoHelper


--handler for entryPage
entryPage :: EntryId -> CtrlV
entryPage i = do
    mEntry <- query (CalendarAcid.EntryById i)
    entryExist i (\e -> okResponse $ "peeked at the description and saw: " ++ show e) mEntry

createCalendarEntry :: UserId -> String -> CtrlV
createCalendarEntry userId description = do
    mUser <- query (UserAcid.UserById userId)
    userExist userId (\u -> do
            entry <- CalendarRepoHelper.createEntry description u
            okResponse $ "Add Entry: " ++ show (CalendarEntry.entryId entry) ++ "to User: " ++ show userId) mUser

deleteCalendarEntry :: EntryId -> CtrlV
deleteCalendarEntry i = do
    mEntry <- query (CalendarAcid.EntryById i)
    entryExist i (\e -> do
            CalendarRepoHelper.removeCalendar e
            okResponse $ "CalendarEntry with id:" ++ show i ++ "deleted") mEntry

updateCalendarEntry :: EntryId -> String -> CtrlV
updateCalendarEntry id description = do
    mEntry <- query (CalendarAcid.EntryById id)
    entryExist id (\e -> do
        CalendarRepo.updateDescription e description
        okResponse $ "CalendarEntry with id:" ++ show id ++ "updated") mEntry

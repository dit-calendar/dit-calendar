module Controller.CalendarController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Data.Domain.User                         as User
import Data.Domain.CalendarEntry                as CalendarEntry    ( CalendarEntry(..) )
import Data.Domain.Types         ( EntryId, UserId, TaskId )
import Data.Repository.Acid.CalendarAcid        as CalendarAcid
import Data.Repository.Acid.UserAcid            as UserAcid
import Controller.AcidHelper    ( CtrlV )
import Data.Repository.CalendarRepo             as CalendarRepo

--handler for entryPage
entryPage :: EntryId -> CtrlV
entryPage i =
    do
       mEntry <- query (CalendarAcid.EntryById i)
       case mEntry of
            Nothing ->
                ok $ toResponse $ "Could not find a entry with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the description and saw: " ++ show u

createCalendarEntry :: UserId -> String -> CtrlV
createCalendarEntry userId description =
    do
        mUser <- query (UserAcid.UserById userId)
        case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show userId
            (Just u) ->
                do
                    entry <- CalendarRepo.createEntry description u
                    ok $ toResponse $ "Add Entry: " ++ show (CalendarEntry.entryId entry) ++ "to User: " ++ show userId

deleteCalendarEntry :: EntryId -> CtrlV
deleteCalendarEntry i = do
    mEntry <- query (CalendarAcid.EntryById i)
    case mEntry of
        Nothing ->
            ok $ toResponse $ "Could not find a CalendarEntry with id " ++ show i
        (Just u) -> do
            CalendarRepo.removeCalendar u
            ok $ toResponse $ "CalendarEntry with id:" ++ show i ++ "deleted"

updateCalendarEntry :: EntryId -> String -> CtrlV
updateCalendarEntry id description =
    do
       mEntry <- query (CalendarAcid.EntryById id)
       case mEntry of
            Nothing ->
                ok $ toResponse $ "Could not find a CalendarEntry with id " ++ show id
            (Just c) -> do
                 CalendarRepo.updateCalendar c description
                 ok $ toResponse $ "CalendarEntry with id:" ++ show id ++ "updated"
module Controller.CalendarController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Data.Domain.User                         as User
import Data.Domain.CalendarEntry                as CalendarEntry    ( CalendarEntry(..) )
import Data.Domain.Types         ( EntryId, UserId )
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
createCalendarEntry userId desription =
    do
        mUser <- query (UserAcid.UserById userId)
        case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show userId
            (Just u) ->
                do
                    CalendarRepo.createEntry desription u
                    ok $ toResponse $ "Add Entry: x" ++ "to User: " ++ show userId

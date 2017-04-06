module Controller.CalendarController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Domain.Calendar.CalendarEntry               as CalendarEntry    ( CalendarEntry(..) )
import Repository.Calendar.CalendarEntryRepo       as CalendarEntryRepo
import Controller.AcidHelper    ( CtrlV )

--handler for entryPage
entryPage :: Int -> CtrlV
entryPage i =
    do
       mEntry <- query (CalendarEntryRepo.EntryById i)
       case mEntry of
            Nothing ->
                ok $ toResponse $ "Could not find a entry with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the description and saw: " ++ show u

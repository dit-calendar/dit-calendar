module Controller.CalendarController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Domain.CalendarEntry             as CalendarEntry    ( CalendarEntry(..) )
import Domain.Types             ( EntryId )
import Repository.Acid.CalendarAcid     as CalendarAcid
import Controller.AcidHelper    ( CtrlV )

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

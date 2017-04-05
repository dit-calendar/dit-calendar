module Controller.CalendarController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Domain.Calendar          as Calendar       ( Calendar(..) )
import Repository.CalendarRepo  as CalendarRepo
import Controller.AcidHelper    ( CtrlV )

--handler for calendarPage
calendarPage :: Int -> CtrlV
calendarPage i =
    do
       mCalendar <- query (CalendarRepo.CalendarById i)
       case mCalendar of
            Nothing ->
                ok $ toResponse $ "Could not find a calendar with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the description and saw: " ++ show u

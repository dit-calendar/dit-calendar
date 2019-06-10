module Presentation.Route.CalendarRoute
    ( routeCalendarEntry
    , routeCalendarEntryDetails
    ) where

import           Data.Aeson                                 (eitherDecode)
import           Data.Either

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (Description (..),
                                                             EntryId)
import           AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getBody,
                                                             getHttpMethod)
import           Presentation.ResponseHelper                (badRequest,
                                                             notImplemented)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.TaskController     as TaskController
import qualified Presentation.Dto.CalendarEntry             as CalendarDto
import qualified Presentation.Dto.Task                      as TaskDto


routeCalendarEntry :: App Response
routeCalendarEntry = do
    m <- getHttpMethod
    case m of
        POST -> do
            body <- getBody
            case eitherDecode body :: Either String CalendarDto.CalendarEntry of
                Right newCalendar -> callIfAuthorized (CalendarController.createCalendarEntry newCalendar)
                Left errorMessage -> badRequest errorMessage
        GET -> callIfAuthorized CalendarController.calendarEntries
        other -> notImplemented other


routeCalendarEntryDetails :: EntryId -> App Response
routeCalendarEntryDetails entryId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (CalendarController.deleteCalendarEntry entryId)
        --  https://localhost:8443/calendarentries/1
        GET -> CalendarController.entryPage entryId
        PUT -> do
            body <- getBody
            case eitherDecode body :: Either String CalendarDto.CalendarEntry of
                  Right calendarDto ->
                        callIfAuthorized (CalendarController.updateCalendarEntry entryId calendarDto)
                  Left errorMessage -> badRequest errorMessage
        other -> notImplemented other
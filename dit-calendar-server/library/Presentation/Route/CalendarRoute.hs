module Presentation.Route.CalendarRoute
    ( routeCalendarEntry
    , routeCalendarEntryDetails
    , routeCalendarFilter
    , routeCalendarTelegramLinks
    ) where

import           Data.Aeson                                     (eitherDecode)
import           Data.Either

import           Happstack.Server                               (Method (DELETE, GET, POST, PUT),
                                                                 Response)

import           AppContext                                     (App)
import           Auth.Authorization                             (callIfAuthorized)
import           Data.Domain.Types                              (EntryId)
import           Presentation.Dto.CalendarEntry                 (validate)
import           Server.HttpServerHelper                        (getBody,
                                                                 getHttpMethod)
import           Server.ResponseBuilder                         (badRequest,
                                                                 handleResponse,
                                                                 notImplemented)

import qualified Presentation.Controller.CalendarController     as CalendarController
import qualified Presentation.Controller.TelegramLinkController as TelegramLinkController
import qualified Presentation.Dto.CalendarEntry                 as CalendarDto
import qualified Presentation.Dto.CalendarFilter                as FilterDto


routeCalendarEntry :: App Response
routeCalendarEntry = do
    m <- getHttpMethod
    case m of
        POST -> do
            body <- getBody
            case validate (eitherDecode body :: Either String CalendarDto.CalendarEntry) of
                Right newCalendar -> callIfAuthorized (CalendarController.createCalendarEntry newCalendar)
                Left errorMessage -> badRequest errorMessage
        GET -> callIfAuthorized CalendarController.calendarEntries
        other -> notImplemented other

routeCalendarFilter :: App Response
routeCalendarFilter = do
    m <- getHttpMethod
    case m of
        POST   -> do
            body <- getBody
            case eitherDecode body :: Either String FilterDto.CalendarFilter of
                Right filter -> callIfAuthorized (CalendarController.calendarEntriesFilter filter)
                Left errorMessage -> badRequest errorMessage
        other -> notImplemented other

routeCalendarEntryDetails :: EntryId -> App Response
routeCalendarEntryDetails entryId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (CalendarController.deleteCalendarEntry entryId)
        --  https://localhost:8443/calendarentries/1
        GET -> callIfAuthorized (CalendarController.entryPage entryId)
        PUT -> do
            body <- getBody
            case validate (eitherDecode body :: Either String CalendarDto.CalendarEntry) of
                  Right calendarDto ->
                        callIfAuthorized (CalendarController.updateCalendarEntry entryId calendarDto)
                  Left errorMessage -> badRequest errorMessage
        other -> notImplemented other

routeCalendarTelegramLinks :: EntryId -> App Response
routeCalendarTelegramLinks entryId = do
    m <- getHttpMethod
    case m of
        GET    -> callIfAuthorized (TelegramLinkController.calendarTasksTelegramLinks entryId)
        other  -> notImplemented other

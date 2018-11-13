module Presentation.Route.CalendarRoute
    ( routeCalendarEntry
    ) where

import           Data.Aeson                                 (eitherDecode, decode)
import           Data.Either

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (Description (..),
                                                             EntryId)
import           Presentation.AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getBody,
                                                             getHttpMethod)
import           Presentation.ResponseHelper                (badRequest)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.TaskController     as TaskController
import qualified Presentation.Dto.CalendarEntry             as CalendarDto
import qualified Presentation.Dto.Task                      as TaskDto

routeCalendarEntry :: EntryId -> App Response
routeCalendarEntry entryId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (CalendarController.deleteCalendarEntry entryId)
        --  https://localhost:8443/calendarentry/1
        GET -> CalendarController.entryPage entryId
        POST -> do
            body <- getBody
            case eitherDecode body :: Either String TaskDto.Task of
                 Right taskDto ->
                      TaskController.createTask entryId taskDto
                 Left errorMessage -> badRequest errorMessage
        PUT -> do
            body <- getBody
            case decode body :: Maybe CalendarDto.CalendarEntry of
                  Just calendarDto ->
                        callIfAuthorized (CalendarController.updateCalendarEntry calendarDto)
                  Nothing -> badRequest "Could not parse"

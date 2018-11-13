module Presentation.Route.CalendarRoute
    ( routeCalendarEntry
    ) where

import           Data.Aeson                                 (decode)
import           Data.Text                                  (pack)

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (Description (..),
                                                             EntryId)
import           Presentation.AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getBody,
                                                             getHttpMethod)
import           Presentation.ResponseHelper                (badResponse)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.TaskController     as TaskController
import qualified Presentation.Dto.CalendarEntry             as CalendarDto

routeCalendarEntry :: EntryId -> App Response
routeCalendarEntry entryId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (CalendarController.deleteCalendarEntry entryId)
        --  https://localhost:8443/calendarentry/1
        GET -> CalendarController.entryPage entryId
        POST -> do
            description <- look "description"
            TaskController.createTask entryId $ pack description
        PUT -> do
            body <- getBody
            case decode body :: Maybe CalendarDto.CalendarEntry of
                  Just calendarDto ->
                        callIfAuthorized (CalendarController.updateCalendarEntry calendarDto)
                  Nothing -> badResponse "Could not parse"

module Presentation.Route.CalendarRoute
    ( routeCalendarEntry
    ) where

import           Data.Text                                  (pack)

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             Response, look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (Description (..),
                                                             EntryId)
import           Presentation.AcidHelper                    (App)
import           Presentation.HttpServerHelper              (getHttpMethod)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.TaskController     as TaskController

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
            description <- look "description"
            callIfAuthorized (CalendarController.updateCalendarEntry entryId $ pack description)

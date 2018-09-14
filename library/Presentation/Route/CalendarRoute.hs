module Presentation.Route.CalendarRoute (routeCalendarEntry) where

import           Happstack.Server                           (Method (DELETE, GET, POST, PUT),
                                                             look)

import           Auth.Authorization                         (callIfAuthorized)
import           Data.Domain.Types                          (EntryId)
import           Happstack.Foundation                       (lift)
import           Presentation.AcidHelper                    (CtrlV)
import           Presentation.HttpServerHelper              (getHttpMethod)

import qualified Presentation.Controller.CalendarController as CalendarController
import qualified Presentation.Controller.TaskController     as TaskController

routeCalendarEntry :: EntryId -> CtrlV
routeCalendarEntry entryId = do
  m <- getHttpMethod
  case m of
    DELETE ->
      lift $ callIfAuthorized (CalendarController.deleteCalendarEntry entryId)
    GET -> lift $ CalendarController.entryPage entryId
    POST -> do
      description <- look "description"
      lift $ TaskController.createTask entryId description
    PUT -> do
      description <- look "description"
      lift $ callIfAuthorized (CalendarController.updateCalendarEntry entryId description)

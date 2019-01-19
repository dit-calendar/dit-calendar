module Presentation.Controller.CalendarController where

import           Data.Aeson                     (encode)
import           Data.Maybe                     (fromJust)
import           Happstack.Server               (Response)

import           Data.Domain.CalendarEntry      as CalendarEntry
import           Data.Domain.Types              (Description, EntryId, UserId)
import           AcidHelper        (App)
import           Presentation.ResponseHelper    (okResponse, okResponseJson,
                                                 onEntryExist, onUserExist,
                                                 preconditionFailedResponse)

import qualified Data.Domain.CalendarEntry      as DomainCalendar
import qualified Data.Domain.User               as DomainUser
import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Service.CalendarEntry     as CalendarService
import qualified Presentation.Dto.CalendarEntry as CalendarDto

--handler for entryPage
entryPage :: EntryId -> App Response
entryPage i = onEntryExist i (okResponseJson . encode . CalendarDto.transformToDto)

createCalendarEntry :: CalendarDto.CalendarEntry -> DomainUser.User -> App Response
createCalendarEntry calendarDto loggedUser = onUserExist userId createCalendar
    where
        createCalendar user = do
            entry <- CalendarService.createEntry calendarDto user
            okResponseJson $ encode $ CalendarDto.transformToDto entry
        userId = DomainUser.userId loggedUser

deleteCalendarEntry :: EntryId -> DomainUser.User -> App Response
deleteCalendarEntry i loggedUser = onEntryExist i deleteCalendar
    where
        deleteCalendar cEntry = do
            CalendarService.removeCalendar cEntry
            okResponse $ "CalendarEntry with id:" ++ show i ++ "deleted"

updateCalendarEntry :: EntryId -> CalendarDto.CalendarEntry -> DomainUser.User -> App Response
updateCalendarEntry entryId calendarDto loggedUser = onEntryExist entryId updateCalendar
    where
        updateCalendar cEntry = do
            --TODO überprüfe welche werte gesetzt sind und update nur diese, momentan wird nur description aktualisiert
            result <- CalendarRepo.updateCalendar cEntry {CalendarEntry.description = fromJust $ CalendarDto.description calendarDto}
            case result of
                Left errorMessage -> preconditionFailedResponse errorMessage
                Right updatedEntry -> okResponseJson $ encode $ CalendarDto.transformToDto updatedEntry

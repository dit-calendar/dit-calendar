module Presentation.Controller.CalendarController where

import           Data.Aeson                     (encode)
import           Data.Maybe                     (fromJust)
import           Happstack.Server               (Response)

import           Data.Domain.CalendarEntry      as CalendarEntry
import           Data.Domain.Types              (Description, EntryId, UserId)
import           Presentation.AcidHelper        (App)
import           Presentation.ResponseHelper    (okResponse, okResponseJson,
                                                 onEntryExist, onUserExist)

import qualified Data.Domain.User               as DomainUser
import qualified Data.Domain.CalendarEntry      as DomainCalendar
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

updateCalendarEntry :: CalendarDto.CalendarEntry -> DomainUser.User -> App Response
updateCalendarEntry calendarDto loggedUser = onEntryExist entryId updateCalendar --TODO onEntryExist sollte mit dto arbeiten
    where
        entryId = fromJust $ CalendarDto.entryId calendarDto
        updateCalendar cEntry = do
            --TODO überprüfe welche werte gesetzt sind und update nur diese
            CalendarRepo.updateDescription cEntry (fromJust $ CalendarDto.description calendarDto)
            okResponse $ "CalendarEntry with id:" ++ show (DomainCalendar.entryId cEntry) ++ "updated"

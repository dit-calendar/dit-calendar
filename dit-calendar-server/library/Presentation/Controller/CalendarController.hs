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
import Presentation.Mapper.CalendarEntryMapper (transformToDto, transformFromDto)

import qualified Data.Domain.CalendarEntry      as DomainCalendar
import qualified Data.Domain.User               as DomainUser
import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Service.CalendarEntry     as CalendarService
import qualified Presentation.Dto.CalendarEntry as CalendarDto

--handler for entryPage
entryPage :: EntryId -> App Response
entryPage i = onEntryExist i (okResponseJson . encode . transformToDto)

createCalendarEntry :: CalendarDto.CalendarEntry -> DomainUser.User -> App Response
createCalendarEntry calendarDto loggedUser = onUserExist userId createCalendar
    where
        newCalendar = transformFromDto calendarDto Nothing
        createCalendar user = do
            entry <- CalendarService.createEntry newCalendar user
            okResponseJson $ encode $ transformToDto entry
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
            result <- CalendarRepo.updateCalendar (transformFromDto calendarDto $ Just cEntry)
            case result of
                Left errorMessage -> preconditionFailedResponse errorMessage
                Right updatedEntry -> okResponseJson $ encode $ transformToDto updatedEntry

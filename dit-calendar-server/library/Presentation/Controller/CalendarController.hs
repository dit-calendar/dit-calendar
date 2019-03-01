module Presentation.Controller.CalendarController where

import           Data.Aeson                              (encode)
import           Data.Maybe                              (fromJust)
import           Happstack.Server                        (Response)

import           AcidHelper                              (App)
import           Data.Domain.CalendarEntry               as CalendarEntry
import           Data.Domain.Types                       (Description, EntryId,
                                                          UserId)
import           Presentation.Mapper.CalendarEntryMapper (transformFromDto,
                                                          transformToDto)
import           Presentation.ResponseHelper             (okResponse,
                                                          okResponseJson,
                                                          onEntryExist,
                                                          onUserExist,
                                                          preconditionFailedResponse)

import qualified Data.Domain.User                        as DomainUser
import qualified Data.Repository.CalendarRepo            as CalendarRepo
import qualified Data.Service.CalendarEntry              as CalendarService
import qualified Presentation.Dto.CalendarEntry          as CalendarDto

--handler for entryPage
entryPage :: EntryId -> App Response
entryPage i = onEntryExist i (return . Left . transformToDto)

createCalendarEntry :: CalendarDto.CalendarEntry -> DomainUser.User -> App Response
createCalendarEntry calendarDto loggedUser = onUserExist userId createCalendar
    where
        newCalendar = transformFromDto calendarDto Nothing
        createCalendar user = do
            entry <- CalendarService.createEntry newCalendar user
            return $ Left $ transformToDto entry
        userId = DomainUser.userId loggedUser

deleteCalendarEntry :: EntryId -> DomainUser.User -> App Response
deleteCalendarEntry i loggedUser = onEntryExist i deleteCalendar
    where
        deleteCalendar cEntry = do
            CalendarService.removeCalendar cEntry
            return $ Left (""::String)

updateCalendarEntry :: EntryId -> CalendarDto.CalendarEntry -> DomainUser.User -> App Response
updateCalendarEntry entryId calendarDto loggedUser = onEntryExist entryId updateCalendar
    where
        updateCalendar cEntry = do
            result <- CalendarRepo.updateCalendar (transformFromDto calendarDto $ Just cEntry)
            return $ either Right (Left . transformToDto) result

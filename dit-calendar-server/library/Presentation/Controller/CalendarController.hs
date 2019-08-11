module Presentation.Controller.CalendarController where

import           Data.Maybe                              (fromJust)

import           AcidHelper                              (App)
import           Data.Domain.Types                       (EitherResult, EntryId,
                                                          UserId)
import           Presentation.Mapper.BaseMapper          (transformToDtoE,
                                                          transformToDtoList)
import           Presentation.Mapper.CalendarEntryMapper (transformFromDto,
                                                          transformToDto)
import           Presentation.ResponseHelper             (onEntryExist,
                                                          onUserExist)

import qualified Data.Domain.User                        as DomainUser
import qualified Data.Repository.CalendarRepo            as CalendarRepo
import qualified Data.Service.CalendarEntry              as CalendarService
import qualified Presentation.Dto.CalendarEntry          as CalendarDto

calendarEntries :: DomainUser.User -> App (EitherResult [CalendarDto.CalendarEntry])
calendarEntries user = do
    result <- CalendarRepo.findAllCalendarEntries user
    return (Right $ transformToDtoList result)


entryPage :: EntryId -> App (EitherResult CalendarDto.CalendarEntry)
entryPage i = onEntryExist i (return . Right . transformToDto)

createCalendarEntry :: CalendarDto.CalendarEntry -> DomainUser.User -> App (EitherResult CalendarDto.CalendarEntry)
createCalendarEntry calendarDto loggedUser = onUserExist userId createCalendar
    where
        newCalendar = transformFromDto calendarDto Nothing
        createCalendar user = do
            entry <- CalendarService.createEntry newCalendar user
            return $ Right $ transformToDto entry
        userId = DomainUser.userId loggedUser

deleteCalendarEntry :: EntryId -> DomainUser.User -> App (EitherResult ())
deleteCalendarEntry i loggedUser = onEntryExist i deleteCalendar
    where
        deleteCalendar cEntry = do
            CalendarService.removeCalendar cEntry
            return $ Right ()

updateCalendarEntry :: EntryId -> CalendarDto.CalendarEntry -> DomainUser.User -> App (EitherResult CalendarDto.CalendarEntry)
updateCalendarEntry entryId calendarDto loggedUser = onEntryExist entryId updateCalendar
  where
    updateCalendar cEntry = do
        result <- CalendarRepo.updateCalendar (transformFromDto calendarDto $ Just cEntry)
        return $ transformToDtoE result

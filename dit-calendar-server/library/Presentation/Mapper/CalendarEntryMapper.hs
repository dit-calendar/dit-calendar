module Presentation.Mapper.CalendarEntryMapper
    ( transformToDto
    , transformFromDto
    ) where

import           Data.Default
import           Data.Generics.Aliases          (orElse)
import           Data.Maybe                     (fromJust, fromMaybe)

import qualified Data.Domain.CalendarEntry      as Domain
import           Presentation.Dto.CalendarEntry

transformToDto :: Domain.CalendarEntry -> CalendarEntry
transformToDto domain =
    CalendarEntry
        { description = Just (Domain.description domain)
        , entryId = Just (Domain.entryId domain)
        , version = Just $ Domain.version domain
        , userId = Domain.userId domain
        , tasks = Just (Domain.tasks domain)
        , date = Domain.date domain
        }

transformFromDto :: CalendarEntry -> Maybe Domain.CalendarEntry -> Domain.CalendarEntry
transformFromDto dto mDbCalendar =
    case mDbCalendar of
        Nothing ->
            def
                { Domain.entryId = 0
                , Domain.version = 0
                , Domain.description = fromJust (description dto)
                , Domain.userId = userId dto
                , Domain.tasks = fromMaybe [] (tasks dto)
                , Domain.date = date dto
                }
        Just dbCalendar ->
            def
                { Domain.description = fromMaybe (Domain.description dbCalendar) (description dto)
                , Domain.entryId = fromJust (entryId dto)
                , Domain.version = fromJust $ version dto
                , Domain.userId = userId dto
                , Domain.tasks = fromMaybe (Domain.tasks dbCalendar) (tasks dto)
                , Domain.date = date dto
                }

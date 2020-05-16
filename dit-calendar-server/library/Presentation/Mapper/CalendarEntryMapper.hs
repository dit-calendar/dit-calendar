{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.CalendarEntryMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Maybe                     (fromMaybe)

import qualified Data.Domain.CalendarEntry      as Domain
import           Presentation.Dto.CalendarEntry
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.CalendarEntry CalendarEntry where
    transformToDto domain =
        CalendarEntry
            { title       = Domain.title domain
            , description = Domain.description domain
            , entryId = Just (Domain.entryId domain)
            , version = Just $ Domain.version domain
            , startDate = Domain.startDate domain
            , endDate = Domain.endDate domain
            }

    transformFromDto dto mDbCalendar =
        case mDbCalendar of
            Nothing ->
                def
                    { Domain.entryId = 0
                    , Domain.version = 0
                    , Domain.title = title dto
                    , Domain.description = description dto
                    , Domain.tasks = []
                    , Domain.startDate = startDate dto
                    , Domain.endDate = endDate dto
                    }
            Just dbCalendar ->
                    Domain.CalendarEntry
                    { Domain.title  = title dto
                    , Domain.description = description dto
                    , Domain.entryId = Domain.entryId dbCalendar
                    , Domain.version = fromMaybe (-1) (version dto)
                    , Domain.owner = Domain.owner dbCalendar
                    , Domain.tasks = Domain.tasks dbCalendar
                    , Domain.startDate = startDate dto
                    , Domain.endDate = endDate dto
                    }
